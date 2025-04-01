import logging
from random import randint
from time import time
from typing import cast

from geniusweb.actions.Accept import Accept
from geniusweb.actions.Action import Action
from geniusweb.actions.Offer import Offer
from geniusweb.actions.PartyId import PartyId
from geniusweb.bidspace.AllBidsList import AllBidsList
from geniusweb.inform.ActionDone import ActionDone
from geniusweb.inform.Finished import Finished
from geniusweb.inform.Inform import Inform
from geniusweb.inform.Settings import Settings
from geniusweb.inform.YourTurn import YourTurn
from geniusweb.issuevalue.Bid import Bid
from geniusweb.issuevalue.Domain import Domain
from geniusweb.party.Capabilities import Capabilities
from geniusweb.party.DefaultParty import DefaultParty
from geniusweb.profile.utilityspace.LinearAdditiveUtilitySpace import (
    LinearAdditiveUtilitySpace,
)
from geniusweb.profileconnection.ProfileConnectionFactory import (
    ProfileConnectionFactory,
)
from geniusweb.progress.ProgressTime import ProgressTime
from geniusweb.references.Parameters import Parameters
from tudelft_utilities_logging.ReportToLogger import ReportToLogger

from .utils.opponent_model import OpponentModel


class TemplateAgent(DefaultParty):
    """
    Template of a Python geniusweb agent.
    """

    FORTUNATE = "fortunate"
    SELFISH = "selfish"
    CONCESSION = "concession"
    UNFORTUNATE = "unfortunate"
    NICE = "nice"
    SILENT = "silent"

    def __init__(self):
        super().__init__()
        self.logger: ReportToLogger = self.getReporter()

        self.domain: Domain = None
        self.parameters: Parameters = None
        self.profile: LinearAdditiveUtilitySpace = None
        self.progress: ProgressTime = None
        self.me: PartyId = None
        self.other: str = None
        self.settings: Settings = None
        self.storage_dir: str = None

        self.last_received_bid: Bid = None
        self.opponent_model: OpponentModel = None

        self.received_bids = []
        self.dans_categories = []
        self.dans_stats = {
            self.FORTUNATE: 0.0,
            self.SELFISH: 0.0,
            self.CONCESSION: 0.0,
            self.UNFORTUNATE: 0.0,
            self.NICE: 0.0,
            self.SILENT: 0.0
        }
        self.gamma = 0.002
        self.concession_rate = 0.0
        self.utility_changes = []
        self.avg_utility_change = 0.0
        self.std_utility_change = 0.0
        self.logger.log(logging.INFO, "party is initialized")

    def notifyChange(self, data: Inform):
        """MUST BE IMPLEMENTED
        This is the entry point of all interaction with your agent after is has been initialised.
        How to handle the received data is based on its class type.

        Args:
            info (Inform): Contains either a request for action or information.
        """

        # a Settings message is the first message that will be send to your
        # agent containing all the information about the negotiation session.
        if isinstance(data, Settings):
            self.settings = cast(Settings, data)
            self.me = self.settings.getID()

            # progress towards the deadline has to be tracked manually through the use of the Progress object
            self.progress = self.settings.getProgress()

            self.parameters = self.settings.getParameters()
            self.storage_dir = self.parameters.get("storage_dir")

            # the profile contains the preferences of the agent over the domain
            profile_connection = ProfileConnectionFactory.create(
                data.getProfile().getURI(), self.getReporter()
            )
            self.profile = profile_connection.getProfile()
            self.domain = self.profile.getDomain()
            profile_connection.close()

        # ActionDone informs you of an action (an offer or an accept)
        # that is performed by one of the agents (including yourself).
        elif isinstance(data, ActionDone):
            action = cast(ActionDone, data).getAction()
            actor = action.getActor()

            # ignore action if it is our action
            if actor != self.me:
                # obtain the name of the opponent, cutting of the position ID.
                self.other = str(actor).rsplit("_", 1)[0]

                # process action done by opponent
                self.opponent_action(action)
        # YourTurn notifies you that it is your turn to act
        elif isinstance(data, YourTurn):
            # execute a turn
            self.my_turn()

        # Finished will be send if the negotiation has ended (through agreement or deadline)
        elif isinstance(data, Finished):
            self.save_data()
            # terminate the agent MUST BE CALLED
            self.logger.log(logging.INFO, "party is terminating:")
            super().terminate()
        else:
            self.logger.log(logging.WARNING, "Ignoring unknown info " + str(data))

    def getCapabilities(self) -> Capabilities:
        """MUST BE IMPLEMENTED
        Method to indicate to the protocol what the capabilities of this agent are.
        Leave it as is for the ANL 2022 competition

        Returns:
            Capabilities: Capabilities representation class
        """
        return Capabilities(
            set(["SAOP"]),
            set(["geniusweb.profile.utilityspace.LinearAdditive"]),
        )

    def send_action(self, action: Action):
        """Sends an action to the opponent(s)

        Args:
            action (Action): action of this agent
        """
        self.getConnection().send(action)

    # give a description of your agent
    def getDescription(self) -> str:
        """MUST BE IMPLEMENTED
        Returns a description of your agent. 1 or 2 sentences.

        Returns:
            str: Agent description
        """
        return "Template agent for the ANL 2022 competition"

    def opponent_action(self, action):
        """Process an action that was received from the opponent.

        Args:
            action (Action): action of opponent
        """
        # if it is an offer, set the last received bid
        if isinstance(action, Offer):
            # create opponent model if it was not yet initialised
            if self.opponent_model is None:
                self.opponent_model = OpponentModel(self.domain)

            bid = cast(Offer, action).getBid()

            self.received_bids.append(bid)

            # update opponent model with bid
            self.opponent_model.update(bid)

            if len(self.received_bids) >= 2:
                self.analyze_dans()

            # set bid as last received
            self.last_received_bid = bid

    def analyze_dans(self):
        current_bid = self.received_bids[-1]
        previous_bid = self.received_bids[-2]

        # Utility change for our own agent
        delta_m = self.profile.getUtility(current_bid) - self.profile.getUtility(previous_bid)
        # Utility change for opponent model
        delta_o = self.opponent_model.get_predicted_utility(current_bid) - self.opponent_model.get_predicted_utility(previous_bid)

        self.utility_changes.append(delta_o)

        if len(self.utility_changes) > 0:
            self.avg_utility_change = sum(self.utility_changes) / len(self.utility_changes)

        if len(self.utility_changes) >= 2:
            variance = sum((x - self.avg_utility_change) ** 2 for x in self.utility_changes) / len(self.utility_changes)
            self.std_utility_change = variance ** 0.5

        category = self.categorize_dans(delta_m, delta_o)
        self.dans_categories.append(category)

        for x in self.dans_stats:
            self.dans_stats[x] = self.dans_categories.count(x) / len(self.dans_categories)

        if len(self.dans_categories) > 0:
            selfish_count = self.dans_categories.count(self.SELFISH)
            self.concession_rate = 1.0 - (selfish_count / len(self.dans_categories))


    def categorize_dans(self, delta_m, delta_o):
        gamma = self.gamma

        if delta_o > gamma and delta_m > gamma:
            return self.FORTUNATE
        elif delta_o > gamma and delta_m < -gamma:
            return self.SELFISH
        elif delta_o < -gamma and delta_m > gamma:
            return self.CONCESSION
        elif delta_o < -gamma and delta_m < -gamma:
            return self.UNFORTUNATE
        elif -gamma <= delta_o <= gamma and delta_m > gamma:
            return self.NICE
        else:
            return self.SILENT

    def get_most_common_dans_category(self):
        if not self.dans_categories:
            return self.SILENT # just a default for when there is no data
        max_count = 0
        max_category = self.SILENT

        for category in self.dans_stats:
            count = self.dans_categories.count(category)
            if count > max_count:
                max_count = count
                max_category = category
        return max_category


    def my_turn(self):
        """This method is called when it is our turn. It should decide upon an action
        to perform and send this action to the opponent.
        """
        # check if the last received offer is good enough
        if self.accept_condition(self.last_received_bid):
            # if so, accept the offer
            action = Accept(self.me, self.last_received_bid)
        else:
            # if not, find a bid to propose as counter offer
            bid = self.find_bid()
            action = Offer(self.me, bid)

        # send the action
        self.send_action(action)

    def save_data(self):
        """This method is called after the negotiation is finished. It can be used to store data
        for learning capabilities. Note that no extensive calculations can be done within this method.
        Taking too much time might result in your agent being killed, so use it for storage only.
        """
        data = "Data for learning (see README.md)"

        if self.opponent_model is None or self.other is None or len(self.received_bids) == 0:
            return


        data = f"# Opponent Model Data for {self.other}\n\n"

        data += "## DANS Analysis\n\n"
        data += f"Most common DANS category: {self.get_most_common_dans_category()}\n"
        for category, value in self.dans_stats.items():
            data += f"- {category}: {value:.4f}\n"

        data += "\n## Concession Data\n\n"
        data += f"Concession rate: {self.concession_rate:.4f}\n"
        data += f"Average utility change: {self.avg_utility_change:.4f}\n"
        data += f"Standard deviation of utility changes: {self.std_utility_change:.4f}\n"

        data += "\n## Estimated Issue Weights\n\n"
        for issue_id, estimator in self.opponent_model.issue_estimators.items():
            data += f"- {issue_id}: {estimator.weight:.4f}\n"

        with open(f"{self.storage_dir}/{self.other}_data.md", "w") as f:
            f.write(data)

        self.logger.log(logging.INFO, f"Saved opponent data for {self.other}")


    # returns the recent DANS type (analyzes the behavior in the last few bids, currently set to 5)
    def get_current_dans_type(self, window_size: int = 5) -> str:

        if not self.dans_categories:
            return self.SILENT

        recent_categories = self.dans_categories[-window_size:]

        temp_counts = {
            self.FORTUNATE: 0,
            self.SELFISH: 0,
            self.CONCESSION: 0,
            self.UNFORTUNATE: 0,
            self.NICE: 0,
            self.SILENT: 0
        }

        for cat in recent_categories:
            temp_counts[cat] += 1

        current_category = max(temp_counts, key=temp_counts.get)
        return current_category

    ###########################################################################################
    ################################## Example methods below ##################################
    ###########################################################################################

    def accept_condition(self, bid: Bid) -> bool:
        if bid is None:
            return False

        # progress of the negotiation session between 0 and 1 (1 is deadline)
        progress = self.progress.get(time() * 1000)

        # very basic approach that accepts if the offer is valued above 0.7 and
        # 95% of the time towards the deadline has passed
        conditions = [
            self.profile.getUtility(bid) > 0.8,
            progress > 0.95,
        ]
        return all(conditions)

    # dynamically calculates the threshold of the bid, so minimum utility that we will accept for a bid (used in find bid)
    def get_minimum_utility_threshold(self) -> float:
        progress = self.progress.get(time() * 1000)

        U_max = 0.95
        U_min = 0.70

        # b controls the function shape (so if we concede early or only at the end)
        # b > 1 -> concede fast, b < 1 concede slowly, b = 1 -> linear model
        # change dynamically from based on time prpgress
        b_early = 0.5
        b_late = 2.0
        b = b_early + (b_late - b_early) * progress

        # time-based function used as baseline (based on Faratin et al 1998)
        baseline_threshold = U_min + (U_max - U_min) * (1.0 - progress ** (1.0 / b))

        # check opponents most recent behavior
        recent_dans = self.get_current_dans_type(window_size=5)

        # adjust the threshold based on the opponents recent behavior (it can increase or decrease)
        offset_map = {
            self.FORTUNATE: 0.01,
            self.SELFISH: 0.05,
            self.CONCESSION: 0.02,
            self.UNFORTUNATE: -0.02,
            self.NICE: 0.01,
            self.SILENT: 0.0
        }

        offset = offset_map.get(recent_dans, 0.0)

        threshold = baseline_threshold * (1 + offset)

        final_threshold = max(0.0, min(threshold, 1.0))

        return final_threshold


    def find_bid(self) -> Bid:
        # compose a list of all possible bids
        domain = self.profile.getDomain()
        all_bids = AllBidsList(domain)

        best_bid_score = 0.0
        best_bid = None

        best_fallback_bid = None
        best_fallback_score = 0.0

        alpha, eps = self.compute_dynamic_parameters()

        sample_size = min(all_bids.size(), 500)

        for _ in range(sample_size):
            bid = all_bids.get(randint(0, all_bids.size() - 1))

            bid_score = self.score_bid(bid, alpha, eps)

            if bid_score > best_fallback_score:
                best_fallback_bid, best_fallback_score = bid, bid_score

            our_util = self.profile.getUtility(bid)

            if our_util >= self.get_minimum_utility_threshold():
                if bid_score > best_bid_score:
                    best_bid_score, best_bid = bid_score, bid



        if best_bid is None:
            return best_fallback_bid

        return best_bid

    def score_bid(self, bid: Bid, alpha: float = 0.95, eps: float = 0.1) -> float:
        """Calculate heuristic score for a bid

        Args:
            bid (Bid): Bid to score
            alpha (float, optional): Trade-off factor between self interested and
                altruistic behaviour. Defaults to 0.95.
            eps (float, optional): Time pressure factor, balances between conceding
                and Boulware behaviour over time. Defaults to 0.1.

        Returns:
            float: score
        """
        progress = self.progress.get(time() * 1000)

        our_utility = float(self.profile.getUtility(bid))

        time_pressure = 1.0 - progress ** (1 / eps)
        score = alpha * time_pressure * our_utility

        if self.opponent_model is not None:
            opponent_utility = self.opponent_model.get_predicted_utility(bid)
            opponent_score = (1.0 - alpha * time_pressure) * opponent_utility
            score += opponent_score

        return score

    # dynamically change eps and alpha
    def compute_dynamic_parameters(self):
        progress = self.progress.get(time() * 1000)

        recent_categories = self.dans_categories[-5:]

        if not recent_categories:
            return (0.95, 0.2)

        freq_map = {
            self.FORTUNATE: 0.0,
            self.SELFISH: 0.0,
            self.CONCESSION: 0.0,
            self.UNFORTUNATE: 0.0,
            self.NICE: 0.0,
            self.SILENT: 0.0
        }

        for cat in recent_categories:
            freq_map[cat] += 1

        for k in freq_map:
            freq_map[k] /= len(recent_categories)

        sorted_dans = sorted(freq_map.items(), key=lambda x: x[1], reverse=True)
        top1, top2 = sorted_dans[0][0], sorted_dans[1][0]
        w1, w2 = sorted_dans[0][1], sorted_dans[1][1]

        def get_params(dans_type):
            table = {
                self.SELFISH: (1.00, 0.01),
                self.CONCESSION: (0.92, 0.25),
                self.FORTUNATE: (0.90, 0.5),
                self.UNFORTUNATE: (1.00, 0.01),
                self.NICE: (0.85, 0.4),
                self.SILENT: (0.95, 0.2),
            }
            return table.get(dans_type, (0.95, 0.2))

        a1, e1 = get_params(top1)
        a2, e2 = get_params(top2)

        alpha_base = (w1 * a1 + w2 * a2) / (w1 + w2 + 1e-6)
        eps_base = (w1 * e1 + w2 * e2) / (w1 + w2 + 1e-6)

        alpha = alpha_base * progress + (1.0 - progress) * 1.0
        eps = eps_base * progress + (1.0 - progress) * 0.01

        alpha = max(0.85, min(alpha, 1.0))
        eps = max(0.01, min(eps, 0.5))

        return alpha, eps
