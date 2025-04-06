import json
import time
from pathlib import Path
import numpy as np
from scipy.stats import norm
from scipy.optimize import minimize
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import Matern
from utils.runners import run_session
import plotly.graph_objects as go
import pandas as pd
import random


# bounds for all parameters
param_bounds_dict = {
    "t": (0.85, 0.9),
    "t_prime": (0.98, 0.999),
    "final_utility": (0.6, 0.9),
    "U_max": (0.9, 1.0),
    "U_min": (0.6, 0.85),
    "b_early": (0.1, 1.0),
    "b_late": (1.0, 3.0),
    "FORTUNATE_offset": (-0.02, 0.05),
    "SELFISH_offset": (0.01, 0.1),
    "CONCESSION_offset": (0.01, 0.1),
    "UNFORTUNATE_offset": (-0.05, 0.05),
    "NICE_offset": (-0.02, 0.05),
    "SILENT_offset": (-0.02, 0.05),
    "default_alpha": (0.85, 1.0),
    "default_eps": (0.01, 0.5),
    "FORTUNATE_params_alpha": (0.8, 1.0),
    "SELFISH_params_alpha": (0.85, 1.0),
    "CONCESSION_params_alpha": (0.8, 1.0),
    "UNFORTUNATE_params_alpha": (0.85, 1.0),
    "NICE_params_alpha": (0.8, 1.0),
    "SILENT_params_alpha": (0.8, 1.0),
    "FORTUNATE_params_eps": (0.1, 0.5),
    "SELFISH_params_eps": (0.01, 0.2),
    "CONCESSION_params_eps": (0.1, 0.5),
    "UNFORTUNATE_params_eps": (0.01, 0.3),
    "NICE_params_eps": (0.1, 0.4),
    "SILENT_params_eps": (0.1, 0.4),
}

# for dict use later
param_names = list(param_bounds_dict.keys())
param_bounds = list(param_bounds_dict.values())

# for saving results
RESULTS_DIR = Path("results", time.strftime('%Y%m%d-%H%M%S'))
RESULTS_DIR.mkdir(parents=True, exist_ok=True)

def run_single_session(param_values, profile, opponent):
    params = {k: float(v) for k, v in zip(param_names, param_values)}

    settings = {
        "agents": [
            {
                "class": "agents.group33_agent.group33_agent.TemplateAgent",
                "parameters": {
                    "storage_dir": "agent_storage/TemplateAgent",
                    **params,
                },
            },
            {
                "class": opponent,
            },
        ],
        "profiles": profile,
        "deadline_time_ms": 10000,
    }

    try:
        trace, summary = run_session(settings)
    except Exception as e:
        print(f"Run failed with {opponent} on {profile}: {e}")
        return 0.0, {}, {}

    for key, value in summary.items():
        if key.startswith("agent_") and "TemplateAgent" in value:
            idx = key.split("_")[1]
            return summary.get(f"utility_{idx}", 0.0), summary, trace

# function that runs the negotiation
def run_negotiation_with_params(param_values):
    params = {k: float(v) for k, v in zip(param_names, param_values)}

    opponents = [
        "agents.boulware_agent.boulware_agent.BoulwareAgent",
        "agents.conceder_agent.conceder_agent.ConcederAgent",
        "agents.hardliner_agent.hardliner_agent.HardlinerAgent",
        "agents.linear_agent.linear_agent.LinearAgent",
        "agents.random_agent.random_agent.RandomAgent",
        "agents.stupid_agent.stupid_agent.StupidAgent",
        "agents.CSE3210.agent2.agent2.Agent2",
        "agents.CSE3210.agent3.agent3.Agent3",
        "agents.CSE3210.agent7.agent7.Agent7",
        "agents.CSE3210.agent11.agent11.Agent11",
        "agents.CSE3210.agent14.agent14.Agent14",
        "agents.CSE3210.agent18.agent18.Agent18",
        "agents.CSE3210.agent19.agent19.Agent19",
        "agents.CSE3210.agent22.agent22.Agent22",
        "agents.CSE3210.agent24.agent24.Agent24",
        "agents.CSE3210.agent25.agent25.Agent25",
        "agents.CSE3210.agent26.agent26.Agent26",
        "agents.CSE3210.agent27.agent27.Agent27",
        "agents.CSE3210.agent29.agent29.Agent29",
        "agents.CSE3210.agent32.agent32.Agent32",
        "agents.CSE3210.agent33.agent33.Agent33",
        "agents.CSE3210.agent41.agent41.Agent41",
        "agents.CSE3210.agent43.agent43.Agent43",
        "agents.CSE3210.agent50.agent50.Agent50",
        "agents.CSE3210.agent52.agent52.Agent52",
        "agents.CSE3210.agent55.agent55.Agent55",
        "agents.CSE3210.agent58.agent58.Agent58",
        "agents.CSE3210.agent61.agent61.Agent61",
        "agents.CSE3210.agent64.agent64.Agent64",
        "agents.CSE3210.agent67.agent67.Agent67",
        "agents.CSE3210.agent68.agent68.Agent68",
        "agents.ANL2022.agent007.agent007.Agent007",
        "agents.ANL2022.agent4410.agent_4410.Agent4410",
        "agents.ANL2022.agentfish.agentfish.AgentFish",
        "agents.ANL2022.AgentFO2.AgentFO2.AgentFO2",
        "agents.ANL2022.BIU_agent.BIU_agent.BIU_agent",
        "agents.ANL2022.charging_boul.charging_boul.ChargingBoul",
        "agents.ANL2022.compromising_agent.compromising_agent.CompromisingAgent",
        "agents.ANL2022.dreamteam109_agent.dreamteam109_agent.DreamTeam109Agent",
        "agents.ANL2022.gea_agent.gea_agent.GEAAgent",
        "agents.ANL2022.learning_agent.learning_agent.LearningAgent",
        "agents.ANL2022.LuckyAgent2022.LuckyAgent2022.LuckyAgent2022",
        "agents.ANL2022.micro_agent.micro_agent.micro_agent.MiCROAgent",
        "agents.ANL2022.Pinar_Agent.Pinar_Agent.Pinar_Agent",
        "agents.ANL2022.procrastin_agent.procrastin_agent.ProcrastinAgent",
        "agents.ANL2022.rg_agent.rg_agent.RGAgent",
        "agents.ANL2022.smart_agent.smart_agent.SmartAgent",
        "agents.ANL2022.super_agent.super_agent.SuperAgent",
        "agents.ANL2022.thirdagent.third_agent.ThirdAgent",
        "agents.ANL2022.tjaronchery10_agent.tjaronchery10_agent.Tjaronchery10Agent",
    ]

    all_domains = [f"domain{str(i).zfill(2)}" for i in range(50)]
    selected_domains = random.sample(all_domains, 2)

    profiles = [
        [f"domains/{d}/profileA.json", f"domains/{d}/profileB.json"]
        for d in selected_domains
    ]

    utilities = []
    summaries = []
    traces = []

    for profile in profiles:
        for opponent in opponents:
            util, summary, trace = run_single_session(param_values, profile, opponent)
            utilities.append(util)
            summaries.append(summary)
            traces.append(trace)

    avg_utility = np.mean(utilities) if utilities else 0.0
    return -avg_utility, summaries, traces

def expected_improvement(X, model, Y_sample, xi=0.1):
    mu, sigma = model.predict(X, return_std=True)
    min_curr = np.min(Y_sample)
    imp = mu - min_curr - xi
    Z = imp / (sigma + 1e-9)
    # expected improvement
    ei = imp * norm.cdf(Z) + sigma * norm.pdf(Z)
    return ei

def propose_location(acquisition, model, Y_sample, bounds, n_restarts=5):
    def min_obj(X):
        return -acquisition(X.reshape(1, -1), model, Y_sample)

    best_x = None
    best_acq = -np.inf
    for _ in range(n_restarts):
        x0 = np.array([np.random.uniform(b[0], b[1]) for b in bounds])
        res = minimize(min_obj, x0=x0, bounds=bounds, method='L-BFGS-B')
        if res.success and -res.fun > best_acq:
            best_acq = -res.fun
            best_x = res.x
    return best_x

# main function for optimiziation
def bayesian_optimization(n_init=3, n_iter=5):
    X_sample = []
    Y_sample = []
    all_summaries = []
    all_traces = []

    # first phase -> random samples
    for k in range(n_init):
        x = [np.random.uniform(low, high) for (low, high) in param_bounds]
        y, summary, trace = run_negotiation_with_params(x)
        all_summaries.append(summary)
        all_traces.append(trace)
        X_sample.append(x)
        Y_sample.append(y)

        params_dict = dict(zip(param_names, x))
        params_dict["avg_template_utility"] = -y

        with open(RESULTS_DIR / f"params_random_iter_{k + 1}.json", "w") as f:
            json.dump(params_dict, f, indent=2)

    # second phase -> actual bayesian
    for i in range(n_iter):
        model = GaussianProcessRegressor(kernel=Matern(nu=2.5), alpha=1e-6)
        model.fit(np.array(X_sample), np.array(Y_sample))

        x_next = propose_location(expected_improvement, model, np.array(Y_sample), param_bounds)
        y_next, summary, trace = run_negotiation_with_params(x_next)
        all_summaries.append(summary)
        all_traces.append(trace)
        X_sample.append(x_next)
        Y_sample.append(y_next)

        params_dict = dict(zip(param_names, x_next))
        params_dict["avg_template_utility"] = -y_next

        with open(RESULTS_DIR / f"params_bayesian_iter_{i + 1}.json", "w") as f:
            json.dump(params_dict, f, indent=2)

        print(f"Iteration {i+1}/{n_iter}, score: {-y_next:.4f}")

    # save best params
    best_idx = np.argmin(Y_sample)
    best_params = {k: v for k, v in zip(param_names, X_sample[best_idx])}

    # save to files
    with open(RESULTS_DIR / "best_params.json", "w") as f:
        json.dump(best_params, f, indent=2)

    print("\n=== BEST FOUND PARAMS ===")
    print(json.dumps(best_params, indent=2))
    print(f"Best utility_2: {-Y_sample[best_idx]:.4f}")

    utilities = [-y for y in Y_sample]
    best_so_far = np.maximum.accumulate(utilities)

    fig = go.Figure()
    fig.add_trace(go.Scatter(
        x=list(range(1, len(best_so_far) + 1)),
        y=best_so_far,
        mode="lines+markers",
        name="Best utility so far"
    ))
    fig.update_layout(
        title="Bayesian Optimization Convergence",
        xaxis_title="Iteration",
        yaxis_title="Utility (TemplateAgent)",
        template="plotly_white"
    )
    html_path = RESULTS_DIR / "convergence_plot.html"
    fig.write_html(str(html_path))

    print(f"\nSaved plot to: {html_path}")

    best_summaries = all_summaries[best_idx]
    with open(RESULTS_DIR / "best_summaries.json", "w") as f:
        json.dump(best_summaries, f, indent=2)

    print(f"Saved detailed summaries for best parameters to: {RESULTS_DIR / 'best_summaries.json'}")

    print("\nSaving all summaries in csv per iteration")
    csv_rows = []

    for iter_idx, summaries in enumerate(all_summaries, start=1):
        template_utilities = []
        opponent_utilities = []
        nash_values = []
        social_values = []

        for s in summaries:
            template_idx = None
            for key, val in s.items():
                if key.startswith("agent_") and val == "TemplateAgent":
                    template_idx = key.split("_")[1]
                    break

            if template_idx is None:
                continue

            opponent_idx = str(1 + (int(template_idx)))

            template_utility = s.get(f"utility_{template_idx}", 0.0)
            opponent_utility = s.get(f"utility_{opponent_idx}", 0.0)

            template_utilities.append(template_utility)
            opponent_utilities.append(opponent_utility)
            nash_values.append(s.get("nash_product", 0.0))
            social_values.append(s.get("social_welfare", 0.0))

        csv_rows.append({
            "iteration": iter_idx,
            "avg_utility_template": np.mean(template_utilities),
            "avg_utility_opponent": np.mean(opponent_utilities),
            "avg_nash_product": np.mean(nash_values),
            "avg_social_welfare": np.mean(social_values),
        })

    df = pd.DataFrame(csv_rows)
    df.to_csv(RESULTS_DIR / "iteration_averages.csv", index=False)
    print(f"Saved iteration averages to: {RESULTS_DIR / 'iteration_averages.csv'}")

    print("\nSaving all traces per iteration")

    for iter_idx, traces in enumerate(all_traces):
        iteration_folder = RESULTS_DIR / f"iteration_{iter_idx + 1}"
        iteration_folder.mkdir(parents=True, exist_ok=True)

        for trace_idx, trace in enumerate(traces):
            trace_file = iteration_folder / f"trace_{trace_idx + 1}.json"
            with open(trace_file, "w") as f:
                json.dump(trace, f, indent=2)

    print(f"Saved all traces in subfolders iteration_1 to iteration_{len(all_traces)}.")



if __name__ == "__main__":
    bayesian_optimization(n_init=12, n_iter=22)
