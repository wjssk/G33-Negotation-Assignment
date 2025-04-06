from multiprocessing import Pool, cpu_count
from pathlib import Path
import json
import time
from utils.runners import run_session
import csv
from collections import defaultdict
import uuid
import random

RESULTS_DIR = Path("eval_all/final_run")
TRACES_DIR = RESULTS_DIR / "all_traces"
SUMMARIES_DIR = RESULTS_DIR / "all_summaries"
TRACES_DIR.mkdir(parents=True, exist_ok=True)
SUMMARIES_DIR.mkdir(parents=True, exist_ok=True)

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

domains = [f"domain{str(i).zfill(2)}" for i in range(50)]
profiles = []
for domain in domains:
    profiles.append((domain, [f"domains/{domain}/profileA.json", f"domains/{domain}/profileB.json"]))
    profiles.append((domain, [f"domains/{domain}/profileB.json", f"domains/{domain}/profileA.json"]))

tasks = [(domain, profile, opponent, TRACES_DIR, SUMMARIES_DIR) for (domain, profile) in profiles for opponent in opponents]

def run_and_save(args):
    time.sleep(random.uniform(0.05, 0.3))
    unique_id = str(uuid.uuid4())
    domain, profile, opponent, traces_dir, summaries_dir = args

    settings = {
        "agents": [
            {
                "class": "agents.group33_agent.group33_agent.TemplateAgent",
                "parameters": {
                    "storage_dir": f"agent_storage/TemplateAgent_{unique_id}"
                }
            },
            {"class": opponent},
        ],
        "profiles": profile,
        "deadline_time_ms": 10000,
    }

    try:
        trace, summary = run_session(settings)

        trace_id = f"{Path(profile[0]).stem}_{Path(profile[1]).stem}_{opponent.split('.')[-1]}"
        trace_subdir = traces_dir / domain
        summary_subdir = summaries_dir / domain
        trace_subdir.mkdir(parents=True, exist_ok=True)
        summary_subdir.mkdir(parents=True, exist_ok=True)

        with open(trace_subdir / f"trace_{trace_id}.json", "w") as f:
            json.dump(trace, f, indent=2)
        with open(summary_subdir / f"summary_{trace_id}.json", "w") as f:
            json.dump(summary, f, indent=2)

        return (domain, profile, summary)

    except Exception as e:
        print(f"FAILED: {profile} vs {opponent} -> {e}")
        return None

if __name__ == "__main__":
    with Pool(processes=12) as pool:
        results = pool.map(run_and_save, tasks)

    summaries_only = [r[2] for r in results if r is not None]
    with open(SUMMARIES_DIR / "all_summaries.json", "w") as f:
        json.dump(summaries_only, f, indent=2)

    print(f"\nSaved {len(summaries_only)} summaries to: {SUMMARIES_DIR / 'all_summaries.json'}")
    print(f"Traces saved in domain folders inside: {TRACES_DIR}")

    domain_data = defaultdict(list)

    for result in results:
        if result is None:
            continue
        domain, profile, summary = result

        template_idx = None
        for key, val in summary.items():
            if key.startswith("agent_") and val == "TemplateAgent":
                template_idx = key.split("_")[1]
                break

        if template_idx is None:
            continue

        opponent_idx = [key.split("_")[1] for key in summary if key.startswith("agent_") and key.split("_")[1] != template_idx]
        if not opponent_idx:
            continue
        opponent_idx = opponent_idx[0]

        our_utility = summary.get(f"utility_{template_idx}", 0.0)
        opp_utility = summary.get(f"utility_{opponent_idx}", 0.0)

        domain_data[domain].append({
            "our_utility": our_utility,
            "opp_utility": opp_utility,
            "social_welfare": summary.get("social_welfare", 0.0),
            "nash_product": summary.get("nash_product", 0.0),
        })

    csv_path = SUMMARIES_DIR / "domain_summary.csv"
    with open(csv_path, "w", newline="") as csvfile:
        fieldnames = ["domain", "our_avg_utility", "opp_avg_utility",
                      "avg_social_welfare", "avg_nash_product"]
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()

        for domain, records in domain_data.items():
            our_avg = sum(r["our_utility"] for r in records) / len(records)
            opp_avg = sum(r["opp_utility"] for r in records) / len(records)
            sw_avg = sum(r["social_welfare"] for r in records) / len(records)
            nash_avg = sum(r["nash_product"] for r in records) / len(records)

            writer.writerow({
                "domain": domain,
                "our_avg_utility": round(our_avg, 4),
                "opp_avg_utility": round(opp_avg, 4),
                "avg_social_welfare": round(sw_avg, 4),
                "avg_nash_product": round(nash_avg, 4)
            })

    print(f"\nSaved domain-level CSV summary to: {csv_path}")