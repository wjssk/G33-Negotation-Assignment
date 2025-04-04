import random
from utils.runners import run_session

def run_with_params(b_early, b_late):
    params = {
        "storage_dir": "agent_storage/TemplateAgent",
        "b_early": b_early,
        "b_late": b_late,
    }

    settings = {
        "agents": [
            {
                "class": "agents.ANL2022.dreamteam109_agent.dreamteam109_agent.DreamTeam109Agent",
                "parameters": {"storage_dir": "agent_storage/DreamTeam109Agent"},
            },
            {
                "class": "agents.template_agent.template_agent.TemplateAgent",
                "parameters": params,
            },
        ],
        "profiles": ["domains/domain00/profileA.json", "domains/domain00/profileB.json"],
        "deadline_time_ms": 10000,
    }

    trace, summary = run_session(settings)
    utility = summary.get("utility_2", 0.0)
    result = summary.get("result", "no_result")
    print(f"SUMMARY: {summary}")

    if result != "agreement":
        utility = 0.0  # penalizuj brak porozumienia

    return utility, b_early, b_late

best_score = -1
best_config = None

for i in range(20):
    b_early = round(random.uniform(0.1, 1.0), 3)
    b_late = round(random.uniform(1.0, 3.0), 3)

    score, be, bl = run_with_params(b_early, b_late)
    print(f"Run {i+1:02d} | b_early={be} b_late={bl} => utility_2={score:.4f}")

    if score > best_score:
        best_score = score
        best_config = (be, bl)

print("\n🔍 Best config:")
print(f"b_early = {best_config[0]}, b_late = {best_config[1]} (utility_2 = {best_score:.4f})")
