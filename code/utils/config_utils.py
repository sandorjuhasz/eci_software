from pathlib import Path
import yaml


def load_config():
    repo_root = Path("..", "..").resolve()
    with open(repo_root / "config.yml", "r") as f:
        return yaml.safe_load(f)
