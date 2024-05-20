import json
import pandas as pd
from tqdm import tqdm
import sys
import time
from huggingface_hub import HfApi
api = HfApi()


input_path = sys.argv[1]
output_path = sys.argv[2]

model_data_handle = open(input_path)
model_data = json.load(model_data_handle)["components"][4]["props"]["value"]["data"]



repo_data = {
    "name": [],
    "author": [],
    "created_at": [],
    "last_modified": [],
    "downloads": [],
    "library_name": [],
    "tags": [],
    "language": [],
    "datasets": [],
    "base_model": [],
}

for (index, model) in tqdm(enumerate(model_data), total=len(model_data)):
    try:
      repo = api.repo_info(model_data[index][-1])
    except:
        print(f"Failed to fetch: {model_data[index][-1]}")
        continue
    
    if repo.private or repo.gated:
        print(f"Repo {model_data[index][-1]} is private or gated, skipping...")
        continue

    repo_data["name"].append(model_data[index][-1])
    repo_data["author"].append(getattr(repo, 'author', 'unknown'))
    repo_data["created_at"].append(getattr(repo, 'created_at', 'unknown'))
    repo_data["last_modified"].append(getattr(repo, 'last_modified', 'unknown'))
    repo_data["downloads"].append(getattr(repo, 'downloads', 'unknown'))
    repo_data["library_name"].append(getattr(repo, 'library_name', 'unknown'))
    repo_data["tags"].append(getattr(repo, 'tags', 'unknown'))
    
    # Accessing nested attributes in 'card_data'
    repo_data["language"].append(getattr(repo.card_data, 'language', 'unknown') if hasattr(repo, 'card_data') else 'unknown')
    repo_data["datasets"].append(getattr(repo.card_data, 'datasets', 'unknown') if hasattr(repo, 'card_data') else 'unknown')
    repo_data["base_model"].append(getattr(repo.card_data, 'base_model', 'unknown') if hasattr(repo, 'card_data') else 'unknown')





df = pd.DataFrame.from_dict(repo_data)

df.to_csv(output_path, sep=";")