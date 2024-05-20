import pandas as pd
import numpy as np
from tqdm import tqdm
import time
import ast 
from huggingface_hub import HfApi
import sys

input_path = sys.argv[1]
output_path = sys.argv[2]

data = pd.read_csv(input_path, sep=";")

api = HfApi()

def get_num_of_authors(commits):
    authors = []
    for commit in commits:
        for author in commit.authors:
            if author not in ['system', 'leaderboard-pr-bot']:
                authors.append(author)
    
    return (len(set(authors)), authors)

def get_num_of_commits(commits):
    return len(commits)

def get_first_commit(commits):
    return commits[-1].created_at

def get_last_commit(commits):
    return commits[0].created_at

repo_data = {
    "name": [],
    "num_of_authors": [],
    "num_of_commits": [],
    "first_commit": [],
    "last_commit": [],
    "authors": [],
}

for index, model in tqdm(enumerate(data.iterrows()), total=len(data)):
    model = model[1]
    try:
      commits = api.list_repo_commits(model['name'])
    except:
        continue

    (num_of_authors, authors) = get_num_of_authors(commits)

    repo_data["name"].append(model['name'])
    repo_data["num_of_authors"].append(num_of_authors)
    repo_data["num_of_commits"].append(get_num_of_commits(commits))
    repo_data["first_commit"].append(get_first_commit(commits))
    repo_data["last_commit"].append(get_last_commit(commits))
    repo_data["authors"].append(authors)


df = pd.DataFrame.from_dict(repo_data)
df['authors'] = df['authors'].map(set)
df['authors'] = df['authors'].map(list)
df.to_csv("commit_data.csv")

authors = {
    "author": [],
    "repo": []
}

for index, model in tqdm(enumerate(df.iterrows()), total=len(data)):
    model = model[1]
    for author in model['authors']:
        authors['author'].append(author)
        authors['repo'].append(model['name'])

# Create a DataFrame from the authors dictionary
authors_df = pd.DataFrame(authors)

# Group by author and aggregate data
result_df = authors_df.groupby('author').agg(
    num_of_repos=pd.NamedAgg(column='repo', aggfunc='count'),
    list_of_repos=pd.NamedAgg(column='repo', aggfunc=list)
).reset_index()

# Rename columns to match your requirements
result_df.columns = ['author_name', 'num_of_repos', 'list_of_repos']

# Export to CSV
result_df.to_csv(output_path, index=False)



