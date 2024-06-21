#!/bin/bash

rm -r ../data
mkdir ../data
# curl https://huggingfaceh4-open-llm-leaderboard.hf.space/ > ../data/leaderboard.html

curl https://open-llm-leaderboard-open-llm-leaderboard.hf.space/?__theme=light > ../data/leaderboard.html
python3 html2json.py ../data/leaderboard.html ../data/leaderboard.json
python3 json2csv.py ../data/leaderboard.json ../data/leaderboard.csv
python3 dedup.py ../data/leaderboard.csv ../data/leaderboard_deduped.csv
python3 extract_repo_activity.py ../data/leaderboard_deduped.csv ../data/leaderboard_repo_activity.csv ../data/leaderboard_commit_data.csv
python3 extract_repo_details.py ../data/leaderboard.json ../data/leaderboard_repo_detailes.csv
python3 extract_base_models.py ../data/leaderboard_repo_detailes.csv ../data/leaderboard_base_models.csv
python3 extract_datasets.py ../data/leaderboard_repo_detailes.csv ../data/leaderboard_datasets.csv