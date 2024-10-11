#!/bin/bash

rm -r ../../data/v1/
mkdir ../../data/v1/
# curl https://huggingfaceh4-open-llm-leaderboard.hf.space/ > ../../data/v1/leaderboard.html

curl https://open-llm-leaderboard-open-llm-leaderboard.hf.space/?__theme=light > ../../data/v1/leaderboard.html
python3 html2json.py ../../data/v1/leaderboard.html ../../data/v1/leaderboard.json
python3 json2csv.py ../../data/v1/leaderboard.json ../../data/v1/leaderboard.csv
python3 dedup.py ../../data/v1/leaderboard.csv ../../data/v1/leaderboard_deduped.csv
python3 extract_repo_activity.py ../../data/v1/leaderboard_deduped.csv ../../data/v1/leaderboard_repo_activity.csv ../../data/v1/leaderboard_commit_data.csv
python3 extract_repo_details.py ../../data/v1/leaderboard.json ../../data/v1/leaderboard_repo_detailes.csv
python3 extract_base_models.py ../../data/v1/leaderboard_repo_detailes.csv ../../data/v1/leaderboard_base_models.csv
python3 extract_datasets.py ../../data/v1/leaderboard_repo_detailes.csv ../../data/v1/leaderboard_datasets.csv