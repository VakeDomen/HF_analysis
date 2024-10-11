#!/bin/bash

rm -r ../../data/v2/
mkdir ../../data/v2/
# curl https://huggingfaceh4-open-llm-leaderboard.hf.space/ > ../../data/v2/leaderboard.html

curl https://open-llm-leaderboard-open-llm-leaderboard.hf.space/?__theme=light > ../../data/v2/leaderboard.html
python3 html2json.py ../../data/v2/leaderboard.html ../../data/v2/leaderboard.json
python3 json2csv.py ../../data/v2/leaderboard.json ../../data/v2/leaderboard.csv
python3 dedup.py ../../data/v2/leaderboard.csv ../../data/v2/leaderboard_deduped.csv
python3 extract_repo_activity.py ../../data/v2/leaderboard_deduped.csv ../../data/v2/leaderboard_repo_activity.csv ../../data/v2/leaderboard_commit_data.csv
python3 extract_repo_details.py ../../data/v2/leaderboard.json ../../data/v2/leaderboard_repo_detailes.csv
python3 extract_base_models.py ../../data/v2/leaderboard_repo_detailes.csv ../../data/v2/leaderboard_base_models.csv
python3 extract_datasets.py ../../data/v2/leaderboard_repo_detailes.csv ../../data/v2/leaderboard_datasets.csv