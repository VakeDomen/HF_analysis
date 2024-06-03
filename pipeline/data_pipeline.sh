#!/bin/bash

rm -r ../process_dir
mkdir ../process_dir
curl https://huggingfaceh4-open-llm-leaderboard.hf.space/ > ../process_dir/leaderboard.html


python3 html2json.py ../process_dir/leaderboard.html ../process_dir/leaderboard.json
python3 json2csv.py ../process_dir/leaderboard.json ../process_dir/leaderboard.csv
python3 dedup.py ../process_dir/leaderboard.csv ../process_dir/leaderboard_deduped.csv
python3 extract_repo_activity.py ../process_dir/leaderboard_deduped.csv ../process_dir/leaderboard_repo_activity.csv ../process_dir/leaderboard_commit_data.csv
python3 extract_repo_details.py ../process_dir/leaderboard_deduped.json ../process_dir/leaderboard_repo_detailes.csv
python3 extract_base_models.py ../process_dir/leaderboard_repo_detailes.csv ../process_dir/leaderboard_base_models.csv
python3 extract_datasets.py ../process_dir/leaderboard_repo_detailes.csv ../process_dir/leaderboard_datasets.csv