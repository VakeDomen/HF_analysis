#!/bin/bash

rm -r process_dir
mkdir process_dir
curl https://huggingfaceh4-open-llm-leaderboard.hf.space/ > process_dir/leaderboard.html


python3 html2json.py process_dir/leaderboard.html process_dir/leaderboard.json
python3 json2csv.py process_dir/leaderboard.json process_dir/leaderboard.csv