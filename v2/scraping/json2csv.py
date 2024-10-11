import json
import csv
import re
import sys


# File paths
input_json_file = sys.argv[1]  # Path to your JSON file
output_csv_file = sys.argv[2]  # Path where the CSV file will be saved

def extract_urls(s):
    # This function extracts all occurrences of URLs starting with "https://huggingface.co"
    urls = re.search(r'https://huggingface\.co[\S]+', s)
    return urls.group(0) if urls else ''

def json_to_csv(json_fp, csv_fp):
    # Open the JSON file and load data
    with open(json_fp, 'r') as file:
        data = json.load(file)

    # Open the CSV file for writing
    with open(csv_fp, 'w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file, delimiter=';')


        writer.writerow([
            "T",
            "Model",
            "Average ⬆️",
            "IFEval",
            "IFEval Raw",
            "BBH",
            "BBH Raw",
            "MATH Lvl 5",
            "MATH Lvl 5 Raw",
            "GPQA",
            "GPQA Raw",
            "MUSR",
            "MUSR Raw",
            "MMLU-PRO",
            "MMLU-PRO Raw",
            "Type",
            "Architecture",
            "Weight type",
            "Precision",
            "Not_Merged",
            "Hub License",
            "#Params (B)",
            "Hub ❤️",
            "Available on the hub",
            "Model sha",
            "Flagged",
            "MoE",
            "Submission Date",
            "Upload To Hub Date",
            "Chat Template",
            "Maintainer\u0027s Highlight",
            "fullname",
            "Generation",
            "Base Model"
        ])

        # Iterate over each record in the JSON data and write to CSV
        for record in data["components"][5]["props"]["value"]["data"]:
            processed_record = []
            for item in record:
                if isinstance(item, str) and 'https://huggingface.co' in item:
                    # Extract URLs if the string contains 'https://huggingface.co'
                    processed_record.append(extract_urls(item))
                else:
                    # Replace semicolons in other text to avoid format issues
                    processed_record.append(str(item).replace(';', ','))
            processed_record[1] = processed_record[1].rstrip('"')
            writer.writerow(processed_record)

# Call the function with the appropriate file paths
json_to_csv(input_json_file, output_csv_file)
