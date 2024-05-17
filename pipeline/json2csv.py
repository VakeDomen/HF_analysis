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
            "Type",
            "ModelHub",
            "Average ⬆️",
            "ARC",
            "HellaSwag",
            "MMLU",
            "TruthfulQA",
            "Winogrande",
            "GSM8K",
            "TypeString",
            "Architecture",
            "Weight type",
            "Precision",
            "Merged",
            "Hub License",
            "#Params (B)",
            "Hub ❤️",
            "Available on the hub",
            "Model sha",
            "Flagged",
            "MoE",
            "model_name_for_query"
        ])

        # Iterate over each record in the JSON data and write to CSV
        for record in data["components"][4]["props"]["value"]["data"]:
            processed_record = []
            for item in record:
                if isinstance(item, str) and 'https://huggingface.co' in item:
                    # Extract URLs if the string contains 'https://huggingface.co'
                    processed_record.append(extract_urls(item))
                else:
                    # Replace semicolons in other text to avoid format issues
                    processed_record.append(str(item).replace(';', ','))
            writer.writerow(processed_record)

# Call the function with the appropriate file paths
json_to_csv(input_json_file, output_csv_file)
