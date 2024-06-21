# %%
import pandas as pd
import numpy as np
import sys

input_path = sys.argv[1]
output_path = sys.argv[2]

data =  pd.read_csv(input_path, sep=";")

selected_data = pd.DataFrame()
selected_data['name'] = data['name']
selected_data['tags'] = data['tags']
selected_data['base_model'] = data['base_model']

selected_data["base_model"]

def insert_to_map(datamap, dataset, model):
    try:
        values = datamap[dataset]
    except KeyError:
        values = datamap[dataset] = []

    values.append(model)

datasets = []
datasets_map = {}

for index, row in selected_data.iterrows():
    tag_string = str(row["tags"])
    dataset_string = str(row["base_model"])

    if tag_string != "nan":
        tag_list = eval(tag_string)
        for tag in tag_list:
            if "base_model:" in tag:
                tag = tag.removeprefix("base_model:")
                # print(tag)
                datasets.append(tag)
                insert_to_map(datasets_map, tag, row['name'])
                
    
    if dataset_string != "nan" and dataset_string != "unknown":
        if dataset_string.startswith("["):
            dataset_list = eval(dataset_string)
        else:
            dataset_list = [dataset_string]
        if isinstance(dataset_list, (list, tuple, np.ndarray)):
            for tag in dataset_list:
                # print(tag)
                datasets.append(tag)
                insert_to_map(datasets_map, tag, row['name'])

print(f"Len of base_models before dedup: {len(datasets)}")
datasets = set(datasets)
print(f"Len of base_models after dedup: {len(datasets)}")

known_trained_models = set(model for models in datasets_map.values() for model in models)
df = pd.DataFrame(columns=datasets_map.keys(), index=known_trained_models).fillna(0)


for dataset, models in datasets_map.items():
    for model in models:
        df.at[model, dataset] = 1

# Export the DataFrame to CSV
df.to_csv(output_path, sep=";")



