import pandas as pd
from tqdm import tqdm
import sys

input_path = sys.argv[1]
output_path = sys.argv[2]


def process_csv(input_file, output_file):
    # Read the CSV file
    df = pd.read_csv(input_file, sep=";")   
    print(df)
    df = df.sort_values(by=['Model', "Average ⬆️"], ascending=False)
    new_df = pd.DataFrame(columns=df.columns)
    # Initialize last valid row from the first row
    last_valid_row = df.iloc[0, :].copy()
    
    

    # Iterate over the DataFrame, starting from the second row
    for i in tqdm(range(1, len(df))):
        # Get the current row starting from the third column
        current_row = df.iloc[i, :].copy()
        # print(f"{current_row['ModelHub']} --  {last_valid_row['ModelHub']}")
        # Compare the current row with the last valid row
        if not current_row['Model'] == last_valid_row['Model']:
            # Update the last valid row to the current row
            new_df = new_df.append(last_valid_row, ignore_index=True)
            last_valid_row = current_row


    # Define the output file name
    
    # Write the modified DataFrame to a new CSV file, preserving the index if it's part of the data
    new_df.to_csv(output_file, index=False, sep=";")
    print(f"Modified file saved as {output_file}")


process_csv(input_path, output_path)
