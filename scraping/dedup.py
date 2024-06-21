import pandas as pd
from tqdm import tqdm


def process_csv(input_file):
    # Read the CSV file
    df = pd.read_csv(input_file)
    
    # Initialize last valid row from the first row
    last_valid_row = df.iloc[0, 2:].copy()
    
    # Iterate over the DataFrame, starting from the second row
    for i in tqdm(range(1, len(df))):
        # Get the current row starting from the third column
        current_row = df.iloc[i, 2:].copy()
        # Compare the current row with the last valid row
        if current_row.equals(last_valid_row):
            # If they are the same, clear the current row values from the third column onwards
            df.iloc[i, 2:] = ""
        else:
            # Update the last valid row to the current row
            last_valid_row = current_row

    # Define the output file name
    output_file = input_file.replace(".csv", "_modified.csv")
    
    # Write the modified DataFrame to a new CSV file, preserving the index if it's part of the data
    df.to_csv(output_file, index=False)
    print(f"Modified file saved as {output_file}")


process_csv("output.csv")
