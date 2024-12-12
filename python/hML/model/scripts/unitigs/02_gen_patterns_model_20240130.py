#!/usr/bin/env python
# coding: utf-8

import pandas as pd

patt_conv_path = 'data/patterns_conversion.tab'
output_file_path = 'data/pattern_in_cad_model.txt'  # Replace with your desired output path

# Read the .tab file, skipping the first line and importing only the first 5 lines of data
df = pd.read_csv(patt_conv_path, sep='\t', skiprows=1)
print(f"Number of original patterns: {len(df['pattern_id'])}") 
print(df.shape)

# Read the first line to get the total number of samples
with open(patt_conv_path, 'r') as file:
    first_line = file.readline()
    total_samples = int(first_line.split()[1])  # Extract the total samples count

print(total_samples)

# Filter the DataFrame for specific 'no_samples' values
filtered_df = df[df['no_samples'].isin([1, total_samples, total_samples - 1])]
print(filtered_df.shape)

# Filter for rows where 'no_samples' is not 1, total, or total-1
pattern_ids_to_save = df[~df['no_samples'].isin([1, total_samples, total_samples - 1])]['pattern_id']
print(f"Number of patterns after filtering: {len(pattern_ids_to_save)}")

# Write to a text file
with open(output_file_path, 'w') as file:
    for pid in pattern_ids_to_save:
        file.write(f"{pid}\n")

