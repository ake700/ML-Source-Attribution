#!/usr/bin/env python
# coding: utf-8

import pandas as pd

csv_path = 'data/enterobase_sistr_combined_20240201.csv'
accessions_path = 'data/accessions_unitigs_20240205.tab'
output_path = 'data/filtered_Heid_ebase_20240208.tab'
column_name = 'Serovar2'
strains_of_interest = ['Heidelberg']

# Load the CSV file
df = pd.read_csv(csv_path)  # Replace with your actual CSV file path

# Filter the DataFrame for the strains of interest
filtered_df = df[df[column_name].isin(strains_of_interest)]

# Store the corresponding accessions in a list
accessions = filtered_df['Accession2'].tolist()

print(filtered_df[column_name].value_counts(), len(accessions))

# Convert the list of accessions to a set for faster lookups
accessions_set = set(accessions)
found_accessions = set()  # To store accessions found in the file

# Open and read through each line of the file
with open(accessions_path, 'r') as file:
    for line in file:
        # Check each accession in your list to see if it's in the current line
        for accession in accessions_set:
            if accession in line:
                found_accessions.add(accession)  # Add to found_accessions if present

# Determine which accessions were not found by subtracting sets
not_found_accessions = accessions_set - found_accessions

# Print the accessions not found in the file
print(f"Accessions in list but not found in '{accessions_path}':")
for accession in not_found_accessions:
    print(accession)

# Reporting
print(f"\nTotal accessions in list: {len(accessions_set)}")
print(f"Matches found in 'accessions_unitigs_20240205.tab': {len(found_accessions)}")
print(f"Accessions not found: {len(not_found_accessions)}")

# The remainder are the ones that were removed due to failed qc status from SISTR

# Read the original text file and filter based on accessions
with open(accessions_path, 'r') as file:
    lines = file.readlines()

with open(output_path, 'w') as outfile:
    for line in lines:
        if any(accession in line for accession in accessions):
            outfile.write(line)

