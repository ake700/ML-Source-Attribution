#!/usr/bin/env python
# coding: utf-8

import pandas as pd

# Creating the consistent unitig pattern dataset

# set variables 
data_file = "data/patterns_Heid_ebase.tab"
csv_path = 'data/enterobase_sistr_combined_20240201.csv'
output_file = 'data/unitig_Heid_ebase_20240208.csv'

# Load the original CSV file and grab the relevant rows, filtering out failed SISTR QC and an additional accession with low quantity reads

df = pd.read_csv(csv_path, index_col=0)  # Replace with your actual CSV file path
df = df[(df['qc_status'] != 'FAIL') & (df['Accession2'] != 'SRR1183798')]

# Preparing other dfs for merging with patterns.tab file
raw_labels = df[['Accession2', 'Region']].copy()
raw_labels.set_index('Accession2', inplace=True)

include = df[['Accession2']].copy()
include.set_index('Accession2', inplace=True)

# features
raw_features  = pd.read_csv(data_file, sep = "\t", index_col = "pattern_id")

# Renaming specific entries
rename_dict = {
    'British Columbia': 'BC',
    'Alberta': 'AB',
    'Manitoba': 'MB',
    'Quebec': 'QC',
    'Ontario': 'ON',
    'Saskatchewan': 'SK',
    'Prince Edward Island': 'PE',
    'Nova Scotia': 'NS',
    'New Brunswick': 'NB',
    'Newfoundland and Labrador': 'NL'
    # Add other provinces as needed
}

raw_labels['Region'] = raw_labels['Region'].replace(rename_dict)

print(raw_labels, include)

# Transpose raw_features
transposed_features = raw_features.T.reset_index()
print(transposed_features.head())

# Assuming the index of raw_labels is 'Sample' and the column is 'Province'
merged_df = pd.merge(transposed_features, raw_labels, left_on='index', right_index=True)
print(merged_df.head())

# Rename 'index' column for clarity
merged_df.rename(columns={'index': 'Accession', '1_x': '1'}, inplace=True)

# Dynamic renaming for 'Province' column considering it could be named '1', '1_x', or '1_y'
if 1 in merged_df.columns:
    merged_df.rename(columns={'1': 'Province'}, inplace=True)
elif '1_y' in merged_df.columns:
    merged_df.rename(columns={'1_y': 'Province'}, inplace=True)
elif 'Region' in merged_df.columns:
    merged_df.rename(columns={'Region': 'Province'}, inplace=True)

# Create a copy of the DataFrame to avoid fragmentation
unitig_df = merged_df.copy()

# Get the position of the 'Province' column in the copied DataFrame
acc_index = unitig_df.columns.get_loc('Accession')

# Insert the 'Province' column after 'Accession' in the copied DataFrame
unitig_df.insert(acc_index + 1, 'Province', unitig_df.pop('Province'))

# Create mapping for direction
direction_mapping = {province: 'East' for province in ['NB', 'NL', 'NS', 'ON', 'PE', 'QC']}
direction_mapping.update({province: 'West' for province in ['BC', 'AB', 'SK', 'MB', 'NT', 'NU', 'YT']})

# Apply mapping to create 'direction' column
unitig_df['Geography'] = unitig_df['Province'].map(direction_mapping)

# Create a copy of the DataFrame to avoid fragmentation
unitig_final = unitig_df.copy()

# Get the position of the 'Province' column in the copied DataFrame
province_index = unitig_final.columns.get_loc('Province')

# Insert the 'Geography' column after 'Province' in the copied DataFrame
unitig_final.insert(province_index + 1, 'Geography', unitig_final.pop('Geography'))

print(unitig_final.head(), unitig_final.columns, unitig_final.shape)

unitig_final.to_csv(output_file, index=False)

