#!/usr/bin/env python
# coding: utf-8

import pandas as pd
import numpy as np
from sklearn.preprocessing import LabelEncoder
import yaml 
import os

print("Pandas version:", pd.__version__)
print("NumPy version:", np.__version__)
os.getcwd()

script_dir = os.path.dirname(os.path.abspath(__file__))

allele = pd.read_csv('data/Archive/fn_ldex_shovill_20240723.tsv', sep='\t', low_memory=False)

# Create filtered DataFrame
filtered_df = allele

# Drop unnecessary columns
encode_df = filtered_df.drop(['Key', 'SourceState', 'Serovar', 'SourceSite', 'CombinedSites', 'CombinedSite2'], axis=1)
labels_y = filtered_df[['Key', 'SourceState', 'Serovar', 'SourceSite', 'CombinedSites', 'CombinedSite2']]

# Replace '-' with a placeholder
placeholder = 'PLACEHOLDER'
encode_df = encode_df.astype(str).map(lambda x: placeholder if x == '-' else x)

# Flatten the DataFrame to get a single list of unique values
unique_values = pd.unique(encode_df.values.ravel())

# Fit the LabelEncoder on the unique values
le = LabelEncoder()
le.fit(unique_values)

# Transform the DataFrame
encoded_df = encode_df.apply(lambda col: le.transform(col) + 1)

# Replace placeholder back with '-' if needed
encoded_placeholder = le.transform([placeholder])[0] + 1
for col in encoded_df.columns:
    encoded_df[col] = encoded_df[col].apply(lambda x: '-' if x == encoded_placeholder else x)

# Concatenate labels and encoded DataFrame
filtered_df = pd.concat([labels_y.reset_index(drop=True), encoded_df.reset_index(drop=True)], axis=1)
filtered_df.replace({'-': 0}, inplace=True)

filtered_df.to_csv('data/fn_ldex_shovill_LE2_20240725.tsv', sep='\t', index=False)