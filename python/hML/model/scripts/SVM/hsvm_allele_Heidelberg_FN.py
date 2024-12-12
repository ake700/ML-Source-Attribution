#!/usr/bin/env python
# coding: utf-8

import networkx as nx
import os
import time
import copy 
import gzip
import pickle
import pandas as pd
import numpy as np
import yaml 
import glob
import sys
from networkx.drawing.nx_pydot import graphviz_layout
import pydot

from sklearn.model_selection import train_test_split, RandomizedSearchCV, StratifiedKFold, cross_validate
from sklearn.metrics import accuracy_score, balanced_accuracy_score, confusion_matrix, classification_report
from sklearn.preprocessing import OneHotEncoder

sys.path.append( './Modules')
from functions_hML import *
from HC_PyScripting import *

print("Pandas version:", pd.__version__)
print("NumPy version:", np.__version__)
os.getcwd()

start_script_time = time.time()

# File paths for config, unitig patterns, and all allelic profile
config_path = './model_config_hML.yml'

# Load configuration
def load_config(config_path):
    with open(config_path, 'r') as file:
        return yaml.safe_load(file)

config = load_config(config_path)

data_dir = config['setup']['file_paths']['data_dir']
allele_path = config['setup']['file_paths']['allele_path']
results_path = config['setup']['models']['SVM']['results_path']
graph_input_file = config['setup']['file_paths']['graph_input_file']

# Set variables for dictionary
seed = config['setup']['seed'][0]
filter_prov = config['setup']['variables']['filter_prov']
model_type = config['setup']['models']['SVM']['model_type']
stratified = config['setup']['variables']['stratified']
optim = config['setup']['variables']['optim']

# Setting the column_order for the df
template = pd.read_csv(results_path, nrows=0)
columns_order = template.columns.tolist()

# read graph
with open(graph_input_file, 'rb') as file:
    loaded_hierarchy_graph_data = pickle.load(file)

graph = nx.DiGraph(loaded_hierarchy_graph_data)
# Load data
allele = pd.read_csv(allele_path, low_memory=False)

allele.replace({'?':0}, inplace=True) # Set instances 'no gene found' to 0
# allele.replace({'MB / SK':'SK'}, inplace=True)

# Combine the selected columns into a new DataFrame
allele = allele.rename(columns={'SourceState': 'Province'})

# Create mapping for direction
direction_mapping = {province: 'East' for province in ['NB', 'NL', 'NS', 'ON', 'PE', 'QC']}
direction_mapping.update({province: 'West' for province in ['BC', 'AB', 'SK', 'MB', 'MB / SK', 'NT', 'NU', 'YT']})

# Apply mapping to create 'direction' column
allele['Geography'] = allele['Province'].map(direction_mapping)

# Modified code to rearrange columns to prevent fragmentation warning
columns = list(allele.columns)
columns.remove('Geography')

# Get the position of the 'Province' column in the copied DataFrame
province_index = columns.index('Province')

columns.insert(province_index + 1, 'Geography')

# Insert the 'Geography' column after 'Province' in the copied DataFrame
allele = allele.reindex(columns=columns)

for seed in config['setup']['seed']: 

    for test_size in config['setup']['test_size']:

        for feat_eng in config['setup']['feat_eng']:

            for filter_threshold in config['setup']['filter_threshold']:

                for y_col in config['setup']['y_col']:

                    start_run_time = time.time()

                    if 'Heidelberg' == 'All':
                        filtered_df = allele
                    else:
                        filtered_df = allele[allele['Serovar'] == 'Heidelberg']

                    results_dict = hml_svm_allele(filtered_df, 'Serovar', 'Heidelberg', test_size, graph, db='default', y_col=y_col, dataType='allele', feat_eng=feat_eng, filter_threshold=filter_threshold)

                    end_run_time = time.time()
                    run_duration = (end_run_time - start_run_time) / 60

                    # Update the dictionary with global variables and script duration
                    results_dict.update({
                        'seed': seed,
                        'filter_low_provFreq': filter_prov,
                        'model_type': model_type,
                        'stratified': stratified,
                        'Optimization': optim,
                        'runTime_mins': run_duration
                    })

                    # Append to CSV
                    df_to_append = pd.DataFrame([results_dict], columns=columns_order)
                    df_to_append.to_csv(results_path, mode='a', header=False, index=False)

end_script_time = time.time()
script_duration = (end_script_time - start_script_time) / 60
print(f"Script run duration: {script_duration:.2f} minutes")
