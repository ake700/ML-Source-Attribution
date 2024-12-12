#!/usr/bin/env python
# coding: utf-8

# In[1]:


# dependencies 
import networkx as nx
import numpy as np
import pandas as pd
import copy 
import gzip
import pickle
import time
import pydot
import os
from networkx.drawing.nx_pydot import graphviz_layout

from sklearn.model_selection import train_test_split, RandomizedSearchCV, StratifiedKFold, cross_validate
from sklearn.metrics import accuracy_score, balanced_accuracy_score, confusion_matrix, classification_report
from sklearn.preprocessing import OneHotEncoder

from matplotlib import pyplot as plt

# import hierarchical classification package
import sys
sys.path.append( '../../../HierarchicalML/HC_package_mod')
from HC_PyScripting import *

start_script_time = time.time()

# set variables 
seed = 34

example_outputs = "./model_output/allele_model"
csv_path = 'data/enterobase_sistr_combined_20240201.csv'
graph_input_file = 'data/canada_east_west_graph.pkl'
save_feat_file = 'output/allele_feature_Heid_ebase.txt'
strains_of_interest = ['Heidelberg']
column_name = 'Serovar2'
col_remove_csv = 'data/col_remove_heid_ebase.csv'

# Check if output directory exists, and make it if it doesn't
if not os.path.exists(example_outputs):
    os.makedirs(example_outputs)
    
# read features 
df = pd.read_csv(csv_path)
df = df[(df['qc_status'] != 'FAIL') & (df['Accession2'] != 'SRR1183798')]

df = df[df[column_name].isin(strains_of_interest)]

print(df[column_name].value_counts())

# Select 'Accession' and 'Region' columns, and columns from index 53 onwards
selected_columns = df.loc[:, ['Accession2', 'Region', 'Serovar2', 'serovar_cgmlst']]
additional_columns = df.iloc[:, 53:3055]

# Combine the selected columns into a new DataFrame
allele = pd.concat([selected_columns, additional_columns], axis=1)

allele.replace({-1:0, '-1':0, '-':0}, inplace=True) # Set instances 'no gene present' to 0

def remove_columns(dataframe, columns_file):
    """
    Removes a list of columns from a DataFrame based on a provided CSV file.

    Parameters:
    - dataframe: The DataFrame from which columns will be removed.
    - columns_file: Path to the CSV file containing column names to remove.

    Returns:
    - The DataFrame after removing specified columns.
    """
    columns_to_remove = pd.read_csv(columns_file, header=None).iloc[:, 0].tolist()
    modified_df = dataframe.drop(columns=columns_to_remove)
    print(f"This is shape after removing: {modified_df.shape}")
    return modified_df

allele = remove_columns(allele, col_remove_csv)

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

allele['Region'] = allele['Region'].replace(rename_dict)

# Copy df to prevent fragmentation
allele_copy = allele.copy()

# Rename columns
allele_copy = allele_copy.rename(columns={'Region': 'Province'})

# Create mapping for direction
direction_mapping = {province: 'East' for province in ['NB', 'NL', 'NS', 'ON', 'PE', 'QC']}
direction_mapping.update({province: 'West' for province in ['BC', 'AB', 'SK', 'MB', 'NT', 'NU', 'YT']})

# Apply mapping to create 'direction' column
allele_copy['Geography'] = allele_copy['Province'].map(direction_mapping)

# Get the position of the 'Province' column in the copied DataFrame
province_index = allele_copy.columns.get_loc('Province')

# Insert the 'Geography' column after 'Province' in the copied DataFrame
allele_copy.insert(province_index + 1, 'Geography', allele_copy.pop('Geography'))

print(allele_copy['Province'].value_counts(), allele_copy['Geography'].value_counts())

def filter_dataset(allele_copy, min_examples_per_class=25):
    """
    Filters out rows in a dataset based on the number of examples per province.

    Parameters:
    - allele or allele_copy: DataFrame containing the dataset to filter.
    - min_examples_per_class: The minimum number of examples required per province to keep. Default is 25, but can be modified when running the fn

    Returns:
    - allele or allele_copy: Filtered DataFrame.
    """
    # List of provinces to be removed based on min_examples_per_class
    province_counts = allele_copy['Province'].value_counts()
    provinces_to_remove = province_counts[province_counts < min_examples_per_class].index.tolist()

    print("Provinces to remove:", provinces_to_remove)

    # Filter rows based on the values in the 'Province' column
    allele_copy = allele_copy[~allele_copy['Province'].isin(provinces_to_remove)]
    allele_copy.reset_index(drop=True, inplace=True)

    print("Remaining provinces:", allele_copy['Province'].value_counts())
    print("Geography counts:", allele_copy['Geography'].value_counts())
    
    return allele_copy

# Using the function, which can be commented in or out depending on if the model is testing for filtered data
allele_copy = filter_dataset(allele_copy, 2)

def encode_and_concat(dataframe, features):
    """
    Converts selected columns to categorical, applies one-hot encoding, and concatenates
    with the original DataFrame's unencoded columns.

    Parameters:
    - dataframe: The DataFrame to process.

    Returns:
    - The final concatenated DataFrame after encoding.
    """
    features = features.astype(str)
    
    cols_to_convert = dataframe.columns.difference(['Accession2', 'Province', 'Geography', 'Serovar2', 'serovar_cgmlst'])
    for col in cols_to_convert:
        dataframe[col] = dataframe[col].astype('category').astype(str)
    
    encoder = OneHotEncoder(sparse=False)
    feat_encoded = encoder.fit_transform(features)
    encoded_columns = encoder.get_feature_names_out()
    encoded_df = pd.DataFrame(feat_encoded, columns=encoded_columns)
    
    final_df = pd.concat([dataframe[['Accession2', 'Province', 'Geography', 'Serovar2', 'serovar_cgmlst']].reset_index(drop=True), 
                          encoded_df.reset_index(drop=True)], axis=1)
    
    print(final_df.shape)
    print(final_df.head())
    return final_df

features = allele_copy.drop(['Accession2', 'Province', 'Geography', 'Serovar2', 'serovar_cgmlst'], axis=1)

# # Comment out the two lines below if trying OHE
# final_df = encode_and_concat(allele_copy, features)
# features = final_df.drop(['Accession2', 'Province', 'Geography', 'Serovar2', 'serovar_cgmlst'], axis=1)

# Transform the 'Province' column into a list of labels
labels = allele_copy['Province'].tolist()

# Output the labels
print(len(labels))

label_counts = pd.Series(labels).value_counts()
print(label_counts)


# read graph
with open(graph_input_file, 'rb') as file:
    loaded_hierarchy_graph_data = pickle.load(file)

graph = nx.DiGraph(loaded_hierarchy_graph_data)
graph.nodes

# Draw the graph
nx.draw(graph, with_labels=True)
plt.show()


# In[12]:


# split the data into training and testing sets
train_features, test_features, train_labels, test_labels = train_test_split(features, 
                                                                            labels, 
                                                                            test_size = 0.25, 
                                                                            stratify=labels, # stratify on country
                                                                            random_state = seed)


# In[15]:


# Assuming train_labels is a list or a pandas Series
label_distribution = pd.Series(train_labels).value_counts()
print(label_distribution)


# In[17]:


# save feature labels 
save_feat = train_features.columns.values
np.savetxt(save_feat_file, save_feat, fmt="%s")


# In[18]:


# set classifier model for HC 
classifier = RandomForestClassifier(n_estimators = 1000, n_jobs=-1, random_state = seed)


# In[24]:


# set resampler for HC
resampler = RandomBalancingSampler(sampling_strategy = 'mean', random_state=seed)


# In[25]:


# fit hierachical classifier
start_time = time.time()
models = fit_hierarchical_classifier(graph, train_labels, train_features, classifier,
                                     subsampler = resampler,
                                      verbose = True)
train_time = time.time() - start_time
print(" - Training Time(s): ", train_time)


# In[ ]:


# save trained models
with gzip.open("%s/models.pkl.gz" % example_outputs, 'wb') as file:
    pickle.dump(models, file, protocol = 4)


# In[ ]:


# classify and summarise training data
(classification_table_train, classifications_train) = classify_samples_in_hierarchy(graph, train_features, 
                                                                                   models,
                                                                                   mode = 'max', 
                                                                                   threshold = 0.51, 
                                                                                   verbose = True)


# In[ ]:


(summary_train, summary_table_train) = summary_statistics_per_class(graph, train_labels, 
                                                                    classifications_train, 
                                                                    penalty=False)


# In[ ]:


# save training summaries
summary_table_train.to_csv("%s/training_summary.tsv" % example_outputs, 
                           sep = "\t", header = True, index = False)


# In[ ]:


# classify and summarise test data
(classification_table_test, classifications_test) = classify_samples_in_hierarchy(graph, test_features, 
                                                                                   models,
                                                                                   mode = 'max', 
                                                                                   threshold = 0.51)

(summary_test, summary_table_test) = summary_statistics_per_class(graph, test_labels, 
                                                                  classifications_test, 
                                                                  penalty=False)


# In[ ]:


# save test summary
summary_table_test.to_csv("%s/test_summary.tsv" % example_outputs, 
                           sep = "\t", header = True, index = False)


# In[ ]:


# access non-hierarchical statistics per node
(per_node, per_class, clf_reports) = per_node_summary_stats(graph, test_labels, test_features, models, verbose = True)


# In[ ]:


# save per node summary
per_node.to_csv("%s/per_node_summary.tsv" % example_outputs, 
                           sep = "\t", header = True, index = False)

# save per class summary
per_class.to_csv("%s/per_class_summary.tsv" % example_outputs, 
                           sep = "\t", header = True, index = False)


# In[ ]:


# generate overall hierachical summary stats (for entire dataset)
(h_summary) = overall_summary_stats(test_labels, classifications_test, graph, penalty=False)
print(h_summary)

end_script_time = time.time()
script_duration = (end_script_time - start_script_time) / 60
print(f"Script run duration: {script_duration:.2f} minutes")