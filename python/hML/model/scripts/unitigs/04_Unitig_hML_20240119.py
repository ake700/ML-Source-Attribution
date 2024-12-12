#!/usr/bin/env python
# coding: utf-8

# In[10]:


# dependencies 
import networkx as nx
import numpy as np
import pandas as pd
import copy 
import pickle
import time
import pydot
import sys
import os
import gzip
from networkx.drawing.nx_pydot import graphviz_layout

from sklearn.model_selection import train_test_split, RandomizedSearchCV, StratifiedKFold, cross_validate
from sklearn.metrics import accuracy_score, balanced_accuracy_score, confusion_matrix, classification_report

from matplotlib import pyplot as plt

# import hierarchical classification package
import sys
sys.path.append( '../../../HierarchicalML/HC_package_mod')

from HC_PyScripting import *

start_script_time = time.time()

# set variables 
seed = 34

example_outputs = "./model_output/unitig_model_noFilt"
csv_path = 'data/unitig_Heid_ebase_20240208.csv'
graph_input_file = 'data/canada_east_west_graph.pkl'
save_feat_file = 'output/unitig_feature_Heid_ebase.txt'

# Check if output directory exists, and make it if it doesn't
if not os.path.exists(example_outputs):
    os.makedirs(example_outputs)

# In[ ]:


# read features 
meta = pd.read_csv(csv_path)

print(meta['Province'].value_counts(), meta['Geography'].value_counts())

def filter_dataset(meta, min_examples_per_class=25):
    """
    Filters out rows in a dataset based on the number of examples per province.

    Parameters:
    - meta: DataFrame containing the dataset to filter.
    - min_examples_per_class: The minimum number of examples required per province to keep. Default is 25, but can be modified when running the fn

    Returns:
    - meta: Filtered DataFrame.
    """
    # List of provinces to be removed based on min_examples_per_class
    province_counts = meta['Province'].value_counts()
    provinces_to_remove = province_counts[province_counts < min_examples_per_class].index.tolist()

    print("Provinces to remove:", provinces_to_remove)

    # Filter rows based on the values in the 'Province' column
    meta = meta[~meta['Province'].isin(provinces_to_remove)]
    meta.reset_index(drop=True, inplace=True)

    print("Remaining provinces:", meta['Province'].value_counts())
    print("Geography counts:", meta['Geography'].value_counts())
    
    return meta

# Using the function, which can be commented in or out depending on if the model is testing for filtered data
meta = filter_dataset(meta, 2)

features = meta.drop(['Accession', 'Province', 'Geography'], axis = 1)


# In[4]:


# Transform the 'Province' column into a list of labels
labels = meta['Province'].tolist()

# Output the labels
print(len(labels))


# In[8]:


# read graph
with open(graph_input_file, 'rb') as file:
    loaded_hierarchy_graph_data = pickle.load(file)

graph = nx.DiGraph(loaded_hierarchy_graph_data)
graph.nodes


# In[9]:


# Draw the graph
nx.draw(graph, with_labels=True)
plt.show()


# In[ ]:


# split the data into training and testing sets
train_features, test_features, train_labels, test_labels = train_test_split(features, 
                                                                            labels, 
                                                                            test_size = 0.25, 
                                                                            stratify=labels, # stratify on country
                                                                            random_state = seed)


# In[ ]:


# save feature labels 
save_feat = train_features.columns.values
np.savetxt(save_feat_file, save_feat, fmt="%s")


# In[ ]:


# set classifier model for HC 
classifier = RandomForestClassifier(n_estimators = 1000, n_jobs=-1, random_state = seed)


# In[ ]:


# set resampler for HC
resampler = RandomBalancingSampler(sampling_strategy = 'mean', random_state=seed)


# In[ ]:


# fit hierachical classifier
start_time = time.time()
models = fit_hierarchical_classifier(graph, train_labels, train_features, classifier,
                                    subsampler = resampler, verbose = True)
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