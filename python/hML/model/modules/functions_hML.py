#!/usr/bin/env python
# coding: utf-8

import pandas as pd
import imblearn
from imblearn.over_sampling import RandomOverSampler
from imblearn.under_sampling import RandomUnderSampler
from collections import Counter

import os
import time
import numpy as np
import yaml
import glob
import sys
import gzip
import pickle

# Machine Learning models
import sklearn
from sklearn.model_selection import train_test_split, RandomizedSearchCV, StratifiedKFold, cross_validate
from sklearn.metrics import accuracy_score, balanced_accuracy_score, confusion_matrix, classification_report
from sklearn.preprocessing import OneHotEncoder
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.pipeline import make_pipeline
from sklearn.svm import SVC
from logitboost import LogitBoost

# Model selection and validation tools
from sklearn.model_selection import train_test_split, cross_val_score, GridSearchCV, RandomizedSearchCV

# Metrics to evaluate model performance
from sklearn.metrics import (
    confusion_matrix, 
    accuracy_score, 
    balanced_accuracy_score, 
    precision_score, 
    recall_score, 
    f1_score, 
    make_scorer
)

sys.path.append( './Modules')
from HC_PyScripting import *

# Function to load the YAML configuration
def load_config(config_path):
    with open(config_path, 'r') as file:
        return yaml.safe_load(file)

config_path = './model_config_hML.yml' 
config = load_config(config_path)
seed = config['setup']['seed'][0]
data_dir = config['setup']['file_paths']['data_dir']

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
    print(f"This is shape after removing nearzerovar: {modified_df.shape}")
    return modified_df

def filter_dataset(df, y_col, min_examples_per_class=25):
    """
    Filters out rows in a dataset based on the number of examples per province.

    Parameters:
    - allele or allele_copy: DataFrame containing the dataset to filter.
    - min_examples_per_class: The minimum number of examples required per province to keep. Default is 25, but can be modified when running the fn

    Returns:
    - allele or allele_copy: Filtered DataFrame.
    """
    # List of provinces to be removed based on min_examples_per_class
    y_col_counts = df[y_col].value_counts()
    labels_to_remove = y_col_counts[y_col_counts < min_examples_per_class].index.tolist()

    print("Labels to remove:", labels_to_remove)

    # Filter rows based on the values in the 'Province' column
    df = df[~df[y_col].isin(labels_to_remove)]
    df.reset_index(drop=True, inplace=True)

    print("Remaining labels:", df[y_col].value_counts())
    
    return df, labels_to_remove

def encode_and_concat(dataframe, cols_encode):
    """
    Converts selected columns to categorical, applies one-hot encoding, and concatenates
    with the original DataFrame's unencoded columns.

    Parameters:
    - dataframe: The DataFrame to process.

    Returns:
    - The final concatenated DataFrame after encoding.
    """
    cols_to_convert = [col for col in cols_encode if col in dataframe.columns]
    
    for col in cols_encode:
        if col in dataframe.columns:
            dataframe.loc[:, col] = dataframe[col].astype('category').astype(str)
    
    encoder = OneHotEncoder(sparse_output=False)
    feat_encoded = encoder.fit_transform(dataframe[cols_encode])
    encoded_columns = encoder.get_feature_names_out(cols_encode)
    encoded_df = pd.DataFrame(feat_encoded, columns=encoded_columns)
    
    final_df = pd.concat([dataframe.drop(cols_encode, axis=1).reset_index(drop=True), 
                          encoded_df.reset_index(drop=True)], axis=1)
   
    print(final_df.head(), final_df.shape)
    return final_df

def feat_eng_data(df, method, cols_remove=None):
    """
    Performs feature engineering on the given DataFrame according to the specified method.

    Parameters:
    - df: The input DataFrame.
    - method (str): Feature engineering method ('OHE', 'nzv', 'nzv_OHE', or none).
    - cols_remove (list or None): Columns to remove if method is 'nzv' or 'nzv_OHE'.

    Returns:
    - DataFrame: The transformed DataFrame after feature engineering.
    """
    if method == 'OHE':
        cols_encode = df.columns.difference(['Key', 'Province', 'Geography', 'Serovar', 'SourceSite', 'CombinedSites',
                                             'CombinedSite2']).tolist()
        return encode_and_concat(df, cols_encode)
    elif method in ['nzv', 'nzv_OHE']:
        assert cols_remove is not None, "File for cols to remove not provided"
        df = remove_columns(df, cols_remove)
        if method == 'nzv_OHE':
            cols_encode = df.columns.difference(['Key', 'Province', 'Geography', 'Serovar', 'SourceSite', 'CombinedSites',
                                                 'CombinedSite2']).tolist()
            return encode_and_concat(df, cols_encode)
        return df
    else:
        return df 
    
def save_model(best_model, model_type, serovar, db, test_size, method, dataType):
    """
    Saves the trained model to disk as a joblib file.

    Parameters:
    - best_model: The trained model to save.
    - model_type (str): Type of model (e.g., 'RandomForest').
    - serovar (str): Serovar name used in the filename.
    - db (str): Database or dataset name.
    - test_size (float/int): Test split proportion/size.
    - method (str): Additional method info (e.g., sampling method).
    - dataType (str): The type of data used.

    Returns:
    - None
    """
    base_path = config['setup']['models'][model_type]['save_model_path']
    
    model_filename = f"{dataType}_{serovar}_{db}_{test_size}_{method}.joblib"
    model_path = os.path.join(base_path, model_filename)
    
    # Save model
    joblib.dump(best_model, model_path)
    print(f"Model saved to {model_path}")

"""
The following functions are to run the ML models. Docstring is provided for the Random FOrest with allele data, but the same concept applies to the rest using unitig data and/or different models (GradientBoostingClassifier, Support Vector Machine, and Logit Boost)
"""
def hml_rf_allele(df, column_name, serovar, test_size, graph, db='default', y_col='Province', dataType='allele', feat_eng=None, filter_threshold=2): 
    """
    Trains a hierarchical multi-level Random Forest classifier on allele data, 
    applying optional feature engineering and filtering, then evaluates the 
    hierarchical predictions. Saves various summaries and model outputs.

    Parameters:
    - df: The input DataFrame containing features and labels.
    - column_name (str): The column key used to look up the serovar mapping in the config.
    - serovar (str): The serovar to process.
    - test_size (float or int): The proportion of the dataset to include in the test split.
    - graph: A hierarchical structure (graph) defining the classification hierarchy.
    - db (str, optional): The database/serovar calling context (default: 'default').
    - y_col (str, optional): The target variable column name (default: 'Province').
    - dataType (str, optional): The type of data (default: 'allele').
    - feat_eng (str, optional): Feature engineering method to apply ('OHE', 'nzv', 'nzv_OHE', or 'None').
    - filter_threshold (int, optional): Threshold to remove classes with fewer samples.

    Returns:
    - dict: A dictionary containing metadata and performance metrics (F1, Accuracy, 
            hierarchical metrics, feature info, serovar info, and more).
    """
    results = []
    serovar_path = config['column_strains'][column_name][serovar]
    col_remove_path = os.path.join(data_dir, serovar_path)
    file_nzv_path = glob.glob(os.path.join(col_remove_path, 'col*.csv'))
    base_classifier = RandomForestClassifier(random_state=seed, n_jobs=config['setup']['n_jobs'][0])
    resampler = RandomBalancingSampler(sampling_strategy = 'mean', random_state=seed)       
    example_outputs = config['setup']['file_paths']['example_outputs']['allele']
    save_feat_file = config['setup']['file_paths']['save_feat_output']['unitig']
    dataType_mod = f"{feat_eng}_{dataType}" if feat_eng and feat_eng != 'None' else dataType
    
    # Feature engineering with nzv, OHE, or nzv_OHE
    if file_nzv_path:  # Ensure at least one file was found
        cols_remove = file_nzv_path[0]  # Select the first file path
        print(f"File to remove columns is {cols_remove}")
    else:
        print("No NZV file found in the specified path.")  
        
    if y_col == 'Province':
        df_filtered = df[(df[y_col] != 'Not Available')]
    else: 
        df_filtered = df
        
    df_filtered, labels_to_remove = filter_dataset(df_filtered, y_col, filter_threshold)
    
    if feat_eng != 'None':
        print(f"Applying feature engineering: {feat_eng}")
        df_filtered = feat_eng_data(df_filtered, feat_eng, cols_remove)
    else:
        print("No feature engineering applied.")
        
    # Adjust to handle filtered dfs or no filter
    if not df_filtered.empty:
        if column_name != "None":
            db = "FoodNet" if column_name == "Serovar" else "Sero_calling not found"
            print(f"Running {serovar} from {db} using test size {test_size}")
        else:
            print(f"Running on the full dataset using test size {test_size}")

        X = df_filtered.drop(['Key', 'Province', 'Geography', 'Serovar', 'SourceSite', 'CombinedSites',
                              'CombinedSite2'], axis=1)
        y = df_filtered[y_col].tolist()
        
        if y_col == 'Province':
            pred_label = 'Province'
        elif y_col == 'CombinedSites':
            pred_label = 'Animal type'
        elif y_col == 'CombinedSite2':
            pred_label = 'Location or Food'
        else:
            pred_label = 'None'
            
        print(f"Number of features: {X.shape[1]}")
        print(f"Number of rows: {len(y)}")

        X_train, X_test, y_train, y_test = train_test_split(X, y,
                                                            test_size=test_size, 
                                                            stratify=y,
                                                            random_state=seed)
        
        # Assuming train_labels is a list or a pandas Series
        label_distribution = pd.Series(y_train).value_counts()
        print(label_distribution)
        
        save_feat = X_train.columns.values
        np.savetxt(save_feat_file, save_feat, fmt="%s")
        
        # fit hierachical classifier
        models = fit_hierarchical_classifier(graph, y_train, X_train, base_classifier,
                                             subsampler = resampler,
                                              verbose = True)
        
        with gzip.open("%s/models.pkl.gz" % example_outputs, 'wb') as file:
            pickle.dump(models, file, protocol = 4)
        
        # classify and summarise training data
        classification_table_train, classifications_train = classify_samples_in_hierarchy(graph, X_train, 
                                                                                   models,
                                                                                   mode = 'max', 
                                                                                   threshold = 0.51, 
                                                                                   verbose = True)

        summary_train, summary_table_train = summary_statistics_per_class(graph, y_train, 
                                                                    classifications_train, 
                                                                    penalty=False)
        
        summary_table_train.to_csv("%s/training_summary.tsv" % example_outputs, 
                           sep = "\t", header = True, index = False)
        
        classification_table_test, classifications_test = classify_samples_in_hierarchy(graph, X_test, 
                                                                                   models,
                                                                                   mode = 'max', 
                                                                                   threshold = 0.51)

        summary_test, summary_table_test = summary_statistics_per_class(graph, y_test, 
                                                                  classifications_test, 
                                                                  penalty=False)
        
        summary_table_test.to_csv("%s/test_summary.tsv" % example_outputs, 
                           sep = "\t", header = True, index = False)
        
        per_node, per_class, clf_reports = per_node_summary_stats(graph, y_test, X_test, models, verbose = True)

        # save per node summary
        per_node.to_csv("%s/per_node_summary.tsv" % example_outputs, 
                                   sep = "\t", header = True, index = False)

        # save per class summary
        per_class.to_csv("%s/per_class_summary.tsv" % example_outputs, 
                                   sep = "\t", header = True, index = False)
        
        # generate overall hierachical summary stats (for entire dataset)
        h_summary = overall_summary_stats(y_test, classifications_test, graph, penalty=False)
        print(h_summary)
           
        results = {
            'dataType': dataType_mod,
            'Sero_calling': db,
            'Pred_label': pred_label, 
            'Serovar': serovar,
            'train_test_split': test_size,
            'n_original_features': X.shape[1],
            'n_features_used': X_train.shape[1],
            'filter_threshold': filter_threshold,
            'label_filtered': labels_to_remove,
            'n': len(y),
            'sampling_strat': 'RandomBalancingSampler',
            'Recall': h_summary['hR'],
            'Precision': h_summary['hP'],
            'F1': h_summary['hF1'],
            'Accuracy': h_summary['hAcc']
        }
        
    else: 
        print(f"No data found for {serovar} in {column_name} after filtering")

    return results
    
def hml_rf_unitig(file_path, serovar, db, test_size, y_col='Province', dataType='unitig', method='random_upsampling', filter_threshold=2, feat_elim_method=None):
    results=[]
    base_classifier = RandomForestClassifier(random_state=seed, n_jobs=config['setup']['n_jobs'][0])
    resampler = RandomBalancingSampler(sampling_strategy = 'mean', random_state=seed)
    example_outputs = config['setup']['file_paths']['example_outputs']['unitig']
    save_feat_file = config['setup']['file_paths']['save_feat_output']['unitig']
    
    meta = pd.read_csv(file_path, low_memory=False)
    
    if y_col == 'Province':
        meta = meta[(meta[y_col] != 'Not Available')]
    else: 
        meta = meta

    meta, labels_to_remove = filter_dataset(meta, y_col, filter_threshold)
        
    X = meta.drop(['Key', 'Province', 'Geography'], axis = 1)
    y = meta[y_col].tolist()
    
    if y_col == 'Province':
        pred_label = 'Province'
    elif y_col == 'CombinedSites':
        pred_label = 'Animal type'
    elif y_col == 'CombinedSite2':
        pred_label = 'Location or Food'
    else:
        pred_label = 'None'
            
    print(f"Number of features: {X.shape[1]}")
    print(f"Number of rows: {len(y)}")

    X_train, X_test, y_train, y_test = train_test_split(X, y,
                                                        test_size=test_size, 
                                                        stratify=y,
                                                        random_state=seed)

    # Assuming train_labels is a list or a pandas Series
    label_distribution = pd.Series(y_train).value_counts()
    print(label_distribution)

    save_feat = X_train.columns.values
    np.savetxt(save_feat_file, save_feat, fmt="%s")

    # fit hierachical classifier
    models = fit_hierarchical_classifier(graph, y_train, X_train, base_classifier,
                                         subsampler = resampler,
                                          verbose = True)

    with gzip.open("%s/models.pkl.gz" % example_outputs, 'wb') as file:
        pickle.dump(models, file, protocol = 4)

    # classify and summarise training data
    classification_table_train, classifications_train = classify_samples_in_hierarchy(graph, X_train, 
                                                                               models,
                                                                               mode = 'max', 
                                                                               threshold = 0.51, 
                                                                               verbose = True)

    summary_train, summary_table_train = summary_statistics_per_class(graph, y_train, 
                                                                classifications_train, 
                                                                penalty=False)

    summary_table_train.to_csv("%s/training_summary.tsv" % example_outputs, 
                       sep = "\t", header = True, index = False)

    classification_table_test, classifications_test = classify_samples_in_hierarchy(graph, X_test, 
                                                                               models,
                                                                               mode = 'max', 
                                                                               threshold = 0.51)

    summary_test, summary_table_test = summary_statistics_per_class(graph, y_test, 
                                                              classifications_test, 
                                                              penalty=False)

    summary_table_test.to_csv("%s/test_summary.tsv" % example_outputs, 
                       sep = "\t", header = True, index = False)

    per_node, per_class, clf_reports = per_node_summary_stats(graph, y_test, X_test, models, verbose = True)

    # save per node summary
    per_node.to_csv("%s/per_node_summary.tsv" % example_outputs, 
                               sep = "\t", header = True, index = False)

    # save per class summary
    per_class.to_csv("%s/per_class_summary.tsv" % example_outputs, 
                               sep = "\t", header = True, index = False)

    # generate overall hierachical summary stats (for entire dataset)
    h_summary = overall_summary_stats(y_test, classifications_test, graph, penalty=False)
    print(h_summary)

    results = {
        'dataType': dataType,
        'Sero_calling': db,
        'Pred_label': pred_label, 
        'Serovar': serovar,
        'train_test_split': test_size,
        'n_original_features': X.shape[1],
        'n_features_used': X_train.shape[1],
        'filter_threshold': filter_threshold,
        'label_filtered': labels_to_remove,
        'n': len(y),
        'sampling_strat': 'RandomBalancingSampler',
        'Recall': h_summary['hR'],
        'Precision': h_summary['hP'],
        'F1': h_summary['hF1'],
        'Accuracy': h_summary['hAcc']
    }

    return results    

def hml_svm_allele(df, column_name, serovar, test_size, graph, kernel='linear', db='default', y_col='Province', dataType='allele', feat_eng=None, filter_threshold=2): 
    results = []
    serovar_path = config['column_strains'][column_name][serovar]
    col_remove_path = os.path.join(data_dir, serovar_path)
    file_nzv_path = glob.glob(os.path.join(col_remove_path, 'col*.csv'))
    base_classifier = make_pipeline(StandardScaler(), SVC(probability=True, random_state=seed, kernel = kernel))
    resampler = RandomBalancingSampler(sampling_strategy = 'mean', random_state=seed)       
    example_outputs = config['setup']['file_paths']['example_outputs']['allele']
    save_feat_file = config['setup']['file_paths']['save_feat_output']['unitig']
    dataType_mod = f"{feat_eng}_{dataType}" if feat_eng and feat_eng != 'None' else dataType
    
    # Feature engineering with nzv, OHE, or nzv_OHE
    if file_nzv_path:  # Ensure at least one file was found
        cols_remove = file_nzv_path[0]  # Select the first file path
        print(f"File to remove columns is {cols_remove}")
    else:
        print("No NZV file found in the specified path.")  
        
    if y_col == 'Province':
        df_filtered = df[(df[y_col] != 'Not Available')]
    else: 
        df_filtered = df
        
    df_filtered, labels_to_remove = filter_dataset(df_filtered, y_col, filter_threshold)
    
    if feat_eng != 'None':
        print(f"Applying feature engineering: {feat_eng}")
        df_filtered = feat_eng_data(df_filtered, feat_eng, cols_remove)
    else:
        print("No feature engineering applied.")
        
    # Adjust to handle filtered dfs or no filter
    if not df_filtered.empty:
        if column_name != "None":
            db = "FoodNet" if column_name == "Serovar" else "Sero_calling not found"
            print(f"Running {serovar} from {db} using test size {test_size}")
        else:
            print(f"Running on the full dataset using test size {test_size}")

        X = df_filtered.drop(['Key', 'Province', 'Geography', 'Serovar', 'SourceSite', 'CombinedSites',
                              'CombinedSite2'], axis=1)
        y = df_filtered[y_col].tolist()
        
        if y_col == 'Province':
            pred_label = 'Province'
        elif y_col == 'CombinedSites':
            pred_label = 'Animal type'
        elif y_col == 'CombinedSite2':
            pred_label = 'Location or Food'
        else:
            pred_label = 'None'
            
        print(f"Number of features: {X.shape[1]}")
        print(f"Number of rows: {len(y)}")

        X_train, X_test, y_train, y_test = train_test_split(X, y,
                                                            test_size=test_size, 
                                                            stratify=y,
                                                            random_state=seed)
        
        # Assuming train_labels is a list or a pandas Series
        label_distribution = pd.Series(y_train).value_counts()
        print(label_distribution)
        
        save_feat = X_train.columns.values
        np.savetxt(save_feat_file, save_feat, fmt="%s")
        
        # fit hierachical classifier
        models = fit_hierarchical_classifier(graph, y_train, X_train, base_classifier,
                                             subsampler = resampler,
                                              verbose = True)
        
        with gzip.open("%s/models.pkl.gz" % example_outputs, 'wb') as file:
            pickle.dump(models, file, protocol = 4)
        
        # classify and summarise training data
        classification_table_train, classifications_train = classify_samples_in_hierarchy(graph, X_train, 
                                                                                   models,
                                                                                   mode = 'max', 
                                                                                   threshold = 0.51, 
                                                                                   verbose = True)

        summary_train, summary_table_train = summary_statistics_per_class(graph, y_train, 
                                                                    classifications_train, 
                                                                    penalty=False)
        
        summary_table_train.to_csv("%s/training_summary.tsv" % example_outputs, 
                           sep = "\t", header = True, index = False)
        
        classification_table_test, classifications_test = classify_samples_in_hierarchy(graph, X_test, 
                                                                                   models,
                                                                                   mode = 'max', 
                                                                                   threshold = 0.51)

        summary_test, summary_table_test = summary_statistics_per_class(graph, y_test, 
                                                                  classifications_test, 
                                                                  penalty=False)
        
        summary_table_test.to_csv("%s/test_summary.tsv" % example_outputs, 
                           sep = "\t", header = True, index = False)
        
        per_node, per_class, clf_reports = per_node_summary_stats(graph, y_test, X_test, models, verbose = True)

        # save per node summary
        per_node.to_csv("%s/per_node_summary.tsv" % example_outputs, 
                                   sep = "\t", header = True, index = False)

        # save per class summary
        per_class.to_csv("%s/per_class_summary.tsv" % example_outputs, 
                                   sep = "\t", header = True, index = False)
        
        # generate overall hierachical summary stats (for entire dataset)
        h_summary = overall_summary_stats(y_test, classifications_test, graph, penalty=False)
        print(h_summary)
           
        results = {
            'dataType': dataType_mod,
            'Sero_calling': db,
            'Pred_label': pred_label, 
            'Serovar': serovar,
            'train_test_split': test_size,
            'n_original_features': X.shape[1],
            'n_features_used': X_train.shape[1],
            'filter_threshold': filter_threshold,
            'label_filtered': labels_to_remove,
            'n': len(y),
            'sampling_strat': 'RandomBalancingSampler',
            'Recall': h_summary['hR'],
            'Precision': h_summary['hP'],
            'F1': h_summary['hF1'],
            'Accuracy': h_summary['hAcc']
        }
        
    else: 
        print(f"No data found for {serovar} in {column_name} after filtering")

    return results
    
def hml_svm_unitig(file_path, serovar, db, test_size, kernel='linear', y_col='Province', dataType='unitig', method='random_upsampling', filter_threshold=2, feat_elim_method=None):
    results=[]
    base_classifier = make_pipeline(StandardScaler(), SVC(probability=True, random_state=seed, kernel = kernel))
    resampler = RandomBalancingSampler(sampling_strategy = 'mean', random_state=seed)
    example_outputs = config['setup']['file_paths']['example_outputs']['unitig']
    save_feat_file = config['setup']['file_paths']['save_feat_output']['unitig']
    
    meta = pd.read_csv(file_path, low_memory=False)
    
    if y_col == 'Province':
        meta = meta[(meta[y_col] != 'Not Available')]
    else: 
        meta = meta

    meta, labels_to_remove = filter_dataset(meta, y_col, filter_threshold)
        
    X = meta.drop(['Key', 'Province', 'Geography'], axis = 1)
    y = meta[y_col].tolist()
    
    if y_col == 'Province':
        pred_label = 'Province'
    elif y_col == 'CombinedSites':
        pred_label = 'Animal type'
    elif y_col == 'CombinedSite2':
        pred_label = 'Location or Food'
    else:
        pred_label = 'None'
            
    print(f"Number of features: {X.shape[1]}")
    print(f"Number of rows: {len(y)}")

    X_train, X_test, y_train, y_test = train_test_split(X, y,
                                                        test_size=test_size, 
                                                        stratify=y,
                                                        random_state=seed)

    # Assuming train_labels is a list or a pandas Series
    label_distribution = pd.Series(y_train).value_counts()
    print(label_distribution)

    save_feat = X_train.columns.values
    np.savetxt(save_feat_file, save_feat, fmt="%s")

    # fit hierachical classifier
    models = fit_hierarchical_classifier(graph, y_train, X_train, base_classifier,
                                         subsampler = resampler,
                                          verbose = True)

    with gzip.open("%s/models.pkl.gz" % example_outputs, 'wb') as file:
        pickle.dump(models, file, protocol = 4)

    # classify and summarise training data
    classification_table_train, classifications_train = classify_samples_in_hierarchy(graph, X_train, 
                                                                               models,
                                                                               mode = 'max', 
                                                                               threshold = 0.51, 
                                                                               verbose = True)

    summary_train, summary_table_train = summary_statistics_per_class(graph, y_train, 
                                                                classifications_train, 
                                                                penalty=False)

    summary_table_train.to_csv("%s/training_summary.tsv" % example_outputs, 
                       sep = "\t", header = True, index = False)

    classification_table_test, classifications_test = classify_samples_in_hierarchy(graph, X_test, 
                                                                               models,
                                                                               mode = 'max', 
                                                                               threshold = 0.51)

    summary_test, summary_table_test = summary_statistics_per_class(graph, y_test, 
                                                              classifications_test, 
                                                              penalty=False)

    summary_table_test.to_csv("%s/test_summary.tsv" % example_outputs, 
                       sep = "\t", header = True, index = False)

    per_node, per_class, clf_reports = per_node_summary_stats(graph, y_test, X_test, models, verbose = True)

    # save per node summary
    per_node.to_csv("%s/per_node_summary.tsv" % example_outputs, 
                               sep = "\t", header = True, index = False)

    # save per class summary
    per_class.to_csv("%s/per_class_summary.tsv" % example_outputs, 
                               sep = "\t", header = True, index = False)

    # generate overall hierachical summary stats (for entire dataset)
    h_summary = overall_summary_stats(y_test, classifications_test, graph, penalty=False)
    print(h_summary)

    results = {
        'dataType': dataType,
        'Sero_calling': db,
        'Pred_label': pred_label, 
        'Serovar': serovar,
        'train_test_split': test_size,
        'n_original_features': X.shape[1],
        'n_features_used': X_train.shape[1],
        'filter_threshold': filter_threshold,
        'label_filtered': labels_to_remove,
        'n': len(y),
        'sampling_strat': 'RandomBalancingSampler',
        'Recall': h_summary['hR'],
        'Precision': h_summary['hP'],
        'F1': h_summary['hF1'],
        'Accuracy': h_summary['hAcc']
    }

    return results    

def hml_lb_allele(df, column_name, serovar, test_size, graph, db='default', y_col='Province', dataType='allele', feat_eng=None, filter_threshold=2): 
    results = []
    serovar_path = config['column_strains'][column_name][serovar]
    col_remove_path = os.path.join(data_dir, serovar_path)
    file_nzv_path = glob.glob(os.path.join(col_remove_path, 'col*.csv'))
    base_classifier = LogitBoost(random_state=seed)
    resampler = RandomBalancingSampler(sampling_strategy = 'mean', random_state=seed)       
    example_outputs = config['setup']['file_paths']['example_outputs']['allele']
    save_feat_file = config['setup']['file_paths']['save_feat_output']['unitig']
    dataType_mod = f"{feat_eng}_{dataType}" if feat_eng and feat_eng != 'None' else dataType
    
    # Feature engineering with nzv, OHE, or nzv_OHE
    if file_nzv_path:  # Ensure at least one file was found
        cols_remove = file_nzv_path[0]  # Select the first file path
        print(f"File to remove columns is {cols_remove}")
    else:
        print("No NZV file found in the specified path.")  
        
    if y_col == 'Province':
        df_filtered = df[(df[y_col] != 'Not Available')]
    else: 
        df_filtered = df
        
    df_filtered, labels_to_remove = filter_dataset(df_filtered, y_col, filter_threshold)
    
    if feat_eng != 'None':
        print(f"Applying feature engineering: {feat_eng}")
        df_filtered = feat_eng_data(df_filtered, feat_eng, cols_remove)
    else:
        print("No feature engineering applied.")
        
    # Adjust to handle filtered dfs or no filter
    if not df_filtered.empty:
        if column_name != "None":
            db = "FoodNet" if column_name == "Serovar" else "Sero_calling not found"
            print(f"Running {serovar} from {db} using test size {test_size}")
        else:
            print(f"Running on the full dataset using test size {test_size}")

        X = df_filtered.drop(['Key', 'Province', 'Geography', 'Serovar', 'SourceSite', 'CombinedSites',
                              'CombinedSite2'], axis=1)
        y = df_filtered[y_col].tolist()
        
        if y_col == 'Province':
            pred_label = 'Province'
        elif y_col == 'CombinedSites':
            pred_label = 'Animal type'
        elif y_col == 'CombinedSite2':
            pred_label = 'Location or Food'
        else:
            pred_label = 'None'
            
        print(f"Number of features: {X.shape[1]}")
        print(f"Number of rows: {len(y)}")

        X_train, X_test, y_train, y_test = train_test_split(X, y,
                                                            test_size=test_size, 
                                                            stratify=y,
                                                            random_state=seed)
        
        # Assuming train_labels is a list or a pandas Series
        label_distribution = pd.Series(y_train).value_counts()
        print(label_distribution)
        
        save_feat = X_train.columns.values
        np.savetxt(save_feat_file, save_feat, fmt="%s")
        
        # fit hierachical classifier
        models = fit_hierarchical_classifier(graph, y_train, X_train, base_classifier,
                                             subsampler = resampler,
                                              verbose = True)
        
        with gzip.open("%s/models.pkl.gz" % example_outputs, 'wb') as file:
            pickle.dump(models, file, protocol = 4)
        
        # classify and summarise training data
        classification_table_train, classifications_train = classify_samples_in_hierarchy(graph, X_train, 
                                                                                   models,
                                                                                   mode = 'max', 
                                                                                   threshold = 0.51, 
                                                                                   verbose = True)

        summary_train, summary_table_train = summary_statistics_per_class(graph, y_train, 
                                                                    classifications_train, 
                                                                    penalty=False)
        
        summary_table_train.to_csv("%s/training_summary.tsv" % example_outputs, 
                           sep = "\t", header = True, index = False)
        
        classification_table_test, classifications_test = classify_samples_in_hierarchy(graph, X_test, 
                                                                                   models,
                                                                                   mode = 'max', 
                                                                                   threshold = 0.51)

        summary_test, summary_table_test = summary_statistics_per_class(graph, y_test, 
                                                                  classifications_test, 
                                                                  penalty=False)
        
        summary_table_test.to_csv("%s/test_summary.tsv" % example_outputs, 
                           sep = "\t", header = True, index = False)
        
        per_node, per_class, clf_reports = per_node_summary_stats(graph, y_test, X_test, models, verbose = True)

        # save per node summary
        per_node.to_csv("%s/per_node_summary.tsv" % example_outputs, 
                                   sep = "\t", header = True, index = False)

        # save per class summary
        per_class.to_csv("%s/per_class_summary.tsv" % example_outputs, 
                                   sep = "\t", header = True, index = False)
        
        # generate overall hierachical summary stats (for entire dataset)
        h_summary = overall_summary_stats(y_test, classifications_test, graph, penalty=False)
        print(h_summary)
           
        results = {
            'dataType': dataType_mod,
            'Sero_calling': db,
            'Pred_label': pred_label, 
            'Serovar': serovar,
            'train_test_split': test_size,
            'n_original_features': X.shape[1],
            'n_features_used': X_train.shape[1],
            'filter_threshold': filter_threshold,
            'label_filtered': labels_to_remove,
            'n': len(y),
            'sampling_strat': 'RandomBalancingSampler',
            'Recall': h_summary['hR'],
            'Precision': h_summary['hP'],
            'F1': h_summary['hF1'],
            'Accuracy': h_summary['hAcc']
        }
        
    else: 
        print(f"No data found for {serovar} in {column_name} after filtering")

    return results
    
def hml_lb_unitig(file_path, serovar, db, test_size, y_col='Province', dataType='unitig', method='random_upsampling', filter_threshold=2, feat_elim_method=None):
    results=[]
    base_classifier = LogitBoost(random_state=seed)
    resampler = RandomBalancingSampler(sampling_strategy = 'mean', random_state=seed)
    example_outputs = config['setup']['file_paths']['example_outputs']['unitig']
    save_feat_file = config['setup']['file_paths']['save_feat_output']['unitig']
    
    meta = pd.read_csv(file_path, low_memory=False)
    
    if y_col == 'Province':
        meta = meta[(meta[y_col] != 'Not Available')]
    else: 
        meta = meta

    meta, labels_to_remove = filter_dataset(meta, y_col, filter_threshold)
        
    X = meta.drop(['Key', 'Province', 'Geography'], axis = 1)
    y = meta[y_col].tolist()
    
    if y_col == 'Province':
        pred_label = 'Province'
    elif y_col == 'CombinedSites':
        pred_label = 'Animal type'
    elif y_col == 'CombinedSite2':
        pred_label = 'Location or Food'
    else:
        pred_label = 'None'
            
    print(f"Number of features: {X.shape[1]}")
    print(f"Number of rows: {len(y)}")

    X_train, X_test, y_train, y_test = train_test_split(X, y,
                                                        test_size=test_size, 
                                                        stratify=y,
                                                        random_state=seed)

    # Assuming train_labels is a list or a pandas Series
    label_distribution = pd.Series(y_train).value_counts()
    print(label_distribution)

    save_feat = X_train.columns.values
    np.savetxt(save_feat_file, save_feat, fmt="%s")

    # fit hierachical classifier
    models = fit_hierarchical_classifier(graph, y_train, X_train, base_classifier,
                                         subsampler = resampler,
                                          verbose = True)

    with gzip.open("%s/models.pkl.gz" % example_outputs, 'wb') as file:
        pickle.dump(models, file, protocol = 4)

    # classify and summarise training data
    classification_table_train, classifications_train = classify_samples_in_hierarchy(graph, X_train, 
                                                                               models,
                                                                               mode = 'max', 
                                                                               threshold = 0.51, 
                                                                               verbose = True)

    summary_train, summary_table_train = summary_statistics_per_class(graph, y_train, 
                                                                classifications_train, 
                                                                penalty=False)

    summary_table_train.to_csv("%s/training_summary.tsv" % example_outputs, 
                       sep = "\t", header = True, index = False)

    classification_table_test, classifications_test = classify_samples_in_hierarchy(graph, X_test, 
                                                                               models,
                                                                               mode = 'max', 
                                                                               threshold = 0.51)

    summary_test, summary_table_test = summary_statistics_per_class(graph, y_test, 
                                                              classifications_test, 
                                                              penalty=False)

    summary_table_test.to_csv("%s/test_summary.tsv" % example_outputs, 
                       sep = "\t", header = True, index = False)

    per_node, per_class, clf_reports = per_node_summary_stats(graph, y_test, X_test, models, verbose = True)

    # save per node summary
    per_node.to_csv("%s/per_node_summary.tsv" % example_outputs, 
                               sep = "\t", header = True, index = False)

    # save per class summary
    per_class.to_csv("%s/per_class_summary.tsv" % example_outputs, 
                               sep = "\t", header = True, index = False)

    # generate overall hierachical summary stats (for entire dataset)
    h_summary = overall_summary_stats(y_test, classifications_test, graph, penalty=False)
    print(h_summary)

    results = {
        'dataType': dataType,
        'Sero_calling': db,
        'Pred_label': pred_label, 
        'Serovar': serovar,
        'train_test_split': test_size,
        'n_original_features': X.shape[1],
        'n_features_used': X_train.shape[1],
        'filter_threshold': filter_threshold,
        'label_filtered': labels_to_remove,
        'n': len(y),
        'sampling_strat': 'RandomBalancingSampler',
        'Recall': h_summary['hR'],
        'Precision': h_summary['hP'],
        'F1': h_summary['hF1'],
        'Accuracy': h_summary['hAcc']
    }

    return results    

def hml_gbc_allele(df, column_name, serovar, test_size, graph, db='default', y_col='Province', dataType='allele', feat_eng=None, filter_threshold=2): 
    results = []
    serovar_path = config['column_strains'][column_name][serovar]
    col_remove_path = os.path.join(data_dir, serovar_path)
    file_nzv_path = glob.glob(os.path.join(col_remove_path, 'col*.csv'))
    base_classifier = GradientBoostingClassifier(random_state=seed)
    resampler = RandomBalancingSampler(sampling_strategy = 'mean', random_state=seed)       
    example_outputs = config['setup']['file_paths']['example_outputs']['allele']
    save_feat_file = config['setup']['file_paths']['save_feat_output']['unitig']
    dataType_mod = f"{feat_eng}_{dataType}" if feat_eng and feat_eng != 'None' else dataType
    
    # Feature engineering with nzv, OHE, or nzv_OHE
    if file_nzv_path:  # Ensure at least one file was found
        cols_remove = file_nzv_path[0]  # Select the first file path
        print(f"File to remove columns is {cols_remove}")
    else:
        print("No NZV file found in the specified path.")  
        
    if y_col == 'Province':
        df_filtered = df[(df[y_col] != 'Not Available')]
    else: 
        df_filtered = df
        
    df_filtered, labels_to_remove = filter_dataset(df_filtered, y_col, filter_threshold)
    
    if feat_eng != 'None':
        print(f"Applying feature engineering: {feat_eng}")
        df_filtered = feat_eng_data(df_filtered, feat_eng, cols_remove)
    else:
        print("No feature engineering applied.")
        
    # Adjust to handle filtered dfs or no filter
    if not df_filtered.empty:
        if column_name != "None":
            db = "FoodNet" if column_name == "Serovar" else "Sero_calling not found"
            print(f"Running {serovar} from {db} using test size {test_size}")
        else:
            print(f"Running on the full dataset using test size {test_size}")

        X = df_filtered.drop(['Key', 'Province', 'Geography', 'Serovar', 'SourceSite', 'CombinedSites',
                              'CombinedSite2'], axis=1)
        y = df_filtered[y_col].tolist()
        
        if y_col == 'Province':
            pred_label = 'Province'
        elif y_col == 'CombinedSites':
            pred_label = 'Animal type'
        elif y_col == 'CombinedSite2':
            pred_label = 'Location or Food'
        else:
            pred_label = 'None'
            
        print(f"Number of features: {X.shape[1]}")
        print(f"Number of rows: {len(y)}")

        X_train, X_test, y_train, y_test = train_test_split(X, y,
                                                            test_size=test_size, 
                                                            stratify=y,
                                                            random_state=seed)
        
        # Assuming train_labels is a list or a pandas Series
        label_distribution = pd.Series(y_train).value_counts()
        print(label_distribution)
        
        save_feat = X_train.columns.values
        np.savetxt(save_feat_file, save_feat, fmt="%s")
        
        # fit hierachical classifier
        models = fit_hierarchical_classifier(graph, y_train, X_train, base_classifier,
                                             subsampler = resampler,
                                              verbose = True)
        
        with gzip.open("%s/models.pkl.gz" % example_outputs, 'wb') as file:
            pickle.dump(models, file, protocol = 4)
        
        # classify and summarise training data
        classification_table_train, classifications_train = classify_samples_in_hierarchy(graph, X_train, 
                                                                                   models,
                                                                                   mode = 'max', 
                                                                                   threshold = 0.51, 
                                                                                   verbose = True)

        summary_train, summary_table_train = summary_statistics_per_class(graph, y_train, 
                                                                    classifications_train, 
                                                                    penalty=False)
        
        summary_table_train.to_csv("%s/training_summary.tsv" % example_outputs, 
                           sep = "\t", header = True, index = False)
        
        classification_table_test, classifications_test = classify_samples_in_hierarchy(graph, X_test, 
                                                                                   models,
                                                                                   mode = 'max', 
                                                                                   threshold = 0.51)

        summary_test, summary_table_test = summary_statistics_per_class(graph, y_test, 
                                                                  classifications_test, 
                                                                  penalty=False)
        
        summary_table_test.to_csv("%s/test_summary.tsv" % example_outputs, 
                           sep = "\t", header = True, index = False)
        
        per_node, per_class, clf_reports = per_node_summary_stats(graph, y_test, X_test, models, verbose = True)

        # save per node summary
        per_node.to_csv("%s/per_node_summary.tsv" % example_outputs, 
                                   sep = "\t", header = True, index = False)

        # save per class summary
        per_class.to_csv("%s/per_class_summary.tsv" % example_outputs, 
                                   sep = "\t", header = True, index = False)
        
        # generate overall hierachical summary stats (for entire dataset)
        h_summary = overall_summary_stats(y_test, classifications_test, graph, penalty=False)
        print(h_summary)
           
        results = {
            'dataType': dataType_mod,
            'Sero_calling': db,
            'Pred_label': pred_label, 
            'Serovar': serovar,
            'train_test_split': test_size,
            'n_original_features': X.shape[1],
            'n_features_used': X_train.shape[1],
            'filter_threshold': filter_threshold,
            'label_filtered': labels_to_remove,
            'n': len(y),
            'sampling_strat': 'RandomBalancingSampler',
            'Recall': h_summary['hR'],
            'Precision': h_summary['hP'],
            'F1': h_summary['hF1'],
            'Accuracy': h_summary['hAcc']
        }
        
    else: 
        print(f"No data found for {serovar} in {column_name} after filtering")

    return results
    
def hml_gbc_unitig(file_path, serovar, db, test_size, y_col='Province', dataType='unitig', method='random_upsampling', filter_threshold=2, feat_elim_method=None):
    results=[]
    base_classifier = GradientBoostingClassifier(random_state=seed)
    resampler = RandomBalancingSampler(sampling_strategy = 'mean', random_state=seed)
    example_outputs = config['setup']['file_paths']['example_outputs']['unitig']
    save_feat_file = config['setup']['file_paths']['save_feat_output']['unitig']
    
    meta = pd.read_csv(file_path, low_memory=False)
    
    if y_col == 'Province':
        meta = meta[(meta[y_col] != 'Not Available')]
    else: 
        meta = meta

    meta, labels_to_remove = filter_dataset(meta, y_col, filter_threshold)
        
    X = meta.drop(['Key', 'Province', 'Geography'], axis = 1)
    y = meta[y_col].tolist()
    
    if y_col == 'Province':
        pred_label = 'Province'
    elif y_col == 'CombinedSites':
        pred_label = 'Animal type'
    elif y_col == 'CombinedSite2':
        pred_label = 'Location or Food'
    else:
        pred_label = 'None'
            
    print(f"Number of features: {X.shape[1]}")
    print(f"Number of rows: {len(y)}")

    X_train, X_test, y_train, y_test = train_test_split(X, y,
                                                        test_size=test_size, 
                                                        stratify=y,
                                                        random_state=seed)

    # Assuming train_labels is a list or a pandas Series
    label_distribution = pd.Series(y_train).value_counts()
    print(label_distribution)

    save_feat = X_train.columns.values
    np.savetxt(save_feat_file, save_feat, fmt="%s")

    # fit hierachical classifier
    models = fit_hierarchical_classifier(graph, y_train, X_train, base_classifier,
                                         subsampler = resampler,
                                          verbose = True)

    with gzip.open("%s/models.pkl.gz" % example_outputs, 'wb') as file:
        pickle.dump(models, file, protocol = 4)

    # classify and summarise training data
    classification_table_train, classifications_train = classify_samples_in_hierarchy(graph, X_train, 
                                                                               models,
                                                                               mode = 'max', 
                                                                               threshold = 0.51, 
                                                                               verbose = True)

    summary_train, summary_table_train = summary_statistics_per_class(graph, y_train, 
                                                                classifications_train, 
                                                                penalty=False)

    summary_table_train.to_csv("%s/training_summary.tsv" % example_outputs, 
                       sep = "\t", header = True, index = False)

    classification_table_test, classifications_test = classify_samples_in_hierarchy(graph, X_test, 
                                                                               models,
                                                                               mode = 'max', 
                                                                               threshold = 0.51)

    summary_test, summary_table_test = summary_statistics_per_class(graph, y_test, 
                                                              classifications_test, 
                                                              penalty=False)

    summary_table_test.to_csv("%s/test_summary.tsv" % example_outputs, 
                       sep = "\t", header = True, index = False)

    per_node, per_class, clf_reports = per_node_summary_stats(graph, y_test, X_test, models, verbose = True)

    # save per node summary
    per_node.to_csv("%s/per_node_summary.tsv" % example_outputs, 
                               sep = "\t", header = True, index = False)

    # save per class summary
    per_class.to_csv("%s/per_class_summary.tsv" % example_outputs, 
                               sep = "\t", header = True, index = False)

    # generate overall hierachical summary stats (for entire dataset)
    h_summary = overall_summary_stats(y_test, classifications_test, graph, penalty=False)
    print(h_summary)

    results = {
        'dataType': dataType,
        'Sero_calling': db,
        'Pred_label': pred_label, 
        'Serovar': serovar,
        'train_test_split': test_size,
        'n_original_features': X.shape[1],
        'n_features_used': X_train.shape[1],
        'filter_threshold': filter_threshold,
        'label_filtered': labels_to_remove,
        'n': len(y),
        'sampling_strat': 'RandomBalancingSampler',
        'Recall': h_summary['hR'],
        'Precision': h_summary['hP'],
        'F1': h_summary['hF1'],
        'Accuracy': h_summary['hAcc']
    }

    return results    