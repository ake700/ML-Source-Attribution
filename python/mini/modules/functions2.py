#!/usr/bin/env python
# coding: utf-8
import pandas as pd
import imblearn
from sklearn.preprocessing import OneHotEncoder
from imblearn.over_sampling import SMOTE, RandomOverSampler
from imblearn.under_sampling import RandomUnderSampler
from imblearn.combine import SMOTEENN
from imblearn.pipeline import make_pipeline
from collections import Counter

import os
import time
import numpy as np
import joblib
import yaml
import glob
import sys

# Machine Learning models
import sklearn
from sklearn.ensemble import RandomForestClassifier  # Random Forest algorithm
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.preprocessing import OneHotEncoder # For categorical variables from testing the model
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC
from sklearn.feature_selection import RFE, RFECV
from sklearn.model_selection import StratifiedKFold
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
    fbeta_score, 
    roc_auc_score, 
    cohen_kappa_score,
    make_scorer
)

# Function to load the YAML configuration
def load_config(config_path):
    with open(config_path, 'r') as file:
        return yaml.safe_load(file)

config_path = './model_config.yml' 
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

def read_columns_to_keep(file_path):
    """
    Read the list of columns to keep from a text file.

    Parameters:
    - file_path: Path to the text file containing the column names.

    Returns:
    - List of column names to keep.
    """
    with open(file_path, 'r') as file:
        # Read each line, strip newline characters, and return as a list
        cols_keep = [line.strip() for line in file if line.strip()]
    return cols_keep

def filter_salm_columns(df, cols_to_keep):
    """
    Filters out the SALM_ columns in the DataFrame that are not in the given list.

    Parameters:
    - df: The DataFrame to filter.
    - cols_to_keep: A list of SALM_ columns to keep.

    Returns:
    - A DataFrame with only the specified SALM_ columns and other non-SALM_ columns.
    """
    # Step 1: Identify all columns that start with 'SALM_'
    salm_cols = [col for col in df.columns if col.startswith('SALM_')]

    # Step 2: Filter SALM_ columns to keep only those present in cols_to_keep
    salm_cols_to_keep = [col for col in salm_cols if col in cols_to_keep]

    # Step 3: Combine the filtered SALM_ columns with non-SALM_ columns
    non_salm_cols = [col for col in df.columns if not col.startswith('SALM_')]
    
    # Step 4: Return a DataFrame with only the necessary columns
    return df[non_salm_cols + salm_cols_to_keep]

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

def extract_base_columns(cols):
    """
    Extract the base columns (e.g., 'SALM_16953') from the full column names 
    (e.g., 'SALM_16953_8').

    Parameters:
    - cols: List of column names (e.g., ['SALM_16953_8', 'SALM_15104_35'])

    Returns:
    - List of base column names (e.g., ['SALM_16953', 'SALM_15104'])
    """
    base_cols = base_cols = ['_'.join(col.split('_')[:2]) for col in cols if col.startswith('SALM_')]
    return base_cols

def get_sampler(method, random_state=None, min_class_size=None):
    if method == 'SMOTE':
        k_neighbors = min(5, min_class_size - 1) if min_class_size and min_class_size > 1 else 1
        return SMOTE(random_state=random_state, k_neighbors=k_neighbors)
    elif method == 'random_upsampling':
        return RandomOverSampler(random_state=random_state)
    elif method == 'SMOTEENN':
        k_neighbors = min(5, min_class_size - 1) if min_class_size and min_class_size > 1 else 1
        return SMOTEENN(random_state=random_state, smote=SMOTE(random_state=random_state, k_neighbors=k_neighbors))
    else:
        return None 

def feat_eng_data(df, method, cols_remove=None, file_path=None):
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
        cols_encode = df.columns.difference(['Key', 'Province', 'Geography', 'Serovar', 'SourceSite', 'CombinedSites', 'sample_id', 'CombinedSite2', 'serovar_cgmlst']).tolist()
        df_encoded = encode_and_concat(df, cols_encode)
        # If a file path is provided, read the columns to keep from the .txt file
        if file_path:
            cols_to_keep = read_columns_to_keep(file_path)
            # Filter out unnecessary SALM_ columns
            df_encoded = filter_salm_columns(df_encoded, cols_to_keep)
        
        return df_encoded
    
    elif method in ['nzv', 'nzv_OHE']:
        assert cols_remove is not None, "File for cols to remove not provided"
        df = remove_columns(df, cols_remove)
        if method == 'nzv_OHE':
            cols_encode = df.columns.difference(['Key', 'Province', 'Geography', 'Serovar', 'SourceSite', 'CombinedSites', 'sample_id', 'CombinedSite2', 'serovar_cgmlst']).tolist()
            return encode_and_concat(df, cols_encode)
        return df
    else:
        return df 

def feat_elim_data(X, y, feat_elim_method, estimator, n_splits):
    """
    Performs feature elimination (RFE or RFECV) on the dataset with the given estimator.

    Parameters:
    - X: Feature matrix.
    - y: Target vector.
    - feat_elim_method (str): 'RFE' or 'RFECV' for feature elimination method.
    - estimator: The classifier model used for feature selection.
    - n_splits (int): Number of CV splits for RFECV.

    Returns:
    - dict: Dictionary of selectors keyed by number of features or 'RFECV'.
    """
    selectors = {}  # To store selectors for different feature set sizes
    n_step = config['setup']['feat_elim_params']['rfecv']['n_step']
    proportions_list = config['setup']['feat_elim_params']['rfe_proportion_list']  
    min_features_rfecv = config['setup']['feat_elim_params']['rfecv']['min_features_select']
    
    if feat_elim_method == 'RFE':
        # Define feature set sizes as proportions of total features
        feature_set_sizes = [max(int(X.shape[1] * prop), 1) for prop in proportions_list]
        for size in feature_set_sizes:
            selector = RFE(estimator=estimator, n_features_to_select=size, step=n_step)
            selector = selector.fit(X, y)
            selectors[size] = selector

    elif feat_elim_method == 'RFECV':
        cv = StratifiedKFold(n_splits=n_splits)
        min_feature_size = max(int(X.shape[1] * min_features_rfecv), 1) 
        selector = RFECV(estimator=estimator, step=n_step, cv=cv, scoring='f1_macro', min_features_to_select=min_feature_size)
        selector = selector.fit(X, y)
        print(f"Optimal number of features with RFECV: {selector.n_features_}")
        selectors['RFECV'] = selector  

    else:
        print("No feature elimination method specified.")
    
    return selectors
        
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
    # Base path
    base_path = config['setup']['models'][model_type]['save_model_path']
    
    model_filename = f"{dataType}_{serovar}_{db}_{test_size}_{method}.joblib"
    model_path = os.path.join(base_path, model_filename)
    
    # Save model
    joblib.dump(best_model, model_path)
    print(f"Model saved to {model_path}")

def train_eval_model(X_train, X_test, y_train, y_test, pipeline, cv_splits, scoring='f1_macro', param_grid=None, n_jobs=5):
    """
    Trains and evaluates a model using GridSearchCV and computes various performance metrics.

    Parameters:
    - X_train, X_test: Training and test feature sets.
    - y_train, y_test: Training and test labels.
    - pipeline: The pipeline/estimator to tune.
    - cv_splits (int): Number of cross-validation splits.
    - scoring (str): Scoring metric for GridSearchCV.
    - param_grid (dict, optional): Parameter grid for hyperparameter tuning.

    Returns:
    - dict: A dictionary of metrics including 'Recall', 'Precision', 'F1', 'Accuracy', 'Bal_acc', 'Kappa', 'cv_split', and 'n_features_used'.
    """
    grid_search = GridSearchCV(estimator=pipeline, param_grid=param_grid, scoring=scoring, cv=cv_splits, n_jobs=n_jobs, verbose=1)
    grid_search.fit(X_train, y_train)
    y_pred = grid_search.predict(X_test)
    
    # Return necessary values
    metrics = {
        'n_features_used': X_train.shape[1],
        'Recall': recall_score(y_test, y_pred, average='macro'),
        'Precision': precision_score(y_test, y_pred, average='macro', zero_division=0),
        'F1': f1_score(y_test, y_pred, average='macro'),
        'Accuracy': accuracy_score(y_test, y_pred),
        'Bal_acc': balanced_accuracy_score(y_test, y_pred),
        'Kappa': cohen_kappa_score(y_test, y_pred),
        'cv_split': cv_splits
    }
    
    print(f"Best parameters: {grid_search.best_params_}")
    print(f"Best estimator: {grid_search.best_estimator_}")
    
    return metrics, grid_search.best_estimator_, y_pred

"""
The following functions are to run the ML models. Docstring is provided for the Random FOrest with allele data, but the same concept applies to the rest using unitig data and/or different models (GradientBoostingClassifier, Support Vector Machine, and Logit Boost)
"""

def rf_allele(df, column_name, serovar, test_size, db='default', y_col='Province', dataType='allele', method='random_upsampling', filter_threshold=2, feat_eng=None, feat_elim_method=None):    
    """
    Processes input data, applies feature engineering and optional feature elimination, 
    trains a RandomForest-based pipeline with optional sampling strategies, and 
    evaluates the model using cross-validation and test data.

    Parameters:
    - df: The DataFrame containing input features and labels.
    - column_name (str): A key indicating which column mapping to use from config.
    - serovar (str): The serovar (biological variant) to filter or process.
    - test_size (float or int): The proportion or number of samples for the test split.
    - db (str, optional): The database or sero_calling context (default: 'default').
    - y_col (str, optional): The column name of the target variable (default: 'Province').
    - dataType (str, optional): Type of data (default: 'allele').
    - method (str, optional): Sampling strategy to apply ('random_upsampling', 'SMOTE', 'SMOTEENN', etc.).
    - filter_threshold (int, optional): Threshold to filter classes with low frequency.
    - feat_eng (str, optional): Feature engineering method to apply ('OHE', 'nzv', 'nzv_OHE', or 'None').
    - feat_elim_method (str, optional): Feature elimination method ('RFE', 'RFECV', or None).

    Returns:
    - list of dict: A list of results dictionaries, where each dictionary contains 
      performance metrics (F1, Accuracy, etc.), feature information, and metadata about 
      the run (serovar, sampling strategy, feature engineering, etc.).
    """
    results = []
    serovar_path = config['column_strains'][column_name][serovar]
    col_remove_path = os.path.join(data_dir, serovar_path)
    file_nzv_path = glob.glob(os.path.join(col_remove_path, 'col*.csv'))
    dataType_mod = f"{feat_eng}_{dataType}" if feat_eng and feat_eng != 'None' else dataType
    base_classifier = RandomForestClassifier(random_state=seed, n_jobs=config['setup']['n_jobs'][0])
    param_grid_config = config['setup']['models']['Random_Forest']['param_grid']
    param_grid = {f'randomforestclassifier__{key}': value for key, value in param_grid_config.items()}
    proportions_list = config['setup']['feat_elim_params']['rfe_proportion_list']  
    
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
            print(f"Running {serovar} from {db} using test size {test_size} and {feat_eng}")
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
        
        
                                             
        # After filtering and before initializing GridSearchCV
        min_class_size = pd.Series(y_train).value_counts().min()
        n_splits = max(2, min(10, min_class_size))
        k_neighbors = min(5, min_class_size - 1)
    
        if method == 'No_strat':
            print("Running without any sampling strat")
            pipeline = make_pipeline(base_classifier)
        elif method in ['SMOTE', 'SMOTEENN']:
            if min_class_size <= 1 or (min_class_size - 1) <= k_neighbors:
                print(f"Skipping {method} due to small class size")
                return None # To prevent further execution of code if the SMOTE/SMOTEENN failed
            else:
                print(f"Running {method} successfully")
                sampler = get_sampler(method, random_state=seed, min_class_size=min_class_size)
                pipeline = make_pipeline(sampler, base_classifier)
        else:
            print(f"Running {method} sampling strat")
            sampler = get_sampler(method, random_state=seed)
            pipeline = make_pipeline(sampler, base_classifier)
            
        if feat_elim_method in ['RFE', 'RFECV']:
            selectors = feat_elim_data(X_train, y_train, feat_elim_method, base_classifier, n_splits)
            
            if feat_elim_method == 'RFE':
                for proportion in proportions_list:
                    size = max(int(X_train.shape[1] * proportion), 1)
                    selector = selectors.get(size)
                    if selector:
                        X_train_transformed, X_test_transformed = selector.transform(X_train), selector.transform(X_test)
                        metrics = train_eval_model(X_train_transformed, X_test_transformed, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
                        metrics.update({
                            'dataType': dataType_mod,
                            'Sero_calling': db,
                            'Pred_label': pred_label, 
                            'Serovar': serovar,
                            'train_test_split': test_size,
                            'n_original_features': X.shape[1],
                            'filter_threshold': filter_threshold,
                            'label_filtered': labels_to_remove,
                            'n': len(y),
                            'sampling_strat': method
                    })
                        results.append(metrics)
   
            elif feat_elim_method == 'RFECV':
                selector = selectors.get('RFECV')
                X_train_transformed, X_test_transformed = selector.transform(X_train), selector.transform(X_test)
                metrics = train_eval_model(X_train_transformed, X_test_transformed, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
                metrics.update({
                    'dataType': dataType_mod,
                    'Sero_calling': db,
                    'Pred_label': pred_label, 
                    'Serovar': serovar,
                    'train_test_split': test_size,
                    'n_original_features': X.shape[1],
                    'filter_threshold': filter_threshold,
                    'label_filtered': labels_to_remove,
                    'n': len(y),
                    'sampling_strat': method
                })
                results.append(metrics)
        else:
            # Case when no feature elimination method is specified
            metrics = train_eval_model(X_train, X_test, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
            metrics.update({
                'dataType': dataType_mod,
                'Sero_calling': db,
                'Pred_label': pred_label, 
                'Serovar': serovar,
                'train_test_split': test_size,
                'n_original_features': X.shape[1],
                'filter_threshold': filter_threshold,
                'label_filtered': labels_to_remove,
                'n': len(y),
                'sampling_strat': method
             })
            results.append(metrics)
    
    else: 
        print(f"No data found for {serovar} in {column_name} after filtering")

    print(f"Finishing a run with test_size of {test_size}, cv split of {n_splits}, sampling strat {method}, feature engineering {feat_eng}, feature selection method {feat_elim_method}")
        
    # Return necessary values
    return results
    
def rf_unitig(file_path, serovar, db, test_size, y_col='Province', dataType='unitig', method='random_upsampling', filter_threshold=2, feat_elim_method=None):
    results=[]
    base_classifier = RandomForestClassifier(random_state=seed, n_jobs=config['setup']['n_jobs'][0])
    param_grid_config = config['setup']['models']['Random_Forest']['param_grid']
    param_grid = {f'randomforestclassifier__{key}': value for key, value in param_grid_config.items()}
    proportions_list = config['setup']['feat_elim_params']['rfe_proportion_list']  
    
    meta = pd.read_csv(file_path, low_memory=False)
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
    
    # After filtering and before initializing GridSearchCV
    min_class_size = pd.Series(y_train).value_counts().min()
    n_splits = min(10, min_class_size)
    k_neighbors = min(5, min_class_size - 1)
       
    if method == 'No_strat':
            print("Running without any sampling strat")
            pipeline = make_pipeline(base_classifier)
    elif method in ['SMOTE', 'SMOTEENN']:
        if min_class_size <= 1 or (min_class_size - 1) <= k_neighbors:
            print(f"Skipping {method} due to small class size")
            return None # To prevent further execution of code if the SMOTE/SMOTEENN failed
        else:
            print(f"Running {method} successfully")
            sampler = get_sampler(method, random_state=seed, min_class_size=min_class_size)
            pipeline = make_pipeline(sampler, base_classifier)
    else:
        print(f"Running {method} sampling strat")
        sampler = get_sampler(method, random_state=seed)
        pipeline = make_pipeline(sampler, base_classifier)

    if feat_elim_method in ['RFE', 'RFECV']:
        selectors = feat_elim_data(X_train, y_train, feat_elim_method, base_classifier, n_splits)
            
        if feat_elim_method == 'RFE':
            for proportion in proportions_list:
                size = max(int(X_train.shape[1] * proportion), 1)
                selector = selectors.get(size)
                if selector:
                    X_train_transformed, X_test_transformed = selector.transform(X_train), selector.transform(X_test)
                    metrics = train_eval_model(X_train_transformed, X_test_transformed, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
                    metrics.update({
                        'dataType': dataType,
                        'Sero_calling': db,
                        'Pred_label': pred_label, 
                        'Serovar': serovar,
                        'train_test_split': test_size,
                        'n_original_features': X.shape[1],
                        'filter_threshold': filter_threshold,
                        'label_filtered': labels_to_remove,
                        'n': len(y),
                        'sampling_strat': method
                })
                    results.append(metrics)

        elif feat_elim_method == 'RFECV':
            selector = selectors.get('RFECV')
            X_train_transformed, X_test_transformed = selector.transform(X_train), selector.transform(X_test)
            metrics = train_eval_model(X_train_transformed, X_test_transformed, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
            metrics.update({
                'dataType': dataType,
                'Sero_calling': db,
                'Pred_label': pred_label, 
                'Serovar': serovar,
                'train_test_split': test_size,
                'n_original_features': X.shape[1],
                'filter_threshold': filter_threshold,
                'label_filtered': labels_to_remove,
                'n': len(y),
                'sampling_strat': method
            })
            results.append(metrics)
    else:
        # Case when no feature elimination method is specified
        metrics = train_eval_model(X_train, X_test, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
        metrics.update({
            'dataType': dataType,
            'Sero_calling': db,
            'Pred_label': pred_label, 
            'Serovar': serovar,
            'train_test_split': test_size,
            'n_original_features': X.shape[1],
            'filter_threshold': filter_threshold,
            'label_filtered': labels_to_remove,
            'n': len(y),
            'sampling_strat': method
        })
        results.append(metrics)
                
    print(f"Finishing a run with test_size of {test_size}, cv split of {n_splits}, sampling strat {method}, feature selection method {feat_elim_method}")
        
    # Return necessary values
    return results

def svm_allele(df, column_name, serovar, test_size, kernel='linear', db='default', y_col='Province', dataType='allele', method='random_upsampling', filter_threshold=2, feat_eng=None, feat_elim_method=None):    
    results = []
    serovar_path = config['column_strains'][column_name][serovar]
    col_remove_path = os.path.join(data_dir, serovar_path)
    file_nzv_path = glob.glob(os.path.join(col_remove_path, 'col*.csv'))
    dataType_mod = f"{feat_eng}_{dataType}" if feat_eng and feat_eng != 'None' else dataType
    base_classifier = SVC(probability=True, random_state=seed, kernel = kernel)
    param_grid_config = config['setup']['models']['SVM']['param_grid']
    param_grid = {f'svc__{key}': value for key, value in param_grid_config.items()}
    proportions_list = config['setup']['feat_elim_params']['rfe_proportion_list']  
    
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
            print(f"Running {serovar} from {db} using test size {test_size} and {feat_eng}")
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
                                             
        # After filtering and before initializing GridSearchCV
        min_class_size = pd.Series(y_train).value_counts().min()
        n_splits = max(2, min(10, min_class_size))
        k_neighbors = min(5, min_class_size - 1)
    
        if method == 'No_strat':
            print("Running without any sampling strat")
            pipeline = make_pipeline(StandardScaler(), base_classifier)
        elif method in ['SMOTE', 'SMOTEENN']:
            if min_class_size <= 1 or (min_class_size - 1) <= k_neighbors:
                print(f"Skipping {method} due to small class size")
                return None # To prevent further execution of code if the SMOTE/SMOTEENN failed
            else:
                print(f"Running {method} successfully")
                sampler = get_sampler(method, random_state=seed, min_class_size=min_class_size)
                pipeline = make_pipeline(StandardScaler(), sampler, base_classifier)
        else:
            print(f"Running {method} sampling strat")
            sampler = get_sampler(method, random_state=seed)
            pipeline = make_pipeline(StandardScaler(), sampler, base_classifier)
            
        if kernel == 'linear' and feat_elim_method in ['RFE', 'RFECV']:
            selectors = feat_elim_data(X_train, y_train, feat_elim_method, base_classifier, n_splits)
            
            if feat_elim_method == 'RFE':
                for proportion in proportions_list:
                    size = max(int(X_train.shape[1] * proportion), 1)
                    selector = selectors.get(size)
                    if selector:
                        X_train_transformed, X_test_transformed = selector.transform(X_train), selector.transform(X_test)
                        metrics = train_eval_model(X_train_transformed, X_test_transformed, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
                        metrics.update({
                            'dataType': dataType_mod,
                            'Sero_calling': db,
                            'Pred_label': pred_label, 
                            'Serovar': serovar,
                            'train_test_split': test_size,
                            'n_original_features': X.shape[1],
                            'filter_threshold': filter_threshold,
                            'label_filtered': labels_to_remove,
                            'n': len(y),
                            'sampling_strat': method
                    })
                        results.append(metrics)
   
            elif feat_elim_method == 'RFECV':
                selector = selectors.get('RFECV')
                X_train_transformed, X_test_transformed = selector.transform(X_train), selector.transform(X_test)
                metrics = train_eval_model(X_train_transformed, X_test_transformed, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
                metrics.update({
                    'dataType': dataType_mod,
                    'Sero_calling': db,
                    'Pred_label': pred_label, 
                    'Serovar': serovar,
                    'train_test_split': test_size,
                    'n_original_features': X.shape[1],
                    'filter_threshold': filter_threshold,
                    'label_filtered': labels_to_remove,
                    'n': len(y),
                    'sampling_strat': method
                })
                results.append(metrics)
        else:
            # Case when no feature elimination method is specified
            metrics = train_eval_model(X_train, X_test, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
            metrics.update({
                'dataType': dataType_mod,
                'Sero_calling': db,
                'Pred_label': pred_label, 
                'Serovar': serovar,
                'train_test_split': test_size,
                'n_original_features': X.shape[1],
                'filter_threshold': filter_threshold,
                'label_filtered': labels_to_remove,
                'n': len(y),
                'sampling_strat': method
             })
            results.append(metrics)
    
    else: 
        print(f"No data found for {serovar} in {column_name} after filtering")

    print(f"Finishing a run with test_size of {test_size}, cv split of {n_splits}, sampling strat {method}, feature engineering {feat_eng}, feature selection method {feat_elim_method}")
        
    # Return necessary values
    return results
    
def svm_unitig(file_path, serovar, db, test_size, kernel = 'linear', y_col='Province', dataType='unitig', method='random_upsampling', filter_threshold=2, feat_elim_method=None):
    results=[]
    base_classifier = SVC(probability=True, random_state=seed)
    param_grid_config = config['setup']['models']['SVM']['param_grid']
    param_grid = {f'svc__{key}': value for key, value in param_grid_config.items()}
    proportions_list = config['setup']['feat_elim_params']['rfe_proportion_list']  
    
    meta = pd.read_csv(file_path, low_memory=False)
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
    
    # After filtering and before initializing GridSearchCV
    min_class_size = pd.Series(y_train).value_counts().min()
    n_splits = min(10, min_class_size)
    k_neighbors = min(5, min_class_size - 1)
       
    if method == 'No_strat':
            print("Running without any sampling strat")
            pipeline = make_pipeline(StandardScaler(), base_classifier)
    elif method in ['SMOTE', 'SMOTEENN']:
        if min_class_size <= 1 or (min_class_size - 1) <= k_neighbors:
            print(f"Skipping {method} due to small class size")
            return None # To prevent further execution of code if the SMOTE/SMOTEENN failed
        else:
            print(f"Running {method} successfully")
            sampler = get_sampler(method, random_state=seed, min_class_size=min_class_size)
            pipeline = make_pipeline(StandardScaler(), sampler, base_classifier)
    else:
        print(f"Running {method} sampling strat")
        sampler = get_sampler(method, random_state=seed)
        pipeline = make_pipeline(StandardScaler(), sampler, base_classifier)

    if feat_elim_method in ['RFE', 'RFECV']:
        selectors = feat_elim_data(X_train, y_train, feat_elim_method, base_classifier, n_splits)
            
        if feat_elim_method == 'RFE':
            for proportion in proportions_list:
                size = max(int(X_train.shape[1] * proportion), 1)
                selector = selectors.get(size)
                if selector:
                    X_train_transformed, X_test_transformed = selector.transform(X_train), selector.transform(X_test)
                    metrics = train_eval_model(X_train_transformed, X_test_transformed, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
                    metrics.update({
                        'dataType': dataType,
                        'Sero_calling': db,
                        'Pred_label': pred_label, 
                        'Serovar': serovar,
                        'train_test_split': test_size,
                        'n_original_features': X.shape[1],
                        'filter_threshold': filter_threshold,
                        'label_filtered': labels_to_remove,
                        'n': len(y),
                        'sampling_strat': method
                })
                    results.append(metrics)

        elif feat_elim_method == 'RFECV':
            selector = selectors.get('RFECV')
            X_train_transformed, X_test_transformed = selector.transform(X_train), selector.transform(X_test)
            metrics = train_eval_model(X_train_transformed, X_test_transformed, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
            metrics.update({
                'dataType': dataType,
                'Sero_calling': db,
                'Pred_label': pred_label, 
                'Serovar': serovar,
                'train_test_split': test_size,
                'n_original_features': X.shape[1],
                'filter_threshold': filter_threshold,
                'label_filtered': labels_to_remove,
                'n': len(y),
                'sampling_strat': method
            })
            results.append(metrics)
    else:
        # Case when no feature elimination method is specified
        metrics = train_eval_model(X_train, X_test, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
        metrics.update({
            'dataType': dataType,
            'Sero_calling': db,
            'Pred_label': pred_label, 
            'Serovar': serovar,
            'train_test_split': test_size,
            'n_original_features': X.shape[1],
            'filter_threshold': filter_threshold,
            'label_filtered': labels_to_remove,
            'n': len(y),
            'sampling_strat': method
        })
        results.append(metrics)
                
    print(f"Finishing a run with test_size of {test_size}, cv split of {n_splits}, sampling strat {method}, feature selection method {feat_elim_method}")
        
    # Return necessary values
    return results

def lb_allele(df, column_name, serovar, test_size, db='default', y_col='Province', dataType='allele', method='random_upsampling', filter_threshold=2, feat_eng=None):    
    results = []
    serovar_path = config['column_strains'][column_name][serovar]
    col_remove_path = os.path.join(data_dir, serovar_path)
    file_nzv_path = glob.glob(os.path.join(col_remove_path, 'col*.csv'))
    dataType_mod = f"{feat_eng}_{dataType}" if feat_eng and feat_eng != 'None' else dataType
    base_classifier = LogitBoost(random_state=seed)
    param_grid_config = config['setup']['models']['Logit_Boost']['param_grid']
    param_grid = {f'logitboost__{key}': value for key, value in param_grid_config.items()}
    proportions_list = config['setup']['feat_elim_params']['rfe_proportion_list']  
    
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
            print(f"Running {serovar} from {db} using test size {test_size} and {feat_eng}")
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
        
        # After filtering and before initializing GridSearchCV
        min_class_size = pd.Series(y_train).value_counts().min()
        n_splits = max(2, min(10, min_class_size))
        k_neighbors = min(5, min_class_size - 1)
    
        if method == 'No_strat':
            print("Running without any sampling strat")
            pipeline = make_pipeline(base_classifier)
        elif method in ['SMOTE', 'SMOTEENN']:
            if min_class_size <= 1 or (min_class_size - 1) <= k_neighbors:
                print(f"Skipping {method} due to small class size")
                return None # To prevent further execution of code if the SMOTE/SMOTEENN failed
            else:
                print(f"Running {method} successfully")
                sampler = get_sampler(method, random_state=seed, min_class_size=min_class_size)
                pipeline = make_pipeline(sampler, base_classifier)
        else:
            print(f"Running {method} sampling strat")
            sampler = get_sampler(method, random_state=seed)
            pipeline = make_pipeline(sampler, base_classifier)
            
        metrics = train_eval_model(X_train, X_test, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
        metrics.update({
            'dataType': dataType_mod,
            'Sero_calling': db,
            'Pred_label': pred_label, 
            'Serovar': serovar,
            'train_test_split': test_size,
            'n_original_features': X.shape[1],
            'filter_threshold': filter_threshold,
            'label_filtered': labels_to_remove,
            'n': len(y),
            'sampling_strat': method
        })
        results.append(metrics)
    
    else: 
        print(f"No data found for {serovar} in {column_name} after filtering")

    print(f"Finishing a run with test_size of {test_size}, cv split of {n_splits}, sampling strat {method}, feature engineering {feat_eng}")
        
    # Return necessary values
    return results
    
def lb_unitig(file_path, serovar, db, test_size, y_col='Province', dataType='unitig', method='random_upsampling', filter_threshold=2):
    results=[]
    base_classifier = LogitBoost(random_state=seed)
    param_grid_config = config['setup']['models']['Logit_Boost']['param_grid']
    param_grid = {f'logitboost__{key}': value for key, value in param_grid_config.items()}
    proportions_list = config['setup']['feat_elim_params']['rfe_proportion_list']  
    
    meta = pd.read_csv(file_path, low_memory=False)
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
    
    # After filtering and before initializing GridSearchCV
    min_class_size = pd.Series(y_train).value_counts().min()
    n_splits = min(10, min_class_size)
    k_neighbors = min(5, min_class_size - 1)
       
    if method == 'No_strat':
            print("Running without any sampling strat")
            pipeline = make_pipeline(base_classifier)
    elif method in ['SMOTE', 'SMOTEENN']:
        if min_class_size <= 1 or (min_class_size - 1) <= k_neighbors:
            print(f"Skipping {method} due to small class size")
            return None # To prevent further execution of code if the SMOTE/SMOTEENN failed
        else:
            print(f"Running {method} successfully")
            sampler = get_sampler(method, random_state=seed, min_class_size=min_class_size)
            pipeline = make_pipeline(sampler, base_classifier)
    else:
        print(f"Running {method} sampling strat")
        sampler = get_sampler(method, random_state=seed)
        pipeline = make_pipeline(sampler, base_classifier)

    metrics = train_eval_model(X_train, X_test, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
    metrics.update({
        'dataType': dataType,
        'Sero_calling': db,
        'Pred_label': pred_label, 
        'Serovar': serovar,
        'train_test_split': test_size,
        'n_original_features': X.shape[1],
        'filter_threshold': filter_threshold,
        'label_filtered': labels_to_remove,
        'n': len(y),
        'sampling_strat': method
    })
    results.append(metrics)
                
    print(f"Finishing a run with test_size of {test_size}, cv split of {n_splits}, sampling strat {method}")
        
    # Return necessary values
    return results

def gbc_allele(df, column_name, serovar, test_size, db='default', y_col='Province', dataType='allele', method='random_upsampling', filter_threshold=2, feat_eng=None, feat_elim_method=None):    
    results = []
    serovar_path = config['column_strains'][column_name][serovar]
    col_remove_path = os.path.join(data_dir, serovar_path)
    file_nzv_path = glob.glob(os.path.join(col_remove_path, 'col*.csv'))
    dataType_mod = f"{feat_eng}_{dataType}" if feat_eng and feat_eng != 'None' else dataType
    base_classifier = GradientBoostingClassifier(random_state=seed)
    param_grid_config = config['setup']['models']['GBC']['param_grid']
    param_grid = {f'gradientboostingclassifier__{key}': value for key, value in param_grid_config.items()}
    proportions_list = config['setup']['feat_elim_params']['rfe_proportion_list']  
    
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
            print(f"Running {serovar} from {db} using test size {test_size} and {feat_eng}")
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
        
        
                                             
        # After filtering and before initializing GridSearchCV
        min_class_size = pd.Series(y_train).value_counts().min()
        n_splits = max(2, min(10, min_class_size))
        k_neighbors = min(5, min_class_size - 1)
    
        if method == 'No_strat':
            print("Running without any sampling strat")
            pipeline = make_pipeline(base_classifier)
        elif method in ['SMOTE', 'SMOTEENN']:
            if min_class_size <= 1 or (min_class_size - 1) <= k_neighbors:
                print(f"Skipping {method} due to small class size")
                return None # To prevent further execution of code if the SMOTE/SMOTEENN failed
            else:
                print(f"Running {method} successfully")
                sampler = get_sampler(method, random_state=seed, min_class_size=min_class_size)
                pipeline = make_pipeline(sampler, base_classifier)
        else:
            print(f"Running {method} sampling strat")
            sampler = get_sampler(method, random_state=seed)
            pipeline = make_pipeline(sampler, base_classifier)
            
        if feat_elim_method in ['RFE', 'RFECV']:
            selectors = feat_elim_data(X_train, y_train, feat_elim_method, base_classifier, n_splits)
            
            if feat_elim_method == 'RFE':
                for proportion in proportions_list:
                    size = max(int(X_train.shape[1] * proportion), 1)
                    selector = selectors.get(size)
                    if selector:
                        X_train_transformed, X_test_transformed = selector.transform(X_train), selector.transform(X_test)
                        metrics = train_eval_model(X_train_transformed, X_test_transformed, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
                        metrics.update({
                            'dataType': dataType_mod,
                            'Sero_calling': db,
                            'Pred_label': pred_label, 
                            'Serovar': serovar,
                            'train_test_split': test_size,
                            'n_original_features': X.shape[1],
                            'filter_threshold': filter_threshold,
                            'label_filtered': labels_to_remove,
                            'n': len(y),
                            'sampling_strat': method
                    })
                        results.append(metrics)
   
            elif feat_elim_method == 'RFECV':
                selector = selectors.get('RFECV')
                X_train_transformed, X_test_transformed = selector.transform(X_train), selector.transform(X_test)
                metrics = train_eval_model(X_train_transformed, X_test_transformed, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
                metrics.update({
                    'dataType': dataType_mod,
                    'Sero_calling': db,
                    'Pred_label': pred_label, 
                    'Serovar': serovar,
                    'train_test_split': test_size,
                    'n_original_features': X.shape[1],
                    'filter_threshold': filter_threshold,
                    'label_filtered': labels_to_remove,
                    'n': len(y),
                    'sampling_strat': method
                })
                results.append(metrics)
        else:
            # Case when no feature elimination method is specified
            metrics = train_eval_model(X_train, X_test, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
            metrics.update({
                'dataType': dataType_mod,
                'Sero_calling': db,
                'Pred_label': pred_label, 
                'Serovar': serovar,
                'train_test_split': test_size,
                'n_original_features': X.shape[1],
                'filter_threshold': filter_threshold,
                'label_filtered': labels_to_remove,
                'n': len(y),
                'sampling_strat': method
             })
            results.append(metrics)
    
    else: 
        print(f"No data found for {serovar} in {column_name} after filtering")

    print(f"Finishing a run with test_size of {test_size}, cv split of {n_splits}, sampling strat {method}, feature engineering {feat_eng}, feature selection method {feat_elim_method}")
        
    # Return necessary values
    return results
    
def gbc_unitig(file_path, serovar, db, test_size, y_col='Province', dataType='unitig', method='random_upsampling', filter_threshold=2, feat_elim_method=None):
    results=[]
    base_classifier = GradientBoostingClassifier(random_state=seed)
    param_grid_config = config['setup']['models']['GBC']['param_grid']
    param_grid = {f'gradientboostingclassifier__{key}': value for key, value in param_grid_config.items()}
    proportions_list = config['setup']['feat_elim_params']['rfe_proportion_list']  
    
    meta = pd.read_csv(file_path, low_memory=False)
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
    
    # After filtering and before initializing GridSearchCV
    min_class_size = pd.Series(y_train).value_counts().min()
    n_splits = min(10, min_class_size)
    k_neighbors = min(5, min_class_size - 1)
       
    if method == 'No_strat':
            print("Running without any sampling strat")
            pipeline = make_pipeline(base_classifier)
    elif method in ['SMOTE', 'SMOTEENN']:
        if min_class_size <= 1 or (min_class_size - 1) <= k_neighbors:
            print(f"Skipping {method} due to small class size")
            return None # To prevent further execution of code if the SMOTE/SMOTEENN failed
        else:
            print(f"Running {method} successfully")
            sampler = get_sampler(method, random_state=seed, min_class_size=min_class_size)
            pipeline = make_pipeline(sampler, base_classifier)
    else:
        print(f"Running {method} sampling strat")
        sampler = get_sampler(method, random_state=seed)
        pipeline = make_pipeline(sampler, base_classifier)

    if feat_elim_method in ['RFE', 'RFECV']:
        selectors = feat_elim_data(X_train, y_train, feat_elim_method, base_classifier, n_splits)
            
        if feat_elim_method == 'RFE':
            for proportion in proportions_list:
                size = max(int(X_train.shape[1] * proportion), 1)
                selector = selectors.get(size)
                if selector:
                    X_train_transformed, X_test_transformed = selector.transform(X_train), selector.transform(X_test)
                    metrics = train_eval_model(X_train_transformed, X_test_transformed, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
                    metrics.update({
                        'dataType': dataType,
                        'Sero_calling': db,
                        'Pred_label': pred_label, 
                        'Serovar': serovar,
                        'train_test_split': test_size,
                        'n_original_features': X.shape[1],
                        'filter_threshold': filter_threshold,
                        'label_filtered': labels_to_remove,
                        'n': len(y),
                        'sampling_strat': method
                })
                    results.append(metrics)

        elif feat_elim_method == 'RFECV':
            selector = selectors.get('RFECV')
            X_train_transformed, X_test_transformed = selector.transform(X_train), selector.transform(X_test)
            metrics = train_eval_model(X_train_transformed, X_test_transformed, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
            metrics.update({
                'dataType': dataType,
                'Sero_calling': db,
                'Pred_label': pred_label, 
                'Serovar': serovar,
                'train_test_split': test_size,
                'n_original_features': X.shape[1],
                'filter_threshold': filter_threshold,
                'label_filtered': labels_to_remove,
                'n': len(y),
                'sampling_strat': method
            })
            results.append(metrics)
    else:
        # Case when no feature elimination method is specified
        metrics = train_eval_model(X_train, X_test, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid)
        metrics.update({
            'dataType': dataType,
            'Sero_calling': db,
            'Pred_label': pred_label, 
            'Serovar': serovar,
            'train_test_split': test_size,
            'n_original_features': X.shape[1],
            'filter_threshold': filter_threshold,
            'label_filtered': labels_to_remove,
            'n': len(y),
            'sampling_strat': method
        })
        results.append(metrics)
                
    print(f"Finishing a run with test_size of {test_size}, cv split of {n_splits}, sampling strat {method}, feature selection method {feat_elim_method}")
        
    # Return necessary values
    return results