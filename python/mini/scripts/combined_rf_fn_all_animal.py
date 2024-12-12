#!/usr/bin/env python
# coding: utf-8

import os
import time
import pandas as pd
import numpy as np
import yaml 
import glob
import sys
import joblib

from sklearn.ensemble import RandomForestClassifier 
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline

sys.path.append( './Modules')
from functions2 import *

print("Pandas version:", pd.__version__)
print("NumPy version:", np.__version__)
os.getcwd()

start_script_time = time.time()

# File paths for config, unitig patterns, and all allelic profile
# Get the directory of the current script
script_dir = os.path.dirname(os.path.abspath(__file__))

# Define the path to the config file relative to the script's directory
config_path = os.path.join(script_dir, '../model_config_best_combined.yml')

# Load configuration
def load_config(config_path):
    with open(config_path, 'r') as file:
        return yaml.safe_load(file)

config = load_config(config_path)

data_dir = config['setup']['file_paths']['data_dir']
allele_path = config['setup']['file_paths']['allele_path']
results_path = config['setup']['models']['Random_Forest']['results_path']

# Set variables for dictionary
filter_prov = config['setup']['variables']['filter_prov']
model_type = config['setup']['models']['Random_Forest']['model_type']
stratified = config['setup']['variables']['stratified']
optim = config['setup']['variables']['optim']

# Setting the column_order for the df
template = pd.read_csv(results_path, nrows=0)
columns_order = template.columns.tolist()
# Load data
allele = pd.read_csv(allele_path, sep='\t', low_memory=False)

allele.replace({'-':0}, inplace=True) # Set instances 'no gene found' to 0

allele['CombinedSites'] = allele['CombinedSites'].replace({'Pork': 'Pig'})

# allele = allele[allele['CombinedSite2'] != "Pig_food"]

# Define fixed values for the parameters
test_size = 0.2
db = 'default'
y_col = 'CombinedSites'
dataType = 'allele'
method = 'SMOTE'
filter_threshold = 10
feat_eng = 'OHE'
feat_elim_method = None
serovar = 'All'
column_name = 'Serovar'
seeds = [34]
cols_keep_file = os.path.join(script_dir, '../data/all_and_serovar_feat_2df.txt')
cm_base_path = os.path.join(script_dir, '../output/cm') 
pred_label_base_path = os.path.join(script_dir, '../output/test_pred_labels')
save_base_path = os.path.join(script_dir, '../output/save_models')
feat_base_path = os.path.join(script_dir, '../output/features')
n_features = 100

# Read in the list of cols to keep and get the OHE cols
selected_feats = read_columns_to_keep(cols_keep_file)
print(selected_feats)

# Filtering step before running anything model-related
if serovar == 'All':
    filtered_df = allele
else:
    filtered_df = allele[allele['Serovar'] == serovar]
    
for seed in seeds:
    start_script_time = time.time()
    results = []
    dataType_mod = f"{feat_eng}_{dataType}" if feat_eng and feat_eng != 'None' else dataType
    base_classifier = RandomForestClassifier(random_state=seed, n_jobs=config['setup']['n_jobs'][0])
    param_grid_config = config['setup']['models']['Random_Forest']['param_grid']
    param_grid = {f'randomforestclassifier__{key}': value for key, value in param_grid_config.items()}
    proportions_list = config['setup']['feat_elim_params']['rfe_proportion_list']  

    # Feature engineering with nzv, OHE, or nzv_OHE
    if y_col == 'Province':
        df_filtered = filtered_df[(filtered_df[y_col] != 'Not Available')]
    else: 
        df_filtered = filtered_df

    df_filtered, labels_to_remove = filter_dataset(df_filtered, y_col, filter_threshold)

    selected_feats = read_columns_to_keep(cols_keep_file)
    print(f"selected feats = {selected_feats}")
    base_cols_to_keep = extract_base_columns(selected_feats)
    print(f"base_cols_to_keep = {base_cols_to_keep}")
    cols_in_df = [col for col in df_filtered.columns if not col.startswith('SALM_') or col in base_cols_to_keep]
    print(f"cols in df: {cols_in_df}")
    print(f"len cols_in_df: {len(cols_in_df)}")
    df_filtered = df_filtered[cols_in_df]
    print(f"Df after keeping cols: {df_filtered.head()}")

    if feat_eng != 'None':
        print(f"Applying feature engineering: {feat_eng}")
        df_filtered = feat_eng_data(df_filtered, feat_eng, file_path=cols_keep_file)
        print(df_filtered.shape)
        print(df_filtered.head())
    else:
        print("No feature engineering applied.")

    # Adjust to handle filtered dfs or no filter
    if not df_filtered.empty:
        if column_name != "None":
            db = "Combined" if column_name == "Serovar" else "Sero_calling not found"
            print(f"Running {serovar} from {db} using test size {test_size} and {feat_eng}")
        else:
            print(f"Running on the full dataset using test size {test_size}")

        X = df_filtered.drop(['CombinedSites', 'CombinedSite2', 'Serovar', 'sample_id'], axis=1)
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

        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_size, stratify=y, random_state=seed)

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
                        selected_feature_indices = selector.support_
                        selected_feature_names = X_train.columns[selected_feature_indices]             
                        X_train_transformed, X_test_transformed = selector.transform(X_train), selector.transform(X_test)
                        metrics, best_estimator, y_pred = train_eval_model(X_train_transformed, X_test_transformed, y_train, y_test, pipeline, cv_splits=n_splits, param_grid=param_grid, n_jobs=config['setup']['n_jobs'][0])

                        classifier_name = list(best_estimator.named_steps.keys())[-1]
                        print(f"Classifier_name is {classifier_name}")
                        classifier = best_estimator.named_steps[classifier_name]
                        print(f"Classifier is {classifier}")

                        # Check for feature importances or coefficients
                        if hasattr(classifier, 'feature_importances_'):
                            feature_importances = classifier.feature_importances_
                        elif hasattr(classifier, 'coef_'):
                            feature_importances = np.abs(classifier.coef_[0])
                        else:
                            feature_importances = None

                        if feature_importances is not None:
                            feature_importance_df = pd.DataFrame({
                                'Model': model_type,
                                'db': db,
                                'Serovar': serovar,
                                'Pred_label': y_col,
                                'Feature': selected_feature_names,
                                'Importance': feature_importances
                            })
                            top_features = feature_importance_df.sort_values(by='Importance', ascending=False).head(n_features)
                            total_importance = feature_importances.sum()
                            top_contribution = top_features['Importance'].sum()
                            top_percentage = (top_contribution / total_importance) * 100

                            print(f"\nTop impactful features after RFE (of {len(selected_feature_names)}) for seed {seed}:")
                            print(top_features)
                            print(f"\nThe top features contribute {top_contribution:.2f} out of {total_importance:.2f} for seed {seed}"
                                  f" ({top_percentage:.2f}% of the total feature importance).")

                            feat_output_file = os.path.join(feat_base_path, f"miniFeat_{db}_{serovar}_{model_type}_{y_col}_{seed}.csv")
                            top_features.to_csv(feat_output_file)
                        else:
                            print("Feature importance not available for this classifier.")

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
                if selector:
                    selected_feature_indices = selector.support_
                    selected_feature_names = X_train.columns[selected_feature_indices]
                    X_train_transformed = selector.transform(X_train)
                    X_test_transformed = selector.transform(X_test)
                    metrics, best_estimator, y_pred = train_eval_model(
                        X_train_transformed, X_test_transformed, y_train, y_test,
                        pipeline, cv_splits=n_splits, param_grid=param_grid,
                        n_jobs=config['setup']['n_jobs'][0]
                    )

                    classifier_name = list(best_estimator.named_steps.keys())[-1]
                    print(f"Classifier_name is {classifier_name}")
                    classifier = best_estimator.named_steps[classifier_name]
                    print(f"Classifier is {classifier}")

                    # Check for feature importances or coefficients
                    if hasattr(classifier, 'feature_importances_'):
                        feature_importances = classifier.feature_importances_
                    elif hasattr(classifier, 'coef_'):
                        feature_importances = np.abs(classifier.coef_[0])
                    else:
                        feature_importances = None

                    if feature_importances is not None:
                        feature_importance_df = pd.DataFrame({
                            'Model': model_type,
                            'db': db,
                            'Serovar': serovar,
                            'Pred_label': y_col,
                            'Feature': selected_feature_names,
                            'Importance': feature_importances
                        })
                        top_features = feature_importance_df.sort_values(
                            by='Importance', ascending=False
                        ).head(n_features)
                        total_importance = feature_importances.sum()
                        top_contribution = top_features['Importance'].sum()
                        top_percentage = (top_contribution / total_importance) * 100

                        print(f"\nTop impactful features after RFECV (of {len(selected_feature_names)}) for seed {seed}:")
                        print(top_features)
                        print(
                            f"\nThe top features contribute {top_contribution:.2f} out of {total_importance:.2f} for seed {seed}"
                            f" ({top_percentage:.2f}% of the total feature importance)."
                        )

                        feat_output_file = os.path.join(
                            feat_base_path,
                            f"miniFeat_{db}_{serovar}_{model_type}_{y_col}_{seed}.csv"
                        )
                        top_features.to_csv(feat_output_file)
                    else:
                        print("Feature importance not available for this classifier.")

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
            metrics, best_estimator, y_pred = train_eval_model(
                X_train, X_test, y_train, y_test,
                pipeline, cv_splits=n_splits, param_grid=param_grid,
                n_jobs=config['setup']['n_jobs'][0]
            )

            classifier_name = list(best_estimator.named_steps.keys())[-1]
            print(f"Classifier_name is {classifier_name}")
            classifier = best_estimator.named_steps[classifier_name]
            print(f"Classifier is {classifier}")

            # Since no feature elimination was applied, all features are used
            selected_feature_names = X_train.columns

            # Check for feature importances or coefficients
            if hasattr(classifier, 'feature_importances_'):
                feature_importances = classifier.feature_importances_
            elif hasattr(classifier, 'coef_'):
                feature_importances = np.abs(classifier.coef_[0])
            else:
                feature_importances = None

            if feature_importances is not None:
                feature_importance_df = pd.DataFrame({
                    'Model': model_type,
                    'db': db,
                    'Serovar': serovar,
                    'Pred_label': y_col,
                    'Feature': selected_feature_names,
                    'Importance': feature_importances
                })
                top_features = feature_importance_df.sort_values(
                    by='Importance', ascending=False
                ).head(n_features)
                total_importance = feature_importances.sum()
                top_contribution = top_features['Importance'].sum()
                top_percentage = (top_contribution / total_importance) * 100

                print(f"\nTop impactful features (of {len(selected_feature_names)}) for seed {seed}:")
                print(top_features)
                print(
                    f"\nThe top features contribute {top_contribution:.2f} out of {total_importance:.2f} for seed {seed}"
                    f" ({top_percentage:.2f}% of the total feature importance)."
                )

                feat_output_file = os.path.join(
                    feat_base_path,
                    f"miniFeat_{db}_{serovar}_{model_type}_{y_col}_{seed}.csv"
                )
                top_features.to_csv(feat_output_file, index=False)
            else:
                print("Feature importance not available for this classifier.")

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
        labels = sorted(set(y_test))

        # Compute confusion matrix
        cm = confusion_matrix(y_test, y_pred, labels=labels)

        # Convert confusion matrix to a DataFrame for better readability
        cm_df = pd.DataFrame(cm, index=labels, columns=labels)

        cm_path = os.path.join(cm_base_path, f"miniFeat_cm_{serovar}_{model_type}_{y_col}_{db}_{seed}.csv")

        # Optionally, save to a CSV file as well
        cm_df.to_csv(cm_path)

        test_output_df = pd.DataFrame({
            'Model': model_type, 
            'Sample': df_filtered.loc[X_test.index, 'sample_id'],
            'Serovar': df_filtered.loc[X_test.index, 'Serovar'],
            'Seed': seed,
            'Pred_label': y_col,
            'true_label': y_test,
            'pred_label': y_pred})

        output_path = os.path.join(pred_label_base_path, f"miniFeat_pred_{serovar}_{model_type}_{y_col}_{db}_{seed}.csv")
        test_output_df.to_csv(output_path, index=False)

        model_save_path = os.path.join(save_base_path, f"miniFeat_model_{serovar}_{y_col}_{db}_{seed}.joblib")
        joblib.dump(best_estimator, model_save_path)
    else: 
        print(f"No data found for {serovar} in {column_name} after filtering")

    print(f"Finishing a run with test_size of {test_size}, cv split of {n_splits}, sampling strat {method}, feature engineering {feat_eng}, feature selection method {feat_elim_method}")

    # Handle results as needed, e.g., print or save to file
    print(results)

    end_script_time = time.time()
    script_duration = (end_script_time - start_script_time) / 60
    print(f"Script run duration: {script_duration:.2f} minutes")

    for result in results:
        result.update({
            'seed': seed,
            'filter_low_provFreq': filter_prov,
            'model_type': model_type,
            'stratified': stratified,
            'Optimization': optim,
            'Feat_elim_method': feat_elim_method if feat_elim_method else 'None',
            'runTime_mins': script_duration
        })

        # Append to CSV
        df_to_append = pd.DataFrame([result], columns=columns_order)
        df_to_append.to_csv(results_path, mode='a', header=False, index=False)