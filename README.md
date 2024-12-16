# Machine Learning and Source Attribution
# Overview

This repository hosts frameworks for running multiple types of ML models using Python and analyses using R (_publications in progress_):

1. **Flat ML (flatML)**: A "flat" ML approach using customizable model/parameter configuration files.
2. **Hierarchical ML (hML)**: An extension of the flat ML concept that introduces hierarchical data structures and analyses, based on previous work done ([SionBayliss/HierarchicalML repository](https://github.com/SionBayliss/HierarchicalML/tree/main)).
3. **Mini ML (mini)**: A simplified/compact version of the flat ML approach, focusing on a smaller set of features and can use larger (>10,000 samples) datasets. This includes additional custom configurations and data preprocessing steps.

Additionally, there are R scripts for statistical analysis, sample selection, data visualization, and generating input data for modelling, and an supplementary Python script for label encoding.

## Contents

- [Directory Structure](#directory-structure)
- [Python Flat ML Framework](#python-flat-ml-framework)
  - [Environment File (Flat ML)](#environment-file)
  - [Model Configuration (Flat ML)](#model-configuration)
  - [Module Functions (Flat ML)](#module-functions)
  - [Output Storage (Flat ML)](#output-storage)
  - [Model Scripts (Flat ML)](#model-scripts)
  - [Submitting Jobs to SLURM (Flat ML)](#submitting-jobs-to-slurm)
- [Python Hierarchical ML Framework](#python-hierarchical-ml-framework)
  - [Environment File (hML)](#environment-file-hml)
  - [Model Configuration (hML)](#model-configuration-hml)
  - [Data (hML)](#data-hml)
  - [Module Functions (hML)](#module-functions-hml)
  - [Output Storage (hML)](#output-storage-hml)
  - [Model Scripts (hML)](#model-scripts-hml)
  - [Submitting Jobs to SLURM (hML)](#submitting-jobs-to-slurm-hml)
- [Python Mini ML Framework](#python-mini-ml-framework)
  - [Environment File (Mini ML)](#environment-file-mini-ml)
  - [Model Configuration (Mini ML)](#model-configuration-mini-ml)
  - [Data (Mini ML)](#data-mini-ml)
  - [Module Functions (Mini ML)](#module-functions-mini-ml)
  - [Output Storage (Mini ML)](#output-storage-mini-ml)
  - [Model Scripts (Mini ML)](#model-scripts-mini-ml)
  - [Submitting Jobs to SLURM (Mini ML)](#submitting-jobs-to-slurm-mini-ml)
- [Label Encoding Script](#label-encoding-script)
- [R Scripts for Analysis](#r-scripts-for-analysis)
- [Additional Notes](#additional-notes)

## Directory Structure
```bash
.
├── R_scripts
│   ├── best_model_analysis.R
│   ├── ml_optimization_analysis.R
│   ├── functions.R
│   ├── host_selection.R
│   ├── locidex_analysis.R
│   └── global_locidex_analysis.R
└── python
    ├── label_encoding.py
    ├── flatML
    │   ├── flatML_env.yaml
    │   └── model
    │       ├── model_config_34.yml
    │       ├── submit.script.sh
    │       ├── module
    │       │   └── function.py
    │       ├── output
    │       │   └── [blank CSV output file with headers]
    │       └── scripts
    │           ├── GBC
    │           │   └── [GBC script(s)]
    │           ├── SVM
    │           │   └── [SVM script(s)]
    │           ├── RandomForest
    │           │   └── [RandomForest script(s)]
    │           └── Logitboost
    │               └── [Logitboost script(s)]
    ├── hML
    │   ├── hML_env.yaml
    │   ├── model
    │   │   ├── model_config_hML.yml
    │   │   ├── submit_script_hML.sh
    │   │   ├── data
    │   │   │   └── FN_v2_canada_east_west_graph.pkl
    │   │   ├── modules
    │   │   │   └── functions_hML.py
    │   │   ├── output
    │   │   │   ├── [blank CSV output file with headers]
    │   │   │   └── hML_output
    │   │   │       └── [example hierarchical output files]
    │   │   └── scripts
    │   │       ├── GBC
    │   │       │   └── [GBC script(s)]
    │   │       ├── SVM
    │   │       │   └── [SVM script(s)]
    │   │       ├── RandomForest
    │   │       │   └── [RandomForest script(s)]
    │   │       ├── Logitboost
    │   │       │   └── [Logitboost script(s)]
    │   │       └── unitigs
    │   │           └── [unitig scripts]
    └── mini
        ├── flatML_env.yaml
        └── model
            ├── model_config.yml
            ├── model_config*.yml
            ├── data
            │   └── [*.txt files with feature names]
            ├── modules
            │   └── functions2.py
            ├── output
            │   ├── [blank CSV output file with headers]
            │   └── [confusion matrices, test predictions, top features, saved models]
            ├── scripts
            │   └── [ML scripts (e.g., RF, SVM, GBC)]
            └── submit_script.sh

```

## Python Flat ML Framework

### Environment File

- **`flatML_env.yaml`**: A YAML file exporting the Python environment used to run the flat ML models.
  - Use `conda env create -f flatML_env.yaml` to replicate the environment.

### Model Configuration

- **`model_config_34.yml`**: A YAML configuration file defining a user-customizable list of models and parameters to test.
  - The ML scripts loop over these models and parameters, allowing you to run one or multiple combinations in a single execution.

### Module Functions

- **`function.py`** (in `module` directory):
  - Contains reusable functions (with docstrings) for operations such as:
    - One-Hot Encoding (OHE)
    - Near Zero Variance (nzv) filtering - see `nzv.R` in `R_scripts`
    - Recursive Feature Elimination (RFE/RFECV)

### Output Storage

- **`output` directory**:
  - Contains a blank CSV file preformatted with headers matching the ML scripts’ output.
  - As models complete, results are appended here for consistent result formatting.

### Model Scripts

- **`scripts` directory**:
  - Contains subdirectories for each ML model type (`GBC`, `SVM`, `RandomForest`, `Logitboost`).
  - Each subdirectory has an example Python script that:
    - Loads the model and parameters from `model_config_34.yml`
    - Trains and evaluates the model
    - Writes results to the `output` CSV

### Submitting Jobs to SLURM

- **`submit.script.sh`**:
  - A shell script for submitting Python ML jobs to a SLURM workload manager.
  - Includes `sbatch` parameters and automatically activates the Python environment before running the target script.

- **`flatML_env.yaml`**:  
  A YAML file exporting the Python environment for flat ML models.  
  Use `conda env create -f flatML_env.yaml` to replicate the environment.

## Python Hierarchical ML Framework

The hML approach extends the concept of flat ML by incorporating hierarchical data structures. This approach is based on work from the [HierarchicalML repository by Sion Bayliss](https://github.com/SionBayliss/HierarchicalML/tree/main).

### Environment File (hML)

- **`hML_env.yaml`**:  
  A YAML file exporting the Python environment for hierarchical ML models.  
  Use `conda env create -f hML_env.yaml` to replicate the environment.

### Model Configuration (hML)

- **`model_config_hML.yml`**:  
  Defines models and parameters for hierarchical ML runs, similar to the flat ML configuration file but adapted to hierarchical structures.

### Data (hML)

- **`data` directory**:  
  Contains hierarchical data structures and graphs.  
  - **`FN_v2_canada_east_west_graph.pkl`**: A NetworkX graph representing hierarchical categories (e.g., root -> east/west -> provinces in Canada).

### Module Functions (hML)

- **`functions_hML.py`**:  
  Custom functions tailored for hierarchical modelling, including specialized feature processing based on hierarchical data.

### Output Storage (hML)

- **`output` directory**:  
  Contains a blank CSV file with headers specific to hML outputs.  
  Includes **`hML_output`** directory with example output files, ensuring that hierarchical results are clearly separated and consistently formatted.

### Model Scripts (hML)

- **`scripts` directory**:  
  Structured similarly to the flat ML scripts but with an additional `unitigs` folder:
  - **`GBC`, `SVM`, `RandomForest`, `Logitboost`**: Similar to flat ML, each folder contains scripts that load parameters from `model_config_hML.yml`, run hierarchical models, and write results to the hML output.
  - **`unitigs`**: Contains Python and shell scripts to generate unitigs (sequence features) used as hierarchical model features.

### Submitting Jobs to SLURM (hML)

- **`submit_script_hML.sh`**:  
  Similar to the flat ML version, this shell script submits hML jobs to SLURM, activating the hML environment and running the appropriate scripts.

## Python Mini ML Framework

The mini ML framework uses a smaller, filtered feature set derived from analyses in `global_locidex_analysis.R`. It follows a similar structure to flat ML but with customized functions and data subsets.

### Environment File (Mini ML)

- **`flatML_env.yaml` (in mini folder)**:  
  Reuses the flat ML environment or a similarly structured one.
  - Create environment: `conda env create -f flatML_env.yaml`

### Model Configuration (Mini ML)

- **`model_config.yml` and `model_config*.yml`**:  
  Defines models/parameters for mini ML runs. Multiple configuration files are provided to showcase additional customization.

### Data (Mini ML)

- **`data` directory**:  
  Contains various `.txt` files listing column names (one per line).  
  These files represent features identified by `global_locidex_analysis.R` that are common between datasets and used in mini models.

### Module Functions (Mini ML)

- **`functions2.py`**:  
  Contains functions similar to `function.py` in flat ML but further tailored for mini models.  
  Includes custom logic for applying OHE across all features.

### Output Storage (Mini ML)

- **`output` directory**:  
  Includes a blank CSV similar to flat ML output.  
  Also stores confusion matrices, test set predictions (actual vs predicted), top 100 features, and saved model artifacts (e.g., via `joblib`).

### Model Scripts (Mini ML)

- **`scripts` directory**:  
  Organized into `RF`, `SVM`, `GBC` subdirectories containing example scripts for running mini models based on `model_config.yml` (or variations).

### Submitting Jobs to SLURM (Mini ML)

- **`submit_script.sh`**:  
  Similar to the flat ML submission script, tailored to run mini ML jobs on SLURM.

## Label Encoding Script

- **`label_encoding.py`** (in `python` folder):  
  This Python script applies label encoding to string features:
  - Iterates over each column in the dataset
  - Converts categorical/string values into integer labels
  - Outputs a transformed dataset for subsequent modeling steps
    
## R Scripts for Analysis

- **`functions.R`**: List of functions used across different R scripts
- **`ml_optimization_analysis.R`**: Runs the Kruskal-Wallis test and Dunn’s test to identify the optimized model/parameter sets.
- **`best_model_analysis.R`**: Performs additional analyses on the best model, including testing with [Locidex](https://github.com/phac-nml/locidex/tree/main) and other datasets.
- **`host_selection.R`**: Source code for deriving host (animal/animalPlus, sometimes referred to as CombinedSites/CombinedSite2, respectively). This script also highlights how we used Enterobase HierCC data as a sampling strategy to select genomically diverse samples across _Salmonella_ serovars.
- **`Locidex_Analysis.R`**: Analyses of optimized models using Locidex features, includes analyses of entropy/confusion matrices.
- **`global_locidex_analysis.R`**: Analyses of full/mini models using a global Enterobase data, including how shared features were determined for mini models and analyses of the mini model performances.
- **`nzv.r`**: Generates the list of columns that is `TRUE` for meeting the Near Zero Variance criteria (see [caret](https://github.com/topepo/caret/blob/master/pkg/caret/R/nearZeroVar.R) for more details).

## Additional Notes

- Before running scripts, ensure you have the appropriate environment set up using the corresponding `*_env.yaml` file.
- Adjust `model_config_34.yml` or `model_config_hML.yml` files to customize models and parameters.
- Adapt `submit.script.sh` and `submit_script_hML.sh` for different job schedulers as needed.
- Check R script headers for any required R packages.
