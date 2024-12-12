#!/usr/bin/env bash

# Activate environment
source /opt/miniconda3/etc/profile.d/conda.sh
conda deactivate
conda activate unitig-pipeline

# Exporting the lib used in unitig pipeline?
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/CSCScience.ca/ake/.conda/envs/unitig-pipeline/lib/

# Define the base path as a variable
base_path="./unitigs_v2_Feb2024/kentucky_sistr/data/"

# Directory with the reads and unitig pipeline files
variants_script="./var_to_patterns"
variants_patterns="${base_path}counts.rtab"
output_table="${base_path}output_table"
output_conversion="${base_path}patterns_conversion.tab"

# Identify unitigs from the Canadian collection present in new samples
 # -i|--input		input vcf/table [required]
 # -o|--output		output vcf/table [required]
 # -c|--conversion		output conversion file [required]

# Run the pipeline using the specified directories
"$variants_script" -i "$variants_patterns" -o "$output_table" -c "$output_conversion"

conda deactivate