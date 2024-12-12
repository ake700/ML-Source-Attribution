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
var_patt_script="../HierarchicalML/unitig_pipeline/variants_to_known_patterns"
input_unitig="${base_path}counts.rtab"
output_unitig="${base_path}patterns.tab" 
pattern_conversion="${base_path}patterns_conversion.tab"
pregen_patterns="${base_path}pattern_in_cad_model.txt"

# convert unitigs to patterns present in the UKHSA model 
# -i input unitig PA file
# -o output pattern PA file
# -c pregenerated pattern conversion file  
# -p list of patterns in the Cad model

# "$var_patt_script" -i "$input_unitig" -o "$output_unitig" -c "$pattern_conversion" -p "$pregen_patterns"
"$var_patt_script" -i "$input_unitig" -o "$output_unitig" -c "$pattern_conversion" 

conda deactivate