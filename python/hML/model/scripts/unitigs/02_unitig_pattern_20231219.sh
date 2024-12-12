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
input_unitigs="./unitig_output"
unitig_list="../HierarchicalML/unitig_pipeline/call_unitigs_from_list.pl"
accession_list="${base_path}filtered_accessions.tab" 
output_dir="${base_path}"

# Make directory if not there
mkdir -p "$output_dir"

# Identify unitigs from the Canadian collection present in new samples
# -r directory structured as per the output of the unitig_calling pipeline
# -o output directory
# -l list of sample IDs
# -q set of query unitigs (in this instance from Canadian dataset) (OPTIONAL)

# Run the pipeline using the specified directories
"$unitig_list" -r "$input_unitigs" -o "$output_dir" -l "$accession_list" -t 8


conda deactivate