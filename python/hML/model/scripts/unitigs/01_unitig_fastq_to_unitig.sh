#!/usr/bin/env bash

# Activate environment
source /opt/miniconda3/etc/profile.d/conda.sh
conda deactivate
conda activate unitig-pipeline

# Exporting the lib used in unitig pipeline?
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/CSCScience.ca/ake/.conda/envs/unitig-pipeline/lib/

# Directory with the reads and unitig pipeline files
unitig_pipeline="../../../../HierarchicalML/unitig_pipeline/unitig_pipeline.pl"
#paired_reads="../data/enterobase/accession_file_list_filtered.tab" 
paired_reads="../../data/fastq_samples_FN.txt" 
output_dir="../../output/unitig_out/"

# Make directory if not there
mkdir -p "$output_dir"

# Run the pipeline using the specified directories
"$unitig_pipeline" --reads "$paired_reads" -o "$output_dir" --cpus 6

echo "pipeline finished"

# Deactivate the environment
conda deactivate