Order of scripts to run:
# Folder indicates the working directory

## Should only need to run Step 1 once at the beginning
1) 01_unitig_fastq_to_unitig.sh 
2) folder/01_Accession_filtering_20240122.py
3) 02_unitig_pattern_20231219.sh
4) 03_variants_to_patterns.sh
5) folder/02_gen_patterns_model_20240130.py
6) 04_unitig_pattern_pt2_v2_20240130.sh
7) folder/03_Unitig_clean_20240124.py
8) folder/04_Allele_hML, / folder/04_Unitig_hML 

## Run all unitig scripts with 8 cores and 128G mem
## Run ML scripts per Run.sh specifications
## Run all others with 2 cores and 8G mem