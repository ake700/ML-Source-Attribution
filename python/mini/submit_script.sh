#!/usr/bin/env bash

# This is submit_script.sh
SCRIPT_PATH=$1
SCRIPT_NAME=$(basename "$SCRIPT_PATH" .py)

sbatch <<- EOF
#!/usr/bin/env bash
#SBATCH -p NMLResearch
#SBATCH -c 11
#SBATCH --mem=5G
#SBATCH --job-name="${SCRIPT_NAME}"
#SBATCH --output=slurm_output/"${SCRIPT_NAME}_%j.out"
#SBATCH --time=5-00:00:00

# Activate environment
source /opt/miniconda3/etc/profile.d/conda.sh
conda deactivate
conda activate ake_ml

# Set the temporary directory to the current directory
export TMPDIR=$PWD

# Print TMPDIR to the output file
echo "TMPDIR is set to: $TMPDIR"

python "$SCRIPT_PATH"
conda deactivate
EOF

# Run on command line or separate shell script
#for script in folder1/*.py; do
#  ./submit_script.sh "$script"
#done