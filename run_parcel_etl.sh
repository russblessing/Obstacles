#!/bin/bash
#SBATCH -p general
#SBATCH -N 1
#SBATCH --cpus-per-task=1
#SBATCH --mem=30G
#SBATCH -t 01-00:00:00
#SBATCH --job-name=parcel_etl
#SBATCH --output=parcel_etl_%j.out
#SBATCH --error=parcel_etl_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=user@email.com
#SBATCH --chdir=/proj/mhinolab/users/rbless/Obstacles

set -euo pipefail

module purge
module load r/4.5.0
module load gdal/3.11.0
module load geos/3.12.0
module load proj/9.2.1

export QUARTO_PATH="$HOME/.local/quarto/quarto-1.6.42/bin/quarto"
export PATH="$HOME/.local/quarto/quarto-1.6.42/bin:$PATH"

echo "PWD at start: $(pwd)"
"$QUARTO_PATH" --version
ls -lh notebooks/01_parcel_ETL.qmd
test -f notebooks/01_parcel_ETL.qmd

Rscript render_qmd.R
