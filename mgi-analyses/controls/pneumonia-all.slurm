#!  /bin/bash
#SBATCH --job-name Pneumonia
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=20G
#SBATCH --time=00:90:00
#SBATCH --account=davebrid0
#SBATCH --partition=standard
#SBATCH --mail-user=davebrid@umich.edu
#SBATCH --mail-type=END,FAIL
#SBATCH --output=/home/%u/logs/%x-%j.log
#SBATCH --error=/home/%u/error_logs/error-%x-%j.log

module purge
module load R
module load RStudio

echo "Running from $(pwd)"


# requires 30 mins and 64G
#Rscript -e "rmarkdown::render('cleaning_comorbidities.Rmd')"
#Rscript -e "rmarkdown::render('cleaning_demographics.Rmd')"
# merging cleaned data
Rscript -e "rmarkdown::render('combined_data.Rmd')"
