#!  /bin/bash
#SBATCH --job-name PneumoniaAnalysis
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=40G
#SBATCH --time=01:00:00
#SBATCH --account=davebrid0
#SBATCH --partition=standard
#SBATCH --mail-user=davebrid@umich.edu
#SBATCH --mail-type=END,FAIL
#SBATCH --output=/home/%u/logs/%x-%j.log
#SBATCH --error=/home/%u/error_logs/error-%x-%j.log

module purge
module load R
module load RStudio
module load pandoc

echo "Running from $(pwd)"

# requires 30 mins and 64G
Rscript -e "rmarkdown::render('cases/cleaning_comorbidities.Rmd')"
Rscript -e "rmarkdown::render('cases/cleaning_demographics.Rmd')"
Rscript -e "rmarkdown::render('cases/index_cases.Rmd')"
Rscript -e "rmarkdown::render('cases/combined_data.Rmd')"
Rscript -e "rmarkdown::render('cases/summary_demographics_bacterial.Rmd')"
Rscript -e "rmarkdown::render('controls/cleaning_comorbidities.Rmd')"
Rscript -e "rmarkdown::render('controls/cleaning_demographics.Rmd')"
Rscript -e "rmarkdown::render('controls/combined_data.Rmd')"
Rscript -e "rmarkdown::render('controls/summary_demographics_bacterial_case_control.Rmd')"
Rscript -e "rmarkdown::render('propensity_matching.qmd')"

