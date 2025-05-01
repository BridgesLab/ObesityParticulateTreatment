---
title: "Identifying and cleaning the comorbidity data for pneumonia"
author: "Dave Bridges"
date: "February 1, 2024"
output:
  html_document:
    highlight: tango
    keep_md: yes
    number_sections: yes
    toc: yes
---

## Purpose

To analyse the subset of patients with viral or bacterial pneumonia.   This script combines the cleaned datasets and writes out a complete datafile for analyses.  This script can be found in /nfs/turbo/precision-health/DataDirect/HUM00229632 - Genome-wide associations of bacteria/2024-11-12/cases and was most recently run on Wed Nov 13 10:34:18 2024.


``` r
library(knitr)
#figures made will go to directory called figures, will make them as both png and pdf files 
opts_chunk$set(fig.path='figures-cm/',
               echo=TRUE, warning=FALSE, message=FALSE,dev=c('png','pdf'))
options(scipen = 2, digits = 3)

library(readr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
library(tidyr)
library(knitr)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

``` r
comorbiditites.datafile <- 'Comorbidities_clean.csv'
pneumonia.encounters.datafile <- 'PneumoniaEncounterData.csv'
demographic.datafile <- 'DemographicDataClean.csv'
```

# Combining  files


``` r
cm.data <- read_csv(comorbiditites.datafile)
pneu.data <- read_csv(pneumonia.encounters.datafile)
demographic.data <- read_csv(demographic.datafile)

combined.data <-
  left_join(pneu.data,cm.data,by="DeID_PatientID") %>%
  left_join(demographic.data,by="DeID_PatientID") %>%
  mutate(Diabetes = ifelse(DiabetesWithChronicComplication==1|DiabetesWithoutChronicComplication==1,1,0)) %>%
  mutate(DiabetesFirst = pmin(DiabetesWithoutChronicComplicationFirst,DiabetesWithChronicComplicationFirst,na.rm=T)) %>% #first date between both diabetes
  mutate(Hypertension = ifelse(HypertensionComplicated==1|HypertensionUncomplicated==1,1,0)) %>% 
  mutate(HypertensionFirst = pmin(HypertensionComplicatedFirst,HypertensionUncomplicatedFirst,na.rm=T)) %>%
  mutate(Survival = as.Date(mdy_hm(DeID_DeathIndexDeceasedDate))-ymd(AdmitDate)) %>%
  mutate(Survival.30day = if_else(Survival <= 30 , "Within 30 Days","Surival Past 30 Days")) %>%
  mutate(Survival.60day = if_else(Survival <= 60 , "Within 60 Days","Surival Past 60 Days")) %>%
  mutate(PriorObesity = case_when(Obesity == 1 & ObesityFirst<AdmitDate ~ 1,
                                Obesity == 1 & ObesityFirst>=AdmitDate ~ 0,
                                Obesity==0~0,
                                is.na(Obesity)~0,
                                is.na(ObesityFirst)~0,
                                .default=0
                                )) %>% 
    mutate(PriorCOPD = case_when(COPD == 1 & COPDFirst<AdmitDate ~ 1,
                                COPD == 1 & COPDFirst>=AdmitDate ~ 0,
                                COPD==0~0,
                                is.na(COPD)~0,
                                is.na(COPDFirst)~0,
                                .default=0
                                )) %>%
  mutate(PriorDiabetes = case_when(Diabetes == 1 & DiabetesFirst<AdmitDate ~ 1,
                                Diabetes == 1 & DiabetesFirst>=AdmitDate ~ 0,
                                Diabetes==0~0,
                                is.na(Diabetes)~0,
                                is.na(DiabetesFirst)~0,
                                .default=0
                                )) %>%
  mutate(PriorHypertension = case_when(Hypertension == 1 & HypertensionFirst<AdmitDate ~ 1,
                                Hypertension == 1 & HypertensionFirst>=AdmitDate ~ 0,
                                Hypertension==0~0,
                                is.na(Hypertension)~0,
                                is.na(HypertensionFirst)~0,
                                .default=0
                                )) %>%
  mutate(PriorCardiacArrhythmias = case_when(CardiacArrhythmias == 1 & HypertensionFirst<AdmitDate ~ 1,
                                CardiacArrhythmias == 1 & CardiacArrhythmiasFirst>=AdmitDate ~ 0,
                                CardiacArrhythmias==0~0,
                                is.na(CardiacArrhythmias)~0,
                                is.na(HypertensionFirst)~0,
                                .default=0
                                )) 

complete.filename <- 'DatasetComplete.csv'
write_csv(combined.data, complete.filename)
```


Wrote this out to DatasetComplete.csv.

# Session Information


``` r
sessionInfo()
```

```
## R version 4.4.0 (2024-04-24)
## Platform: x86_64-pc-linux-gnu
## Running under: Red Hat Enterprise Linux 8.8 (Ootpa)
## 
## Matrix products: default
## BLAS:   /sw/pkgs/arc/stacks/gcc/13.2.0/R/4.4.0/lib64/R/lib/libRblas.so 
## LAPACK: /sw/pkgs/arc/stacks/gcc/13.2.0/R/4.4.0/lib64/R/lib/libRlapack.so;  LAPACK version 3.12.0
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## time zone: America/Detroit
## tzcode source: system (glibc)
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] lubridate_1.9.3 tidyr_1.3.1     dplyr_1.1.4     readr_2.1.5    
## [5] knitr_1.48     
## 
## loaded via a namespace (and not attached):
##  [1] crayon_1.5.3      vctrs_0.6.5       cli_3.6.3         rlang_1.1.4      
##  [5] xfun_0.45         purrr_1.0.2       generics_0.1.3    jsonlite_1.8.8   
##  [9] bit_4.0.5         glue_1.8.0        htmltools_0.5.8.1 sass_0.4.9       
## [13] hms_1.1.3         fansi_1.0.6       rmarkdown_2.27    evaluate_0.24.0  
## [17] jquerylib_0.1.4   tibble_3.2.1      tzdb_0.4.0        fastmap_1.2.0    
## [21] yaml_2.3.9        lifecycle_1.0.4   compiler_4.4.0    timechange_0.3.0 
## [25] pkgconfig_2.0.3   digest_0.6.36     R6_2.5.1          tidyselect_1.2.1 
## [29] utf8_1.2.4        parallel_4.4.0    vroom_1.6.5       pillar_1.9.0     
## [33] magrittr_2.0.3    bslib_0.7.0       bit64_4.0.5       tools_4.4.0      
## [37] cachem_1.1.0
```
