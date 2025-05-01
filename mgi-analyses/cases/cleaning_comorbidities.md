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

To analyse the subset of patients with viral or bacterial pneumonia.  This script is to generate data about comorbidities.  This script can be found in /nfs/turbo/precision-health/DataDirect/HUM00229632 - Genome-wide associations of bacteria/2024-11-12/cases and was most recently run on Wed Nov 13 10:32:59 2024.


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
elixhauser.datafile <- 'ComorbiditiesElixhauserComprehensive.csv'
charlson.datafile <- 'ComorbiditiesCharlsonComprehensive.csv'
encounter.datafile <- 'EncounterAll.csv'
```

Combining demographic files


``` r
#need to know when the first diagnosis was so need encounter data
encounter.data <- read_csv(encounter.datafile) %>%
  mutate(EncounterDate = as.Date(mdy_hm(DeID_AdmitDate),format="%y-%m-%d"))

cm.elix.data <- read_csv(elixhauser.datafile,na='-99') %>%
  left_join(encounter.data,by=c('DeID_PatientID','DeID_EncounterID')) %>%
  filter(!is.na(EncounterDate)) %>% #remove enounters without dates
  arrange(EncounterDate) %>%
  ungroup() %>%
  group_by(DeID_PatientID) %>%
  summarize(CardiacArrhythmias = max(CardiacArrhythmias,na.rm=T),
            CardiacArrhythmiasFirst = first(EncounterDate[CardiacArrhythmias==1]),
            HypertensionComplicated = max(HypertensionComplicated,na.rm=T),
            HypertensionComplicatedFirst = first(EncounterDate[HypertensionComplicated==1]),
            HypertensionUncomplicated = max(HypertensionUncomplicated,na.rm=T),
            HypertensionUncomplicatedFirst = first(EncounterDate[HypertensionUncomplicated==1]),
            LiverDisease = max(LiverDisease,na.rm=T),
            LiverDiseaseFirst = first(EncounterDate[LiverDisease==1]),
            Obesity = max(Obesity,na.rm=T),
            ObesityFirst = first(EncounterDate[Obesity==1]),
            RenalFailure = max(RenalFailure,na.rm=T),
            RenalFailureFirst = first(EncounterDate[RenalFailure==1]))


cm.charlson.data <- read_csv(charlson.datafile,na="-99") %>%
  left_join(encounter.data,by=c('DeID_PatientID','DeID_EncounterID')) %>%
  filter(!is.na(EncounterDate)) %>% #remove enounters without dates
  arrange(EncounterDate) %>%
  ungroup() %>%
  group_by(DeID_PatientID) %>%
  summarize(CerebrovascularDisease = max(CerebrovascularDisease,na.rm=T),
            CerebrovascularDiseaseFirst = first(EncounterDate[CerebrovascularDisease==1]),
            DiabetesWithChronicComplication = max(DiabetesWithChronicComplication,na.rm=T),
            DiabetesWithChronicComplicationFirst = first(EncounterDate[DiabetesWithChronicComplication==1]),
            DiabetesWithoutChronicComplication = max(DiabetesWithoutChronicComplication,na.rm=T),
            DiabetesWithoutChronicComplicationFirst = first(EncounterDate[DiabetesWithoutChronicComplication==1]))
```

# Separate Diagnoses

For COPD need to pull this information from the dignosis summary file.


``` r
diagnosis.datafile <- 'DiagnosesComprehensiveAll.csv'
diagnosis.data <- read_csv(diagnosis.datafile) 

copd.diagnoses <- 
  diagnosis.data %>%
  filter(grepl("OBSTRUCTIVE", TermNameSource)) %>%
  left_join(encounter.data, by=c("DeID_PatientID","DeID_EncounterID")) %>%
  arrange(EncounterDate) %>%
  distinct(DeID_PatientID,.keep_all = T) %>%
  mutate(COPD=1) %>%
  rename(COPDFirst=EncounterDate)
```

There were a total of 426 patients with COPD diagnoses in the dataset.  These were combined with the other comorbidity data.

# Combining and Exporting


``` r
combined.data <-
  full_join(cm.charlson.data,cm.elix.data,by="DeID_PatientID") %>%
  left_join(copd.diagnoses,by="DeID_PatientID")

comorbidities.filename <- 'Comorbidities_clean.csv'
write_csv(combined.data, comorbidities.filename)
```

Wrote this out to Comorbidities_clean.csv.

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
##  [1] bit_4.0.5         jsonlite_1.8.8    compiler_4.4.0    crayon_1.5.3     
##  [5] tidyselect_1.2.1  parallel_4.4.0    jquerylib_0.1.4   yaml_2.3.9       
##  [9] fastmap_1.2.0     R6_2.5.1          generics_0.1.3    tibble_3.2.1     
## [13] bslib_0.7.0       pillar_1.9.0      tzdb_0.4.0        rlang_1.1.4      
## [17] utf8_1.2.4        cachem_1.1.0      xfun_0.45         sass_0.4.9       
## [21] bit64_4.0.5       timechange_0.3.0  cli_3.6.3         withr_3.0.0      
## [25] magrittr_2.0.3    digest_0.6.36     vroom_1.6.5       hms_1.1.3        
## [29] lifecycle_1.0.4   vctrs_0.6.5       evaluate_0.24.0   glue_1.8.0       
## [33] fansi_1.0.6       rmarkdown_2.27    purrr_1.0.2       tools_4.4.0      
## [37] pkgconfig_2.0.3   htmltools_0.5.8.1
```
