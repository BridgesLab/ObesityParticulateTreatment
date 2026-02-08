---
title: "Identifying the index cases for pneumonia"
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

To analyse the subset of patients with viral or bacterial pneumonia.  This script is to idenitify the index cases and ranges for when we are interested in outcomes.  This script can be found in /nfs/turbo/precision-health/DataDirect/HUM00229632 - Genome-wide associations of bacteria/2025-05-12/cases and was most recently run on Mon Dec  1 09:45:56 2025.


``` r
library(knitr)
#figures made will go to directory called figures, will make them as both png and pdf files 
opts_chunk$set(fig.path='figures-index/',
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
diagnosis.datafile <- 'DiagnosesComprehensiveAll.csv'
encounter.datafile <- 'EncounterAll.csv'
encounter.anth.datafile <- "EncounterAnthropometricsBMI.csv"
icd.datafile <- 'Bacterial Pneumonia Phecodes.txt'

phecode.datafile <- 'Phecode_map_v1_2_icd9_icd10cm_09_30_2024.csv.zip'
```

Read in diagnoses from Bacterial Pneumonia Phecodes.txt.  This script matches these to diagnoses in DiagnosesComprehensiveAll.csv, which in term are matched to dates given in the EncounterAll.csv.


``` r
# library(readxl)
diagnosis.data <- read_csv(diagnosis.datafile) 
# diagnosis.datasheet <- 'Pneumonia ICD9 and ICD10 Codes.xlsx'
# mapped.diagnoses <- bind_rows(
#   read_excel(diagnosis.datasheet, sheet="ICD9") %>% rename(ICD=1),
#   read_excel(diagnosis.datasheet, sheet="ICD10", skip=1) %>% rename(ICD=1))%>% 
#   select(ICD,Type)

library(readr)
#bacterial.diagnoses <- read_lines(icd.datafile)

phecode.data <- read_csv(phecode.datafile)
filter(phecode.data, Phecode %in% c('480.1','480.11', '480.12', '480.13')) %>% 
  pull(ICD) -> 
  bacterial.pneumonia.icd
#70 ICD codes

filter(phecode.data, Phecode %in% c('480.11')) %>%
  pull(ICD) -> pneumococcal.pneumonia.icd 
filter(phecode.data, Phecode %in% c('480.12')) %>%
  pull(ICD) -> pseudonomas.pneumonia.icd 
filter(phecode.data, Phecode %in% c('480.13')) %>%
  pull(ICD) -> mrsa.pneumonia.icd 

c('J13','481','481.0') -> klebsiella.icd 

pneumonia.diagnosis <- 
  diagnosis.data %>%
  filter(TermCodeSource %in% bacterial.pneumonia.icd) %>% 
  mutate(Type='Bacterial') %>%
  mutate(SubType = case_when(
    TermCodeSource %in% klebsiella.icd ~ "Klebsiella",
    TermCodeSource %in% pseudonomas.pneumonia.icd ~ "Pseudomonas",
    TermCodeSource %in% mrsa.pneumonia.icd ~ "MRSA",
    TermCodeSource %in% pneumococcal.pneumonia.icd ~ "Pneumococcal"))

pneumonia.diagnosis %>%
  group_by(Type) %>%
  distinct(DeID_PatientID,.keep_all = T) %>%
  count %>%
  kable(caption="Diagnosis type by patient")
```



Table: Diagnosis type by patient

|Type      |    n|
|:---------|----:|
|Bacterial | 2646|

``` r
pneumonia.diagnosis %>%
  group_by(Type,SubType) %>%
  distinct(DeID_PatientID,.keep_all = T) %>%
  count %>%
  kable(caption="Diagnosis type by patient")
```



Table: Diagnosis type by patient

|Type      |SubType      |    n|
|:---------|:------------|----:|
|Bacterial |Klebsiella   |  276|
|Bacterial |MRSA         |  201|
|Bacterial |Pneumococcal |  963|
|Bacterial |Pseudomonas  |  347|
|Bacterial |NA           | 1525|

``` r
pneumonia.diagnosis %>%
  group_by(Type,SubType) %>%
  count %>%
  kable(caption="Diagnoses of each type")
```



Table: Diagnoses of each type

|Type      |SubType      |     n|
|:---------|:------------|-----:|
|Bacterial |Klebsiella   |  1461|
|Bacterial |MRSA         |  1129|
|Bacterial |Pneumococcal |  5124|
|Bacterial |Pseudomonas  |  4848|
|Bacterial |NA           | 21519|

# Merging with Encounter Data


``` r
encounter.data <- read_csv(encounter.datafile)
encounter.anthro.data <-read_csv(encounter.anth.datafile)

encounter.data.ano <- left_join(encounter.data,encounter.anthro.data,by=c("DeID_PatientID","DeID_EncounterID"))

#when was the encounter for each pneumonia diagnosis
pneumonia.enc.data <-
   left_join(pneumonia.diagnosis,encounter.data.ano,by=c("DeID_PatientID","DeID_EncounterID")) %>%
   mutate(Duration=difftime(mdy_hm(DeID_DischargeDate),mdy_hm(DeID_AdmitDate),units="days")) #calculate duration between admit and discharge in days
# 
# patient.enc.data <- 
#   pneumonia.enc.data %>%
#   #select(DeID_PatientID,DeID_EncounterID,DeID_AdmitDate,DeID_DischargeDate,AgeInYears,EmergencyVisit,Duration,Type,BMI) %>%
#   mutate(AdmitMonth=format_ISO8601(mdy_hm(DeID_AdmitDate),precision="ym")) %>% #calculated month and year of admit
#   group_by(DeID_PatientID,AdmitMonth,Type,SubType) %>% #summarize encounters by patient,type and admit month, this is defined as one event, if its a different type of pneumonia its considered a new event
#   summarize(AgeInYears=median(AgeInYears,na.rm=T), #year at the event
#             BMI = median(BMI,na.rm=T), #bmi at the event
#             EmergencyVisit=max(EmergencyVisit), # whether there was an emergency visit
#             MaxStay=max(Duration), #the max of each duration within this event
#             AdmitDate=min(format_ISO8601(mdy_hm(DeID_AdmitDate),precision="ymd")), #when they were admitted
#             DischargeDate=max(format_ISO8601(mdy_hm(DeID_DischargeDate),precision="ymd"))) %>% #when they were discharged
#   distinct(DeID_PatientID,AdmitMonth,Type,.keep_all = T) 

# Sort and group into "visits"
patient.enc.data <- 
  pneumonia.enc.data %>%
  mutate(
    DeID_AdmitDate = mdy_hm(DeID_AdmitDate),
    DeID_DischargeDate = mdy_hm(DeID_DischargeDate)
  ) %>%
  arrange(DeID_PatientID, DeID_AdmitDate) %>%
  group_by(DeID_PatientID) %>%
  mutate(
    DateGap = as.numeric(DeID_AdmitDate - lag(DeID_DischargeDate, default = first(DeID_AdmitDate)), units = "days"),
    NewVisit = if_else(row_number() == 1 | DateGap > 14, 1, 0),
    VisitID = cumsum(NewVisit),
    AdmitDate = format_ISO8601(DeID_AdmitDate, precision="ymd"),
    MaxStay = Duration,
    DischargeDate = format_ISO8601(DeID_DischargeDate, precision="ymd")
  ) %>%
  #summarize(AgeInYears=median(AgeInYears,na.rm=T), #year at the event
  #    VisitID = first(VisitID), #carry over visit ID
  #    BMI = median(BMI,na.rm=T), #bmi at the event
  #    EmergencyVisit=max(EmergencyVisit), # whether there was an emergency visit
  #    MaxStay=max(Duration), #the max of each duration within this event
  #    AdmitDate=first(format_ISO8601(DeID_AdmitDate),precision="ymd"), #when they were admitted
  #    DischargeDate=first(format_ISO8601(mdy_hm(DeID_DischargeDate),precision="ymd"))) %>%
  ungroup() %>%
  distinct(DeID_PatientID,VisitID,.keep_all=T)


index.filename <- 'PneumoniaEncounterData.csv'
write_csv(patient.enc.data,index.filename)
```

After grouping multiple encounters into events there are 7604 encounters from 2646

This index encounter data is written out to PneumoniaEncounterData.csv which contains data about the indexed encounter.

## Summary of Patient Encounters


``` r
patient.enc.data %>% 
  ungroup %>%
  group_by(DeID_PatientID) %>%
  count(name="Admissions") %>%
  arrange(desc(Admissions)) ->
  patient.encounter.counts

kable(patient.encounter.counts %>% ungroup %>% count(Admissions,name="Number of Patients"))
```



| Admissions| Number of Patients|
|----------:|------------------:|
|          1|               1825|
|          2|                381|
|          3|                148|
|          4|                 73|
|          5|                 33|
|          6|                 23|
|          7|                 16|
|          8|                  8|
|          9|                 13|
|         10|                  6|
|         11|                  8|
|         12|                  8|
|         13|                  4|
|         14|                  5|
|         15|                  9|
|         16|                  1|
|         17|                  4|
|         18|                  3|
|         19|                  2|
|         20|                  5|
|         21|                  1|
|         22|                  1|
|         23|                  4|
|         24|                  2|
|         25|                  5|
|         26|                  1|
|         27|                  3|
|         28|                  5|
|         29|                  2|
|         30|                  4|
|         31|                  1|
|         32|                  1|
|         34|                  2|
|         35|                  2|
|         36|                  4|
|         37|                  1|
|         38|                  3|
|         39|                  2|
|         40|                  1|
|         41|                  1|
|         42|                  1|
|         44|                  2|
|         45|                  1|
|         46|                  2|
|         47|                  2|
|         49|                  1|
|         50|                  2|
|         57|                  1|
|         61|                  1|
|         63|                  1|
|         64|                  1|
|         66|                  1|
|         68|                  1|
|         70|                  1|
|         71|                  1|
|         72|                  1|
|         74|                  1|
|         76|                  1|
|         77|                  1|
|         78|                  1|
|        113|                  1|

``` r
kable(patient.enc.data  %>% count(SubType,name="Number of Patients"))
```



|SubType      | Number of Patients|
|:------------|------------------:|
|Klebsiella   |                337|
|MRSA         |                246|
|Pneumococcal |               1326|
|Pseudomonas  |                785|
|NA           |               4910|

``` r
library(ggplot2)
ggplot(patient.encounter.counts,(aes(x=Admissions))) +
  geom_histogram(stat='count',binwidth = 1) +
  labs(x="Admissions for Pneumonia",
       y="Number of Participants")
```

![](figures-index/patient-encounters-1.png)<!-- -->

``` r
patient.enc.data %>% 
  ungroup %>%
  group_by(DeID_PatientID,Type) %>%
  count(name="Admissions") %>%
  arrange(desc(Admissions)) ->
  patient.encounter.type.counts

ggplot(patient.encounter.type.counts,(aes(x=Admissions))) +
  geom_histogram(stat='count',binwidth = 1) +
  facet_grid(~Type)+
  labs(x="Admissions for Pneumonia",
       y="Number of Participants")
```

![](figures-index/patient-encounters-2.png)<!-- -->


# Session Information


``` r
sessionInfo()
```

```
## R version 4.4.3 (2025-02-28)
## Platform: x86_64-pc-linux-gnu
## Running under: Red Hat Enterprise Linux 8.10 (Ootpa)
## 
## Matrix products: default
## BLAS:   /sw/pkgs/arc/stacks/gcc/13.2.0/R/4.4.3/lib64/R/lib/libRblas.so 
## LAPACK: /sw/pkgs/arc/stacks/gcc/13.2.0/R/4.4.3/lib64/R/lib/libRlapack.so;  LAPACK version 3.12.0
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
## [1] ggplot2_3.5.1   lubridate_1.9.3 tidyr_1.3.1     dplyr_1.1.4    
## [5] readr_2.1.5     knitr_1.48     
## 
## loaded via a namespace (and not attached):
##  [1] bit_4.0.5         gtable_0.3.6      jsonlite_1.8.8    highr_0.11       
##  [5] compiler_4.4.3    crayon_1.5.3      tidyselect_1.2.1  parallel_4.4.3   
##  [9] jquerylib_0.1.4   scales_1.3.0      yaml_2.3.9        fastmap_1.2.0    
## [13] R6_2.5.1          labeling_0.4.3    generics_0.1.3    tibble_3.2.1     
## [17] munsell_0.5.1     bslib_0.7.0       pillar_1.9.0      tzdb_0.4.0       
## [21] rlang_1.1.4       utf8_1.2.4        cachem_1.1.0      xfun_0.45        
## [25] sass_0.4.9        bit64_4.0.5       timechange_0.3.0  cli_3.6.3        
## [29] withr_3.0.0       magrittr_2.0.3    grid_4.4.3        digest_0.6.36    
## [33] vroom_1.6.5       hms_1.1.3         lifecycle_1.0.4   vctrs_0.6.5      
## [37] evaluate_0.24.0   glue_1.8.0        farver_2.1.2      colorspace_2.1-0 
## [41] fansi_1.0.6       rmarkdown_2.27    purrr_1.0.2       tools_4.4.3      
## [45] pkgconfig_2.0.3   htmltools_0.5.8.1
```
