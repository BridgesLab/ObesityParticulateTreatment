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

To analyse the subset of patients with viral or bacterial pneumonia.  This script is to idenitify the index cases and ranges for when we are interested in outcomes.  This script can be found in /nfs/turbo/precision-health/DataDirect/HUM00229632 - Genome-wide associations of bacteria and was most recently run on Fri Apr 19 08:15:11 2024.


```r
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

```r
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

```r
diagnosis.datafile <- 'DiagnosesComprehensiveAll.csv'
encounter.datafile <- 'EncounterAll.csv'
encounter.anth.datafile <- "EncounterAnthropometricsBMI.csv"
```


```r
diagnosis.data <- read_csv(diagnosis.datafile) 
mapped.diagnoses <- read_csv("Mapped Diagnoses.csv")

bacterial.diagnoses <- filter(mapped.diagnoses, Type=="Bacterial") %>% pull(TermNameMapped)
viral.diagnoses <- filter(mapped.diagnoses, Type=="Viral") %>% pull(TermNameMapped)
other.diagnoses <- filter(mapped.diagnoses, Type=="Other") %>% pull(TermNameMapped)

pneumonia.diagnosis <- 
  diagnosis.data %>%
  filter(grepl('pneum',TermNameMapped)) %>%
  mutate(Type=case_when(grepl('viral',TermNameMapped)~"Viral",
                        grepl('bacter',TermNameMapped)~"Bacterial",
                        .default=NA)) %>%
  mutate(Type=case_when(TermNameMapped %in% bacterial.diagnoses ~ 'Bacterial',
                        TermNameMapped %in% viral.diagnoses ~ 'Viral',
                        TermNameMapped %in% other.diagnoses ~ 'Other',
                        .default=NA)) %>%
  filter(!is.na(Type)) #remove dignoses not in one of these categories

pneumonia.diagnosis %>%
  group_by(Type) %>%
  distinct(DeID_PatientID,.keep_all = T) %>%
  count %>%
  kable(caption="Diagnosis type by patient")
```



Table: Diagnosis type by patient

|Type      |   n|
|:---------|---:|
|Bacterial | 300|
|Other     | 106|
|Viral     | 501|

# Merging with Encounter Data


```r
encounter.data <- read_csv(encounter.datafile)
encounter.anthro.data <-read_csv(encounter.anth.datafile)

encounter.data.ano <- left_join(encounter.data,encounter.anthro.data,by=c("DeID_PatientID","DeID_EncounterID"))

#when was the encoutner for each pneumonia diagnosis
pneumonia.enc.data <-
  left_join(pneumonia.diagnosis,encounter.data.ano,by=c("DeID_PatientID","DeID_EncounterID")) %>%
  mutate(Duration=difftime(mdy_hm(DeID_DischargeDate),mdy_hm(DeID_AdmitDate),units="days")) #calculate duration between admit and discharge in days

patient.enc.data <- 
  pneumonia.enc.data %>%
  #select(DeID_PatientID,DeID_EncounterID,DeID_AdmitDate,DeID_DischargeDate,AgeInYears,EmergencyVisit,Duration,Type,BMI) %>%
  mutate(AdmitMonth=format_ISO8601(mdy_hm(DeID_AdmitDate),precision="ym")) %>% #calculated month and year of admit
  group_by(DeID_PatientID,AdmitMonth,Type) %>% #summarize encounters by patient,type and admit month, this is defined as one event, if its a different type of pneumonia its considered a new event
  summarize(AgeInYears=median(AgeInYears,na.rm=T), #year at the event
            BMI = median(BMI,na.rm=T), #bmi at the event
            EmergencyVisit=max(EmergencyVisit), # whether there was an emergency visit
            MaxStay=max(Duration), #the max of each duration within this event
            AdmitDate=min(format_ISO8601(mdy_hm(DeID_AdmitDate),precision="ymd")), #when they were admitted
            DischargeDate=max(format_ISO8601(mdy_hm(DeID_DischargeDate),precision="ymd"))) %>% #when they were discharged
  distinct(DeID_PatientID,AdmitMonth,Type,.keep_all = T) 

index.filename <- 'PneumoniaEncounterData.csv'
write_csv(patient.enc.data,index.filename)
```

After grouping multiple encounters into events there are 1822 encounters from 1601

This index encounter data is written out to PneumoniaEncounterData.csv which contains data about the indexed encounter.

## Summary of Patient Encounters


```r
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
|          1|                469|
|          2|                160|
|          3|                 39|
|          4|                 25|
|          5|                 11|
|          6|                  6|
|          7|                  4|
|          8|                  5|
|         10|                  2|
|         14|                  4|
|         15|                  1|
|         16|                  1|
|         18|                  4|
|         20|                  2|
|         22|                  2|
|         23|                  2|
|         24|                  2|
|         26|                  1|
|         27|                  2|
|         29|                  1|
|         30|                  1|
|         33|                  1|
|         40|                  1|
|         42|                  1|
|         46|                  1|

```r
library(ggplot2)
ggplot(patient.encounter.counts,(aes(x=Admissions))) +
  geom_histogram(stat='count',binwidth = 1) +
  labs(x="Admissions for Pneumonia",
       y="Number of Participants")
```

![](figures-index/patient-encounters-1.png)<!-- -->

```r
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


```r
sessionInfo()
```

```
## R version 4.3.1 (2023-06-16)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Red Hat Enterprise Linux 8.6 (Ootpa)
## 
## Matrix products: default
## BLAS:   /sw/pkgs/arc/stacks/gcc/10.3.0/R/4.3.1/lib64/R/lib/libRblas.so 
## LAPACK: /sw/pkgs/arc/stacks/gcc/10.3.0/R/4.3.1/lib64/R/lib/libRlapack.so;  LAPACK version 3.11.0
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
## [1] ggplot2_3.4.3   lubridate_1.9.2 tidyr_1.3.0     dplyr_1.1.3    
## [5] readr_2.1.4     knitr_1.44     
## 
## loaded via a namespace (and not attached):
##  [1] bit_4.0.5        gtable_0.3.4     jsonlite_1.8.7   compiler_4.3.1  
##  [5] crayon_1.5.2     tidyselect_1.2.0 parallel_4.3.1   jquerylib_0.1.4 
##  [9] scales_1.2.1     yaml_2.3.7       fastmap_1.1.1    R6_2.5.1        
## [13] labeling_0.4.3   generics_0.1.3   tibble_3.2.1     munsell_0.5.0   
## [17] bslib_0.5.1      pillar_1.9.0     tzdb_0.4.0       rlang_1.1.1     
## [21] utf8_1.2.3       cachem_1.0.8     xfun_0.40        sass_0.4.7      
## [25] bit64_4.0.5      timechange_0.2.0 cli_3.6.1        withr_2.5.0     
## [29] magrittr_2.0.3   grid_4.3.1       digest_0.6.33    vroom_1.6.3     
## [33] hms_1.1.3        lifecycle_1.0.3  vctrs_0.6.3      evaluate_0.21   
## [37] glue_1.6.2       farver_2.1.1     colorspace_2.1-0 fansi_1.0.4     
## [41] rmarkdown_2.25   purrr_1.0.2      tools_4.3.1      pkgconfig_2.0.3 
## [45] htmltools_0.5.6
```
