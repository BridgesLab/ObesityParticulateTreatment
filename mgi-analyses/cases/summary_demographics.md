---
title: "Demographic Summary Across all Types of Pneumonia"
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

To analyse the subset of patients with viral or bacterial pneumonia.   This script combines the cleaned datasets and writes out a complete datafile for analyses.  This script can be found in /nfs/turbo/precision-health/DataDirect/HUM00229632 - Genome-wide associations of bacteria and was most recently run on Fri Apr 19 12:24:18 2024.


```r
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
complete.filename <- 'DatasetComplete.csv'
combined.data <- read_csv(complete.filename)
```

```
## Rows: 1822 Columns: 44
```

```
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (12): DeID_PatientID, AdmitMonth, Type, GenderCode, RaceName, Ethnicity...
## dbl  (21): AgeInYears, BMI, EmergencyVisit, MaxStay, CerebrovascularDisease,...
## dttm  (9): CerebrovascularDiseaseFirst, DiabetesWithChronicComplicationFirst...
## date  (2): AdmitDate, DischargeDate
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```



# Summary Demographics


```r
quant.demo <- 
  combined.data %>%
  summarize(across(.cols=c('AgeInYears','BMI','MaxStay'),
                   .fns=list(mean = ~mean(.,na.rm=T),
                             sd = ~sd(.,na.rm=T),
                             n = ~length(!(is.na(.)))))) %>%
  pivot_longer(cols=everything(),
               names_sep="_",names_to=c("Variable","Statistic")) %>%
  pivot_wider(names_from=Statistic,values_from=value)

kable(quant.demo, caption="Summary of quantitative values")
```



Table: Summary of quantitative values

|Variable   |  mean|    sd|    n|
|:----------|-----:|-----:|----:|
|AgeInYears | 56.12| 16.78| 1822|
|BMI        | 29.61|  9.92| 1822|
|MaxStay    |  7.58| 18.49| 1822|

```r
ances.demo.ql <- 
  combined.data %>%
  group_by(MajorityAncestry) %>%
  mutate(Type="Ancestry") %>%
  rename("Group"="MajorityAncestry") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ancestry")

type.demo.ql <- 
  combined.data %>%
  group_by(Type) %>%
  rename("Group"="Type") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Pneumonia Type")

emerg.demo.ql <- 
  combined.data %>%
  group_by(EmergencyVisit) %>%
  rename("Group"="EmergencyVisit") %>%
  count %>%
  mutate(Group=as.factor(Group)) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Emergency Visit") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

# coded as dates not yes/no
# cvd.demo.ql <- 
#   combined.data %>%
#   group_by(CerebrovascularDiseaseFirst) %>%
#   rename("Group"="CerebrovascularDiseaseFirst") %>%
#   count %>%
#   ungroup %>% 
#   mutate(Group=as.factor(Group)) %>%
#   mutate(Pct=n/sum(n)*100) %>%
#   mutate(Type="Cerebrovascular Disease")

gender.demo.ql <- 
  combined.data %>%
  group_by(GenderCode) %>%
  rename("Group"="GenderCode") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Gender")

race.demo.ql <- 
combined.data %>%
  group_by(RaceName) %>%
  count %>%
  ungroup %>%
  rename("Group"="RaceName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Race")

ethnicity.demo.ql <- 
combined.data %>%
  group_by(EthnicityName) %>%
  count %>%
  ungroup %>%
  rename("Group"="EthnicityName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ethnicity")

diabetes.demo.ql <- 
  combined.data %>%
  group_by(Diabetes) %>%
  count %>%
  ungroup %>%
  rename("Group"="Diabetes") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Diabetes") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

smoking.demo.ql <- 
  combined.data %>%
  group_by(SmokingStatusMapped) %>%
  #mutate(Type="Smoking") %>%
  rename("Group"="SmokingStatusMapped") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Smoking") 

#update to hypertension first
hypertension.demo.ql <- 
  combined.data %>%
  group_by(Hypertension) %>%
  rename("Group"="Hypertension") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Hypertension") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(CardiacArrhythmias) %>%
  rename("Group"="CardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Cardiac Arrhythmias") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(CardiacArrhythmias) %>%
  rename("Group"="CardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Cardiac Arrhythmias") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

death.demo.ql <- 
  combined.data %>%
  group_by(is.na(DeID_DeathIndexDeceasedDate)) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Death")  %>%
  mutate(Group=case_when(Group==TRUE~"Deceased",
                         Group==FALSE~"Alive"))

surv30d.demo.ql <- 
  combined.data %>%
  group_by(Survival.30day) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="30d Survival") 

surv60d.demo.ql <- 
  combined.data %>%
  group_by(Survival.60day) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="60d Survival") 

summary.discrete <-
  bind_rows(gender.demo.ql,
            race.demo.ql,
            ethnicity.demo.ql,
            ances.demo.ql,
            smoking.demo.ql,
            type.demo.ql,
            emerg.demo.ql, 
            death.demo.ql,
            surv30d.demo.ql,
            surv60d.demo.ql,
            diabetes.demo.ql,
            hypertension.demo.ql,
            arrythmia.demo.ql,
            ) %>%
  select(Type,Group,n,Pct)

kable(summary.discrete, caption="Summary of discrete variables for all pneumonia cases")
```



Table: Summary of discrete variables for all pneumonia cases

|Type                |Group                            |    n|    Pct|
|:-------------------|:--------------------------------|----:|------:|
|Gender              |F                                |  774| 42.481|
|Gender              |M                                | 1048| 57.519|
|Race                |African American                 |  193| 10.593|
|Race                |American Indian or Alaska Native |   11|  0.604|
|Race                |Asian                            |   39|  2.141|
|Race                |Caucasian                        | 1519| 83.370|
|Race                |Other                            |   55|  3.019|
|Race                |Patient Refused                  |    1|  0.055|
|Race                |Unknown                          |    4|  0.220|
|Ethnicity           |Hispanic or Latino               |   52|  2.854|
|Ethnicity           |Non-Hispanic or Latino           | 1755| 96.323|
|Ethnicity           |Patient Refused                  |    4|  0.220|
|Ethnicity           |Unknown                          |   11|  0.604|
|Ancestry            |AFR                              |   70|  3.842|
|Ancestry            |AMR                              |    3|  0.165|
|Ancestry            |CSA                              |    3|  0.165|
|Ancestry            |EAS                              |    6|  0.329|
|Ancestry            |EUR                              | 1250| 68.606|
|Ancestry            |WAS                              |   19|  1.043|
|Ancestry            |NA                               |  471| 25.851|
|Smoking             |Current                          |   88|  4.830|
|Smoking             |Former                           |  854| 46.872|
|Smoking             |Never                            |  763| 41.877|
|Smoking             |Unknown                          |   31|  1.701|
|Smoking             |NA                               |   86|  4.720|
|Pneumonia Type      |Bacterial                        |  481| 26.400|
|Pneumonia Type      |Other                            |  137|  7.519|
|Pneumonia Type      |Viral                            | 1204| 66.081|
|Emergency Visit     |NA                               | 1189| 65.258|
|Emergency Visit     |Yes                              |  622| 34.138|
|Emergency Visit     |NA                               |   11|  0.604|
|Death               |Alive                            |  763| 41.877|
|Death               |Deceased                         | 1059| 58.123|
|30d Survival        |Surival Past 30 Days             |  685| 37.596|
|30d Survival        |Within 30 Days                   |   73|  4.007|
|30d Survival        |NA                               | 1064| 58.397|
|60d Survival        |Surival Past 60 Days             |  638| 35.016|
|60d Survival        |Within 60 Days                   |  120|  6.586|
|60d Survival        |NA                               | 1064| 58.397|
|Diabetes            |NA                               |  863| 47.366|
|Diabetes            |Yes                              |  959| 52.634|
|Hypertension        |NA                               |  389| 21.350|
|Hypertension        |Yes                              | 1433| 78.650|
|Cardiac Arrhythmias |NA                               |  291| 15.971|
|Cardiac Arrhythmias |Yes                              | 1531| 84.029|

## By Type of Pneumonia



```r
type.demo <- 
  combined.data %>%
  group_by(Type) %>%
  summarize(across(.cols=c('AgeInYears','BMI','MaxStay'),
                   .fns=list(mean = ~mean(.,na.rm=T),
                             sd = ~sd(.,na.rm=T),
                             n = ~length(!(is.na(.)))))) %>%
  pivot_longer(cols=-c(Type),
               names_sep="_",names_to=c("Variable","Statistic")) %>%
  pivot_wider(names_from=Statistic,values_from=value) %>%
  arrange(Variable,Type)

kable(type.demo)
```



|Type      |Variable   |  mean|    sd|    n|
|:---------|:----------|-----:|-----:|----:|
|Bacterial |AgeInYears | 59.63| 14.94|  481|
|Other     |AgeInYears | 60.19| 15.25|  137|
|Viral     |AgeInYears | 54.26| 17.33| 1204|
|Bacterial |BMI        | 28.93| 10.01|  481|
|Other     |BMI        | 27.79|  6.42|  137|
|Viral     |BMI        | 30.14| 10.23| 1204|
|Bacterial |MaxStay    | 16.55| 29.84|  481|
|Other     |MaxStay    | 11.67| 16.56|  137|
|Viral     |MaxStay    |  3.54|  8.99| 1204|

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
## [1] lubridate_1.9.2 tidyr_1.3.0     dplyr_1.1.3     readr_2.1.4    
## [5] knitr_1.44     
## 
## loaded via a namespace (and not attached):
##  [1] bit_4.0.5        jsonlite_1.8.7   compiler_4.3.1   crayon_1.5.2    
##  [5] tidyselect_1.2.0 parallel_4.3.1   jquerylib_0.1.4  yaml_2.3.7      
##  [9] fastmap_1.1.1    R6_2.5.1         generics_0.1.3   tibble_3.2.1    
## [13] bslib_0.5.1      pillar_1.9.0     tzdb_0.4.0       rlang_1.1.1     
## [17] utf8_1.2.3       cachem_1.0.8     xfun_0.40        sass_0.4.7      
## [21] bit64_4.0.5      timechange_0.2.0 cli_3.6.1        withr_2.5.0     
## [25] magrittr_2.0.3   digest_0.6.33    vroom_1.6.3      rstudioapi_0.13 
## [29] hms_1.1.3        lifecycle_1.0.3  vctrs_0.6.3      evaluate_0.21   
## [33] glue_1.6.2       fansi_1.0.4      rmarkdown_2.25   purrr_1.0.2     
## [37] tools_4.3.1      pkgconfig_2.0.3  htmltools_0.5.6
```
