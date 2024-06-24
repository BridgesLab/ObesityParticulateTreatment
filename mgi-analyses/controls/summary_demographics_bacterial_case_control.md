---
title: "Demographic Summary for Bacterial Pneumonia Comparing Cases to Controls"
author: "Dave Bridges"
date: "June 18, 2024"
output:
  html_document:
    highlight: tango
    keep_md: yes
    number_sections: yes
    toc: yes
---

## Purpose

This script combines the cleaned datasets of cases of bactrial pneumonia and controls with no types of pneumonia and compares demographic characteristics.  This script can be found in /nfs/turbo/precision-health/DataDirect/HUM00229632 - Genome-wide associations of bacteria/2024-06-12/controls and was most recently run on Wed Jun 19 19:40:41 2024.


``` r
library(knitr)
#figures made will go to directory called figures, will make them as both png and pdf files 
opts_chunk$set(fig.path='figures-case-control/',
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
control.filename <- 'DatasetComplete.csv'
control.data <- read_csv(control.filename) %>%
  mutate(Class="Controls") %>%
  rename("PriorCardiacArrhythmias"="CardiacArrhythmias",
         "PriorCOPD"="COPD",
         "PriorDiabetes"="Diabetes",
         "PriorHypertension"="Hypertension",
         "PriorObesity"="Obesity")
```

```
## Rows: 88191 Columns: 47
```

```
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (17): DeID_PatientID, DeID_EncounterID, TermNameMapped, TermCodeSource,...
## dbl  (20): CerebrovascularDisease, DiabetesWithChronicComplication, Diabetes...
## dttm (10): CerebrovascularDiseaseFirst, DiabetesWithChronicComplicationFirst...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
case.filename <- '../cases/DatasetComplete.csv'
case.data <- read_csv(case.filename) %>%
  filter(Type=="Bacterial") %>%
  arrange(AdmitDate) %>%
  distinct(DeID_PatientID,.keep_all = T) %>%
  mutate(Class="Cases") %>%
  select(-ends_with('First')) %>%
  rename("AgeInYears"="AgeInYears.x") 
```

```
## Rows: 81470 Columns: 63
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (21): DeID_PatientID, AdmitMonth, Type, DeID_EncounterID, TermNameMappe...
## dbl  (28): AgeInYears.x, BMI, EmergencyVisit.x, MaxStay, CerebrovascularDise...
## date (14): AdmitDate, DischargeDate, CerebrovascularDiseaseFirst, DiabetesWi...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
combined.data <-
  bind_rows(case.data,control.data)
```

# Case vs Control


``` r
quant.demo <- 
  combined.data %>%
  group_by(Class) %>%
  summarize(across(.cols=c('AgeInYears','BMI','MaxStay'),
                   .fns=list(mean = ~mean(.,na.rm=T),
                             sd = ~sd(.,na.rm=T),
                             #shapiro.p = ~shapiro.test(.)$p.value,
                             n = ~length(!(is.na(.)))))) %>%
  pivot_longer(cols=contains("_"), #fix this
               names_sep="_",names_to=c("Variable","Statistic")) %>%
  pivot_wider(names_from=Statistic,values_from=value) %>%
  arrange(Variable)

kable(quant.demo, caption="Summary of quantitative values")
```



Table: Summary of quantitative values

|Class    |Variable   |  mean|    sd|     n|
|:--------|:----------|-----:|-----:|-----:|
|Cases    |AgeInYears | 56.43| 16.39|  8352|
|Controls |AgeInYears | 62.80| 11.23| 88191|
|Cases    |BMI        | 30.40|  8.45|  8352|
|Controls |BMI        |   NaN|    NA| 88191|
|Cases    |MaxStay    |  3.66| 11.40|  8352|
|Controls |MaxStay    |   NaN|    NA| 88191|

``` r
# wilcoxon tests, not normally distributed

quant.t.tests <-
  combined.data %>%
  summarize(across(.cols=c('AgeInYears'), #add BMI
                   .fns=list(Mann.Whitney=~wilcox.test(.~Class)$p.value)))

kable(quant.t.tests,caption="Mann-Whitney tests for 30 day survival",digits=c(99,99,99))
```



Table: Mann-Whitney tests for 30 day survival

| AgeInYears_Mann.Whitney|
|-----------------------:|
|                2.38e-85|

``` r
ances.demo.ql <- 
  combined.data %>%
  group_by(MajorityAncestry,Class) %>%
  mutate(Type="Ancestry") %>%
  rename("Group"="MajorityAncestry") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ancestry")

type.demo.ql <- 
  combined.data %>%
  group_by(Type,Class) %>%
  rename("Group"="Type") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Pneumonia Type")

emerg.demo.ql <- 
  combined.data %>%
  group_by(EmergencyVisit.x,Class) %>%
  rename("Group"="EmergencyVisit.x") %>%
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
  group_by(GenderCode,Class) %>%
  rename("Group"="GenderCode") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Gender")

race.demo.ql <- 
combined.data %>%
  group_by(RaceName,Class) %>%
  count %>%
  ungroup %>%
  rename("Group"="RaceName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Race")

ethnicity.demo.ql <- 
combined.data %>%
  group_by(EthnicityName,Class) %>%
  count %>%
  ungroup %>%
  rename("Group"="EthnicityName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ethnicity")

diabetes.demo.ql <- 
  combined.data %>%
  group_by(PriorDiabetes,Class) %>%
  count %>%
  ungroup %>%
  rename("Group"="PriorDiabetes") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorDiabetes") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

smoking.demo.ql <- 
  combined.data %>%
  group_by(SmokingStatusMapped,Class) %>%
  #mutate(Type="Smoking") %>%
  rename("Group"="SmokingStatusMapped") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Smoking") 

#update to hypertension first
hypertension.demo.ql <- 
  combined.data %>%
  group_by(PriorHypertension,Class) %>%
  rename("Group"="PriorHypertension") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorHypertension") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(PriorCardiacArrhythmias,Class) %>%
  rename("Group"="PriorCardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Cardiac Arrhythmias") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

obesity.demo.ql <- 
  combined.data %>%
  group_by(PriorObesity,Class) %>%
  rename("Group"="PriorObesity") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Obesity") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

copd.demo.ql <- 
  combined.data %>%
  group_by(PriorCOPD,Class) %>%
  rename("Group"="PriorCOPD") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior COPD") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

death.demo.ql <- 
  combined.data %>%
  group_by(is.na(DeID_DeathIndexDeceasedDate),Class) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Death")  %>%
  mutate(Group=case_when(Group==TRUE~"Deceased",
                         Group==FALSE~"Alive"))

surv30d.demo.ql <- 
  combined.data %>%
  group_by(Survival.30day,Class) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="30d Survival") 

surv60d.demo.ql <- 
  combined.data %>%
  group_by(Survival.60day,Class) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="60d Survival") 

total.survival <- combined.data %>% group_by(Class) %>% count() %>% pull(n)

summary.discrete <-
  bind_rows(gender.demo.ql,
            race.demo.ql,
            ethnicity.demo.ql,
            ances.demo.ql,
            smoking.demo.ql,
            type.demo.ql,
            #emerg.demo.ql, 
            death.demo.ql,
            surv30d.demo.ql,
            surv60d.demo.ql,
            diabetes.demo.ql,
            hypertension.demo.ql,
            arrythmia.demo.ql,
            copd.demo.ql,
            obesity.demo.ql
            ) %>%
  select(Class,Type,Group,n,Pct) %>%
  ungroup %>%
  select(-Pct) %>%
  pivot_wider(names_from = Class,values_from = n, values_fn = sum) %>%
  arrange(Type,Group) %>%
  replace(.=="NULL", NA) %>%
  mutate(Cases = case_when(is.na(Cases)~0,
                            !(is.na(Cases))~Cases)) %>%
  mutate(Controls = case_when(is.na(Controls)~0,
                            !(is.na(Controls))~Controls)) %>%
  rowwise %>%
  mutate(Pct=Cases/(Cases+Controls)*100) %>%
  mutate(Chisq.p = chisq.test(x=c(Cases,Controls),p=total.survival,rescale.p = T)$p.value) %>%
  mutate(Sig = case_when(Chisq.p < 0.01 ~ '**',
                         Chisq.p < 0.05 ~ '*'))

kable(summary.discrete, 
      caption=paste("Summary of discrete variables for bacterial pneumonia cases, compared to controls.  Overall", round(total.survival[1]/(total.survival[2]+total.survival[1])*100,2),"% cases"),
      digits=c(0,0,0,0,1,99,0))
```



Table: Summary of discrete variables for bacterial pneumonia cases, compared to controls.  Overall 8.65 % cases

|Type                      |Group                                      | Cases| Controls|   Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|-----:|--------:|-----:|--------:|:---|
|30d Survival              |Surival Past 30 Days                       |  2039|        0| 100.0| 0.00e+00|**  |
|30d Survival              |Within 30 Days                             |   216|        0| 100.0| 0.00e+00|**  |
|30d Survival              |NA                                         |  6097|    88191|   6.5| 0.00e+00|**  |
|60d Survival              |Surival Past 60 Days                       |  1931|        0| 100.0| 0.00e+00|**  |
|60d Survival              |Within 60 Days                             |   324|        0| 100.0| 0.00e+00|**  |
|60d Survival              |NA                                         |  6097|    88191|   6.5| 0.00e+00|**  |
|Ancestry                  |AFR                                        |   351|     2692|  11.5| 1.53e-08|**  |
|Ancestry                  |AMR                                        |    29|      277|   9.5| 6.07e-01|NA  |
|Ancestry                  |CSA                                        |    37|      601|   5.8| 1.04e-02|*   |
|Ancestry                  |EAS                                        |    44|      854|   4.9| 6.37e-05|**  |
|Ancestry                  |EUR                                        |  4892|    43906|  10.0| 3.58e-27|**  |
|Ancestry                  |WAS                                        |    49|      591|   7.7| 3.71e-01|NA  |
|Ancestry                  |NA                                         |  2950|    39270|   7.0| 4.98e-34|**  |
|Death                     |Alive                                      |  2256|     7380|  23.4| 0.00e+00|**  |
|Death                     |Deceased                                   |  6096|    80811|   7.0| 4.99e-66|**  |
|Ethnicity                 |Hispanic or Latino                         |   195|     3110|   5.9| 1.85e-08|**  |
|Ethnicity                 |Non-Hispanic or Latino                     |  7956|    81723|   8.9| 1.88e-02|*   |
|Ethnicity                 |Patient Refused                            |    38|      504|   7.0| 1.74e-01|NA  |
|Ethnicity                 |Unknown                                    |   149|     1986|   7.0| 5.99e-03|**  |
|Ethnicity                 |NA                                         |    14|      868|   1.6| 8.49e-14|**  |
|Gender                    |F                                          |  4624|    48931|   8.6| 8.89e-01|NA  |
|Gender                    |M                                          |  3728|    39251|   8.7| 8.66e-01|NA  |
|Gender                    |U                                          |     0|        9|   0.0| 3.56e-01|NA  |
|Pneumonia Type            |Bacterial                                  |  8352|        0| 100.0| 0.00e+00|**  |
|Pneumonia Type            |NA                                         |     0|    88191|   0.0| 0.00e+00|**  |
|Prior COPD                |Yes                                        |   241|     4282|   5.3| 1.88e-15|**  |
|Prior COPD                |NA                                         |  8111|    83909|   8.8| 7.80e-02|NA  |
|Prior Cardiac Arrhythmias |Yes                                        |  2471|    27209|   8.3| 4.60e-02|*   |
|Prior Cardiac Arrhythmias |NA                                         |  5881|    60982|   8.8| 1.84e-01|NA  |
|Prior Obesity             |Yes                                        |  2167|    30910|   6.6| 4.98e-42|**  |
|Prior Obesity             |NA                                         |  6185|    57281|   9.7| 1.05e-22|**  |
|PriorDiabetes             |Yes                                        |  1803|    17022|   9.6| 6.11e-06|**  |
|PriorDiabetes             |NA                                         |  6549|    71169|   8.4| 2.60e-02|*   |
|PriorHypertension         |Yes                                        |  3262|    39349|   7.7| 2.63e-13|**  |
|PriorHypertension         |NA                                         |  5090|    48842|   9.4| 8.07e-11|**  |
|Race                      |African American                           |   672|     6464|   9.4| 2.14e-02|*   |
|Race                      |American Indian or Alaska Native           |    53|      537|   9.0| 7.74e-01|NA  |
|Race                      |Asian                                      |   123|     3395|   3.5| 1.50e-27|**  |
|Race                      |Caucasian                                  |  7266|    73063|   9.0| 7.05e-05|**  |
|Race                      |Native Hawaiian and Other Pacific Islander |     6|       87|   6.5| 4.51e-01|NA  |
|Race                      |Other                                      |   153|     2432|   5.9| 7.75e-07|**  |
|Race                      |Patient Refused                            |    30|      479|   5.9| 2.69e-02|*   |
|Race                      |Unknown                                    |    37|     1018|   3.5| 2.79e-09|**  |
|Race                      |NA                                         |    12|      716|   1.6| 1.80e-11|**  |
|Smoking                   |Current                                    |   783|     8696|   8.3| 1.76e-01|NA  |
|Smoking                   |Former                                     |  3472|    26142|  11.7| 5.99e-79|**  |
|Smoking                   |Never                                      |  3811|    48814|   7.2| 1.32e-30|**  |
|Smoking                   |Unknown                                    |    87|     2257|   3.7| 1.79e-17|**  |
|Smoking                   |NA                                         |   199|     2282|   8.0| 2.64e-01|NA  |
`

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
## [5] knitr_1.47     
## 
## loaded via a namespace (and not attached):
##  [1] bit_4.0.5         jsonlite_1.8.8    compiler_4.4.0    crayon_1.5.2     
##  [5] tidyselect_1.2.1  parallel_4.4.0    jquerylib_0.1.4   yaml_2.3.8       
##  [9] fastmap_1.2.0     R6_2.5.1          generics_0.1.3    tibble_3.2.1     
## [13] bslib_0.7.0       pillar_1.9.0      tzdb_0.4.0        rlang_1.1.4      
## [17] utf8_1.2.4        cachem_1.1.0      xfun_0.44         sass_0.4.9       
## [21] bit64_4.0.5       timechange_0.3.0  cli_3.6.2         withr_3.0.0      
## [25] magrittr_2.0.3    digest_0.6.35     vroom_1.6.5       rstudioapi_0.16.0
## [29] hms_1.1.3         lifecycle_1.0.4   vctrs_0.6.5       evaluate_0.24.0  
## [33] glue_1.7.0        fansi_1.0.6       rmarkdown_2.27    purrr_1.0.2      
## [37] tools_4.4.0       pkgconfig_2.0.3   htmltools_0.5.8.1
```
