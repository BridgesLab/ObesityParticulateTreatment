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

This script combines the cleaned datasets of cases of bactrial pneumonia and controls with no types of pneumonia and compares demographic characteristics.  This script can be found in /nfs/turbo/precision-health/DataDirect/HUM00229632 - Genome-wide associations of bacteria/2024-11-12/controls and was most recently run on Wed Nov 13 11:00:02 2024.


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
## Rows: 88191 Columns: 48
```

```
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (17): DeID_PatientID, DeID_EncounterID, TermNameMapped, TermCodeSource,...
## dbl  (20): CerebrovascularDisease, DiabetesWithChronicComplication, Diabetes...
## lgl   (1): BMI
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
## Rows: 6986 Columns: 63
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
|Cases    |AgeInYears | 57.40|  16.7|  2367|
|Controls |AgeInYears | 62.80|  11.2| 88191|
|Cases    |BMI        | 32.77| 153.6|  2367|
|Controls |BMI        |   NaN|    NA| 88191|
|Cases    |MaxStay    |  9.19|  19.1|  2367|
|Controls |MaxStay    |   NaN|    NA| 88191|

``` r
# wilcoxon tests, not normally distributed

# quant.t.tests <-
#    combined.data %>%
#   mutate(Class=as.factor(Class)) %>%
#   summarize(across(.cols=c('AgeInYears','BMI'), 
#                    .fns=list(Mann.Whitney=~wilcox.test(.~Class)$p.value)))
# 
# kable(quant.t.tests,caption="Mann-Whitney tests for 30 day survival",digits=c(99,99,99))

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



Table: Summary of discrete variables for bacterial pneumonia cases, compared to controls.  Overall 2.61 % cases

|Type                      |Group                                      | Cases| Controls|   Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|-----:|--------:|-----:|--------:|:---|
|30d Survival              |Surival Past 30 Days                       |   712|        0| 100.0| 0.00e+00|**  |
|30d Survival              |Within 30 Days                             |    98|        0| 100.0| 0.00e+00|**  |
|30d Survival              |NA                                         |  1557|    88191|   1.7| 3.44e-61|**  |
|60d Survival              |Surival Past 60 Days                       |   651|        0| 100.0| 0.00e+00|**  |
|60d Survival              |Within 60 Days                             |   159|        0| 100.0| 0.00e+00|**  |
|60d Survival              |NA                                         |  1557|    88191|   1.7| 3.44e-61|**  |
|Ancestry                  |AFR                                        |   109|     2692|   3.9| 2.25e-05|**  |
|Ancestry                  |AMR                                        |     8|      277|   2.8| 8.38e-01|NA  |
|Ancestry                  |CSA                                        |    12|      601|   2.0| 3.09e-01|NA  |
|Ancestry                  |EAS                                        |    11|      854|   1.3| 1.34e-02|*   |
|Ancestry                  |EUR                                        |  1340|    43906|   3.0| 3.54e-06|**  |
|Ancestry                  |WAS                                        |    18|      591|   3.0| 5.97e-01|NA  |
|Ancestry                  |NA                                         |   869|    39270|   2.2| 1.74e-08|**  |
|Death                     |Alive                                      |   810|     7380|   9.9| 0.00e+00|**  |
|Death                     |Deceased                                   |  1557|    80811|   1.9| 1.01e-38|**  |
|Ethnicity                 |Hispanic or Latino                         |    57|     3110|   1.8| 4.09e-03|**  |
|Ethnicity                 |Non-Hispanic or Latino                     |  2265|    81723|   2.7| 1.32e-01|NA  |
|Ethnicity                 |Patient Refused                            |    10|      504|   1.9| 3.42e-01|NA  |
|Ethnicity                 |Unknown                                    |    34|     1986|   1.7| 8.75e-03|**  |
|Ethnicity                 |NA                                         |     1|      868|   0.1| 3.90e-06|**  |
|Gender                    |F                                          |  1140|    48931|   2.3| 2.28e-06|**  |
|Gender                    |M                                          |  1227|    39251|   3.0| 1.41e-07|**  |
|Gender                    |U                                          |     0|        9|   0.0| 6.23e-01|NA  |
|Pneumonia Type            |Bacterial                                  |  2367|        0| 100.0| 0.00e+00|**  |
|Pneumonia Type            |NA                                         |     0|    88191|   0.0| 0.00e+00|**  |
|Prior COPD                |Yes                                        |    78|     4282|   1.8| 6.41e-04|**  |
|Prior COPD                |NA                                         |  2289|    83909|   2.7| 4.43e-01|NA  |
|Prior Cardiac Arrhythmias |Yes                                        |   676|    27209|   2.4| 4.73e-02|*   |
|Prior Cardiac Arrhythmias |NA                                         |  1691|    60982|   2.7| 1.86e-01|NA  |
|Prior Obesity             |Yes                                        |   486|    30910|   1.5| 2.51e-32|**  |
|Prior Obesity             |NA                                         |  1881|    57281|   3.2| 6.53e-18|**  |
|PriorDiabetes             |Yes                                        |   477|    17022|   2.7| 3.53e-01|NA  |
|PriorDiabetes             |NA                                         |  1890|    71169|   2.6| 6.49e-01|NA  |
|PriorHypertension         |Yes                                        |   791|    39349|   2.0| 6.64e-16|**  |
|PriorHypertension         |NA                                         |  1576|    48842|   3.1| 5.73e-13|**  |
|Race                      |African American                           |   222|     6464|   3.3| 2.93e-04|**  |
|Race                      |American Indian                            |    13|        0| 100.0| 0.00e+00|**  |
|Race                      |American Indian or Alaska Native           |     0|      537|   0.0| 1.47e-04|**  |
|Race                      |Asian                                      |    32|     3395|   0.9| 7.08e-10|**  |
|Race                      |Caucasian                                  |  2043|    73063|   2.7| 6.77e-02|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |     2|       87|   2.2| 8.28e-01|NA  |
|Race                      |Other                                      |    38|     2432|   1.5| 8.09e-04|**  |
|Race                      |Patient Refused                            |     2|      479|   0.4| 2.52e-03|**  |
|Race                      |Unknown                                    |     9|     1018|   0.9| 4.83e-04|**  |
|Race                      |NA                                         |     6|      716|   0.8| 2.68e-03|**  |
|Smoking                   |Current                                    |   142|     8696|   1.6| 2.95e-09|**  |
|Smoking                   |Former                                     |   934|    26142|   3.4| 6.72e-18|**  |
|Smoking                   |Never                                      |   920|    48814|   1.8| 1.28e-26|**  |
|Smoking                   |Unknown                                    |    38|     2257|   1.7| 4.02e-03|**  |
|Smoking                   |NA                                         |   333|     2282|  12.7| 0.00e+00|**  |
`
# Output File for Genetic Analysis


``` r
genetics_outfile <- '../Case_Control_Pneumonia_Incidence.csv'

combined.data %>%
  mutate(CaCo_Incidence = if_else(Class=="Cases",1,0)) %>%
  rename('Age'='AgeInYears') %>%
  mutate(AgeOver65 = case_when(Age>=65~1,
                               Age<65~0)) %>%
  mutate(COPD.History = case_when(COPD==1~1,
                                  .default=0)) %>%
  select(DeID_PatientID,starts_with("CaCo"),Age,AgeOver65,GenderCode,BMI,SmokingStatusMapped,COPD.History,Diabetes) %>% 
  write_csv(genetics_outfile)
```

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
