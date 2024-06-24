---
title: "Demographic Summary for Bacterial Pneumonia"
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

To analyse the subset of patients with viral or bacterial pneumonia.   This script combines the cleaned datasets and writes out a complete datafile for analyses.  This script can be found in /nfs/turbo/precision-health/DataDirect/HUM00229632 - Genome-wide associations of bacteria/2024-06-12/cases and was most recently run on Wed Jun 19 19:38:52 2024.


``` r
library(knitr)
#figures made will go to directory called figures, will make them as both png and pdf files 
opts_chunk$set(fig.path='figures-bacterial/',
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
complete.filename <- 'DatasetComplete.csv'
combined.data <- read_csv(complete.filename) %>%
  filter(Type=="Bacterial") %>%
  arrange(AdmitDate) %>%
  distinct(DeID_PatientID,.keep_all = T)
```

```
## Rows: 81470 Columns: 63
```

```
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (21): DeID_PatientID, AdmitMonth, Type, DeID_EncounterID, TermNameMappe...
## dbl  (28): AgeInYears.x, BMI, EmergencyVisit.x, MaxStay, CerebrovascularDise...
## date (14): AdmitDate, DischargeDate, CerebrovascularDiseaseFirst, DiabetesWi...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```



# Summary Demographics


``` r
quant.demo <- 
  combined.data %>%
  summarize(across(.cols=c('AgeInYears.x','BMI','MaxStay'),
                   .fns=list(mean = ~mean(.,na.rm=T),
                             sd = ~sd(.,na.rm=T),
                             n = ~length(!(is.na(.)))))) %>%
  pivot_longer(cols=everything(),
               names_sep="_",names_to=c("Variable","Statistic")) %>%
  pivot_wider(names_from=Statistic,values_from=value)

kable(quant.demo, caption="Summary of quantitative values")
```



Table: Summary of quantitative values

|Variable     |  mean|    sd|    n|
|:------------|-----:|-----:|----:|
|AgeInYears.x | 56.43| 16.39| 8352|
|BMI          | 30.40|  8.45| 8352|
|MaxStay      |  3.66| 11.40| 8352|

``` r
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
  group_by(EmergencyVisit.x) %>%
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
  group_by(PriorDiabetes) %>%
  count %>%
  ungroup %>%
  rename("Group"="PriorDiabetes") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorDiabetes") %>%
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
  group_by(PriorHypertension) %>%
  rename("Group"="PriorHypertension") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorHypertension") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

obesity.demo.ql <- 
  combined.data %>%
  group_by(PriorObesity) %>%
  rename("Group"="PriorObesity") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Obesity") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

copd.demo.ql <- 
  combined.data %>%
  group_by(PriorCOPD) %>%
  rename("Group"="PriorCOPD") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior COPD") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(PriorCardiacArrhythmias) %>%
  rename("Group"="PriorCardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Cardiac Arrhythmias") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(PriorCardiacArrhythmias) %>%
  rename("Group"="PriorCardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Cardiac Arrhythmias") %>%
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
            obesity.demo.ql,
            copd.demo.ql
            ) %>%
  select(Type,Group,n,Pct)

kable(summary.discrete, caption="Summary of discrete variables for all pneumonia cases")
```



Table: Summary of discrete variables for all pneumonia cases

|Type                      |Group                                      |    n|     Pct|
|:-------------------------|:------------------------------------------|----:|-------:|
|Gender                    |F                                          | 4624|  55.364|
|Gender                    |M                                          | 3728|  44.636|
|Race                      |African American                           |  672|   8.046|
|Race                      |American Indian or Alaska Native           |   53|   0.635|
|Race                      |Asian                                      |  123|   1.473|
|Race                      |Caucasian                                  | 7266|  86.997|
|Race                      |Native Hawaiian and Other Pacific Islander |    6|   0.072|
|Race                      |Other                                      |  153|   1.832|
|Race                      |Patient Refused                            |   30|   0.359|
|Race                      |Unknown                                    |   37|   0.443|
|Race                      |NA                                         |   12|   0.144|
|Ethnicity                 |Hispanic or Latino                         |  195|   2.335|
|Ethnicity                 |Non-Hispanic or Latino                     | 7956|  95.259|
|Ethnicity                 |Patient Refused                            |   38|   0.455|
|Ethnicity                 |Unknown                                    |  149|   1.784|
|Ethnicity                 |NA                                         |   14|   0.168|
|Ancestry                  |AFR                                        |  351|   4.203|
|Ancestry                  |AMR                                        |   29|   0.347|
|Ancestry                  |CSA                                        |   37|   0.443|
|Ancestry                  |EAS                                        |   44|   0.527|
|Ancestry                  |EUR                                        | 4892|  58.573|
|Ancestry                  |WAS                                        |   49|   0.587|
|Ancestry                  |NA                                         | 2950|  35.321|
|Smoking                   |Current                                    |  783|   9.375|
|Smoking                   |Former                                     | 3472|  41.571|
|Smoking                   |Never                                      | 3811|  45.630|
|Smoking                   |Unknown                                    |   87|   1.042|
|Smoking                   |NA                                         |  199|   2.383|
|Pneumonia Type            |Bacterial                                  | 8352| 100.000|
|Emergency Visit           |NA                                         | 5970|  71.480|
|Emergency Visit           |Yes                                        | 2380|  28.496|
|Emergency Visit           |NA                                         |    2|   0.024|
|Death                     |Alive                                      | 2256|  27.011|
|Death                     |Deceased                                   | 6096|  72.989|
|30d Survival              |Surival Past 30 Days                       | 2039|  24.413|
|30d Survival              |Within 30 Days                             |  216|   2.586|
|30d Survival              |NA                                         | 6097|  73.000|
|60d Survival              |Surival Past 60 Days                       | 1931|  23.120|
|60d Survival              |Within 60 Days                             |  324|   3.879|
|60d Survival              |NA                                         | 6097|  73.000|
|PriorDiabetes             |NA                                         | 6549|  78.412|
|PriorDiabetes             |Yes                                        | 1803|  21.588|
|PriorHypertension         |NA                                         | 5090|  60.943|
|PriorHypertension         |Yes                                        | 3262|  39.057|
|Prior Cardiac Arrhythmias |NA                                         | 5881|  70.414|
|Prior Cardiac Arrhythmias |Yes                                        | 2471|  29.586|
|Prior Obesity             |NA                                         | 6185|  74.054|
|Prior Obesity             |Yes                                        | 2167|  25.946|
|Prior COPD                |NA                                         | 8111|  97.114|
|Prior COPD                |Yes                                        |  241|   2.886|

# By 30 Day Survival


``` r
combined.data <- 
  combined.data %>%
  mutate(Survival.30day.Group = as.factor(case_when(Survival.30day=="Within 30 Days" ~ "No",
                                    Survival.30day!="Survival Past 30 Days" ~ "Yes",
                                    is.na(Survival.30day) ~ "Yes",
                                    .default="Yes")))

quant.demo <- 
  combined.data %>%
  group_by(Survival.30day.Group) %>%
  summarize(across(.cols=c('AgeInYears.x','BMI','MaxStay'),
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

|Survival.30day.Group |Variable     |  mean|    sd|    n|
|:--------------------|:------------|-----:|-----:|----:|
|No                   |AgeInYears.x | 65.50| 13.57|  216|
|Yes                  |AgeInYears.x | 56.19| 16.39| 8136|
|No                   |BMI          | 27.80|  7.20|  216|
|Yes                  |BMI          | 30.48|  8.47| 8136|
|No                   |MaxStay      | 10.04|  7.37|  216|
|Yes                  |MaxStay      |  3.49| 11.44| 8136|

``` r
# wilcoxon tests, not normally distributed

quant.t.tests <-
  combined.data %>%
  summarize(across(.cols=c('AgeInYears.x','BMI','MaxStay'),
                   .fns=list(Mann.Whitney=~wilcox.test(.~Survival.30day.Group)$p.value)))

kable(quant.t.tests,caption="Mann-Whitney tests for 30 day survival",digits=c(99,99,99))
```



Table: Mann-Whitney tests for 30 day survival

| AgeInYears.x_Mann.Whitney| BMI_Mann.Whitney| MaxStay_Mann.Whitney|
|-------------------------:|----------------:|--------------------:|
|                  2.28e-16|       0.00000154|             2.67e-76|

``` r
ances.demo.ql <- 
  combined.data %>%
  group_by(MajorityAncestry,Survival.30day.Group) %>%
  mutate(Type="Ancestry") %>%
  rename("Group"="MajorityAncestry") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ancestry")

type.demo.ql <- 
  combined.data %>%
  group_by(Type,Survival.30day.Group) %>%
  rename("Group"="Type") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Pneumonia Type")

emerg.demo.ql <- 
  combined.data %>%
  group_by(EmergencyVisit.x,Survival.30day.Group) %>%
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
  group_by(GenderCode,Survival.30day.Group) %>%
  rename("Group"="GenderCode") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Gender")

race.demo.ql <- 
combined.data %>%
  group_by(RaceName,Survival.30day.Group) %>%
  count %>%
  ungroup %>%
  rename("Group"="RaceName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Race")

ethnicity.demo.ql <- 
combined.data %>%
  group_by(EthnicityName,Survival.30day.Group) %>%
  count %>%
  ungroup %>%
  rename("Group"="EthnicityName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ethnicity")

diabetes.demo.ql <- 
  combined.data %>%
  group_by(PriorDiabetes,Survival.30day.Group) %>%
  count %>%
  ungroup %>%
  rename("Group"="PriorDiabetes") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorDiabetes") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

smoking.demo.ql <- 
  combined.data %>%
  group_by(SmokingStatusMapped,Survival.30day.Group) %>%
  #mutate(Type="Smoking") %>%
  rename("Group"="SmokingStatusMapped") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Smoking") 

#update to hypertension first
hypertension.demo.ql <- 
  combined.data %>%
  group_by(PriorHypertension,Survival.30day.Group) %>%
  rename("Group"="PriorHypertension") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorHypertension") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(PriorCardiacArrhythmias,Survival.30day.Group) %>%
  rename("Group"="PriorCardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Cardiac Arrhythmias") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

obesity.demo.ql <- 
  combined.data %>%
  group_by(PriorObesity,Survival.30day.Group) %>%
  rename("Group"="PriorObesity") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Obesity") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

copd.demo.ql <- 
  combined.data %>%
  group_by(PriorCOPD,Survival.30day.Group) %>%
  rename("Group"="PriorCOPD") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior COPD") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

death.demo.ql <- 
  combined.data %>%
  group_by(is.na(DeID_DeathIndexDeceasedDate),Survival.30day.Group) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Death")  %>%
  mutate(Group=case_when(Group==TRUE~"Deceased",
                         Group==FALSE~"Alive"))

surv30d.demo.ql <- 
  combined.data %>%
  group_by(Survival.30day,Survival.30day.Group) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="30d Survival") 

surv60d.demo.ql <- 
  combined.data %>%
  group_by(Survival.60day,Survival.30day.Group) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="60d Survival") 

total.survival <- combined.data %>% group_by(Survival.30day.Group) %>% count() %>% pull(n)

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
            obesity.demo.ql,
            copd.demo.ql
            ) %>%
  select(Survival.30day.Group,Type,Group,n,Pct) %>%
  ungroup %>%
  select(-Pct) %>%
  pivot_wider(names_from = Survival.30day.Group,values_from = n, values_fn = sum) %>%
  arrange(Type,Group) %>%
  replace(.=="NULL", NA) %>%
  mutate(No = case_when(is.na(No)~0,
                            !(is.na(No))~No)) %>%
  mutate(Yes = case_when(is.na(Yes)~0,
                            !(is.na(Yes))~Yes)) %>%
  rowwise %>%
  mutate(Pct=No/(No+Yes)*100) %>%
  mutate(Chisq.p = chisq.test(x=c(No,Yes),p=total.survival,rescale.p = T)$p.value) %>%
  mutate(Sig = case_when(Chisq.p < 0.01 ~ '**',
                         Chisq.p < 0.05 ~ '*'))

kable(summary.discrete, 
      caption=paste("Summary of discrete variables for bacterial pneumonia cases, stratified by 30 day survival.  Overall", round(total.survival[1]/(total.survival[2]+total.survival[1])*100,2),"% visited died at 30d"),
      digits=c(0,0,0,0,1,99,0))
```



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by 30 day survival.  Overall 2.59 % visited died at 30d

|Type                      |Group                                      |  No|  Yes|   Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|---:|----:|-----:|--------:|:---|
|30d Survival              |Surival Past 30 Days                       |   0| 2039|   0.0| 1.87e-13|**  |
|30d Survival              |Within 30 Days                             | 216|    0| 100.0| 0.00e+00|**  |
|30d Survival              |NA                                         |   0| 6097|   0.0| 4.42e-37|**  |
|60d Survival              |Surival Past 60 Days                       |   0| 1931|   0.0| 8.07e-13|**  |
|60d Survival              |Within 60 Days                             | 216|  108|  66.7| 0.00e+00|**  |
|60d Survival              |NA                                         |   0| 6097|   0.0| 4.42e-37|**  |
|Ancestry                  |AFR                                        |   8|  343|   2.3| 7.17e-01|NA  |
|Ancestry                  |AMR                                        |   2|   27|   6.9| 1.44e-01|NA  |
|Ancestry                  |CSA                                        |   1|   36|   2.7| 9.64e-01|NA  |
|Ancestry                  |EAS                                        |   0|   44|   0.0| 2.80e-01|NA  |
|Ancestry                  |EUR                                        | 153| 4739|   3.1| 1.71e-02|*   |
|Ancestry                  |WAS                                        |   1|   48|   2.0| 8.10e-01|NA  |
|Ancestry                  |NA                                         |  51| 2899|   1.7| 3.35e-03|**  |
|Death                     |Alive                                      | 216| 2040|   9.6| 4.16e-97|**  |
|Death                     |Deceased                                   |   0| 6096|   0.0| 4.48e-37|**  |
|Emergency Visit           |Yes                                        | 165| 2215|   6.9| 1.04e-40|**  |
|Emergency Visit           |NA                                         |  51| 5921|   0.9| 3.35e-17|**  |
|Ethnicity                 |Hispanic or Latino                         |   4|  191|   2.1| 6.38e-01|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 206| 7750|   2.6| 9.86e-01|NA  |
|Ethnicity                 |Patient Refused                            |   0|   38|   0.0| 3.15e-01|NA  |
|Ethnicity                 |Unknown                                    |   6|  143|   4.0| 2.68e-01|NA  |
|Ethnicity                 |NA                                         |   0|   14|   0.0| 5.42e-01|NA  |
|Gender                    |F                                          |  79| 4545|   1.7| 1.70e-04|**  |
|Gender                    |M                                          | 137| 3591|   3.7| 2.82e-05|**  |
|Pneumonia Type            |Bacterial                                  | 216| 8136|   2.6| 1.00e+00|NA  |
|Prior COPD                |Yes                                        |   7|  234|   2.9| 7.56e-01|NA  |
|Prior COPD                |NA                                         | 209| 7902|   2.6| 9.57e-01|NA  |
|Prior Cardiac Arrhythmias |Yes                                        |  90| 2381|   3.6| 9.42e-04|**  |
|Prior Cardiac Arrhythmias |NA                                         | 126| 5755|   2.1| 3.20e-02|*   |
|Prior Obesity             |Yes                                        |  49| 2118|   2.3| 3.40e-01|NA  |
|Prior Obesity             |NA                                         | 167| 6018|   2.7| 5.73e-01|NA  |
|PriorDiabetes             |Yes                                        |  59| 1744|   3.3| 6.64e-02|NA  |
|PriorDiabetes             |NA                                         | 157| 6392|   2.4| 3.36e-01|NA  |
|PriorHypertension         |Yes                                        | 103| 3159|   3.2| 3.98e-02|*   |
|PriorHypertension         |NA                                         | 113| 4977|   2.2| 9.98e-02|NA  |
|Race                      |African American                           |  17|  655|   2.5| 9.27e-01|NA  |
|Race                      |American Indian or Alaska Native           |   0|   53|   0.0| 2.36e-01|NA  |
|Race                      |Asian                                      |   1|  122|   0.8| 2.15e-01|NA  |
|Race                      |Caucasian                                  | 193| 7073|   2.7| 7.07e-01|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |   0|    6|   0.0| 6.90e-01|NA  |
|Race                      |Other                                      |   4|  149|   2.6| 9.82e-01|NA  |
|Race                      |Patient Refused                            |   0|   30|   0.0| 3.72e-01|NA  |
|Race                      |Unknown                                    |   1|   36|   2.7| 9.64e-01|NA  |
|Race                      |NA                                         |   0|   12|   0.0| 5.72e-01|NA  |
|Smoking                   |Current                                    |  21|  762|   2.7| 8.66e-01|NA  |
|Smoking                   |Former                                     | 100| 3372|   2.9| 2.75e-01|NA  |
|Smoking                   |Never                                      |  81| 3730|   2.1| 7.31e-02|NA  |
|Smoking                   |Unknown                                    |   3|   84|   3.4| 6.12e-01|NA  |
|Smoking                   |NA                                         |  11|  188|   5.5| 8.94e-03|**  |

# By 60 Day Survival


``` r
combined.data <- 
  combined.data %>%
  mutate(Survival.60.day.Group = case_when(Survival.60day=="Within 60 Days" ~ "No",
                                    Survival.60day!="Survival Past 60 Days" ~ "Yes",
                                    is.na(Survival.60day) ~ "Yes",
                                    .default="Yes"))

quant.demo <- 
  combined.data %>%
  group_by(Survival.60.day.Group) %>%
  summarize(across(.cols=c('AgeInYears.x','BMI','MaxStay'),
                   .fns=list(mean = ~mean(.,na.rm=T),
                             sd = ~sd(.,na.rm=T),
                             #shapiro.p = ~shapiro.test(.)$p.value,
                             n = ~length(!(is.na(.)))))) %>%
  pivot_longer(cols=contains("_"), #all variables separated by underscore
               names_sep="_",names_to=c("Variable","Statistic")) %>%
  pivot_wider(names_from=Statistic,values_from=value) %>%
  arrange(Variable)

kable(quant.demo, caption="Summary of quantitative values")
```



Table: Summary of quantitative values

|Survival.60.day.Group |Variable     |  mean|    sd|    n|
|:---------------------|:------------|-----:|-----:|----:|
|No                    |AgeInYears.x | 65.33| 13.12|  324|
|Yes                   |AgeInYears.x | 56.07| 16.41| 8028|
|No                    |BMI          | 27.88|  7.41|  324|
|Yes                   |BMI          | 30.51|  8.47| 8028|
|No                    |MaxStay      | 12.12| 11.57|  324|
|Yes                   |MaxStay      |  3.32| 11.26| 8028|

``` r
quant.t.tests <-
  combined.data %>%
  summarize(across(.cols=c('AgeInYears.x','BMI','MaxStay'),
                   .fns=list(Mann.Whitney=~wilcox.test(.~Survival.30day.Group)$p.value)))

kable(quant.t.tests,captionn="Mann-Whitney tests for 30 day survival", digits=c(99,99,99))
```



| AgeInYears.x_Mann.Whitney| BMI_Mann.Whitney| MaxStay_Mann.Whitney|
|-------------------------:|----------------:|--------------------:|
|                  2.28e-16|       0.00000154|             2.67e-76|

``` r
ances.demo.ql <- 
  combined.data %>%
  group_by(MajorityAncestry,Survival.60.day.Group) %>%
  mutate(Type="Ancestry") %>%
  rename("Group"="MajorityAncestry") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ancestry")

type.demo.ql <- 
  combined.data %>%
  group_by(Type,Survival.60.day.Group) %>%
  rename("Group"="Type") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Pneumonia Type")

emerg.demo.ql <- 
  combined.data %>%
  group_by(EmergencyVisit.x,Survival.60.day.Group) %>%
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
  group_by(GenderCode,Survival.60.day.Group) %>%
  rename("Group"="GenderCode") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Gender")

race.demo.ql <- 
combined.data %>%
  group_by(RaceName,Survival.60.day.Group) %>%
  count %>%
  ungroup %>%
  rename("Group"="RaceName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Race")

ethnicity.demo.ql <- 
combined.data %>%
  group_by(EthnicityName,Survival.60.day.Group) %>%
  count %>%
  ungroup %>%
  rename("Group"="EthnicityName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ethnicity")

diabetes.demo.ql <- 
  combined.data %>%
  group_by(PriorDiabetes,Survival.60.day.Group) %>%
  count %>%
  ungroup %>%
  rename("Group"="PriorDiabetes") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorDiabetes") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

smoking.demo.ql <- 
  combined.data %>%
  group_by(SmokingStatusMapped,Survival.60.day.Group) %>%
  #mutate(Type="Smoking") %>%
  rename("Group"="SmokingStatusMapped") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Smoking") 

#update to hypertension first
hypertension.demo.ql <- 
  combined.data %>%
  group_by(PriorHypertension,Survival.60.day.Group) %>%
  rename("Group"="PriorHypertension") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorHypertension") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(PriorCardiacArrhythmias,Survival.60.day.Group) %>%
  rename("Group"="PriorCardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Cardiac Arrhythmias") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

obesity.demo.ql <- 
  combined.data %>%
  group_by(PriorObesity,Survival.60.day.Group) %>%
  rename("Group"="PriorObesity") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Obesity") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

copd.demo.ql <- 
  combined.data %>%
  group_by(PriorCOPD,Survival.60.day.Group) %>%
  rename("Group"="PriorCOPD") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior COPD") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

death.demo.ql <- 
  combined.data %>%
  group_by(is.na(DeID_DeathIndexDeceasedDate),Survival.60.day.Group) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Death")  %>%
  mutate(Group=case_when(Group==TRUE~"Deceased",
                         Group==FALSE~"Alive"))

surv30d.demo.ql <- 
  combined.data %>%
  group_by(Survival.30day,Survival.60.day.Group) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="60d Survival") 

surv60d.demo.ql <- 
  combined.data %>%
  group_by(Survival.60day,Survival.60.day.Group) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="60d Survival") 

total.survival <- combined.data %>% group_by(Survival.60.day.Group) %>% count() %>% pull(n)

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
            obesity.demo.ql,
            copd.demo.ql
            ) %>%
  select(Survival.60.day.Group,Type,Group,n,Pct) %>%
  ungroup %>%
  select(-Pct) %>%
  pivot_wider(names_from = Survival.60.day.Group,values_from = n, values_fn = sum) %>%
  arrange(Type,Group) %>%
  replace(.=="NULL", NA) %>%
  mutate(No = case_when(is.na(No)~0,
                            !(is.na(No))~No)) %>%
  mutate(Yes = case_when(is.na(Yes)~0,
                            !(is.na(Yes))~Yes)) %>%
  rowwise %>%
  mutate(Pct=No/(No+Yes)*100) %>% #because want not survived
  mutate(Chisq.p = chisq.test(x=c(No,Yes),p=total.survival,rescale.p = T)$p.value) %>%
  mutate(Sig = case_when(Chisq.p < 0.01 ~ '**',
                         Chisq.p < 0.05 ~ '*'))

kable(summary.discrete, 
      caption=paste("Summary of discrete variables for bacterial pneumonia cases, stratified by 60 day survival.  Overall", round(total.survival[1]/(total.survival[2]+total.survival[1])*100,2),"% visited died at 60d"),
      digits=c(0,0,0,0,1,99,0))
```



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by 60 day survival.  Overall 3.88 % visited died at 60d

|Type                      |Group                                      |  No|   Yes|   Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|---:|-----:|-----:|--------:|:---|
|60d Survival              |Surival Past 30 Days                       | 108|  1931|   5.3| 9.18e-04|**  |
|60d Survival              |Surival Past 60 Days                       |   0|  1931|   0.0| 1.07e-18|**  |
|60d Survival              |Within 30 Days                             | 216|     0| 100.0| 0.00e+00|**  |
|60d Survival              |Within 60 Days                             | 324|     0| 100.0| 0.00e+00|**  |
|60d Survival              |NA                                         |   0| 12194|   0.0| 0.00e+00|**  |
|Ancestry                  |AFR                                        |  14|   337|   4.0| 9.16e-01|NA  |
|Ancestry                  |AMR                                        |   3|    26|  10.3| 7.14e-02|NA  |
|Ancestry                  |CSA                                        |   1|    36|   2.7| 7.11e-01|NA  |
|Ancestry                  |EAS                                        |   0|    44|   0.0| 1.83e-01|NA  |
|Ancestry                  |EUR                                        | 229|  4663|   4.7| 3.68e-03|**  |
|Ancestry                  |WAS                                        |   1|    48|   2.0| 5.05e-01|NA  |
|Ancestry                  |NA                                         |  76|  2874|   2.6| 2.47e-04|**  |
|Death                     |Alive                                      | 324|  1932|  14.4| 0.00e+00|**  |
|Death                     |Deceased                                   |   0|  6096|   0.0| 1.91e-55|**  |
|Emergency Visit           |Yes                                        | 235|  2145|   9.9| 8.19e-52|**  |
|Emergency Visit           |NA                                         |  89|  5883|   1.5| 1.17e-21|**  |
|Ethnicity                 |Hispanic or Latino                         |   4|   191|   2.1| 1.86e-01|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 313|  7643|   3.9| 8.00e-01|NA  |
|Ethnicity                 |Patient Refused                            |   0|    38|   0.0| 2.16e-01|NA  |
|Ethnicity                 |Unknown                                    |   7|   142|   4.7| 6.05e-01|NA  |
|Ethnicity                 |NA                                         |   0|    14|   0.0| 4.52e-01|NA  |
|Gender                    |F                                          | 124|  4500|   2.7| 2.47e-05|**  |
|Gender                    |M                                          | 200|  3528|   5.4| 2.64e-06|**  |
|Pneumonia Type            |Bacterial                                  | 324|  8028|   3.9| 1.00e+00|NA  |
|Prior COPD                |Yes                                        |  12|   229|   5.0| 3.77e-01|NA  |
|Prior COPD                |NA                                         | 312|  7799|   3.8| 8.79e-01|NA  |
|Prior Cardiac Arrhythmias |Yes                                        | 139|  2332|   5.6| 6.97e-06|**  |
|Prior Cardiac Arrhythmias |NA                                         | 185|  5696|   3.1| 3.58e-03|**  |
|Prior Obesity             |Yes                                        |  78|  2089|   3.6| 5.00e-01|NA  |
|Prior Obesity             |NA                                         | 246|  5939|   4.0| 6.90e-01|NA  |
|PriorDiabetes             |Yes                                        |  85|  1718|   4.7| 6.63e-02|NA  |
|PriorDiabetes             |NA                                         | 239|  6310|   3.6| 3.35e-01|NA  |
|PriorHypertension         |Yes                                        | 157|  3105|   4.8| 5.75e-03|**  |
|PriorHypertension         |NA                                         | 167|  4923|   3.3| 2.71e-02|*   |
|Race                      |African American                           |  25|   647|   3.7| 8.31e-01|NA  |
|Race                      |American Indian or Alaska Native           |   0|    53|   0.0| 1.44e-01|NA  |
|Race                      |Asian                                      |   2|   121|   1.6| 1.96e-01|NA  |
|Race                      |Caucasian                                  | 291|  6975|   4.0| 5.79e-01|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |   0|     6|   0.0| 6.23e-01|NA  |
|Race                      |Other                                      |   5|   148|   3.3| 6.95e-01|NA  |
|Race                      |Patient Refused                            |   0|    30|   0.0| 2.71e-01|NA  |
|Race                      |Unknown                                    |   1|    36|   2.7| 7.11e-01|NA  |
|Race                      |NA                                         |   0|    12|   0.0| 4.86e-01|NA  |
|Smoking                   |Current                                    |  26|   757|   3.3| 4.18e-01|NA  |
|Smoking                   |Former                                     | 160|  3312|   4.6| 2.61e-02|*   |
|Smoking                   |Never                                      | 120|  3691|   3.1| 1.95e-02|*   |
|Smoking                   |Unknown                                    |   4|    83|   4.6| 7.29e-01|NA  |
|Smoking                   |NA                                         |  14|   185|   7.0| 2.11e-02|*   |

# By ER Visit


``` r
combined.data <- 
  combined.data %>%
  mutate(EmergencyVisit.Group = case_when(EmergencyVisit.x==0 ~ "No",
                                    EmergencyVisit.x==1 ~ "Yes",
                                    is.na(EmergencyVisit.x) ~ "No",
                                    .default="No"))

quant.demo <- 
  combined.data %>%
  group_by(EmergencyVisit.Group) %>%
  summarize(across(.cols=c('AgeInYears.x','BMI','MaxStay'),
                   .fns=list(mean = ~mean(.,na.rm=T),
                             sd = ~sd(.,na.rm=T),
                             #shapiro.p = ~shapiro.test(.)$p.value,
                             n = ~length(!(is.na(.)))))) %>%
  pivot_longer(cols=contains("_"),#all varaibles separated by underscore
               names_sep="_",names_to=c("Variable","Statistic")) %>%
  pivot_wider(names_from=Statistic,values_from=value) %>%
  arrange(Variable)

kable(quant.demo, caption="Summary of quantitative values")
```



Table: Summary of quantitative values

|EmergencyVisit.Group |Variable     |  mean|    sd|    n|
|:--------------------|:------------|-----:|-----:|----:|
|No                   |AgeInYears.x | 55.77| 16.22| 5972|
|Yes                  |AgeInYears.x | 58.07| 16.71| 2380|
|No                   |BMI          | 30.60|  8.44| 5972|
|Yes                  |BMI          | 29.95|  8.44| 2380|
|No                   |MaxStay      |  2.11| 10.09| 5972|
|Yes                  |MaxStay      |  7.55| 13.39| 2380|

``` r
# wilcoxon tests, not normally distributed

quant.t.tests <-
  combined.data %>%
  summarize(across(.cols=c('AgeInYears.x','BMI','MaxStay'),
                   .fns=list(Mann.Whitney=~wilcox.test(.~EmergencyVisit.Group)$p.value)))

kable(quant.t.tests,captionn="Mann-Whitney tests for ER visit",digits=c(99,99,99))
```



| AgeInYears.x_Mann.Whitney| BMI_Mann.Whitney| MaxStay_Mann.Whitney|
|-------------------------:|----------------:|--------------------:|
|                  4.27e-09|        0.0000699|                    0|

``` r
ances.demo.ql <- 
  combined.data %>%
  group_by(MajorityAncestry,EmergencyVisit.Group) %>%
  mutate(Type="Ancestry") %>%
  rename("Group"="MajorityAncestry") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ancestry")

type.demo.ql <- 
  combined.data %>%
  group_by(Type,EmergencyVisit.Group) %>%
  rename("Group"="Type") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Pneumonia Type")

emerg.demo.ql <- 
  combined.data %>%
  group_by(EmergencyVisit.Group) %>%
  rename("Group"="EmergencyVisit.Group") %>%
  count %>%
  mutate(Group=as.factor(Group)) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Emergency Visit") 

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
  group_by(GenderCode,EmergencyVisit.Group) %>%
  rename("Group"="GenderCode") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Gender")

race.demo.ql <- 
combined.data %>%
  group_by(RaceName,EmergencyVisit.Group) %>%
  count %>%
  ungroup %>%
  rename("Group"="RaceName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Race")

ethnicity.demo.ql <- 
combined.data %>%
  group_by(EthnicityName,EmergencyVisit.Group) %>%
  count %>%
  ungroup %>%
  rename("Group"="EthnicityName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ethnicity")

diabetes.demo.ql <- 
  combined.data %>%
  group_by(PriorDiabetes,EmergencyVisit.Group) %>%
  count %>%
  ungroup %>%
  rename("Group"="PriorDiabetes") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorDiabetes") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

smoking.demo.ql <- 
  combined.data %>%
  group_by(SmokingStatusMapped,EmergencyVisit.Group) %>%
  #mutate(Type="Smoking") %>%
  rename("Group"="SmokingStatusMapped") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Smoking") 

#update to hypertension first
hypertension.demo.ql <- 
  combined.data %>%
  group_by(PriorHypertension,EmergencyVisit.Group) %>%
  rename("Group"="PriorHypertension") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorHypertension") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(PriorCardiacArrhythmias,EmergencyVisit.Group) %>%
  rename("Group"="PriorCardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Cardiac Arrhythmias") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

obesity.demo.ql <- 
  combined.data %>%
  group_by(PriorObesity,EmergencyVisit.Group) %>%
  rename("Group"="PriorObesity") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Obesity") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

copd.demo.ql <- 
  combined.data %>%
  group_by(PriorCOPD,EmergencyVisit.Group) %>%
  rename("Group"="PriorCOPD") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior COPD") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

death.demo.ql <- 
  combined.data %>%
  group_by(is.na(DeID_DeathIndexDeceasedDate),EmergencyVisit.Group) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Death")  %>%
  mutate(Group=case_when(Group==TRUE~"Deceased",
                         Group==FALSE~"Alive"))

surv30d.demo.ql <- 
  combined.data %>%
  group_by(Survival.30day,EmergencyVisit.Group) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="30d Survival") 

surv60d.demo.ql <- 
  combined.data %>%
  group_by(Survival.60day,EmergencyVisit.Group) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="60d Survival") 

total.survival <- combined.data %>% group_by(EmergencyVisit.Group) %>% count() %>% pull(n)

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
            obesity.demo.ql,
            copd.demo.ql
            ) %>%
  select(EmergencyVisit.Group,Type,Group,n,Pct) %>%
  ungroup %>%
  select(-Pct) %>%
  pivot_wider(names_from = EmergencyVisit.Group,values_from = n, values_fn = sum) %>%
  arrange(Type,Group) %>%
  replace(.=="NULL", NA) %>%
  mutate(No = case_when(is.na(No)~0,
                            !(is.na(No))~No)) %>%
  mutate(Yes = case_when(is.na(Yes)~0,
                            !(is.na(Yes))~Yes)) %>%
  rowwise %>%
  mutate(Pct=Yes/(No+Yes)*100) %>%
  mutate(Chisq.p = chisq.test(x=c(No,Yes),p=total.survival,rescale.p = T)$p.value) %>%
  mutate(Sig = case_when(Chisq.p < 0.01 ~ '**',
                         Chisq.p < 0.05 ~ '*'))

kable(summary.discrete, 
      caption=paste("Summary of discrete variables for bacterial pneumonia cases, stratified by ER visit survival.  Overall", round(total.survival[2]/(total.survival[2]+total.survival[1])*100,2),"% visited ER"),
      digits=c(0,0,0,0,1,99,0))
```



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by ER visit survival.  Overall 28.5 % visited ER

|Type                      |Group                                      |   No|  Yes|  Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|----:|----:|----:|--------:|:---|
|30d Survival              |Surival Past 30 Days                       | 1240|  799| 39.2| 1.09e-26|**  |
|30d Survival              |Within 30 Days                             |   51|  165| 76.4| 8.08e-55|**  |
|30d Survival              |NA                                         | 4681| 1416| 23.2| 7.58e-20|**  |
|60d Survival              |Surival Past 60 Days                       | 1202|  729| 37.8| 2.04e-19|**  |
|60d Survival              |Within 60 Days                             |   89|  235| 72.5| 5.04e-69|**  |
|60d Survival              |NA                                         | 4681| 1416| 23.2| 7.58e-20|**  |
|Ancestry                  |AFR                                        |  205|  146| 41.6| 5.42e-08|**  |
|Ancestry                  |AMR                                        |   18|   11| 37.9| 2.60e-01|NA  |
|Ancestry                  |CSA                                        |   29|    8| 21.6| 3.54e-01|NA  |
|Ancestry                  |EAS                                        |   35|    9| 20.5| 2.37e-01|NA  |
|Ancestry                  |EUR                                        | 3491| 1401| 28.6| 8.25e-01|NA  |
|Ancestry                  |WAS                                        |   30|   19| 38.8| 1.11e-01|NA  |
|Ancestry                  |NA                                         | 2164|  786| 26.6| 2.58e-02|*   |
|Death                     |Alive                                      | 1292|  964| 42.7| 1.03e-50|**  |
|Death                     |Deceased                                   | 4680| 1416| 23.2| 8.12e-20|**  |
|Ethnicity                 |Hispanic or Latino                         |  132|   63| 32.3| 2.38e-01|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 5665| 2291| 28.8| 5.54e-01|NA  |
|Ethnicity                 |Patient Refused                            |   33|    5| 13.2| 3.62e-02|*   |
|Ethnicity                 |Unknown                                    |  128|   21| 14.1| 9.84e-05|**  |
|Ethnicity                 |NA                                         |   14|    0|  0.0| 1.82e-02|*   |
|Gender                    |F                                          | 3518| 1106| 23.9| 5.36e-12|**  |
|Gender                    |M                                          | 2454| 1274| 34.2| 1.59e-14|**  |
|Pneumonia Type            |Bacterial                                  | 5972| 2380| 28.5| 1.00e+00|NA  |
|Prior COPD                |Yes                                        |  129|  112| 46.5| 6.31e-10|**  |
|Prior COPD                |NA                                         | 5843| 2268| 28.0| 2.87e-01|NA  |
|Prior Cardiac Arrhythmias |Yes                                        | 1571|  900| 36.4| 2.58e-18|**  |
|Prior Cardiac Arrhythmias |NA                                         | 4401| 1480| 25.2| 1.53e-08|**  |
|Prior Obesity             |Yes                                        | 1504|  663| 30.6| 3.04e-02|*   |
|Prior Obesity             |NA                                         | 4468| 1717| 27.8| 2.00e-01|NA  |
|PriorDiabetes             |Yes                                        | 1183|  620| 34.4| 3.00e-08|**  |
|PriorDiabetes             |NA                                         | 4789| 1760| 26.9| 3.64e-03|**  |
|PriorHypertension         |Yes                                        | 2226| 1036| 31.8| 3.64e-05|**  |
|PriorHypertension         |NA                                         | 3746| 1344| 26.4| 9.48e-04|**  |
|Race                      |African American                           |  401|  271| 40.3| 1.09e-11|**  |
|Race                      |American Indian or Alaska Native           |   44|    9| 17.0| 6.33e-02|NA  |
|Race                      |Asian                                      |   93|   30| 24.4| 3.13e-01|NA  |
|Race                      |Caucasian                                  | 5259| 2007| 27.6| 9.87e-02|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |    5|    1| 16.7| 5.21e-01|NA  |
|Race                      |Other                                      |   99|   54| 35.3| 6.25e-02|NA  |
|Race                      |Patient Refused                            |   27|    3| 10.0| 2.48e-02|*   |
|Race                      |Unknown                                    |   33|    4| 10.8| 1.72e-02|*   |
|Race                      |NA                                         |   11|    1|  8.3| 1.22e-01|NA  |
|Smoking                   |Current                                    |  566|  217| 27.7| 6.28e-01|NA  |
|Smoking                   |Former                                     | 2425| 1047| 30.2| 3.03e-02|*   |
|Smoking                   |Never                                      | 2745| 1066| 28.0| 4.73e-01|NA  |
|Smoking                   |Unknown                                    |   61|   26| 29.9| 7.74e-01|NA  |
|Smoking                   |NA                                         |  175|   24| 12.1| 2.80e-07|**  |

# By Longer than Median Stay

The median stay was 0.042.


``` r
combined.data <- 
  combined.data %>%
  mutate(StayAboveMedian=case_when(MaxStay>median(MaxStay,na.rm=T)~"Yes",
                                MaxStay<median(MaxStay,na.rm=T)~"No"))

quant.demo <- 
  combined.data %>%
  group_by(StayAboveMedian) %>%
  summarize(across(.cols=c('AgeInYears.x','BMI','MaxStay'),
                   .fns=list(mean = ~mean(.,na.rm=T),
                             sd = ~sd(.,na.rm=T),
                             #shapiro.p = ~shapiro.test(.)$p.value,
                             n = ~length(!(is.na(.)))))) %>%
  pivot_longer(cols=2:10,
               names_sep="_",names_to=c("Variable","Statistic")) %>%
  pivot_wider(names_from=Statistic,values_from=value) %>%
  arrange(Variable)

kable(quant.demo, caption="Summary of quantitative values for above average length of stay")
```



Table: Summary of quantitative values for above average length of stay

|StayAboveMedian |Variable     |   mean|     sd|    n|
|:---------------|:------------|------:|------:|----:|
|No              |AgeInYears.x | 54.893| 16.361| 4167|
|Yes             |AgeInYears.x | 57.951| 16.292| 4173|
|NA              |AgeInYears.x | 62.200| 11.223|   12|
|No              |BMI          | 30.744|  8.281| 4167|
|Yes             |BMI          | 30.076|  8.582| 4173|
|NA              |BMI          | 31.622| 10.541|   12|
|No              |MaxStay      |  0.003|  0.022| 4167|
|Yes             |MaxStay      |  7.328| 15.266| 4173|
|NA              |MaxStay      |  0.042|  0.000|   12|

``` r
# wilcoxon tests, not normally distributed

quant.t.tests <-
  combined.data %>%
  summarize(across(.cols=c('AgeInYears.x','BMI'),
                   .fns=list(Mann.Whitney=~wilcox.test(.~StayAboveMedian)$p.value)))

kable(quant.t.tests,captionn="Mann-Whitney tests for ER visit",digits=c(99,99,99))
```



| AgeInYears.x_Mann.Whitney| BMI_Mann.Whitney|
|-------------------------:|----------------:|
|                  2.18e-18|        0.0000131|

``` r
ances.demo.ql <- 
  combined.data %>%
  group_by(MajorityAncestry,StayAboveMedian) %>%
  mutate(Type="Ancestry") %>%
  rename("Group"="MajorityAncestry") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ancestry")

type.demo.ql <- 
  combined.data %>%
  group_by(Type,StayAboveMedian) %>%
  rename("Group"="Type") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Pneumonia Type")

emerg.demo.ql <- 
  combined.data %>%
  group_by(EmergencyVisit.Group,StayAboveMedian) %>%
  rename("Group"="EmergencyVisit.Group") %>%
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
  group_by(GenderCode,StayAboveMedian) %>%
  rename("Group"="GenderCode") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Gender")

race.demo.ql <- 
combined.data %>%
  group_by(RaceName,StayAboveMedian) %>%
  count %>%
  ungroup %>%
  rename("Group"="RaceName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Race")

ethnicity.demo.ql <- 
combined.data %>%
  group_by(EthnicityName,StayAboveMedian) %>%
  count %>%
  ungroup %>%
  rename("Group"="EthnicityName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ethnicity")

diabetes.demo.ql <- 
  combined.data %>%
  group_by(PriorDiabetes,StayAboveMedian) %>%
  count %>%
  ungroup %>%
  rename("Group"="PriorDiabetes") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorDiabetes") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

smoking.demo.ql <- 
  combined.data %>%
  group_by(SmokingStatusMapped,StayAboveMedian) %>%
  #mutate(Type="Smoking") %>%
  rename("Group"="SmokingStatusMapped") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Smoking") 

#update to hypertension first
hypertension.demo.ql <- 
  combined.data %>%
  group_by(PriorHypertension,StayAboveMedian) %>%
  rename("Group"="PriorHypertension") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorHypertension") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(PriorCardiacArrhythmias,StayAboveMedian) %>%
  rename("Group"="PriorCardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Cardiac Arrhythmias") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

obesity.demo.ql <- 
  combined.data %>%
  group_by(PriorObesity,StayAboveMedian) %>%
  rename("Group"="PriorObesity") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Obesity") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

copd.demo.ql <- 
  combined.data %>%
  group_by(PriorCOPD,StayAboveMedian) %>%
  rename("Group"="PriorCOPD") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior COPD") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

death.demo.ql <- 
  combined.data %>%
  group_by(is.na(DeID_DeathIndexDeceasedDate),StayAboveMedian) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Death")  %>%
  mutate(Group=case_when(Group==TRUE~"Deceased",
                         Group==FALSE~"Alive"))

surv30d.demo.ql <- 
  combined.data %>%
  group_by(EmergencyVisit.Group,StayAboveMedian) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="30d Survival") 

surv60d.demo.ql <- 
  combined.data %>%
  group_by(Survival.60day,StayAboveMedian) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="60d Survival") 

total.survival <- combined.data %>% group_by(StayAboveMedian) %>% count() %>% pull(n) 

summary.discrete <-
  bind_rows(gender.demo.ql,
            race.demo.ql,
            ethnicity.demo.ql,
            ances.demo.ql,
            smoking.demo.ql,
            type.demo.ql,
            emerg.demo.ql, 
            death.demo.ql,
            surv30d.demo.ql %>% mutate(Group=as.character(Group)),
            surv60d.demo.ql,
            diabetes.demo.ql,
            hypertension.demo.ql,
            arrythmia.demo.ql,
            obesity.demo.ql,
            copd.demo.ql
            ) %>%
  select(StayAboveMedian,Type,Group,n,Pct) %>%
  ungroup %>%
  select(-Pct) %>%
  pivot_wider(names_from = StayAboveMedian,values_from = n, values_fn = sum) %>%
  arrange(Type,Group) %>%
  replace(.=="NULL", NA) %>%
  mutate(No = case_when(is.na(No)~0,
                            !(is.na(No))~No)) %>%
  mutate(Yes = case_when(is.na(Yes)~0,
                            !(is.na(Yes))~Yes)) %>%
  rowwise %>%
  mutate(Pct=Yes/(No+Yes)*100) %>%
  filter(No+Yes>0) %>%
  mutate(Chisq.p = chisq.test(x=c(No,Yes),p=total.survival[1:2],rescale.p = T)$p.value) %>%
  mutate(Sig = case_when(Chisq.p < 0.01 ~ '**',
                         Chisq.p < 0.05 ~ '*'))

kable(summary.discrete, 
      caption="Summary of discrete variables for bacterial pneumonia cases, stratified by above average length of stay",
      digits=c(0,0,0,0,1,99,0))
```



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by above average length of stay

|Type                      |Group                                      |   No|  Yes| NA|   Pct| Chisq.p|Sig |
|:-------------------------|:------------------------------------------|----:|----:|--:|-----:|-------:|:---|
|30d Survival              |No                                         | 4165| 1795| 12| 30.12|       0|**  |
|30d Survival              |Yes                                        |    2| 2378| NA| 99.92|       0|**  |
|60d Survival              |Surival Past 60 Days                       |  762| 1167|  2| 60.50|       0|**  |
|60d Survival              |Within 60 Days                             |   12|  312| NA| 96.30|       0|**  |
|60d Survival              |NA                                         | 3393| 2694| 10| 44.26|       0|**  |
|Ancestry                  |AFR                                        |  137|  214| NA| 60.97|       0|**  |
|Ancestry                  |AMR                                        |    8|   21| NA| 72.41|       0|*   |
|Ancestry                  |CSA                                        |   19|   18| NA| 48.65|       1|NA  |
|Ancestry                  |EAS                                        |   21|   23| NA| 52.27|       1|NA  |
|Ancestry                  |EUR                                        | 2415| 2469|  8| 50.55|       0|NA  |
|Ancestry                  |WAS                                        |   19|   30| NA| 61.22|       0|NA  |
|Ancestry                  |NA                                         | 1548| 1398|  4| 47.45|       0|**  |
|Death                     |Alive                                      |  774| 1479|  3| 65.65|       0|**  |
|Death                     |Deceased                                   | 3393| 2694|  9| 44.26|       0|**  |
|Emergency Visit           |NA                                         | 4167| 4173| 12| 50.04|       1|NA  |
|Ethnicity                 |Hispanic or Latino                         |   91|  104| NA| 53.33|       0|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 3946| 3999| 11| 50.33|       1|NA  |
|Ethnicity                 |Patient Refused                            |   27|   10|  1| 27.03|       0|**  |
|Ethnicity                 |Unknown                                    |   90|   59| NA| 39.60|       0|*   |
|Ethnicity                 |NA                                         |   13|    1| NA|  7.14|       0|**  |
|Gender                    |F                                          | 2574| 2042|  8| 44.24|       0|**  |
|Gender                    |M                                          | 1593| 2131|  4| 57.22|       0|**  |
|Pneumonia Type            |Bacterial                                  | 4167| 4173| 12| 50.04|       1|NA  |
|Prior COPD                |Yes                                        |   71|  170| NA| 70.54|       0|**  |
|Prior COPD                |NA                                         | 4096| 4003| 12| 49.43|       0|NA  |
|Prior Cardiac Arrhythmias |Yes                                        | 1063| 1406|  2| 56.95|       0|**  |
|Prior Cardiac Arrhythmias |NA                                         | 3104| 2767| 10| 47.13|       0|**  |
|Prior Obesity             |Yes                                        | 1069| 1096|  2| 50.62|       1|NA  |
|Prior Obesity             |NA                                         | 3098| 3077| 10| 49.83|       1|NA  |
|PriorDiabetes             |Yes                                        |  826|  975|  2| 54.14|       0|**  |
|PriorDiabetes             |NA                                         | 3341| 3198| 10| 48.91|       0|NA  |
|PriorHypertension         |Yes                                        | 1551| 1709|  2| 52.42|       0|**  |
|PriorHypertension         |NA                                         | 2616| 2464| 10| 48.50|       0|*   |
|Race                      |African American                           |  276|  396| NA| 58.93|       0|**  |
|Race                      |American Indian or Alaska Native           |   33|   20| NA| 37.74|       0|NA  |
|Race                      |Asian                                      |   60|   63| NA| 51.22|       1|NA  |
|Race                      |Caucasian                                  | 3673| 3582| 11| 49.37|       0|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |    4|    2| NA| 33.33|       0|NA  |
|Race                      |Other                                      |   66|   86|  1| 56.58|       0|NA  |
|Race                      |Patient Refused                            |   24|    6| NA| 20.00|       0|**  |
|Race                      |Unknown                                    |   22|   15| NA| 40.54|       0|NA  |
|Race                      |NA                                         |    9|    3| NA| 25.00|       0|NA  |
|Smoking                   |Current                                    |  428|  355| NA| 45.34|       0|**  |
|Smoking                   |Former                                     | 1629| 1834|  9| 52.96|       0|**  |
|Smoking                   |Never                                      | 2006| 1802|  3| 47.32|       0|**  |
|Smoking                   |Unknown                                    |   33|   54| NA| 62.07|       0|*   |
|Smoking                   |NA                                         |   71|  128| NA| 64.32|       0|**  |



# By Need for Ventillation



``` r
procedure.file <- 'ProceduresComprehensive.csv'
procedure.data <- read_csv(procedure.file)

combined.data <- 
  combined.data %>%
  mutate(Ventillation=if_else(DeID_PatientID %in% procedure.data$DeID_PatientID,
                              "Yes",
                              "No"))

quant.demo <- 
  combined.data %>%
  group_by(Ventillation) %>%
  summarize(across(.cols=c('AgeInYears.x','BMI','MaxStay'),
                   .fns=list(mean = ~mean(.,na.rm=T),
                             sd = ~sd(.,na.rm=T),
                             #shapiro.p = ~shapiro.test(.)$p.value,
                             n = ~length(!(is.na(.)))))) %>%
  pivot_longer(cols=2:10,
               names_sep="_",names_to=c("Variable","Statistic")) %>%
  pivot_wider(names_from=Statistic,values_from=value) %>%
  arrange(Variable)

kable(quant.demo, caption="Summary of quantitative values for the need for ventillation")
```



Table: Summary of quantitative values for the need for ventillation

|Ventillation |Variable     |  mean|    sd|    n|
|:------------|:------------|-----:|-----:|----:|
|No           |AgeInYears.x | 56.02| 16.71| 6899|
|Yes          |AgeInYears.x | 58.38| 14.66| 1453|
|No           |BMI          | 30.21|  8.12| 6899|
|Yes          |BMI          | 31.20|  9.69| 1453|
|No           |MaxStay      |  1.97|  6.97| 6899|
|Yes          |MaxStay      | 11.70| 20.94| 1453|

``` r
# wilcoxon tests, not normally distributed

quant.t.tests <-
  combined.data %>%
  summarize(across(.cols=c('AgeInYears.x','BMI'),
                   .fns=list(Mann.Whitney=~wilcox.test(.~Ventillation)$p.value)))

kable(quant.t.tests,captionn="Mann-Whitney tests for need for ventillation",digits=c(99,99,99))
```



| AgeInYears.x_Mann.Whitney| BMI_Mann.Whitney|
|-------------------------:|----------------:|
|                0.00000363|          0.00532|

``` r
ances.demo.ql <- 
  combined.data %>%
  group_by(MajorityAncestry,Ventillation) %>%
  mutate(Type="Ancestry") %>%
  rename("Group"="MajorityAncestry") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ancestry")

type.demo.ql <- 
  combined.data %>%
  group_by(Type,Ventillation) %>%
  rename("Group"="Type") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Pneumonia Type")

emerg.demo.ql <- 
  combined.data %>%
  group_by(EmergencyVisit.Group,Ventillation) %>%
  rename("Group"="EmergencyVisit.Group") %>%
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
  group_by(GenderCode,Ventillation) %>%
  rename("Group"="GenderCode") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Gender")

race.demo.ql <- 
combined.data %>%
  group_by(RaceName,Ventillation) %>%
  count %>%
  ungroup %>%
  rename("Group"="RaceName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Race")

ethnicity.demo.ql <- 
combined.data %>%
  group_by(EthnicityName,Ventillation) %>%
  count %>%
  ungroup %>%
  rename("Group"="EthnicityName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ethnicity")

diabetes.demo.ql <- 
  combined.data %>%
  group_by(PriorDiabetes,Ventillation) %>%
  count %>%
  ungroup %>%
  rename("Group"="PriorDiabetes") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorDiabetes") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

smoking.demo.ql <- 
  combined.data %>%
  group_by(SmokingStatusMapped,Ventillation) %>%
  #mutate(Type="Smoking") %>%
  rename("Group"="SmokingStatusMapped") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Smoking") 

#update to hypertension first
hypertension.demo.ql <- 
  combined.data %>%
  group_by(PriorHypertension,Ventillation) %>%
  rename("Group"="PriorHypertension") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorHypertension") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(PriorCardiacArrhythmias,Ventillation) %>%
  rename("Group"="PriorCardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Cardiac Arrhythmias") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

obesity.demo.ql <- 
  combined.data %>%
  group_by(PriorObesity,Ventillation) %>%
  rename("Group"="PriorObesity") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Obesity") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

copd.demo.ql <- 
  combined.data %>%
  group_by(PriorCOPD,Ventillation) %>%
  rename("Group"="PriorCOPD") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior COPD") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

death.demo.ql <- 
  combined.data %>%
  group_by(is.na(DeID_DeathIndexDeceasedDate),Ventillation) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Death")  %>%
  mutate(Group=case_when(Group==TRUE~"Deceased",
                         Group==FALSE~"Alive"))

surv30d.demo.ql <- 
  combined.data %>%
  group_by(EmergencyVisit.Group,Ventillation) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="30d Survival") 

surv60d.demo.ql <- 
  combined.data %>%
  group_by(Survival.60day,Ventillation) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="60d Survival") 

total.survival <- combined.data %>% group_by(Ventillation) %>% count() %>% pull(n) 

summary.discrete <-
  bind_rows(gender.demo.ql,
            race.demo.ql,
            ethnicity.demo.ql,
            ances.demo.ql,
            smoking.demo.ql,
            type.demo.ql,
            emerg.demo.ql, 
            death.demo.ql,
            surv30d.demo.ql %>% mutate(Group=as.character(Group)),
            surv60d.demo.ql,
            diabetes.demo.ql,
            hypertension.demo.ql,
            arrythmia.demo.ql,
            obesity.demo.ql,
            copd.demo.ql
            ) %>%
  select(Ventillation,Type,Group,n,Pct) %>%
  ungroup %>%
  select(-Pct) %>%
  pivot_wider(names_from = Ventillation,values_from = n, values_fn = sum) %>%
  arrange(Type,Group) %>%
  replace(.=="NULL", NA) %>%
  mutate(No = case_when(is.na(No)~0,
                            !(is.na(No))~No)) %>%
  mutate(Yes = case_when(is.na(Yes)~0,
                            !(is.na(Yes))~Yes)) %>%
  rowwise %>%
  mutate(Pct=Yes/(No+Yes)*100) %>%
  filter(No+Yes>0) %>%
  mutate(Chisq.p = chisq.test(x=c(No,Yes),p=total.survival[1:2],rescale.p = T)$p.value) %>%
  mutate(Sig = case_when(Chisq.p < 0.01 ~ '**',
                         Chisq.p < 0.05 ~ '*'))

kable(summary.discrete, 
      caption=paste("Summary of discrete variables for bacterial pneumonia cases, stratified by ventillation status.  Overall", round(total.survival[2]/(total.survival[2]+total.survival[1])*100,2),"% required ventillation"),
      digits=c(0,0,0,0,1,99,0))
```



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by ventillation status.  Overall 17.4 % required ventillation

|Type                      |Group                                      |   No|  Yes|  Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|----:|----:|----:|--------:|:---|
|30d Survival              |No                                         | 5194|  778| 13.0| 5.21e-19|**  |
|30d Survival              |Yes                                        | 1705|  675| 28.4| 3.28e-45|**  |
|60d Survival              |Surival Past 60 Days                       | 1337|  594| 30.8| 3.95e-54|**  |
|60d Survival              |Within 60 Days                             |  178|  146| 45.1| 2.05e-39|**  |
|60d Survival              |NA                                         | 5384|  713| 11.7| 7.36e-32|**  |
|Ancestry                  |AFR                                        |  266|   85| 24.2| 7.51e-04|**  |
|Ancestry                  |AMR                                        |   21|    8| 27.6| 1.48e-01|NA  |
|Ancestry                  |CSA                                        |   35|    2|  5.4| 5.43e-02|NA  |
|Ancestry                  |EAS                                        |   41|    3|  6.8| 6.42e-02|NA  |
|Ancestry                  |EUR                                        | 3987|  905| 18.5| 4.19e-02|*   |
|Ancestry                  |WAS                                        |   44|    5| 10.2| 1.84e-01|NA  |
|Ancestry                  |NA                                         | 2505|  445| 15.1| 9.23e-04|**  |
|Death                     |Alive                                      | 1515|  741| 32.8| 1.80e-83|**  |
|Death                     |Deceased                                   | 5384|  712| 11.7| 5.23e-32|**  |
|Emergency Visit           |NA                                         | 6899| 1453| 17.4| 1.00e+00|NA  |
|Ethnicity                 |Hispanic or Latino                         |  161|   34| 17.4| 9.89e-01|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 6567| 1389| 17.5| 8.85e-01|NA  |
|Ethnicity                 |Patient Refused                            |   28|   10| 26.3| 1.47e-01|NA  |
|Ethnicity                 |Unknown                                    |  130|   19| 12.8| 1.35e-01|NA  |
|Ethnicity                 |NA                                         |   13|    1|  7.1| 3.11e-01|NA  |
|Gender                    |F                                          | 4004|  620| 13.4| 8.37e-13|**  |
|Gender                    |M                                          | 2895|  833| 22.3| 1.61e-15|**  |
|Pneumonia Type            |Bacterial                                  | 6899| 1453| 17.4| 1.00e+00|NA  |
|Prior COPD                |Yes                                        |  135|  106| 44.0| 1.32e-27|**  |
|Prior COPD                |NA                                         | 6764| 1347| 16.6| 6.06e-02|NA  |
|Prior Cardiac Arrhythmias |Yes                                        | 1649|  822| 33.3| 3.60e-96|**  |
|Prior Cardiac Arrhythmias |NA                                         | 5250|  631| 10.7| 1.83e-41|**  |
|Prior Obesity             |Yes                                        | 1608|  559| 25.8| 6.10e-25|**  |
|Prior Obesity             |NA                                         | 5291|  894| 14.5| 1.03e-09|**  |
|PriorDiabetes             |Yes                                        | 1211|  592| 32.8| 5.46e-67|**  |
|PriorDiabetes             |NA                                         | 5688|  861| 13.1| 1.16e-19|**  |
|PriorHypertension         |Yes                                        | 2372|  890| 27.3| 3.51e-50|**  |
|PriorHypertension         |NA                                         | 4527|  563| 11.1| 8.80e-33|**  |
|Race                      |African American                           |  514|  158| 23.5| 2.90e-05|**  |
|Race                      |American Indian or Alaska Native           |   45|    8| 15.1| 6.58e-01|NA  |
|Race                      |Asian                                      |  115|    8|  6.5| 1.44e-03|**  |
|Race                      |Caucasian                                  | 6028| 1238| 17.0| 4.20e-01|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |    4|    2| 33.3| 3.03e-01|NA  |
|Race                      |Other                                      |  127|   26| 17.0| 8.95e-01|NA  |
|Race                      |Patient Refused                            |   24|    6| 20.0| 7.07e-01|NA  |
|Race                      |Unknown                                    |   30|    7| 18.9| 8.07e-01|NA  |
|Race                      |NA                                         |   12|    0|  0.0| 1.12e-01|NA  |
|Smoking                   |Current                                    |  666|  117| 14.9| 7.00e-02|NA  |
|Smoking                   |Former                                     | 2750|  722| 20.8| 1.28e-07|**  |
|Smoking                   |Never                                      | 3255|  556| 14.6| 4.82e-06|**  |
|Smoking                   |Unknown                                    |   41|   46| 52.9| 2.57e-18|**  |
|Smoking                   |NA                                         |  187|   12|  6.0| 2.34e-05|**  |
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
