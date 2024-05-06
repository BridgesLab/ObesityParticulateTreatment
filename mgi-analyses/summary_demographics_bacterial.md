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

To analyse the subset of patients with viral or bacterial pneumonia.   This script combines the cleaned datasets and writes out a complete datafile for analyses.  This script can be found in /nfs/turbo/precision-health/DataDirect/HUM00229632 - Genome-wide associations of bacteria and was most recently run on Fri Apr 19 14:00:09 2024.


```r
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
combined.data <- read_csv(complete.filename) %>%
  filter(Type=="Bacterial")
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

|Variable   | mean|   sd|   n|
|:----------|----:|----:|---:|
|AgeInYears | 59.6| 14.9| 481|
|BMI        | 28.9| 10.0| 481|
|MaxStay    | 16.5| 29.8| 481|

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

|Type                |Group                            |   n|     Pct|
|:-------------------|:--------------------------------|---:|-------:|
|Gender              |F                                | 164|  34.096|
|Gender              |M                                | 317|  65.904|
|Race                |African American                 |  60|  12.474|
|Race                |American Indian or Alaska Native |   4|   0.832|
|Race                |Asian                            |   3|   0.624|
|Race                |Caucasian                        | 410|  85.239|
|Race                |Other                            |   3|   0.624|
|Race                |Unknown                          |   1|   0.208|
|Ethnicity           |Hispanic or Latino               |   6|   1.247|
|Ethnicity           |Non-Hispanic or Latino           | 472|  98.129|
|Ethnicity           |Unknown                          |   3|   0.624|
|Ancestry            |AFR                              |  23|   4.782|
|Ancestry            |AMR                              |   1|   0.208|
|Ancestry            |EAS                              |   2|   0.416|
|Ancestry            |EUR                              | 354|  73.597|
|Ancestry            |WAS                              |   7|   1.455|
|Ancestry            |NA                               |  94|  19.543|
|Smoking             |Current                          |  17|   3.534|
|Smoking             |Former                           | 220|  45.738|
|Smoking             |Never                            | 193|  40.125|
|Smoking             |Unknown                          |  15|   3.119|
|Smoking             |NA                               |  36|   7.484|
|Pneumonia Type      |Bacterial                        | 481| 100.000|
|Emergency Visit     |NA                               | 291|  60.499|
|Emergency Visit     |Yes                              | 187|  38.877|
|Emergency Visit     |NA                               |   3|   0.624|
|Death               |Alive                            | 254|  52.807|
|Death               |Deceased                         | 227|  47.193|
|30d Survival        |Surival Past 30 Days             | 221|  45.946|
|30d Survival        |Within 30 Days                   |  30|   6.237|
|30d Survival        |NA                               | 230|  47.817|
|60d Survival        |Surival Past 60 Days             | 199|  41.372|
|60d Survival        |Within 60 Days                   |  52|  10.811|
|60d Survival        |NA                               | 230|  47.817|
|Diabetes            |NA                               | 215|  44.699|
|Diabetes            |Yes                              | 266|  55.301|
|Hypertension        |NA                               |  95|  19.751|
|Hypertension        |Yes                              | 386|  80.249|
|Cardiac Arrhythmias |NA                               |  84|  17.464|
|Cardiac Arrhythmias |Yes                              | 397|  82.536|

# By 30 Day Survival


```r
combined.data <- 
  combined.data %>%
  mutate(Survival.30day.Group = case_when(Survival.30day=="Within 30 Days" ~ "No",
                                    Survival.30day!="Survival Past 30 Days" ~ "Yes",
                                    is.na(Survival.30day) ~ "Yes",
                                    .default="Yes"))

quant.demo <- 
  combined.data %>%
  group_by(Survival.30day.Group) %>%
  summarize(across(.cols=c('AgeInYears','BMI','MaxStay'),
                   .fns=list(mean = ~mean(.,na.rm=T),
                             sd = ~sd(.,na.rm=T),
                             n = ~length(!(is.na(.)))))) %>%
  pivot_longer(cols=2:10,
               names_sep="_",names_to=c("Variable","Statistic")) %>%
  pivot_wider(names_from=Statistic,values_from=value) %>%
  arrange(Variable)

kable(quant.demo, caption="Summary of quantitative values")
```



Table: Summary of quantitative values

|Survival.30day.Group |Variable   | mean|    sd|   n|
|:--------------------|:----------|----:|-----:|---:|
|No                   |AgeInYears | 69.5| 12.41|  30|
|Yes                  |AgeInYears | 59.0| 14.87| 451|
|No                   |BMI        | 27.5|  7.79|  30|
|Yes                  |BMI        | 29.0| 10.14| 451|
|No                   |MaxStay    | 11.2|  9.04|  30|
|Yes                  |MaxStay    | 16.9| 30.71| 451|

```r
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
  group_by(EmergencyVisit,Survival.30day.Group) %>%
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
  group_by(Diabetes,Survival.30day.Group) %>%
  count %>%
  ungroup %>%
  rename("Group"="Diabetes") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Diabetes") %>%
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
  group_by(Hypertension,Survival.30day.Group) %>%
  rename("Group"="Hypertension") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Hypertension") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(CardiacArrhythmias,Survival.30day.Group) %>%
  rename("Group"="CardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Cardiac Arrhythmias") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(CardiacArrhythmias,Survival.30day.Group) %>%
  rename("Group"="CardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Cardiac Arrhythmias") %>%
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
  mutate(Chisq.p = chisq.test(x=c(No,Yes),p=total.survival,rescale.p = T)$p.value) %>%
  mutate(Sig = case_when(Chisq.p < 0.01 ~ '**',
                         Chisq.p < 0.05 ~ '*'))

kable(summary.discrete, caption="Summary of discrete variables for bacterial pneumonia cases, stratified by 30 day survival")
```



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by 30 day survival

|Type                |Group                            | No| Yes| Chisq.p|Sig |
|:-------------------|:--------------------------------|--:|---:|-------:|:---|
|30d Survival        |Surival Past 30 Days             |  0| 221|   0.000|**  |
|30d Survival        |Within 30 Days                   | 30|   0|   0.000|**  |
|30d Survival        |NA                               |  0| 230|   0.000|**  |
|60d Survival        |Surival Past 60 Days             |  0| 199|   0.000|**  |
|60d Survival        |Within 60 Days                   | 30|  22|   0.000|**  |
|60d Survival        |NA                               |  0| 230|   0.000|**  |
|Ancestry            |AFR                              |  2|  21|   0.626|NA  |
|Ancestry            |AMR                              |  1|   0|   0.000|**  |
|Ancestry            |EAS                              |  0|   2|   0.715|NA  |
|Ancestry            |EUR                              | 20| 334|   0.648|NA  |
|Ancestry            |WAS                              |  0|   7|   0.495|NA  |
|Ancestry            |NA                               |  7|  87|   0.628|NA  |
|Cardiac Arrhythmias |Yes                              | 30| 367|   0.277|NA  |
|Cardiac Arrhythmias |NA                               |  0|  84|   0.018|*   |
|Death               |Alive                            | 30| 224|   0.000|**  |
|Death               |Deceased                         |  0| 227|   0.000|**  |
|Diabetes            |Yes                              | 20| 246|   0.387|NA  |
|Diabetes            |NA                               | 10| 205|   0.336|NA  |
|Emergency Visit     |Yes                              | 15| 172|   0.313|NA  |
|Emergency Visit     |NA                               | 15| 279|   0.421|NA  |
|Ethnicity           |Hispanic or Latino               |  0|   6|   0.528|NA  |
|Ethnicity           |Non-Hispanic or Latino           | 29| 443|   0.933|NA  |
|Ethnicity           |Unknown                          |  1|   2|   0.052|NA  |
|Gender              |F                                | 10| 154|   0.941|NA  |
|Gender              |M                                | 20| 297|   0.958|NA  |
|Hypertension        |Yes                              | 29| 357|   0.300|NA  |
|Hypertension        |NA                               |  1|  94|   0.037|*   |
|Pneumonia Type      |Bacterial                        | 30| 451|   1.000|NA  |
|Race                |African American                 |  5|  55|   0.502|NA  |
|Race                |American Indian or Alaska Native |  0|   4|   0.606|NA  |
|Race                |Asian                            |  0|   3|   0.655|NA  |
|Race                |Caucasian                        | 25| 385|   0.907|NA  |
|Race                |Other                            |  0|   3|   0.655|NA  |
|Race                |Unknown                          |  0|   1|   0.796|NA  |
|Smoking             |Current                          |  2|  15|   0.346|NA  |
|Smoking             |Former                           | 15| 205|   0.721|NA  |
|Smoking             |Never                            | 11| 182|   0.757|NA  |
|Smoking             |Unknown                          |  1|  14|   0.945|NA  |
|Smoking             |NA                               |  1|  35|   0.391|NA  |

## By 60 Day Survival


```r
combined.data <- 
  combined.data %>%
  mutate(Survival.60.day.Group = case_when(Survival.60day=="Within 60 Days" ~ "No",
                                    Survival.60day!="Survival Past 60 Days" ~ "Yes",
                                    is.na(Survival.60day) ~ "Yes",
                                    .default="Yes"))

quant.demo <- 
  combined.data %>%
  group_by(Survival.60.day.Group) %>%
  summarize(across(.cols=c('AgeInYears','BMI','MaxStay'),
                   .fns=list(mean = ~mean(.,na.rm=T),
                             sd = ~sd(.,na.rm=T),
                             n = ~length(!(is.na(.)))))) %>%
  pivot_longer(cols=2:10,
               names_sep="_",names_to=c("Variable","Statistic")) %>%
  pivot_wider(names_from=Statistic,values_from=value) %>%
  arrange(Variable)

kable(quant.demo, caption="Summary of quantitative values")
```



Table: Summary of quantitative values

|Survival.60.day.Group |Variable   | mean|   sd|   n|
|:---------------------|:----------|----:|----:|---:|
|No                    |AgeInYears | 68.7| 11.2|  52|
|Yes                   |AgeInYears | 58.5| 15.0| 429|
|No                    |BMI        | 27.4|  7.0|  52|
|Yes                   |BMI        | 29.1| 10.3| 429|
|No                    |MaxStay    | 17.2| 15.2|  52|
|Yes                   |MaxStay    | 16.5| 31.2| 429|

```r
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
  group_by(EmergencyVisit,Survival.60.day.Group) %>%
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
  group_by(Diabetes,Survival.60.day.Group) %>%
  count %>%
  ungroup %>%
  rename("Group"="Diabetes") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Diabetes") %>%
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
  group_by(Hypertension,Survival.60.day.Group) %>%
  rename("Group"="Hypertension") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Hypertension") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(CardiacArrhythmias,Survival.60.day.Group) %>%
  rename("Group"="CardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Cardiac Arrhythmias") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(CardiacArrhythmias,Survival.60.day.Group) %>%
  rename("Group"="CardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Cardiac Arrhythmias") %>%
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
  mutate(Chisq.p = chisq.test(x=c(No,Yes),p=total.survival,rescale.p = T)$p.value) %>%
  mutate(Sig = case_when(Chisq.p < 0.01 ~ '**',
                         Chisq.p < 0.05 ~ '*'))

kable(summary.discrete, caption="Summary of discrete variables for bacterial pneumonia cases, stratified by 60 day survival")
```



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by 60 day survival

|Type                |Group                            | No| Yes| Chisq.p|Sig |
|:-------------------|:--------------------------------|--:|---:|-------:|:---|
|60d Survival        |Surival Past 30 Days             | 22| 199|   0.682|NA  |
|60d Survival        |Surival Past 60 Days             |  0| 199|   0.000|**  |
|60d Survival        |Within 30 Days                   | 30|   0|   0.000|**  |
|60d Survival        |Within 60 Days                   | 52|   0|   0.000|**  |
|60d Survival        |NA                               |  0| 460|   0.000|**  |
|Ancestry            |AFR                              |  4|  19|   0.309|NA  |
|Ancestry            |AMR                              |  1|   0|   0.004|**  |
|Ancestry            |EAS                              |  0|   2|   0.622|NA  |
|Ancestry            |EUR                              | 37| 317|   0.828|NA  |
|Ancestry            |WAS                              |  0|   7|   0.357|NA  |
|Ancestry            |NA                               | 10|  84|   0.957|NA  |
|Cardiac Arrhythmias |Yes                              | 52| 345|   0.142|NA  |
|Cardiac Arrhythmias |NA                               |  0|  84|   0.001|**  |
|Death               |Alive                            | 52| 202|   0.000|**  |
|Death               |Deceased                         |  0| 227|   0.000|**  |
|Diabetes            |Yes                              | 34| 232|   0.301|NA  |
|Diabetes            |NA                               | 18| 197|   0.249|NA  |
|Emergency Visit     |Yes                              | 23| 164|   0.512|NA  |
|Emergency Visit     |NA                               | 29| 265|   0.601|NA  |
|Ethnicity           |Hispanic or Latino               |  0|   6|   0.394|NA  |
|Ethnicity           |Non-Hispanic or Latino           | 51| 421|   0.997|NA  |
|Ethnicity           |Unknown                          |  1|   2|   0.209|NA  |
|Gender              |F                                | 17| 147|   0.854|NA  |
|Gender              |M                                | 35| 282|   0.895|NA  |
|Hypertension        |Yes                              | 49| 337|   0.233|NA  |
|Hypertension        |NA                               |  3|  92|   0.016|*   |
|Pneumonia Type      |Bacterial                        | 52| 429|   1.000|NA  |
|Race                |African American                 |  7|  53|   0.831|NA  |
|Race                |American Indian or Alaska Native |  0|   4|   0.486|NA  |
|Race                |Asian                            |  0|   3|   0.546|NA  |
|Race                |Caucasian                        | 45| 365|   0.914|NA  |
|Race                |Other                            |  0|   3|   0.546|NA  |
|Race                |Unknown                          |  0|   1|   0.728|NA  |
|Smoking             |Current                          |  3|  14|   0.364|NA  |
|Smoking             |Former                           | 29| 191|   0.257|NA  |
|Smoking             |Never                            | 16| 177|   0.259|NA  |
|Smoking             |Unknown                          |  1|  14|   0.605|NA  |
|Smoking             |NA                               |  3|  33|   0.632|NA  |

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
