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

To analyse the subset of patients with viral or bacterial pneumonia.   This script combines the cleaned datasets and writes out a complete datafile for analyses.  This script can be found in /nfs/turbo/precision-health/DataDirect/HUM00229632 - Genome-wide associations of bacteria/2024-11-12/cases and was most recently run on Wed Nov 13 10:34:21 2024.


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
## Rows: 6986 Columns: 63
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
|AgeInYears.x | 57.40|  16.7| 2367|
|BMI          | 32.77| 153.6| 2367|
|MaxStay      |  9.19|  19.1| 2367|

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
|Gender                    |F                                          | 1140|  48.162|
|Gender                    |M                                          | 1227|  51.838|
|Race                      |African American                           |  222|   9.379|
|Race                      |American Indian                            |   13|   0.549|
|Race                      |Asian                                      |   32|   1.352|
|Race                      |Caucasian                                  | 2043|  86.312|
|Race                      |Native Hawaiian and Other Pacific Islander |    2|   0.084|
|Race                      |Other                                      |   38|   1.605|
|Race                      |Patient Refused                            |    2|   0.084|
|Race                      |Unknown                                    |    9|   0.380|
|Race                      |NA                                         |    6|   0.253|
|Ethnicity                 |Hispanic or Latino                         |   57|   2.408|
|Ethnicity                 |Non-Hispanic or Latino                     | 2265|  95.691|
|Ethnicity                 |Patient Refused                            |   10|   0.422|
|Ethnicity                 |Unknown                                    |   34|   1.436|
|Ethnicity                 |NA                                         |    1|   0.042|
|Ancestry                  |AFR                                        |  109|   4.605|
|Ancestry                  |AMR                                        |    8|   0.338|
|Ancestry                  |CSA                                        |   12|   0.507|
|Ancestry                  |EAS                                        |   11|   0.465|
|Ancestry                  |EUR                                        | 1340|  56.612|
|Ancestry                  |WAS                                        |   18|   0.760|
|Ancestry                  |NA                                         |  869|  36.713|
|Smoking                   |Current                                    |  142|   5.999|
|Smoking                   |Former                                     |  934|  39.459|
|Smoking                   |Never                                      |  920|  38.868|
|Smoking                   |Unknown                                    |   38|   1.605|
|Smoking                   |NA                                         |  333|  14.068|
|Pneumonia Type            |Bacterial                                  | 2367| 100.000|
|Emergency Visit           |NA                                         | 1258|  53.147|
|Emergency Visit           |Yes                                        | 1109|  46.853|
|Death                     |Alive                                      |  810|  34.221|
|Death                     |Deceased                                   | 1557|  65.779|
|30d Survival              |Surival Past 30 Days                       |  712|  30.080|
|30d Survival              |Within 30 Days                             |   98|   4.140|
|30d Survival              |NA                                         | 1557|  65.779|
|60d Survival              |Surival Past 60 Days                       |  651|  27.503|
|60d Survival              |Within 60 Days                             |  159|   6.717|
|60d Survival              |NA                                         | 1557|  65.779|
|PriorDiabetes             |NA                                         | 1890|  79.848|
|PriorDiabetes             |Yes                                        |  477|  20.152|
|PriorHypertension         |NA                                         | 1576|  66.582|
|PriorHypertension         |Yes                                        |  791|  33.418|
|Prior Cardiac Arrhythmias |NA                                         | 1691|  71.441|
|Prior Cardiac Arrhythmias |Yes                                        |  676|  28.559|
|Prior Obesity             |NA                                         | 1881|  79.468|
|Prior Obesity             |Yes                                        |  486|  20.532|
|Prior COPD                |NA                                         | 2289|  96.705|
|Prior COPD                |Yes                                        |   78|   3.295|

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

|Survival.30day.Group |Variable     |  mean|     sd|    n|
|:--------------------|:------------|-----:|------:|----:|
|No                   |AgeInYears.x | 68.51|  14.20|   98|
|Yes                  |AgeInYears.x | 56.92|  16.66| 2269|
|No                   |BMI          | 27.80|   7.11|   98|
|Yes                  |BMI          | 33.03| 157.44| 2269|
|No                   |MaxStay      | 12.36|   7.43|   98|
|Yes                  |MaxStay      |  9.06|  19.46| 2269|

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
|                  5.48e-12|            0.091|             5.76e-16|

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



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by 30 day survival.  Overall 4.14 % visited died at 30d

|Type                      |Group                                      | No|  Yes|   Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|--:|----:|-----:|--------:|:---|
|30d Survival              |Surival Past 30 Days                       |  0|  712|   0.0| 2.93e-08|**  |
|30d Survival              |Within 30 Days                             | 98|    0| 100.0| 0.00e+00|**  |
|30d Survival              |NA                                         |  0| 1557|   0.0| 2.39e-16|**  |
|60d Survival              |Surival Past 60 Days                       |  0|  651|   0.0| 1.14e-07|**  |
|60d Survival              |Within 60 Days                             | 98|   61|  61.6| 0.00e+00|**  |
|60d Survival              |NA                                         |  0| 1557|   0.0| 2.39e-16|**  |
|Ancestry                  |AFR                                        |  5|  104|   4.6| 8.15e-01|NA  |
|Ancestry                  |AMR                                        |  1|    7|  12.5| 2.35e-01|NA  |
|Ancestry                  |CSA                                        |  0|   12|   0.0| 4.72e-01|NA  |
|Ancestry                  |EAS                                        |  1|   10|   9.1| 4.10e-01|NA  |
|Ancestry                  |EUR                                        | 60| 1280|   4.5| 5.35e-01|NA  |
|Ancestry                  |WAS                                        |  0|   18|   0.0| 3.78e-01|NA  |
|Ancestry                  |NA                                         | 31|  838|   3.6| 3.97e-01|NA  |
|Death                     |Alive                                      | 98|  712|  12.1| 5.93e-30|**  |
|Death                     |Deceased                                   |  0| 1557|   0.0| 2.39e-16|**  |
|Emergency Visit           |Yes                                        | 66| 1043|   6.0| 2.47e-03|**  |
|Emergency Visit           |NA                                         | 32| 1226|   2.5| 4.48e-03|**  |
|Ethnicity                 |Hispanic or Latino                         |  1|   56|   1.8| 3.66e-01|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 94| 2171|   4.2| 9.81e-01|NA  |
|Ethnicity                 |Patient Refused                            |  0|   10|   0.0| 5.11e-01|NA  |
|Ethnicity                 |Unknown                                    |  3|   31|   8.8| 1.70e-01|NA  |
|Ethnicity                 |NA                                         |  0|    1|   0.0| 8.35e-01|NA  |
|Gender                    |F                                          | 38| 1102|   3.3| 1.71e-01|NA  |
|Gender                    |M                                          | 60| 1167|   4.9| 1.87e-01|NA  |
|Pneumonia Type            |Bacterial                                  | 98| 2269|   4.1| 1.00e+00|NA  |
|Prior COPD                |Yes                                        |  7|   71|   9.0| 3.21e-02|*   |
|Prior COPD                |NA                                         | 91| 2198|   4.0| 6.92e-01|NA  |
|Prior Cardiac Arrhythmias |Yes                                        | 44|  632|   6.5| 1.99e-03|**  |
|Prior Cardiac Arrhythmias |NA                                         | 54| 1637|   3.2| 5.06e-02|NA  |
|Prior Obesity             |Yes                                        | 21|  465|   4.3| 8.41e-01|NA  |
|Prior Obesity             |NA                                         | 77| 1804|   4.1| 9.19e-01|NA  |
|PriorDiabetes             |Yes                                        | 29|  448|   6.1| 3.35e-02|*   |
|PriorDiabetes             |NA                                         | 69| 1821|   3.7| 2.85e-01|NA  |
|PriorHypertension         |Yes                                        | 46|  745|   5.8| 1.80e-02|*   |
|PriorHypertension         |NA                                         | 52| 1524|   3.3| 9.39e-02|NA  |
|Race                      |African American                           | 10|  212|   4.5| 7.85e-01|NA  |
|Race                      |American Indian                            |  0|   13|   0.0| 4.54e-01|NA  |
|Race                      |Asian                                      |  1|   31|   3.1| 7.73e-01|NA  |
|Race                      |Caucasian                                  | 86| 1957|   4.2| 8.75e-01|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |  0|    2|   0.0| 7.69e-01|NA  |
|Race                      |Other                                      |  1|   37|   2.6| 6.41e-01|NA  |
|Race                      |Patient Refused                            |  0|    2|   0.0| 7.69e-01|NA  |
|Race                      |Unknown                                    |  0|    9|   0.0| 5.33e-01|NA  |
|Race                      |NA                                         |  0|    6|   0.0| 6.11e-01|NA  |
|Smoking                   |Current                                    |  2|  140|   1.4| 1.02e-01|NA  |
|Smoking                   |Former                                     | 49|  885|   5.2| 8.98e-02|NA  |
|Smoking                   |Never                                      | 42|  878|   4.6| 5.18e-01|NA  |
|Smoking                   |Unknown                                    |  1|   37|   2.6| 6.41e-01|NA  |
|Smoking                   |NA                                         |  4|  329|   1.2| 7.10e-03|**  |

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

|Survival.60.day.Group |Variable     |  mean|     sd|    n|
|:---------------------|:------------|-----:|------:|----:|
|No                    |AgeInYears.x | 67.79|  13.56|  159|
|Yes                   |AgeInYears.x | 56.66|  16.68| 2208|
|No                    |BMI          | 27.85|   7.45|  159|
|Yes                   |BMI          | 33.20| 160.09| 2208|
|No                    |MaxStay      | 15.86|  13.50|  159|
|Yes                   |MaxStay      |  8.71|  19.38| 2208|

``` r
quant.t.tests <-
  combined.data %>%
  summarize(across(.cols=c('AgeInYears.x','BMI','MaxStay'),
                   .fns=list(Mann.Whitney=~wilcox.test(.~Survival.30day.Group)$p.value)))

kable(quant.t.tests,captionn="Mann-Whitney tests for 30 day survival", digits=c(99,99,99))
```



| AgeInYears.x_Mann.Whitney| BMI_Mann.Whitney| MaxStay_Mann.Whitney|
|-------------------------:|----------------:|--------------------:|
|                  5.48e-12|            0.091|             5.76e-16|

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



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by 60 day survival.  Overall 6.72 % visited died at 60d

|Type                      |Group                                      |  No|  Yes|   Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|---:|----:|-----:|--------:|:---|
|60d Survival              |Surival Past 30 Days                       |  61|  651|   8.6| 4.86e-02|*   |
|60d Survival              |Surival Past 60 Days                       |   0|  651|   0.0| 7.55e-12|**  |
|60d Survival              |Within 30 Days                             |  98|    0| 100.0| 0.00e+00|**  |
|60d Survival              |Within 60 Days                             | 159|    0| 100.0| 0.00e+00|**  |
|60d Survival              |NA                                         |   0| 3114|   0.0| 1.07e-50|**  |
|Ancestry                  |AFR                                        |   8|  101|   7.3| 7.95e-01|NA  |
|Ancestry                  |AMR                                        |   1|    7|  12.5| 5.14e-01|NA  |
|Ancestry                  |CSA                                        |   0|   12|   0.0| 3.53e-01|NA  |
|Ancestry                  |EAS                                        |   1|   10|   9.1| 7.53e-01|NA  |
|Ancestry                  |EUR                                        | 106| 1234|   7.9| 8.10e-02|NA  |
|Ancestry                  |WAS                                        |   0|   18|   0.0| 2.55e-01|NA  |
|Ancestry                  |NA                                         |  43|  826|   4.9| 3.72e-02|*   |
|Death                     |Alive                                      | 159|  651|  19.6| 8.58e-49|**  |
|Death                     |Deceased                                   |   0| 1557|   0.0| 3.36e-26|**  |
|Emergency Visit           |Yes                                        |  99| 1010|   8.9| 3.29e-03|**  |
|Emergency Visit           |NA                                         |  60| 1198|   4.8| 5.78e-03|**  |
|Ethnicity                 |Hispanic or Latino                         |   2|   55|   3.5| 3.33e-01|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 152| 2113|   6.7| 9.90e-01|NA  |
|Ethnicity                 |Patient Refused                            |   1|    9|  10.0| 6.78e-01|NA  |
|Ethnicity                 |Unknown                                    |   4|   30|  11.8| 2.40e-01|NA  |
|Ethnicity                 |NA                                         |   0|    1|   0.0| 7.88e-01|NA  |
|Gender                    |F                                          |  66| 1074|   5.8| 2.11e-01|NA  |
|Gender                    |M                                          |  93| 1134|   7.6| 2.28e-01|NA  |
|Pneumonia Type            |Bacterial                                  | 159| 2208|   6.7| 1.00e+00|NA  |
|Prior COPD                |Yes                                        |  12|   66|  15.4| 2.23e-03|**  |
|Prior COPD                |NA                                         | 147| 2142|   6.4| 5.72e-01|NA  |
|Prior Cardiac Arrhythmias |Yes                                        |  72|  604|  10.7| 4.40e-05|**  |
|Prior Cardiac Arrhythmias |NA                                         |  87| 1604|   5.1| 9.79e-03|**  |
|Prior Obesity             |Yes                                        |  35|  451|   7.2| 6.70e-01|NA  |
|Prior Obesity             |NA                                         | 124| 1757|   6.6| 8.28e-01|NA  |
|PriorDiabetes             |Yes                                        |  43|  434|   9.0| 4.50e-02|*   |
|PriorDiabetes             |NA                                         | 116| 1774|   6.1| 3.14e-01|NA  |
|PriorHypertension         |Yes                                        |  75|  716|   9.5| 1.90e-03|**  |
|PriorHypertension         |NA                                         |  84| 1492|   5.3| 2.78e-02|*   |
|Race                      |African American                           |  13|  209|   5.9| 6.08e-01|NA  |
|Race                      |American Indian                            |   0|   13|   0.0| 3.33e-01|NA  |
|Race                      |Asian                                      |   1|   31|   3.1| 4.17e-01|NA  |
|Race                      |Caucasian                                  | 143| 1900|   7.0| 6.10e-01|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |   1|    1|  50.0| 1.45e-02|*   |
|Race                      |Other                                      |   1|   37|   2.6| 3.14e-01|NA  |
|Race                      |Patient Refused                            |   0|    2|   0.0| 7.04e-01|NA  |
|Race                      |Unknown                                    |   0|    9|   0.0| 4.21e-01|NA  |
|Race                      |NA                                         |   0|    6|   0.0| 5.11e-01|NA  |
|Smoking                   |Current                                    |   7|  135|   4.9| 3.95e-01|NA  |
|Smoking                   |Former                                     |  86|  848|   9.2| 2.36e-03|**  |
|Smoking                   |Never                                      |  60|  860|   6.5| 8.13e-01|NA  |
|Smoking                   |Unknown                                    |   1|   37|   2.6| 3.14e-01|NA  |
|Smoking                   |NA                                         |   5|  328|   1.5| 1.43e-04|**  |

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

|EmergencyVisit.Group |Variable     |  mean|     sd|    n|
|:--------------------|:------------|-----:|------:|----:|
|No                   |AgeInYears.x | 56.92|  16.61| 1258|
|Yes                  |AgeInYears.x | 57.94|  16.84| 1109|
|No                   |BMI          | 29.26|   8.85| 1258|
|Yes                  |BMI          | 36.04| 213.12| 1109|
|No                   |MaxStay      |  8.27|  21.66| 1258|
|Yes                  |MaxStay      | 10.24|  15.70| 1109|

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
|                     0.132|            0.739|              9.2e-94|

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



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by ER visit survival.  Overall 46.85 % visited ER

|Type                      |Group                                      |   No|  Yes|  Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|----:|----:|----:|--------:|:---|
|30d Survival              |Surival Past 30 Days                       |  316|  396| 55.6| 2.77e-06|**  |
|30d Survival              |Within 30 Days                             |   32|   66| 67.3| 4.79e-05|**  |
|30d Survival              |NA                                         |  910|  647| 41.6| 2.79e-05|**  |
|60d Survival              |Surival Past 60 Days                       |  288|  363| 55.8| 5.25e-06|**  |
|60d Survival              |Within 60 Days                             |   60|   99| 62.3| 9.85e-05|**  |
|60d Survival              |NA                                         |  910|  647| 41.6| 2.79e-05|**  |
|Ancestry                  |AFR                                        |   47|   62| 56.9| 3.59e-02|*   |
|Ancestry                  |AMR                                        |    5|    3| 37.5| 5.96e-01|NA  |
|Ancestry                  |CSA                                        |    8|    4| 33.3| 3.48e-01|NA  |
|Ancestry                  |EAS                                        |    6|    5| 45.5| 9.26e-01|NA  |
|Ancestry                  |EUR                                        |  726|  614| 45.8| 4.49e-01|NA  |
|Ancestry                  |WAS                                        |   10|    8| 44.4| 8.38e-01|NA  |
|Ancestry                  |NA                                         |  456|  413| 47.5| 6.91e-01|NA  |
|Death                     |Alive                                      |  348|  462| 57.0| 6.30e-09|**  |
|Death                     |Deceased                                   |  910|  647| 41.6| 2.79e-05|**  |
|Ethnicity                 |Hispanic or Latino                         |   32|   25| 43.9| 6.51e-01|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 1194| 1071| 47.3| 6.80e-01|NA  |
|Ethnicity                 |Patient Refused                            |    5|    5| 50.0| 8.42e-01|NA  |
|Ethnicity                 |Unknown                                    |   26|    8| 23.5| 6.42e-03|**  |
|Ethnicity                 |NA                                         |    1|    0|  0.0| 3.48e-01|NA  |
|Gender                    |F                                          |  653|  487| 42.7| 5.16e-03|**  |
|Gender                    |M                                          |  605|  622| 50.7| 7.02e-03|**  |
|Pneumonia Type            |Bacterial                                  | 1258| 1109| 46.9| 1.00e+00|NA  |
|Prior COPD                |Yes                                        |   36|   42| 53.8| 2.16e-01|NA  |
|Prior COPD                |NA                                         | 1222| 1067| 46.6| 8.19e-01|NA  |
|Prior Cardiac Arrhythmias |Yes                                        |  326|  350| 51.8| 1.03e-02|*   |
|Prior Cardiac Arrhythmias |NA                                         |  932|  759| 44.9| 1.05e-01|NA  |
|Prior Obesity             |Yes                                        |  253|  233| 47.9| 6.30e-01|NA  |
|Prior Obesity             |NA                                         | 1005|  876| 46.6| 8.07e-01|NA  |
|PriorDiabetes             |Yes                                        |  233|  244| 51.2| 5.98e-02|NA  |
|PriorDiabetes             |NA                                         | 1025|  865| 45.8| 3.44e-01|NA  |
|PriorHypertension         |Yes                                        |  399|  392| 49.6| 1.27e-01|NA  |
|PriorHypertension         |NA                                         |  859|  717| 45.5| 2.80e-01|NA  |
|Race                      |African American                           |   94|  128| 57.7| 1.25e-03|**  |
|Race                      |American Indian                            |    7|    6| 46.2| 9.60e-01|NA  |
|Race                      |Asian                                      |   22|   10| 31.2| 7.69e-02|NA  |
|Race                      |Caucasian                                  | 1104|  939| 46.0| 4.20e-01|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |    1|    1| 50.0| 9.29e-01|NA  |
|Race                      |Other                                      |   20|   18| 47.4| 9.49e-01|NA  |
|Race                      |Patient Refused                            |    1|    1| 50.0| 9.29e-01|NA  |
|Race                      |Unknown                                    |    7|    2| 22.2| 1.39e-01|NA  |
|Race                      |NA                                         |    2|    4| 66.7| 3.31e-01|NA  |
|Smoking                   |Current                                    |   66|   76| 53.5| 1.11e-01|NA  |
|Smoking                   |Former                                     |  467|  467| 50.0| 5.39e-02|NA  |
|Smoking                   |Never                                      |  487|  433| 47.1| 8.97e-01|NA  |
|Smoking                   |Unknown                                    |   23|   15| 39.5| 3.62e-01|NA  |
|Smoking                   |NA                                         |  215|  118| 35.4| 2.98e-05|**  |

# By Longer than Median Stay

The median stay was 2.883.


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

|StayAboveMedian |Variable     |   mean|      sd|    n|
|:---------------|:------------|------:|-------:|----:|
|No              |AgeInYears.x | 56.703|  17.150| 1183|
|Yes             |AgeInYears.x | 58.115|  16.267| 1183|
|NA              |AgeInYears.x | 42.000|      NA|    1|
|No              |BMI          | 29.416|   7.613| 1183|
|Yes             |BMI          | 35.354| 204.513| 1183|
|NA              |BMI          | 50.566|      NA|    1|
|No              |MaxStay      |  0.485|   0.766| 1183|
|Yes             |MaxStay      | 17.909|  24.073| 1183|
|NA              |MaxStay      |  2.883|      NA|    1|

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
|                    0.0784|            0.121|

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
|30d Survival              |No                                         |  879|  379| NA|  30.1|       0|**  |
|30d Survival              |Yes                                        |  304|  804|  1|  72.6|       0|**  |
|60d Survival              |Surival Past 60 Days                       |  215|  436| NA|  67.0|       0|**  |
|60d Survival              |Within 60 Days                             |   24|  135| NA|  84.9|       0|**  |
|60d Survival              |NA                                         |  944|  612|  1|  39.3|       0|**  |
|Ancestry                  |AFR                                        |   46|   62|  1|  57.4|       0|NA  |
|Ancestry                  |AMR                                        |    4|    4| NA|  50.0|       1|NA  |
|Ancestry                  |CSA                                        |    7|    5| NA|  41.7|       1|NA  |
|Ancestry                  |EAS                                        |    7|    4| NA|  36.4|       0|NA  |
|Ancestry                  |EUR                                        |  687|  653| NA|  48.7|       0|NA  |
|Ancestry                  |WAS                                        |   10|    8| NA|  44.4|       1|NA  |
|Ancestry                  |NA                                         |  422|  447| NA|  51.4|       0|NA  |
|Death                     |Alive                                      |  239|  571| NA|  70.5|       0|**  |
|Death                     |Deceased                                   |  944|  612|  1|  39.3|       0|**  |
|Emergency Visit           |NA                                         | 1183| 1183|  1|  50.0|       1|NA  |
|Ethnicity                 |Hispanic or Latino                         |   32|   25| NA|  43.9|       0|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 1125| 1139|  1|  50.3|       1|NA  |
|Ethnicity                 |Patient Refused                            |    7|    3| NA|  30.0|       0|NA  |
|Ethnicity                 |Unknown                                    |   18|   16| NA|  47.1|       1|NA  |
|Ethnicity                 |NA                                         |    1|    0| NA|   0.0|       0|NA  |
|Gender                    |F                                          |  653|  486|  1|  42.7|       0|**  |
|Gender                    |M                                          |  530|  697| NA|  56.8|       0|**  |
|Pneumonia Type            |Bacterial                                  | 1183| 1183|  1|  50.0|       1|NA  |
|Prior COPD                |Yes                                        |   27|   50|  1|  64.9|       0|**  |
|Prior COPD                |NA                                         | 1156| 1133| NA|  49.5|       1|NA  |
|Prior Cardiac Arrhythmias |Yes                                        |  241|  434|  1|  64.3|       0|**  |
|Prior Cardiac Arrhythmias |NA                                         |  942|  749| NA|  44.3|       0|**  |
|Prior Obesity             |Yes                                        |  211|  274|  1|  56.5|       0|**  |
|Prior Obesity             |NA                                         |  972|  909| NA|  48.3|       0|NA  |
|PriorDiabetes             |Yes                                        |  173|  304| NA|  63.7|       0|**  |
|PriorDiabetes             |NA                                         | 1010|  879|  1|  46.5|       0|**  |
|PriorHypertension         |Yes                                        |  312|  478|  1|  60.5|       0|**  |
|PriorHypertension         |NA                                         |  871|  705| NA|  44.7|       0|**  |
|Race                      |African American                           |   90|  131|  1|  59.3|       0|**  |
|Race                      |American Indian                            |    6|    7| NA|  53.8|       1|NA  |
|Race                      |Asian                                      |   23|    9| NA|  28.1|       0|*   |
|Race                      |Caucasian                                  | 1034| 1009| NA|  49.4|       1|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |    1|    1| NA|  50.0|       1|NA  |
|Race                      |Other                                      |   22|   16| NA|  42.1|       0|NA  |
|Race                      |Patient Refused                            |    0|    2| NA| 100.0|       0|NA  |
|Race                      |Unknown                                    |    4|    5| NA|  55.6|       1|NA  |
|Race                      |NA                                         |    3|    3| NA|  50.0|       1|NA  |
|Smoking                   |Current                                    |   70|   72| NA|  50.7|       1|NA  |
|Smoking                   |Former                                     |  414|  520| NA|  55.7|       0|**  |
|Smoking                   |Never                                      |  488|  431|  1|  46.9|       0|NA  |
|Smoking                   |Unknown                                    |    5|   33| NA|  86.8|       0|**  |
|Smoking                   |NA                                         |  206|  127| NA|  38.1|       0|**  |



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

|Ventillation |Variable     |  mean|     sd|    n|
|:------------|:------------|-----:|------:|----:|
|No           |AgeInYears.x | 57.58|  17.00| 1745|
|Yes          |AgeInYears.x | 56.90|  15.95|  622|
|No           |BMI          | 28.91|   7.69| 1745|
|Yes          |BMI          | 42.13| 283.87|  622|
|No           |MaxStay      |  4.09|  10.25| 1745|
|Yes          |MaxStay      | 23.50|  28.64|  622|

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
|                     0.195|           0.0154|

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



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by ventillation status.  Overall 26.28 % required ventillation

|Type                      |Group                                      |   No| Yes|  Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|----:|---:|----:|--------:|:---|
|30d Survival              |No                                         |  982| 276| 21.9| 4.72e-04|**  |
|30d Survival              |Yes                                        |  763| 346| 31.2| 1.96e-04|**  |
|60d Survival              |Surival Past 60 Days                       |  407| 244| 37.5| 8.35e-11|**  |
|60d Survival              |Within 60 Days                             |   67|  92| 57.9| 1.45e-19|**  |
|60d Survival              |NA                                         | 1271| 286| 18.4| 1.33e-12|**  |
|Ancestry                  |AFR                                        |   76|  33| 30.3| 3.43e-01|NA  |
|Ancestry                  |AMR                                        |    5|   3| 37.5| 4.71e-01|NA  |
|Ancestry                  |CSA                                        |    9|   3| 25.0| 9.20e-01|NA  |
|Ancestry                  |EAS                                        |    9|   2| 18.2| 5.42e-01|NA  |
|Ancestry                  |EUR                                        |  976| 364| 27.2| 4.61e-01|NA  |
|Ancestry                  |WAS                                        |   15|   3| 16.7| 3.54e-01|NA  |
|Ancestry                  |NA                                         |  655| 214| 24.6| 2.69e-01|NA  |
|Death                     |Alive                                      |  474| 336| 41.5| 8.29e-23|**  |
|Death                     |Deceased                                   | 1271| 286| 18.4| 1.33e-12|**  |
|Emergency Visit           |NA                                         | 1745| 622| 26.3| 1.00e+00|NA  |
|Ethnicity                 |Hispanic or Latino                         |   44|  13| 22.8| 5.52e-01|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 1664| 601| 26.5| 7.82e-01|NA  |
|Ethnicity                 |Patient Refused                            |    7|   3| 30.0| 7.89e-01|NA  |
|Ethnicity                 |Unknown                                    |   29|   5| 14.7| 1.25e-01|NA  |
|Ethnicity                 |NA                                         |    1|   0|  0.0| 5.50e-01|NA  |
|Gender                    |F                                          |  927| 213| 18.7| 5.70e-09|**  |
|Gender                    |M                                          |  818| 409| 33.3| 1.97e-08|**  |
|Pneumonia Type            |Bacterial                                  | 1745| 622| 26.3| 1.00e+00|NA  |
|Prior COPD                |Yes                                        |   40|  38| 48.7| 6.71e-06|**  |
|Prior COPD                |NA                                         | 1705| 584| 25.5| 4.06e-01|NA  |
|Prior Cardiac Arrhythmias |Yes                                        |  396| 280| 41.4| 3.73e-19|**  |
|Prior Cardiac Arrhythmias |NA                                         | 1349| 342| 20.2| 1.55e-08|**  |
|Prior Obesity             |Yes                                        |  320| 166| 34.2| 7.95e-05|**  |
|Prior Obesity             |NA                                         | 1425| 456| 24.2| 4.49e-02|*   |
|PriorDiabetes             |Yes                                        |  271| 206| 43.2| 4.85e-17|**  |
|PriorDiabetes             |NA                                         | 1474| 416| 22.0| 2.50e-05|**  |
|PriorHypertension         |Yes                                        |  491| 300| 37.9| 9.81e-14|**  |
|PriorHypertension         |NA                                         | 1254| 322| 20.4| 1.34e-07|**  |
|Race                      |African American                           |  151|  71| 32.0| 5.35e-02|NA  |
|Race                      |American Indian                            |    8|   5| 38.5| 3.18e-01|NA  |
|Race                      |Asian                                      |   26|   6| 18.8| 3.33e-01|NA  |
|Race                      |Caucasian                                  | 1518| 525| 25.7| 5.51e-01|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |    1|   1| 50.0| 4.46e-01|NA  |
|Race                      |Other                                      |   28|  10| 26.3| 9.96e-01|NA  |
|Race                      |Patient Refused                            |    2|   0|  0.0| 3.98e-01|NA  |
|Race                      |Unknown                                    |    6|   3| 33.3| 6.31e-01|NA  |
|Race                      |NA                                         |    5|   1| 16.7| 5.93e-01|NA  |
|Smoking                   |Current                                    |   97|  45| 31.7| 1.43e-01|NA  |
|Smoking                   |Former                                     |  659| 275| 29.4| 2.80e-02|*   |
|Smoking                   |Never                                      |  707| 213| 23.2| 3.12e-02|*   |
|Smoking                   |Unknown                                    |   11|  27| 71.1| 3.59e-10|**  |
|Smoking                   |NA                                         |  271|  62| 18.6| 1.50e-03|**  |
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
