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

To analyse the subset of patients with viral or bacterial pneumonia.   This script combines the cleaned datasets and writes out a complete datafile for analyses.  This script can be found in /nfs/turbo/precision-health/DataDirect/HUM00229632 - Genome-wide associations of bacteria/2024-05-08/cases and was most recently run on Wed May  8 13:34:06 2024.


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
## Rows: 2142 Columns: 45
```

```
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (12): DeID_PatientID, AdmitMonth, Type, GenderCode, RaceName, Ethnicity...
## dbl  (22): AgeInYears, BMI, EmergencyVisit, MaxStay, CerebrovascularDisease,...
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

|Variable   | mean|    sd|   n|
|:----------|----:|-----:|---:|
|AgeInYears | 58.8| 15.27| 563|
|BMI        | 29.1|  9.66| 563|
|MaxStay    | 17.0| 30.39| 563|

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
|Gender              |F                                | 202|  35.879|
|Gender              |M                                | 361|  64.121|
|Race                |African American                 |  79|  14.032|
|Race                |American Indian or Alaska Native |   4|   0.710|
|Race                |Asian                            |   4|   0.710|
|Race                |Caucasian                        | 472|  83.837|
|Race                |Other                            |   3|   0.533|
|Race                |Unknown                          |   1|   0.178|
|Ethnicity           |Hispanic or Latino               |  13|   2.309|
|Ethnicity           |Non-Hispanic or Latino           | 546|  96.980|
|Ethnicity           |Unknown                          |   4|   0.710|
|Ancestry            |AFR                              |  23|   4.085|
|Ancestry            |AMR                              |   1|   0.178|
|Ancestry            |EAS                              |   2|   0.355|
|Ancestry            |EUR                              | 354|  62.877|
|Ancestry            |WAS                              |   7|   1.243|
|Ancestry            |NA                               | 176|  31.261|
|Smoking             |Current                          |  38|   6.750|
|Smoking             |Former                           | 239|  42.451|
|Smoking             |Never                            | 229|  40.675|
|Smoking             |Unknown                          |  18|   3.197|
|Smoking             |NA                               |  39|   6.927|
|Pneumonia Type      |Bacterial                        | 563| 100.000|
|Emergency Visit     |NA                               | 331|  58.792|
|Emergency Visit     |Yes                              | 228|  40.497|
|Emergency Visit     |NA                               |   4|   0.710|
|Death               |Alive                            | 299|  53.108|
|Death               |Deceased                         | 264|  46.892|
|30d Survival        |Surival Past 30 Days             | 262|  46.536|
|30d Survival        |Within 30 Days                   |  33|   5.861|
|30d Survival        |NA                               | 268|  47.602|
|60d Survival        |Surival Past 60 Days             | 238|  42.274|
|60d Survival        |Within 60 Days                   |  57|  10.124|
|60d Survival        |NA                               | 268|  47.602|
|Diabetes            |NA                               | 269|  47.780|
|Diabetes            |Yes                              | 294|  52.220|
|Hypertension        |NA                               | 114|  20.249|
|Hypertension        |Yes                              | 449|  79.751|
|Cardiac Arrhythmias |NA                               |  88|  15.631|
|Cardiac Arrhythmias |Yes                              | 475|  84.369|

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
                             shapiro.p = ~shapiro.test(.)$p.value,
                             n = ~length(!(is.na(.)))))) %>%
  pivot_longer(cols=2:13,
               names_sep="_",names_to=c("Variable","Statistic")) %>%
  pivot_wider(names_from=Statistic,values_from=value) %>%
  arrange(Variable)

kable(quant.demo, caption="Summary of quantitative values")
```



Table: Summary of quantitative values

|Survival.30day.Group |Variable   | mean|    sd| shapiro.p|   n|
|:--------------------|:----------|----:|-----:|---------:|---:|
|No                   |AgeInYears | 67.9| 13.14|     0.030|  33|
|Yes                  |AgeInYears | 58.2| 15.22|     0.000| 530|
|No                   |BMI        | 27.5|  7.41|     0.016|  33|
|Yes                  |BMI        | 29.2|  9.79|     0.000| 530|
|No                   |MaxStay    | 12.1|  9.12|     0.034|  33|
|Yes                  |MaxStay    | 17.3| 31.22|     0.000| 530|

```r
# wilcoxon tests, not normally distributed

quant.t.tests <-
  combined.data %>%
  summarize(across(.cols=c('AgeInYears','BMI','MaxStay'),
                   .fns=list(Mann.Whitney=~wilcox.test(.~Survival.30day.Group)$p.value)))

kable(quant.t.tests,captionn="Mann-Whitney tests for 30 day survival",digits=c(6,6,6))
```



| AgeInYears_Mann.Whitney| BMI_Mann.Whitney| MaxStay_Mann.Whitney|
|-----------------------:|----------------:|--------------------:|
|                0.000104|            0.269|                 0.34|

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
  mutate(Pct=No/(No+Yes)*100) %>%
  mutate(Chisq.p = chisq.test(x=c(No,Yes),p=total.survival,rescale.p = T)$p.value) %>%
  mutate(Sig = case_when(Chisq.p < 0.01 ~ '**',
                         Chisq.p < 0.05 ~ '*'))

kable(summary.discrete, caption="Summary of discrete variables for bacterial pneumonia cases, stratified by 30 day survival")
```



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by 30 day survival

|Type                |Group                            | No| Yes|    Pct| Chisq.p|Sig |
|:-------------------|:--------------------------------|--:|---:|------:|-------:|:---|
|30d Survival        |Surival Past 30 Days             |  0| 262|   0.00|   0.000|**  |
|30d Survival        |Within 30 Days                   | 33|   0| 100.00|   0.000|**  |
|30d Survival        |NA                               |  0| 268|   0.00|   0.000|**  |
|60d Survival        |Surival Past 60 Days             |  0| 238|   0.00|   0.000|**  |
|60d Survival        |Within 60 Days                   | 33|  24|  57.90|   0.000|**  |
|60d Survival        |NA                               |  0| 268|   0.00|   0.000|**  |
|Ancestry            |AFR                              |  2|  21|   8.70|   0.563|NA  |
|Ancestry            |AMR                              |  1|   0| 100.00|   0.000|**  |
|Ancestry            |EAS                              |  0|   2|   0.00|   0.724|NA  |
|Ancestry            |EUR                              | 20| 334|   5.65|   0.865|NA  |
|Ancestry            |WAS                              |  0|   7|   0.00|   0.509|NA  |
|Ancestry            |NA                               | 10| 166|   5.68|   0.919|NA  |
|Cardiac Arrhythmias |Yes                              | 32| 443|   6.74|   0.417|NA  |
|Cardiac Arrhythmias |NA                               |  1|  87|   1.14|   0.059|NA  |
|Death               |Alive                            | 33| 266|  11.04|   0.000|**  |
|Death               |Deceased                         |  0| 264|   0.00|   0.000|**  |
|Diabetes            |Yes                              | 22| 272|   7.48|   0.237|NA  |
|Diabetes            |NA                               | 11| 258|   4.09|   0.216|NA  |
|Emergency Visit     |Yes                              | 18| 210|   7.89|   0.191|NA  |
|Emergency Visit     |NA                               | 15| 320|   4.48|   0.281|NA  |
|Ethnicity           |Hispanic or Latino               |  1|  12|   7.69|   0.779|NA  |
|Ethnicity           |Non-Hispanic or Latino           | 31| 515|   5.68|   0.855|NA  |
|Ethnicity           |Unknown                          |  1|   3|  25.00|   0.103|NA  |
|Gender              |F                                | 11| 191|   5.45|   0.801|NA  |
|Gender              |M                                | 22| 339|   6.09|   0.851|NA  |
|Hypertension        |Yes                              | 31| 418|   6.90|   0.347|NA  |
|Hypertension        |NA                               |  2| 112|   1.75|   0.062|NA  |
|Pneumonia Type      |Bacterial                        | 33| 530|   5.86|   1.000|NA  |
|Race                |African American                 |  5|  74|   6.33|   0.860|NA  |
|Race                |American Indian or Alaska Native |  0|   4|   0.00|   0.618|NA  |
|Race                |Asian                            |  0|   4|   0.00|   0.618|NA  |
|Race                |Caucasian                        | 28| 444|   5.93|   0.948|NA  |
|Race                |Other                            |  0|   3|   0.00|   0.666|NA  |
|Race                |Unknown                          |  0|   1|   0.00|   0.803|NA  |
|Smoking             |Current                          |  3|  35|   7.89|   0.594|NA  |
|Smoking             |Former                           | 16| 223|   6.70|   0.583|NA  |
|Smoking             |Never                            | 12| 217|   5.24|   0.689|NA  |
|Smoking             |Unknown                          |  1|  17|   5.56|   0.956|NA  |
|Smoking             |NA                               |  1|  38|   2.56|   0.381|NA  |

# By 60 Day Survival


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
                             shapiro.p = ~shapiro.test(.)$p.value,
                             n = ~length(!(is.na(.)))))) %>%
  pivot_longer(cols=2:13,
               names_sep="_",names_to=c("Variable","Statistic")) %>%
  pivot_wider(names_from=Statistic,values_from=value) %>%
  arrange(Variable)

kable(quant.demo, caption="Summary of quantitative values")
```



Table: Summary of quantitative values

|Survival.60.day.Group |Variable   | mean|    sd| shapiro.p|   n|
|:---------------------|:----------|----:|-----:|---------:|---:|
|No                    |AgeInYears | 68.1| 11.68|     0.046|  57|
|Yes                   |AgeInYears | 57.7| 15.28|     0.000| 506|
|No                    |BMI        | 27.5|  6.75|     0.042|  57|
|Yes                   |BMI        | 29.3|  9.95|     0.000| 506|
|No                    |MaxStay    | 17.3| 14.88|     0.002|  57|
|Yes                   |MaxStay    | 16.9| 31.68|     0.000| 506|

```r
quant.t.tests <-
  combined.data %>%
  summarize(across(.cols=c('AgeInYears','BMI','MaxStay'),
                   .fns=list(Mann.Whitney=~wilcox.test(.~Survival.30day.Group)$p.value)))

kable(quant.t.tests,captionn="Mann-Whitney tests for 30 day survival", digits=c(6,6,6))
```



| AgeInYears_Mann.Whitney| BMI_Mann.Whitney| MaxStay_Mann.Whitney|
|-----------------------:|----------------:|--------------------:|
|                0.000104|            0.269|                 0.34|

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
  mutate(Pct=No/(No+Yes)*100) %>%
  mutate(Chisq.p = chisq.test(x=c(No,Yes),p=total.survival,rescale.p = T)$p.value) %>%
  mutate(Sig = case_when(Chisq.p < 0.01 ~ '**',
                         Chisq.p < 0.05 ~ '*'))

kable(summary.discrete, caption="Summary of discrete variables for bacterial pneumonia cases, stratified by 60 day survival")
```



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by 60 day survival

|Type                |Group                            | No| Yes|    Pct| Chisq.p|Sig |
|:-------------------|:--------------------------------|--:|---:|------:|-------:|:---|
|60d Survival        |Surival Past 30 Days             | 24| 238|   9.16|   0.605|NA  |
|60d Survival        |Surival Past 60 Days             |  0| 238|   0.00|   0.000|**  |
|60d Survival        |Within 30 Days                   | 33|   0| 100.00|   0.000|**  |
|60d Survival        |Within 60 Days                   | 57|   0| 100.00|   0.000|**  |
|60d Survival        |NA                               |  0| 536|   0.00|   0.000|**  |
|Ancestry            |AFR                              |  4|  19|  17.39|   0.248|NA  |
|Ancestry            |AMR                              |  1|   0| 100.00|   0.003|**  |
|Ancestry            |EAS                              |  0|   2|   0.00|   0.635|NA  |
|Ancestry            |EUR                              | 37| 317|  10.45|   0.838|NA  |
|Ancestry            |WAS                              |  0|   7|   0.00|   0.375|NA  |
|Ancestry            |NA                               | 15| 161|   8.52|   0.481|NA  |
|Cardiac Arrhythmias |Yes                              | 56| 419|  11.79|   0.229|NA  |
|Cardiac Arrhythmias |NA                               |  1|  87|   1.14|   0.005|**  |
|Death               |Alive                            | 57| 242|  19.06|   0.000|**  |
|Death               |Deceased                         |  0| 264|   0.00|   0.000|**  |
|Diabetes            |Yes                              | 37| 257|  12.59|   0.162|NA  |
|Diabetes            |NA                               | 20| 249|   7.43|   0.144|NA  |
|Emergency Visit     |Yes                              | 27| 201|  11.84|   0.390|NA  |
|Emergency Visit     |NA                               | 30| 305|   8.96|   0.478|NA  |
|Ethnicity           |Hispanic or Latino               |  1|  12|   7.69|   0.771|NA  |
|Ethnicity           |Non-Hispanic or Latino           | 55| 491|  10.07|   0.968|NA  |
|Ethnicity           |Unknown                          |  1|   3|  25.00|   0.324|NA  |
|Gender              |F                                | 19| 183|   9.41|   0.735|NA  |
|Gender              |M                                | 38| 323|  10.53|   0.800|NA  |
|Hypertension        |Yes                              | 53| 396|  11.80|   0.238|NA  |
|Hypertension        |NA                               |  4| 110|   3.51|   0.019|*   |
|Pneumonia Type      |Bacterial                        | 57| 506|  10.12|   1.000|NA  |
|Race                |African American                 |  7|  72|   8.86|   0.710|NA  |
|Race                |American Indian or Alaska Native |  0|   4|   0.00|   0.502|NA  |
|Race                |Asian                            |  0|   4|   0.00|   0.502|NA  |
|Race                |Caucasian                        | 50| 422|  10.59|   0.736|NA  |
|Race                |Other                            |  0|   3|   0.00|   0.561|NA  |
|Race                |Unknown                          |  0|   1|   0.00|   0.737|NA  |
|Smoking             |Current                          |  4|  34|  10.53|   0.935|NA  |
|Smoking             |Former                           | 30| 209|  12.55|   0.213|NA  |
|Smoking             |Never                            | 19| 210|   8.30|   0.359|NA  |
|Smoking             |Unknown                          |  1|  17|   5.56|   0.520|NA  |
|Smoking             |NA                               |  3|  36|   7.69|   0.615|NA  |

# By ER Visit


```r
combined.data <- 
  combined.data %>%
  mutate(EmergencyVisit.Group = case_when(EmergencyVisit==0 ~ "No",
                                    EmergencyVisit==1 ~ "Yes",
                                    is.na(EmergencyVisit) ~ "No",
                                    .default="No"))

quant.demo <- 
  combined.data %>%
  group_by(EmergencyVisit.Group) %>%
  summarize(across(.cols=c('AgeInYears','BMI','MaxStay'),
                   .fns=list(mean = ~mean(.,na.rm=T),
                             sd = ~sd(.,na.rm=T),
                             shapiro.p = ~shapiro.test(.)$p.value,
                             n = ~length(!(is.na(.)))))) %>%
  pivot_longer(cols=2:13,
               names_sep="_",names_to=c("Variable","Statistic")) %>%
  pivot_wider(names_from=Statistic,values_from=value) %>%
  arrange(Variable)

kable(quant.demo, caption="Summary of quantitative values")
```



Table: Summary of quantitative values

|EmergencyVisit.Group |Variable   | mean|    sd| shapiro.p|   n|
|:--------------------|:----------|----:|-----:|---------:|---:|
|No                   |AgeInYears | 58.8| 15.12|         0| 335|
|Yes                  |AgeInYears | 58.6| 15.53|         0| 228|
|No                   |BMI        | 29.4| 11.27|         0| 335|
|Yes                  |BMI        | 28.6|  7.12|         0| 228|
|No                   |MaxStay    | 16.4| 34.91|         0| 335|
|Yes                  |MaxStay    | 17.7| 22.30|         0| 228|

```r
# wilcoxon tests, not normally distributed

quant.t.tests <-
  combined.data %>%
  summarize(across(.cols=c('AgeInYears','BMI','MaxStay'),
                   .fns=list(Mann.Whitney=~wilcox.test(.~EmergencyVisit.Group)$p.value)))

kable(quant.t.tests,captionn="Mann-Whitney tests for ER visit",digits=c(6,6,99))
```



| AgeInYears_Mann.Whitney| BMI_Mann.Whitney| MaxStay_Mann.Whitney|
|-----------------------:|----------------:|--------------------:|
|                   0.663|            0.709|             1.09e-13|

```r
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
  group_by(EmergencyVisit,EmergencyVisit.Group) %>%
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
  group_by(Diabetes,EmergencyVisit.Group) %>%
  count %>%
  ungroup %>%
  rename("Group"="Diabetes") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Diabetes") %>%
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
  group_by(Hypertension,EmergencyVisit.Group) %>%
  rename("Group"="Hypertension") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Hypertension") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(CardiacArrhythmias,EmergencyVisit.Group) %>%
  rename("Group"="CardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Cardiac Arrhythmias") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(CardiacArrhythmias,EmergencyVisit.Group) %>%
  rename("Group"="CardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Cardiac Arrhythmias") %>%
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
  group_by(EmergencyVisit,EmergencyVisit.Group) %>%
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
            emerg.demo.ql, 
            death.demo.ql,
            surv30d.demo.ql %>% mutate(Group=as.character(Group)),
            surv60d.demo.ql,
            diabetes.demo.ql,
            hypertension.demo.ql,
            arrythmia.demo.ql,
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
  mutate(Pct=No/(No+Yes)*100) %>%
  mutate(Chisq.p = chisq.test(x=c(No,Yes),p=total.survival,rescale.p = T)$p.value) %>%
  mutate(Sig = case_when(Chisq.p < 0.01 ~ '**',
                         Chisq.p < 0.05 ~ '*'))

kable(summary.discrete, caption="Summary of discrete variables for bacterial pneumonia cases, stratified by ER visit survival")
```



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by ER visit survival

|Type                |Group                            |  No| Yes|   Pct| Chisq.p|Sig |
|:-------------------|:--------------------------------|---:|---:|-----:|-------:|:---|
|30d Survival        |0                                | 331|   0| 100.0|   0.000|**  |
|30d Survival        |1                                |   0| 228|   0.0|   0.000|**  |
|30d Survival        |NA                               |   4|   0| 100.0|   0.099|NA  |
|60d Survival        |Surival Past 60 Days             | 133| 105|  55.9|   0.255|NA  |
|60d Survival        |Within 60 Days                   |  30|  27|  52.6|   0.291|NA  |
|60d Survival        |NA                               | 172|  96|  64.2|   0.119|NA  |
|Ancestry            |AFR                              |  13|  10|  56.5|   0.771|NA  |
|Ancestry            |AMR                              |   1|   0| 100.0|   0.409|NA  |
|Ancestry            |EAS                              |   1|   1|  50.0|   0.784|NA  |
|Ancestry            |EUR                              | 214| 140|  60.5|   0.716|NA  |
|Ancestry            |WAS                              |   3|   4|  42.9|   0.370|NA  |
|Ancestry            |NA                               | 103|  73|  58.5|   0.791|NA  |
|Cardiac Arrhythmias |Yes                              | 265| 210|  55.8|   0.099|NA  |
|Cardiac Arrhythmias |NA                               |  70|  18|  79.5|   0.000|**  |
|Death               |Alive                            | 167| 132|  55.9|   0.199|NA  |
|Death               |Deceased                         | 168|  96|  63.6|   0.171|NA  |
|Diabetes            |Yes                              | 166| 128|  56.5|   0.288|NA  |
|Diabetes            |NA                               | 169| 100|  62.8|   0.267|NA  |
|Emergency Visit     |Yes                              |   0| 228|   0.0|   0.000|**  |
|Emergency Visit     |NA                               | 335|   0| 100.0|   0.000|**  |
|Ethnicity           |Hispanic or Latino               |   6|   7|  46.2|   0.327|NA  |
|Ethnicity           |Non-Hispanic or Latino           | 327| 219|  59.9|   0.854|NA  |
|Ethnicity           |Unknown                          |   2|   2|  50.0|   0.699|NA  |
|Gender              |F                                | 115|  87|  56.9|   0.456|NA  |
|Gender              |M                                | 220| 141|  60.9|   0.578|NA  |
|Hypertension        |Yes                              | 250| 199|  55.7|   0.099|NA  |
|Hypertension        |NA                               |  85|  29|  74.6|   0.001|**  |
|Pneumonia Type      |Bacterial                        | 335| 228|  59.5|   1.000|NA  |
|Race                |African American                 |  49|  30|  62.0|   0.648|NA  |
|Race                |American Indian or Alaska Native |   2|   2|  50.0|   0.699|NA  |
|Race                |Asian                            |   2|   2|  50.0|   0.699|NA  |
|Race                |Caucasian                        | 282| 190|  59.7|   0.914|NA  |
|Race                |Other                            |   0|   3|   0.0|   0.036|*   |
|Race                |Unknown                          |   0|   1|   0.0|   0.225|NA  |
|Smoking             |Current                          |  15|  23|  39.5|   0.012|*   |
|Smoking             |Former                           | 136| 103|  56.9|   0.413|NA  |
|Smoking             |Never                            | 146|  83|  63.8|   0.190|NA  |
|Smoking             |Unknown                          |  14|   4|  77.8|   0.114|NA  |
|Smoking             |NA                               |  24|  15|  61.5|   0.796|NA  |

# By Longer than Median Stay

The median stay was 5.249.


```r
combined.data <- 
  combined.data %>%
  mutate(StayAboveMedian=case_when(MaxStay>median(MaxStay,na.rm=T)~"Yes",
                                MaxStay<median(MaxStay,na.rm=T)~"No"))

quant.demo <- 
  combined.data %>%
  group_by(StayAboveMedian) %>%
  summarize(across(.cols=c('AgeInYears','BMI','MaxStay'),
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

|StayAboveMedian |Variable   |  mean|    sd|   n|
|:---------------|:----------|-----:|-----:|---:|
|No              |AgeInYears | 60.26| 14.57| 279|
|Yes             |AgeInYears | 57.18| 15.81| 279|
|NA              |AgeInYears | 76.00|    NA|   5|
|No              |BMI        | 28.83|  6.90| 279|
|Yes             |BMI        | 29.31| 11.46| 279|
|NA              |BMI        | 26.41|    NA|   5|
|No              |MaxStay    |  1.22|  1.75| 279|
|Yes             |MaxStay    | 32.77| 36.75| 279|
|NA              |MaxStay    |  5.25|    NA|   5|

```r
# wilcoxon tests, not normally distributed

quant.t.tests <-
  combined.data %>%
  summarize(across(.cols=c('AgeInYears','BMI'),
                   .fns=list(Mann.Whitney=~wilcox.test(.~StayAboveMedian)$p.value)))

kable(quant.t.tests,captionn="Mann-Whitney tests for ER visit",digits=c(6,6,99))
```



| AgeInYears_Mann.Whitney| BMI_Mann.Whitney|
|-----------------------:|----------------:|
|                  0.0149|            0.801|

```r
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
  group_by(EmergencyVisit,StayAboveMedian) %>%
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
  group_by(Diabetes,StayAboveMedian) %>%
  count %>%
  ungroup %>%
  rename("Group"="Diabetes") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Diabetes") %>%
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
  group_by(Hypertension,StayAboveMedian) %>%
  rename("Group"="Hypertension") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Hypertension") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(CardiacArrhythmias,StayAboveMedian) %>%
  rename("Group"="CardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Cardiac Arrhythmias") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(CardiacArrhythmias,StayAboveMedian) %>%
  rename("Group"="CardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Cardiac Arrhythmias") %>%
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
  group_by(EmergencyVisit,StayAboveMedian) %>%
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
  mutate(Pct=No/(No+Yes)*100) %>%
  filter(No+Yes>0) %>%
  mutate(Chisq.p = chisq.test(x=c(No,Yes),p=total.survival[1:2],rescale.p = T)$p.value) %>%
  mutate(Sig = case_when(Chisq.p < 0.01 ~ '**',
                         Chisq.p < 0.05 ~ '*'))

kable(summary.discrete, caption="Summary of discrete variables for bacterial pneumonia cases, stratified by above average length of stay")
```



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by above average length of stay

|Type                |Group                            |  No| Yes| NA|  Pct| Chisq.p|Sig |
|:-------------------|:--------------------------------|---:|---:|--:|----:|-------:|:---|
|30d Survival        |0                                | 196| 135| NA| 59.2|   0.001|**  |
|30d Survival        |1                                |  83| 144|  1| 36.6|   0.000|**  |
|60d Survival        |Surival Past 60 Days             | 129| 108|  1| 54.4|   0.173|NA  |
|60d Survival        |Within 60 Days                   |  17|  40| NA| 29.8|   0.002|**  |
|60d Survival        |NA                               | 133| 131|  4| 50.4|   0.902|NA  |
|Ancestry            |AFR                              |   7|  16| NA| 30.4|   0.061|NA  |
|Ancestry            |AMR                              |   0|   1| NA|  0.0|   0.317|NA  |
|Ancestry            |EAS                              |   1|   1| NA| 50.0|   1.000|NA  |
|Ancestry            |EUR                              | 194| 156|  4| 55.4|   0.042|*   |
|Ancestry            |WAS                              |   4|   3| NA| 57.1|   0.705|NA  |
|Ancestry            |NA                               |  73| 102|  1| 41.7|   0.028|*   |
|Cardiac Arrhythmias |Yes                              | 220| 250|  5| 46.8|   0.166|NA  |
|Cardiac Arrhythmias |NA                               |  59|  29| NA| 67.0|   0.001|**  |
|Death               |Alive                            | 146| 148|  5| 49.7|   0.907|NA  |
|Death               |Deceased                         | 133| 131| NA| 50.4|   0.902|NA  |
|Diabetes            |Yes                              | 144| 145|  5| 49.8|   0.953|NA  |
|Diabetes            |NA                               | 135| 134| NA| 50.2|   0.951|NA  |
|Emergency Visit     |Yes                              |  83| 144|  1| 36.6|   0.000|**  |
|Emergency Visit     |NA                               | 196| 135|  4| 59.2|   0.001|**  |
|Ethnicity           |Hispanic or Latino               |   9|   4| NA| 69.2|   0.166|NA  |
|Ethnicity           |Non-Hispanic or Latino           | 270| 271|  5| 49.9|   0.966|NA  |
|Ethnicity           |Unknown                          |   0|   4| NA|  0.0|   0.046|*   |
|Gender              |F                                | 101| 101| NA| 50.0|   1.000|NA  |
|Gender              |M                                | 178| 178|  5| 50.0|   1.000|NA  |
|Hypertension        |Yes                              | 216| 229|  4| 48.5|   0.538|NA  |
|Hypertension        |NA                               |  63|  50|  1| 55.8|   0.221|NA  |
|Pneumonia Type      |Bacterial                        | 279| 279|  5| 50.0|   1.000|NA  |
|Race                |African American                 |  35|  44| NA| 44.3|   0.311|NA  |
|Race                |American Indian or Alaska Native |   1|   3| NA| 25.0|   0.317|NA  |
|Race                |Asian                            |   2|   2| NA| 50.0|   1.000|NA  |
|Race                |Caucasian                        | 241| 226|  5| 51.6|   0.488|NA  |
|Race                |Other                            |   0|   3| NA|  0.0|   0.083|NA  |
|Race                |Unknown                          |   0|   1| NA|  0.0|   0.317|NA  |
|Smoking             |Current                          |  14|  24| NA| 36.8|   0.105|NA  |
|Smoking             |Former                           | 121| 116|  2| 51.1|   0.745|NA  |
|Smoking             |Never                            | 128|  98|  3| 56.6|   0.046|*   |
|Smoking             |Unknown                          |   2|  16| NA| 11.1|   0.001|**  |
|Smoking             |NA                               |  14|  25| NA| 35.9|   0.078|NA  |

`

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
