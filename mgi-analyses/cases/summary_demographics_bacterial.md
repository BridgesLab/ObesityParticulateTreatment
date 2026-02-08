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

To analyse the subset of patients with viral or bacterial pneumonia.   This script combines the cleaned datasets and writes out a complete datafile for analyses.  This script can be found in /nfs/turbo/precision-health/DataDirect/HUM00229632 - Genome-wide associations of bacteria/2025-05-12/cases and was most recently run on Mon Dec  1 09:46:12 2025.


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
## Rows: 7604 Columns: 78
```

```
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (28): DeID_PatientID, DeID_EncounterID.x, TermNameMapped.x, TermCodeSou...
## dbl  (34): AgeInYears.x, EmergencyVisit.x, HeightCM, WeightKG, BMI, Duration...
## dttm  (2): DeID_AdmitDate.x, DeID_DischargeDate.x
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

|Variable     | mean|    sd|    n|
|:------------|----:|-----:|----:|
|AgeInYears.x | 57.0|  16.9| 2646|
|BMI          | 32.5| 145.3| 2646|
|MaxStay      | 10.3|  20.6| 2646|

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
|Gender                    |F                                          | 1246|  47.090|
|Gender                    |M                                          | 1400|  52.910|
|Race                      |African American                           |  242|   9.146|
|Race                      |American Indian                            |   16|   0.605|
|Race                      |Asian                                      |   37|   1.398|
|Race                      |Caucasian                                  | 2292|  86.621|
|Race                      |Native Hawaiian and Other Pacific Islander |    3|   0.113|
|Race                      |Other                                      |   39|   1.474|
|Race                      |Patient Refused                            |    4|   0.151|
|Race                      |Unknown                                    |   10|   0.378|
|Race                      |NA                                         |    3|   0.113|
|Ethnicity                 |Hispanic or Latino                         |   63|   2.381|
|Ethnicity                 |Non-Hispanic or Latino                     | 2531|  95.654|
|Ethnicity                 |Patient Refused                            |   13|   0.491|
|Ethnicity                 |Unknown                                    |   38|   1.436|
|Ethnicity                 |NA                                         |    1|   0.038|
|Ancestry                  |AFR                                        |  114|   4.308|
|Ancestry                  |AMR                                        |    8|   0.302|
|Ancestry                  |CSA                                        |   13|   0.491|
|Ancestry                  |EAS                                        |   11|   0.416|
|Ancestry                  |EUR                                        | 1494|  56.463|
|Ancestry                  |WAS                                        |   20|   0.756|
|Ancestry                  |NA                                         |  986|  37.264|
|Smoking                   |Current                                    |  161|   6.085|
|Smoking                   |Former                                     | 1057|  39.947|
|Smoking                   |Never                                      | 1019|  38.511|
|Smoking                   |Unknown                                    |   43|   1.625|
|Smoking                   |NA                                         |  366|  13.832|
|Pneumonia Type            |Bacterial                                  | 2646| 100.000|
|Emergency Visit           |NA                                         | 1448|  54.724|
|Emergency Visit           |Yes                                        | 1198|  45.276|
|Death                     |Alive                                      |  954|  36.054|
|Death                     |Deceased                                   | 1692|  63.946|
|30d Survival              |Surival Past 30 Days                       |  845|  31.935|
|30d Survival              |Within 30 Days                             |  109|   4.119|
|30d Survival              |NA                                         | 1692|  63.946|
|60d Survival              |Surival Past 60 Days                       |  773|  29.214|
|60d Survival              |Within 60 Days                             |  181|   6.841|
|60d Survival              |NA                                         | 1692|  63.946|
|PriorDiabetes             |NA                                         | 2128|  80.423|
|PriorDiabetes             |Yes                                        |  518|  19.577|
|PriorHypertension         |NA                                         | 1778|  67.196|
|PriorHypertension         |Yes                                        |  868|  32.804|
|Prior Cardiac Arrhythmias |NA                                         | 1900|  71.807|
|Prior Cardiac Arrhythmias |Yes                                        |  746|  28.193|
|Prior Obesity             |NA                                         | 2119|  80.083|
|Prior Obesity             |Yes                                        |  527|  19.917|
|Prior COPD                |NA                                         | 2565|  96.939|
|Prior COPD                |Yes                                        |   81|   3.061|

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

|Survival.30day.Group |Variable     | mean|     sd|    n|
|:--------------------|:------------|----:|------:|----:|
|No                   |AgeInYears.x | 68.1|  13.46|  109|
|Yes                  |AgeInYears.x | 56.5|  16.87| 2537|
|No                   |BMI          | 27.8|   6.85|  109|
|Yes                  |BMI          | 32.7| 148.98| 2537|
|No                   |MaxStay      | 13.2|   7.74|  109|
|Yes                  |MaxStay      | 10.2|  20.95| 2537|

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
|                  3.82e-12|             0.12|             4.83e-16|

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



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by 30 day survival.  Overall 4.12 % visited died at 30d

|Type                      |Group                                      |  No|  Yes|   Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|---:|----:|-----:|--------:|:---|
|30d Survival              |Surival Past 30 Days                       |   0|  845|   0.0| 1.69e-09|**  |
|30d Survival              |Within 30 Days                             | 109|    0| 100.0| 0.00e+00|**  |
|30d Survival              |NA                                         |   0| 1692|   0.0| 1.51e-17|**  |
|60d Survival              |Surival Past 60 Days                       |   0|  773|   0.0| 8.27e-09|**  |
|60d Survival              |Within 60 Days                             | 109|   72|  60.2| 0.00e+00|**  |
|60d Survival              |NA                                         |   0| 1692|   0.0| 1.51e-17|**  |
|Ancestry                  |AFR                                        |   5|  109|   4.4| 8.86e-01|NA  |
|Ancestry                  |AMR                                        |   1|    7|  12.5| 2.33e-01|NA  |
|Ancestry                  |CSA                                        |   1|   12|   7.7| 5.17e-01|NA  |
|Ancestry                  |EAS                                        |   1|   10|   9.1| 4.07e-01|NA  |
|Ancestry                  |EUR                                        |  69| 1425|   4.6| 3.32e-01|NA  |
|Ancestry                  |WAS                                        |   0|   20|   0.0| 3.54e-01|NA  |
|Ancestry                  |NA                                         |  32|  954|   3.2| 1.67e-01|NA  |
|Death                     |Alive                                      | 109|  845|  11.4| 7.02e-30|**  |
|Death                     |Deceased                                   |   0| 1692|   0.0| 1.51e-17|**  |
|Emergency Visit           |Yes                                        |  73| 1125|   6.1| 5.86e-04|**  |
|Emergency Visit           |NA                                         |  36| 1412|   2.5| 1.77e-03|**  |
|Ethnicity                 |Hispanic or Latino                         |   1|   62|   1.6| 3.12e-01|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 105| 2426|   4.1| 9.41e-01|NA  |
|Ethnicity                 |Patient Refused                            |   0|   13|   0.0| 4.55e-01|NA  |
|Ethnicity                 |Unknown                                    |   3|   35|   7.9| 2.42e-01|NA  |
|Ethnicity                 |NA                                         |   0|    1|   0.0| 8.36e-01|NA  |
|Gender                    |F                                          |  38| 1208|   3.0| 5.75e-02|NA  |
|Gender                    |M                                          |  71| 1329|   5.1| 7.31e-02|NA  |
|Pneumonia Type            |Bacterial                                  | 109| 2537|   4.1| 1.00e+00|NA  |
|Prior COPD                |Yes                                        |   5|   76|   6.2| 3.52e-01|NA  |
|Prior COPD                |NA                                         | 104| 2461|   4.1| 8.69e-01|NA  |
|Prior Cardiac Arrhythmias |Yes                                        |  45|  701|   6.0| 8.57e-03|**  |
|Prior Cardiac Arrhythmias |NA                                         |  64| 1836|   3.4| 9.95e-02|NA  |
|Prior Obesity             |Yes                                        |  21|  506|   4.0| 8.76e-01|NA  |
|Prior Obesity             |NA                                         |  88| 2031|   4.2| 9.38e-01|NA  |
|PriorDiabetes             |Yes                                        |  28|  490|   5.4| 1.41e-01|NA  |
|PriorDiabetes             |NA                                         |  81| 2047|   3.8| 4.67e-01|NA  |
|PriorHypertension         |Yes                                        |  46|  822|   5.3| 8.02e-02|NA  |
|PriorHypertension         |NA                                         |  63| 1715|   3.5| 2.22e-01|NA  |
|Race                      |African American                           |  11|  231|   4.5| 7.39e-01|NA  |
|Race                      |American Indian                            |   0|   16|   0.0| 4.07e-01|NA  |
|Race                      |Asian                                      |   1|   36|   2.7| 6.65e-01|NA  |
|Race                      |Caucasian                                  |  96| 2196|   4.2| 8.68e-01|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |   0|    3|   0.0| 7.20e-01|NA  |
|Race                      |Other                                      |   1|   38|   2.6| 6.25e-01|NA  |
|Race                      |Patient Refused                            |   0|    4|   0.0| 6.78e-01|NA  |
|Race                      |Unknown                                    |   0|   10|   0.0| 5.12e-01|NA  |
|Race                      |NA                                         |   0|    3|   0.0| 7.20e-01|NA  |
|Smoking                   |Current                                    |   3|  158|   1.9| 1.50e-01|NA  |
|Smoking                   |Former                                     |  55| 1002|   5.2| 7.62e-02|NA  |
|Smoking                   |Never                                      |  46|  973|   4.5| 5.26e-01|NA  |
|Smoking                   |Unknown                                    |   1|   42|   2.3| 5.54e-01|NA  |
|Smoking                   |NA                                         |   4|  362|   1.1| 3.58e-03|**  |

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
|No                    |AgeInYears.x | 67.05|  13.81|  181|
|Yes                   |AgeInYears.x | 56.27|  16.87| 2465|
|No                    |BMI          | 27.72|   7.22|  181|
|Yes                   |BMI          | 32.88| 151.59| 2465|
|No                    |MaxStay      | 16.55|  13.68|  181|
|Yes                   |MaxStay      |  9.86|  20.93| 2465|

``` r
quant.t.tests <-
  combined.data %>%
  summarize(across(.cols=c('AgeInYears.x','BMI','MaxStay'),
                   .fns=list(Mann.Whitney=~wilcox.test(.~Survival.30day.Group)$p.value)))

kable(quant.t.tests,captionn="Mann-Whitney tests for 30 day survival", digits=c(99,99,99))
```



| AgeInYears.x_Mann.Whitney| BMI_Mann.Whitney| MaxStay_Mann.Whitney|
|-------------------------:|----------------:|--------------------:|
|                  3.82e-12|             0.12|             4.83e-16|

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



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by 60 day survival.  Overall 6.84 % visited died at 60d

|Type                      |Group                                      |  No|  Yes|   Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|---:|----:|-----:|--------:|:---|
|60d Survival              |Surival Past 30 Days                       |  72|  773|   8.5| 5.30e-02|NA  |
|60d Survival              |Surival Past 60 Days                       |   0|  773|   0.0| 4.92e-14|**  |
|60d Survival              |Within 30 Days                             | 109|    0| 100.0| 0.00e+00|**  |
|60d Survival              |Within 60 Days                             | 181|    0| 100.0| 0.00e+00|**  |
|60d Survival              |NA                                         |   0| 3384|   0.0| 5.57e-56|**  |
|Ancestry                  |AFR                                        |   8|  106|   7.0| 9.40e-01|NA  |
|Ancestry                  |AMR                                        |   1|    7|  12.5| 5.26e-01|NA  |
|Ancestry                  |CSA                                        |   1|   12|   7.7| 9.03e-01|NA  |
|Ancestry                  |EAS                                        |   1|   10|   9.1| 7.67e-01|NA  |
|Ancestry                  |EUR                                        | 118| 1376|   7.9| 1.05e-01|NA  |
|Ancestry                  |WAS                                        |   0|   20|   0.0| 2.26e-01|NA  |
|Ancestry                  |NA                                         |  52|  934|   5.3| 5.13e-02|NA  |
|Death                     |Alive                                      | 181|  773|  19.0| 7.58e-50|**  |
|Death                     |Deceased                                   |   0| 1692|   0.0| 7.46e-29|**  |
|Emergency Visit           |Yes                                        | 113| 1085|   9.4| 3.80e-04|**  |
|Emergency Visit           |NA                                         |  68| 1380|   4.7| 1.23e-03|**  |
|Ethnicity                 |Hispanic or Latino                         |   2|   61|   3.2| 2.49e-01|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 174| 2357|   6.9| 9.46e-01|NA  |
|Ethnicity                 |Patient Refused                            |   1|   12|   7.7| 9.03e-01|NA  |
|Ethnicity                 |Unknown                                    |   4|   34|  10.5| 3.68e-01|NA  |
|Ethnicity                 |NA                                         |   0|    1|   0.0| 7.86e-01|NA  |
|Gender                    |F                                          |  71| 1175|   5.7| 1.10e-01|NA  |
|Gender                    |M                                          | 110| 1290|   7.9| 1.32e-01|NA  |
|Pneumonia Type            |Bacterial                                  | 181| 2465|   6.8| 1.00e+00|NA  |
|Prior COPD                |Yes                                        |  10|   71|  12.3| 4.97e-02|*   |
|Prior COPD                |NA                                         | 171| 2394|   6.7| 7.27e-01|NA  |
|Prior Cardiac Arrhythmias |Yes                                        |  78|  668|  10.5| 9.17e-05|**  |
|Prior Cardiac Arrhythmias |NA                                         | 103| 1797|   5.4| 1.42e-02|*   |
|Prior Obesity             |Yes                                        |  36|  491|   6.8| 9.93e-01|NA  |
|Prior Obesity             |NA                                         | 145| 1974|   6.8| 9.97e-01|NA  |
|PriorDiabetes             |Yes                                        |  44|  474|   8.5| 1.36e-01|NA  |
|PriorDiabetes             |NA                                         | 137| 1991|   6.4| 4.62e-01|NA  |
|PriorHypertension         |Yes                                        |  80|  788|   9.2| 5.55e-03|**  |
|PriorHypertension         |NA                                         | 101| 1677|   5.7| 5.27e-02|NA  |
|Race                      |African American                           |  14|  228|   5.8| 5.15e-01|NA  |
|Race                      |American Indian                            |   0|   16|   0.0| 2.78e-01|NA  |
|Race                      |Asian                                      |   1|   36|   2.7| 3.19e-01|NA  |
|Race                      |Caucasian                                  | 164| 2128|   7.2| 5.50e-01|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |   1|    2|  33.3| 6.91e-02|NA  |
|Race                      |Other                                      |   1|   38|   2.6| 2.90e-01|NA  |
|Race                      |Patient Refused                            |   0|    4|   0.0| 5.88e-01|NA  |
|Race                      |Unknown                                    |   0|   10|   0.0| 3.91e-01|NA  |
|Race                      |NA                                         |   0|    3|   0.0| 6.39e-01|NA  |
|Smoking                   |Current                                    |   8|  153|   5.0| 3.47e-01|NA  |
|Smoking                   |Former                                     | 100|  957|   9.5| 7.39e-04|**  |
|Smoking                   |Never                                      |  67|  952|   6.6| 7.37e-01|NA  |
|Smoking                   |Unknown                                    |   1|   42|   2.3| 2.41e-01|NA  |
|Smoking                   |NA                                         |   5|  361|   1.4| 3.34e-05|**  |

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
|No                   |AgeInYears.x | 56.83|  16.69| 1448|
|Yes                  |AgeInYears.x | 57.22|  17.15| 1198|
|No                   |BMI          | 29.09|   8.91| 1448|
|Yes                  |BMI          | 35.80| 204.94| 1198|
|No                   |MaxStay      |  9.23|  22.97| 1448|
|Yes                  |MaxStay      | 11.63|  17.19| 1198|

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
|                     0.541|            0.555|                1e-99|

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



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by ER visit survival.  Overall 45.28 % visited ER

|Type                      |Group                                      |   No|  Yes|  Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|----:|----:|----:|--------:|:---|
|30d Survival              |Surival Past 30 Days                       |  400|  445| 52.7| 1.60e-05|**  |
|30d Survival              |Within 30 Days                             |   36|   73| 67.0| 5.35e-06|**  |
|30d Survival              |NA                                         | 1012|  680| 40.2| 2.63e-05|**  |
|60d Survival              |Surival Past 60 Days                       |  368|  405| 52.4| 7.02e-05|**  |
|60d Survival              |Within 60 Days                             |   68|  113| 62.4| 3.54e-06|**  |
|60d Survival              |NA                                         | 1012|  680| 40.2| 2.63e-05|**  |
|Ancestry                  |AFR                                        |   50|   64| 56.1| 1.98e-02|*   |
|Ancestry                  |AMR                                        |    5|    3| 37.5| 6.59e-01|NA  |
|Ancestry                  |CSA                                        |    8|    5| 38.5| 6.22e-01|NA  |
|Ancestry                  |EAS                                        |    6|    5| 45.5| 9.91e-01|NA  |
|Ancestry                  |EUR                                        |  832|  662| 44.3| 4.54e-01|NA  |
|Ancestry                  |WAS                                        |   11|    9| 45.0| 9.80e-01|NA  |
|Ancestry                  |NA                                         |  536|  450| 45.6| 8.19e-01|NA  |
|Death                     |Alive                                      |  436|  518| 54.3| 2.17e-08|**  |
|Death                     |Deceased                                   | 1012|  680| 40.2| 2.63e-05|**  |
|Ethnicity                 |Hispanic or Latino                         |   37|   26| 41.3| 5.23e-01|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 1375| 1156| 45.7| 6.88e-01|NA  |
|Ethnicity                 |Patient Refused                            |    7|    6| 46.2| 9.49e-01|NA  |
|Ethnicity                 |Unknown                                    |   28|   10| 26.3| 1.89e-02|*   |
|Ethnicity                 |NA                                         |    1|    0|  0.0| 3.63e-01|NA  |
|Gender                    |F                                          |  732|  514| 41.3| 4.32e-03|**  |
|Gender                    |M                                          |  716|  684| 48.9| 7.10e-03|**  |
|Pneumonia Type            |Bacterial                                  | 1448| 1198| 45.3| 1.00e+00|NA  |
|Prior COPD                |Yes                                        |   42|   39| 48.1| 6.04e-01|NA  |
|Prior COPD                |NA                                         | 1406| 1159| 45.2| 9.26e-01|NA  |
|Prior Cardiac Arrhythmias |Yes                                        |  384|  362| 48.5| 7.46e-02|NA  |
|Prior Cardiac Arrhythmias |NA                                         | 1064|  836| 44.0| 2.64e-01|NA  |
|Prior Obesity             |Yes                                        |  285|  242| 45.9| 7.66e-01|NA  |
|Prior Obesity             |NA                                         | 1163|  956| 45.1| 8.82e-01|NA  |
|PriorDiabetes             |Yes                                        |  266|  252| 48.6| 1.23e-01|NA  |
|PriorDiabetes             |NA                                         | 1182|  946| 44.5| 4.47e-01|NA  |
|PriorHypertension         |Yes                                        |  468|  400| 46.1| 6.33e-01|NA  |
|PriorHypertension         |NA                                         |  980|  798| 44.9| 7.39e-01|NA  |
|Race                      |African American                           |  108|  134| 55.4| 1.60e-03|**  |
|Race                      |American Indian                            |    8|    8| 50.0| 7.04e-01|NA  |
|Race                      |Asian                                      |   24|   13| 35.1| 2.15e-01|NA  |
|Race                      |Caucasian                                  | 1272| 1020| 44.5| 4.57e-01|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |    1|    2| 66.7| 4.57e-01|NA  |
|Race                      |Other                                      |   23|   16| 41.0| 5.94e-01|NA  |
|Race                      |Patient Refused                            |    2|    2| 50.0| 8.49e-01|NA  |
|Race                      |Unknown                                    |    8|    2| 20.0| 1.08e-01|NA  |
|Race                      |NA                                         |    2|    1| 33.3| 6.78e-01|NA  |
|Smoking                   |Current                                    |   78|   83| 51.6| 1.10e-01|NA  |
|Smoking                   |Former                                     |  547|  510| 48.2| 5.21e-02|NA  |
|Smoking                   |Never                                      |  553|  466| 45.7| 7.70e-01|NA  |
|Smoking                   |Unknown                                    |   26|   17| 39.5| 4.49e-01|NA  |
|Smoking                   |NA                                         |  244|  122| 33.3| 4.43e-06|**  |

# By Longer than Exoected Stay

The median stay was 3.11.  We set the a longer than average stay to be **3 days**.


``` r
combined.data <- 
  combined.data %>%
  mutate(LongStay=case_when(MaxStay>=3~"Yes",
                                MaxStay<3~"No"))

quant.demo <- 
  combined.data %>%
  group_by(LongStay) %>%
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

|LongStay |Variable     |   mean|      sd|    n|
|:--------|:------------|------:|-------:|----:|
|No       |AgeInYears.x | 56.586|  17.177| 1289|
|Yes      |AgeInYears.x | 57.404|  16.621| 1357|
|No       |BMI          | 29.253|   7.518| 1289|
|Yes      |BMI          | 34.778| 190.680| 1357|
|No       |MaxStay      |  0.447|   0.829| 1289|
|Yes      |MaxStay      | 19.697|  25.400| 1357|

``` r
# wilcoxon tests, not normally distributed

quant.t.tests <-
  combined.data %>%
  summarize(across(.cols=c('AgeInYears.x','BMI'),
                   .fns=list(Mann.Whitney=~wilcox.test(.~LongStay)$p.value)))

kable(quant.t.tests,captionn="Mann-Whitney tests for ER visit",digits=c(99,99,99))
```



| AgeInYears.x_Mann.Whitney| BMI_Mann.Whitney|
|-------------------------:|----------------:|
|                      0.32|             0.27|

``` r
ances.demo.ql <- 
  combined.data %>%
  group_by(MajorityAncestry,LongStay) %>%
  mutate(Type="Ancestry") %>%
  rename("Group"="MajorityAncestry") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ancestry")

type.demo.ql <- 
  combined.data %>%
  group_by(Type,LongStay) %>%
  rename("Group"="Type") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Pneumonia Type")

emerg.demo.ql <- 
  combined.data %>%
  group_by(EmergencyVisit.Group,LongStay) %>%
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
  group_by(GenderCode,LongStay) %>%
  rename("Group"="GenderCode") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Gender")

race.demo.ql <- 
combined.data %>%
  group_by(RaceName,LongStay) %>%
  count %>%
  ungroup %>%
  rename("Group"="RaceName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Race")

ethnicity.demo.ql <- 
combined.data %>%
  group_by(EthnicityName,LongStay) %>%
  count %>%
  ungroup %>%
  rename("Group"="EthnicityName") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Ethnicity")

diabetes.demo.ql <- 
  combined.data %>%
  group_by(PriorDiabetes,LongStay) %>%
  count %>%
  ungroup %>%
  rename("Group"="PriorDiabetes") %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorDiabetes") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

smoking.demo.ql <- 
  combined.data %>%
  group_by(SmokingStatusMapped,LongStay) %>%
  #mutate(Type="Smoking") %>%
  rename("Group"="SmokingStatusMapped") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Smoking") 

#update to hypertension first
hypertension.demo.ql <- 
  combined.data %>%
  group_by(PriorHypertension,LongStay) %>%
  rename("Group"="PriorHypertension") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="PriorHypertension") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

arrythmia.demo.ql <- 
  combined.data %>%
  group_by(PriorCardiacArrhythmias,LongStay) %>%
  rename("Group"="PriorCardiacArrhythmias") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Cardiac Arrhythmias") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

obesity.demo.ql <- 
  combined.data %>%
  group_by(PriorObesity,LongStay) %>%
  rename("Group"="PriorObesity") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior Obesity") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

copd.demo.ql <- 
  combined.data %>%
  group_by(PriorCOPD,LongStay) %>%
  rename("Group"="PriorCOPD") %>%
  count %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Prior COPD") %>%
  mutate(Group=case_when(Group==1~"Yes",
                         Group==2~"No"))

death.demo.ql <- 
  combined.data %>%
  group_by(is.na(DeID_DeathIndexDeceasedDate),LongStay) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="Death")  %>%
  mutate(Group=case_when(Group==TRUE~"Deceased",
                         Group==FALSE~"Alive"))

surv30d.demo.ql <- 
  combined.data %>%
  group_by(EmergencyVisit.Group,LongStay) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="30d Survival") 

surv60d.demo.ql <- 
  combined.data %>%
  group_by(Survival.60day,LongStay) %>%
  count() %>%
  rename("Group"=1) %>%
  ungroup %>%
  mutate(Pct=n/sum(n)*100) %>%
  mutate(Type="60d Survival") 

total.survival <- combined.data %>% group_by(LongStay) %>% count() %>% pull(n) 

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
  select(LongStay,Type,Group,n,Pct) %>%
  ungroup %>%
  select(-Pct) %>%
  pivot_wider(names_from = LongStay,values_from = n, values_fn = sum) %>%
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

|Type                      |Group                                      |   No|  Yes|   Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|----:|----:|-----:|--------:|:---|
|30d Survival              |No                                         |  977|  471|  32.5| 2.92e-46|**  |
|30d Survival              |Yes                                        |  312|  886|  74.0| 1.53e-55|**  |
|60d Survival              |Surival Past 60 Days                       |  251|  522|  67.5| 1.63e-19|**  |
|60d Survival              |Within 60 Days                             |   26|  155|  85.6| 2.33e-20|**  |
|60d Survival              |NA                                         | 1012|  680|  40.2| 6.77e-20|**  |
|Ancestry                  |AFR                                        |   49|   65|  57.0| 2.21e-01|NA  |
|Ancestry                  |AMR                                        |    4|    4|  50.0| 9.42e-01|NA  |
|Ancestry                  |CSA                                        |    7|    6|  46.2| 7.11e-01|NA  |
|Ancestry                  |EAS                                        |    7|    4|  36.4| 3.22e-01|NA  |
|Ancestry                  |EUR                                        |  735|  759|  50.8| 7.09e-01|NA  |
|Ancestry                  |WAS                                        |   11|    9|  45.0| 5.74e-01|NA  |
|Ancestry                  |NA                                         |  476|  510|  51.7| 7.83e-01|NA  |
|Death                     |Alive                                      |  277|  677|  71.0| 5.03e-34|**  |
|Death                     |Deceased                                   | 1012|  680|  40.2| 6.77e-20|**  |
|Emergency Visit           |NA                                         | 1289| 1357|  51.3| 1.00e+00|NA  |
|Ethnicity                 |Hispanic or Latino                         |   36|   27|  42.9| 1.81e-01|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 1223| 1308|  51.7| 6.92e-01|NA  |
|Ethnicity                 |Patient Refused                            |    9|    4|  30.8| 1.39e-01|NA  |
|Ethnicity                 |Unknown                                    |   20|   18|  47.4| 6.29e-01|NA  |
|Ethnicity                 |NA                                         |    1|    0|   0.0| 3.05e-01|NA  |
|Gender                    |F                                          |  704|  542|  43.5| 3.83e-08|**  |
|Gender                    |M                                          |  585|  815|  58.2| 2.14e-07|**  |
|Pneumonia Type            |Bacterial                                  | 1289| 1357|  51.3| 1.00e+00|NA  |
|Prior COPD                |Yes                                        |   32|   49|  60.5| 9.73e-02|NA  |
|Prior COPD                |NA                                         | 1257| 1308|  51.0| 7.68e-01|NA  |
|Prior Cardiac Arrhythmias |Yes                                        |  273|  473|  63.4| 3.52e-11|**  |
|Prior Cardiac Arrhythmias |NA                                         | 1016|  884|  46.5| 3.33e-05|**  |
|Prior Obesity             |Yes                                        |  240|  287|  54.5| 1.45e-01|NA  |
|Prior Obesity             |NA                                         | 1049| 1070|  50.5| 4.67e-01|NA  |
|PriorDiabetes             |Yes                                        |  197|  321|  62.0| 1.14e-06|**  |
|PriorDiabetes             |NA                                         | 1092| 1036|  48.7| 1.64e-02|*   |
|PriorHypertension         |Yes                                        |  352|  516|  59.4| 1.50e-06|**  |
|PriorHypertension         |NA                                         |  937|  841|  47.3| 7.75e-04|**  |
|Race                      |African American                           |   98|  144|  59.5| 1.05e-02|*   |
|Race                      |American Indian                            |    7|    9|  56.2| 6.91e-01|NA  |
|Race                      |Asian                                      |   25|   12|  32.4| 2.18e-02|*   |
|Race                      |Caucasian                                  | 1129| 1163|  50.7| 6.03e-01|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |    1|    2|  66.7| 5.94e-01|NA  |
|Race                      |Other                                      |   22|   17|  43.6| 3.36e-01|NA  |
|Race                      |Patient Refused                            |    0|    4| 100.0| 5.13e-02|NA  |
|Race                      |Unknown                                    |    5|    5|  50.0| 9.35e-01|NA  |
|Race                      |NA                                         |    2|    1|  33.3| 5.34e-01|NA  |
|Smoking                   |Current                                    |   74|   87|  54.0| 4.85e-01|NA  |
|Smoking                   |Former                                     |  454|  603|  57.0| 1.78e-04|**  |
|Smoking                   |Never                                      |  530|  489|  48.0| 3.53e-02|*   |
|Smoking                   |Unknown                                    |    4|   39|  90.7| 2.33e-07|**  |
|Smoking                   |NA                                         |  227|  139|  38.0| 3.52e-07|**  |



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
|No           |AgeInYears.x | 57.34|  17.13| 1862|
|Yes          |AgeInYears.x | 56.22|  16.30|  784|
|No           |BMI          | 28.94|   9.44| 1862|
|Yes          |BMI          | 39.58| 252.33|  784|
|No           |MaxStay      |  4.02|   7.54| 1862|
|Yes          |MaxStay      | 25.29|  31.27|  784|

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
|                    0.0503|          0.00762|

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



Table: Summary of discrete variables for bacterial pneumonia cases, stratified by ventillation status.  Overall 29.63 % required ventillation

|Type                      |Group                                      |   No| Yes|  Pct|  Chisq.p|Sig |
|:-------------------------|:------------------------------------------|----:|---:|----:|--------:|:---|
|30d Survival              |No                                         | 1087| 361| 24.9| 9.02e-05|**  |
|30d Survival              |Yes                                        |  775| 423| 35.3| 1.67e-05|**  |
|60d Survival              |Surival Past 60 Days                       |  450| 323| 41.8| 1.35e-13|**  |
|60d Survival              |Within 60 Days                             |   73| 108| 59.7| 8.72e-19|**  |
|60d Survival              |NA                                         | 1339| 353| 20.9| 2.85e-15|**  |
|Ancestry                  |AFR                                        |   77|  37| 32.5| 5.09e-01|NA  |
|Ancestry                  |AMR                                        |    5|   3| 37.5| 6.26e-01|NA  |
|Ancestry                  |CSA                                        |   10|   3| 23.1| 6.05e-01|NA  |
|Ancestry                  |EAS                                        |    9|   2| 18.2| 4.06e-01|NA  |
|Ancestry                  |EUR                                        | 1037| 457| 30.6| 4.17e-01|NA  |
|Ancestry                  |WAS                                        |   16|   4| 20.0| 3.46e-01|NA  |
|Ancestry                  |NA                                         |  708| 278| 28.2| 3.24e-01|NA  |
|Death                     |Alive                                      |  523| 431| 45.2| 7.19e-26|**  |
|Death                     |Deceased                                   | 1339| 353| 20.9| 2.85e-15|**  |
|Emergency Visit           |NA                                         | 1862| 784| 29.6| 1.00e+00|NA  |
|Ethnicity                 |Hispanic or Latino                         |   46|  17| 27.0| 6.46e-01|NA  |
|Ethnicity                 |Non-Hispanic or Latino                     | 1775| 756| 29.9| 7.91e-01|NA  |
|Ethnicity                 |Patient Refused                            |    9|   4| 30.8| 9.28e-01|NA  |
|Ethnicity                 |Unknown                                    |   31|   7| 18.4| 1.30e-01|NA  |
|Ethnicity                 |NA                                         |    1|   0|  0.0| 5.16e-01|NA  |
|Gender                    |F                                          |  970| 276| 22.2| 7.41e-09|**  |
|Gender                    |M                                          |  892| 508| 36.3| 4.92e-08|**  |
|Pneumonia Type            |Bacterial                                  | 1862| 784| 29.6| 1.00e+00|NA  |
|Prior COPD                |Yes                                        |   42|  39| 48.1| 2.62e-04|**  |
|Prior COPD                |NA                                         | 1820| 745| 29.0| 5.17e-01|NA  |
|Prior Cardiac Arrhythmias |Yes                                        |  421| 325| 43.6| 7.69e-17|**  |
|Prior Cardiac Arrhythmias |NA                                         | 1441| 459| 24.2| 1.76e-07|**  |
|Prior Obesity             |Yes                                        |  340| 187| 35.5| 3.25e-03|**  |
|Prior Obesity             |NA                                         | 1522| 597| 28.2| 1.42e-01|NA  |
|PriorDiabetes             |Yes                                        |  286| 232| 44.8| 4.18e-14|**  |
|PriorDiabetes             |NA                                         | 1576| 552| 25.9| 1.93e-04|**  |
|PriorHypertension         |Yes                                        |  523| 345| 39.7| 6.69e-11|**  |
|PriorHypertension         |NA                                         | 1339| 439| 24.7| 5.10e-06|**  |
|Race                      |African American                           |  160|  82| 33.9| 1.47e-01|NA  |
|Race                      |American Indian                            |   10|   6| 37.5| 4.91e-01|NA  |
|Race                      |Asian                                      |   29|   8| 21.6| 2.86e-01|NA  |
|Race                      |Caucasian                                  | 1622| 670| 29.2| 6.77e-01|NA  |
|Race                      |Native Hawaiian and Other Pacific Islander |    1|   2| 66.7| 1.60e-01|NA  |
|Race                      |Other                                      |   28|  11| 28.2| 8.46e-01|NA  |
|Race                      |Patient Refused                            |    2|   2| 50.0| 3.72e-01|NA  |
|Race                      |Unknown                                    |    7|   3| 30.0| 9.80e-01|NA  |
|Race                      |NA                                         |    3|   0|  0.0| 2.61e-01|NA  |
|Smoking                   |Current                                    |  103|  58| 36.0| 7.56e-02|NA  |
|Smoking                   |Former                                     |  708| 349| 33.0| 1.58e-02|*   |
|Smoking                   |Never                                      |  749| 270| 26.5| 2.85e-02|*   |
|Smoking                   |Unknown                                    |   10|  33| 76.7| 1.32e-11|**  |
|Smoking                   |NA                                         |  292|  74| 20.2| 8.05e-05|**  |


``` r
genetics_outfile <- '../Case_Control_After_Bacterial_Pneumonia.csv'

combined.data %>%
  rename('Age'='AgeInYears.x') %>%
  mutate(AgeOver65 = case_when(Age>=65~1,
                               Age<65~0)) %>%
  mutate(COPD.History = case_when(COPD==1~1,
                                  .default=0)) %>% 
  select(DeID_PatientID,
         Age,AgeOver65,GenderCode,BMI,
         RaceName,EthnicityName,
         SmokingStatusMapped,COPD.History,Diabetes,
         Survival.30day, Survival.60day, EmergencyVisit.Group, Ventillation,LongStay,
         SubType) %>% 
  write_csv(genetics_outfile)
```



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
## [1] lubridate_1.9.3 tidyr_1.3.1     dplyr_1.1.4     readr_2.1.5    
## [5] knitr_1.48     
## 
## loaded via a namespace (and not attached):
##  [1] bit_4.0.5         jsonlite_1.8.8    compiler_4.4.3    crayon_1.5.3     
##  [5] tidyselect_1.2.1  parallel_4.4.3    jquerylib_0.1.4   yaml_2.3.9       
##  [9] fastmap_1.2.0     R6_2.5.1          generics_0.1.3    tibble_3.2.1     
## [13] bslib_0.7.0       pillar_1.9.0      tzdb_0.4.0        rlang_1.1.4      
## [17] utf8_1.2.4        cachem_1.1.0      xfun_0.45         sass_0.4.9       
## [21] bit64_4.0.5       timechange_0.3.0  cli_3.6.3         withr_3.0.0      
## [25] magrittr_2.0.3    digest_0.6.36     vroom_1.6.5       hms_1.1.3        
## [29] lifecycle_1.0.4   vctrs_0.6.5       evaluate_0.24.0   glue_1.8.0       
## [33] fansi_1.0.6       rmarkdown_2.27    purrr_1.0.2       tools_4.4.3      
## [37] pkgconfig_2.0.3   htmltools_0.5.8.1
```
