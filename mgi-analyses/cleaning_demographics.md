---
title: "Cleaning and merging the demographic data"
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

To clean the dempgraphic datafiles, to get a per-patient demographic profile. This script can be found in /nfs/turbo/precision-health/DataDirect/HUM00229632 - Genome-wide associations of bacteria and was most recently run on Fri Apr 19 08:15:08 2024.


```r
library(knitr)
#figures made will go to directory called figures, will make them as both png and pdf files 
opts_chunk$set(fig.path='figures-demographics/',
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
demographic.datafile <- 'DemographicInfo.csv'
gis.datafile <- 'GisNeighborhoodAffluence.csv'
ancestry.datafile <- 'MGI_Ancestry.csv'
social.datafile <- 'ClaritySocialHistory.csv'
```

Combining demographic files


```r
demo.gis.data <- full_join(read_csv(demographic.datafile),read_csv(gis.datafile), by="DeID_PatientID")
demo.gis.anc.data <- full_join(demo.gis.data,read_csv(ancestry.datafile), by="DeID_PatientID")

social.data <- read_csv(social.datafile) %>% #encounter level data
  group_by(DeID_PatientID) %>%
  summarize(SmokingStatusMapped = last(na.omit(SmokingStatusMapped)), #most recent
            AlcoholUseStatusMapped = last(na.omit(AlcoholUseStatusMapped))) #most recent
demo.gis.anc.social.data <- full_join(demo.gis.anc.data, social.data, by="DeID_PatientID")

combined.data <-
  demo.gis.anc.social.data %>%
  select(DeID_PatientID,GenderCode,RaceName,EthnicityName,MajorityAncestry,DeID_DeathIndexDeceasedDate,affluence13_17_qrtl,disadvantage13_17_qrtl,ethnicimmigrant13_17_qrtl,ped1_13_17_qrtl,
         SmokingStatusMapped,AlcoholUseStatusMapped)

demographics.filename <- 'DemographicDataClean.csv'
write_csv(combined.data, demographics.filename)
```

Wrote this out to DemographicDataClean.csv.

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
## [25] magrittr_2.0.3   digest_0.6.33    vroom_1.6.3      hms_1.1.3       
## [29] lifecycle_1.0.3  vctrs_0.6.3      evaluate_0.21    glue_1.6.2      
## [33] fansi_1.0.4      rmarkdown_2.25   purrr_1.0.2      tools_4.3.1     
## [37] pkgconfig_2.0.3  htmltools_0.5.6
```
