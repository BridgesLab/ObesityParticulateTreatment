# Analysis of Fasted Liver Triglycerides from High Fat Diet Particulate Treatment Study
Kathryn Cyrus, Matt Peloquin, Jyothi Parvathareddy, Sridhar Jaligama, Stephania Cormier and Dave Bridges  
November 13, 2014  

This only looks at animals treated *in utero* and these values are from mice which were fasted 16-20h.  These data were most recently updated on Sun Nov 16 12:03:53 2014.


```r
filename <- '../data/Liver Triglycerides.xlsx'
worksheet <- 'Summary'

library(xlsx)
```

```
## Loading required package: rJava
## Loading required package: xlsxjars
```

```r
data <- read.xlsx2(filename, sheetName=worksheet)
data <- subset(data, Mouse.ID != '206')
data$TG <- as.numeric(as.character(data$TGs..microgram.milligrams.of.tissue.))

library(plyr)
summary <- ddply(data, ~Treatment, summarize,
                 mean = mean(TG),
                 sd = sd(TG),
                 se = sd(TG)/sqrt(length(TG)),
                 n = length(TG),
                 shapiro = shapiro.test(TG)$p.value)

library(car)
levene.result <- leveneTest(TG~Treatment, data=data)
```

The data is located in the file ../data/Liver Triglycerides.xlsx on a worksheet named Summary.  These data are summarized in the Table below and graphed in the Figure.  



```r
library(xtable)
print(xtable(summary, caption = "Summary Data, based on treating mice individually.  Triglycerides are in mmoles/mg of tissue", label="tab:summary-statistics"), include.rownames=F, type='html')
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sun Nov 16 12:03:55 2014 -->
<table border=1>
<caption align="bottom"> Summary Data, based on treating mice individually.  Triglycerides are in mmoles/mg of tissue </caption>
<tr> <th> Treatment </th> <th> mean </th> <th> sd </th> <th> se </th> <th> n </th> <th> shapiro </th>  </tr>
  <tr> <td> Saline </td> <td align="right"> 3.57 </td> <td align="right"> 1.27 </td> <td align="right"> 0.34 </td> <td align="right">  14 </td> <td align="right"> 0.77 </td> </tr>
  <tr> <td> Treatment </td> <td align="right"> 4.19 </td> <td align="right"> 0.90 </td> <td align="right"> 0.29 </td> <td align="right">  10 </td> <td align="right"> 0.37 </td> </tr>
   <a name=tab:summary-statistics></a>
</table>

According to a Shapiro-Wilk Test, the data fit a normal distribution (p>0.3707028).  A Levene's test suggested that the variance can be presumed to be equal (p=0.285091).  Based on this, a Student's T-test has a p-value of 0.2023664.


```r
ymax <- max(summary$mean) + max(summary$se)

plot <- barplot(summary$mean,
                   beside=T,
                   las=1,
                   ylab ="Liver Triglycerides (ug/mg tissue)",
                   names.arg = summary$Treatment,
                   ylim = c(0,ymax))

superpose.eb <- function (x, y, ebl, ebu = ebl, length = 0.08, ...)
  arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3,
  length = length, ...)

superpose.eb(plot, summary$mean, summary$se)
```

![](liver_triglycerides_files/figure-html/barplot-individual-1.png) 

## Session Information

```r
sessionInfo()
```

```
## R version 3.1.1 (2014-07-10)
## Platform: x86_64-apple-darwin13.1.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] xtable_1.7-4   car_2.0-21     plyr_1.8.1     xlsx_0.5.7    
## [5] xlsxjars_0.6.1 rJava_0.9-6   
## 
## loaded via a namespace (and not attached):
##  [1] digest_0.6.4     evaluate_0.5.5   formatR_1.0      htmltools_0.2.6 
##  [5] knitr_1.8        MASS_7.3-35      nnet_7.3-8       Rcpp_0.11.3     
##  [9] rmarkdown_0.3.10 stringr_0.6.2    tools_3.1.1      yaml_2.1.13
```


\end{document}
