# Analysis of Body Composition for High Fat Diet Particulate Treatment Study
Alyse Ragauskas, Matt Peloquin, Jyothi Parvathareddy, Sridhar Jaligama, Stephania Cormier and Dave Bridges  
November 13, 2014  

This only looks at animals treated *in utero*.  These data were most recently updated on Sun Nov 16 13:21:31 2014.


```r
output_file_maternal <- "../data/Raw Weights - Maternal.csv"

data.maternal <- read.csv(output_file_maternal, row.names="X")
#removed sickly, weight losing mouse 206
data.maternal <- subset(data.maternal, animal.MouseID !=206)
data.maternal$animal.MouseID <- as.factor(data.maternal$animal.MouseID)

#find which experiments we did fat masss
fat.mass.experiments <- droplevels(subset(data.maternal, assay.assay=="Total Fat Mass"))$experiment.date

data <- subset(data.maternal, experiment.date %in% fat.mass.experiments)
data$animal.Cage <- as.factor(data$animal.Cage)
data$age.group <- cut(data$age, breaks=2)
data$Treatment <- relevel(data$Treatment, ref='Saline')
library(reshape2)
composition.data <- dcast(data, 
                          animal.MouseID+Treatment+animal.Cage+age~assay.assay, value.var="values", 
                          fun.aggregate = function(x) mean(x)/1000)
#remove whitespace
colnames(composition.data) <- gsub(" ", ".", colnames(composition.data))
composition.data$Fat.Pct <- composition.data$Total.Fat.Mass/composition.data$Body.Weight*100
composition.data$Lean.Pct <- composition.data$Lean.Mass/composition.data$Body.Weight*100
```


## Animals Reared from Particulate Treated Mothers


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
se <- function(x) sd(x, na.rm=T)/sqrt(length(x))
                             
fat.mass.summary <- subset(composition.data, age>145) %>%
  group_by(Treatment) %>%
  distinct(animal.MouseID) %>%
  summarise(mean = mean(Total.Fat.Mass),
            se = se(Total.Fat.Mass ),
            sd = sd(Total.Fat.Mass ),
            rel.sd = sd(Total.Fat.Mass)/mean(Total.Fat.Mass)*100,
            n = length(Total.Fat.Mass),
            shapiro = shapiro.test(Total.Fat.Mass)$p.value)

fat.pct.summary <- subset(composition.data, age>145) %>%
  group_by(Treatment) %>%
  distinct(animal.MouseID) %>%
  summarise(mean = mean(Fat.Pct),
            se = se(Fat.Pct),
            sd = sd(Fat.Pct),
            rel.sd = sd(Fat.Pct)/mean(Fat.Pct)*100,
            n = length(Fat.Pct),
            shapiro = shapiro.test(Fat.Pct)$p.value)

lean.mass.summary <- subset(composition.data, age>145) %>%
  group_by(Treatment) %>%
  distinct(animal.MouseID) %>%
  summarise(mean = mean(Lean.Mass),
            se = se(Lean.Mass),
            sd = sd(Lean.Mass),
            rel.sd = sd(Lean.Mass)/mean(Lean.Mass)*100,
            n = length(Lean.Mass),
            shapiro = shapiro.test(Lean.Mass)$p.value)

lean.pct.summary <- subset(composition.data, age>145) %>%
  group_by(Treatment) %>%
  distinct(animal.MouseID) %>%
  summarise(mean = mean(Lean.Pct),
            se = se(Lean.Pct),
            sd = sd(Lean.Pct),
            rel.sd = sd(Lean.Pct)/mean(Lean.Pct)*100,
            n = length(Lean.Pct),
            shapiro = shapiro.test(Lean.Pct)$p.value)
library(car)
```

# After 12 Weeks of High Fat Diet


```r
library(xtable)
print(xtable(as.data.frame(fat.pct.summary), label="tab:summary-fat-pct", caption="Data for Percent Fat", digits=3), type='html')
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sun Nov 16 13:21:32 2014 -->
<table border=1>
<caption align="bottom"> Data for Percent Fat </caption>
<tr> <th>  </th> <th> Treatment </th> <th> mean </th> <th> se </th> <th> sd </th> <th> rel.sd </th> <th> n </th> <th> shapiro </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> Saline </td> <td align="right"> 40.977 </td> <td align="right"> 0.332 </td> <td align="right"> 1.241 </td> <td align="right"> 3.029 </td> <td align="right">   14 </td> <td align="right"> 0.615 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> MCP230 </td> <td align="right"> 40.948 </td> <td align="right"> 0.676 </td> <td align="right"> 2.137 </td> <td align="right"> 5.218 </td> <td align="right">   10 </td> <td align="right"> 0.787 </td> </tr>
   <a name=tab:summary-fat-pct></a>
</table>

```r
print(xtable(as.data.frame(fat.mass.summary), label="tab:summary-fat-mass", caption="Data for Total Fat", digits=3), type='html')
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sun Nov 16 13:21:32 2014 -->
<table border=1>
<caption align="bottom"> Data for Total Fat </caption>
<tr> <th>  </th> <th> Treatment </th> <th> mean </th> <th> se </th> <th> sd </th> <th> rel.sd </th> <th> n </th> <th> shapiro </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> Saline </td> <td align="right"> 17.955 </td> <td align="right"> 0.396 </td> <td align="right"> 1.480 </td> <td align="right"> 8.245 </td> <td align="right">   14 </td> <td align="right"> 0.854 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> MCP230 </td> <td align="right"> 19.860 </td> <td align="right"> 0.654 </td> <td align="right"> 2.068 </td> <td align="right"> 10.412 </td> <td align="right">   10 </td> <td align="right"> 0.611 </td> </tr>
   <a name=tab:summary-fat-mass></a>
</table>

```r
print(xtable(as.data.frame(lean.mass.summary), label="tab:summary-lean-mass", caption="Data for Total Lean Mass", digits=3), type='html')
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sun Nov 16 13:21:32 2014 -->
<table border=1>
<caption align="bottom"> Data for Total Lean Mass </caption>
<tr> <th>  </th> <th> Treatment </th> <th> mean </th> <th> se </th> <th> sd </th> <th> rel.sd </th> <th> n </th> <th> shapiro </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> Saline </td> <td align="right"> 24.435 </td> <td align="right"> 0.433 </td> <td align="right"> 1.621 </td> <td align="right"> 6.636 </td> <td align="right">   14 </td> <td align="right"> 0.376 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> MCP230 </td> <td align="right"> 27.065 </td> <td align="right"> 0.358 </td> <td align="right"> 1.131 </td> <td align="right"> 4.179 </td> <td align="right">   10 </td> <td align="right"> 0.363 </td> </tr>
   <a name=tab:summary-lean-mass></a>
</table>

## Total Fat Mass


```r
ymax = max(fat.mass.summary$mean + fat.mass.summary$se)
plot <- with(fat.mass.summary, barplot(mean,
                   las=1,
                   ylab ="Total Fat Mass (g)",
                   names.arg=Treatment,
                   ylim = c(0,ymax)))

superpose.eb <- function (x, y, ebl, ebu = ebl, length = 0.08, ...)
  arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3,
  length = length, ...)

superpose.eb(plot, fat.mass.summary$mean, fat.mass.summary$se)
```

![](body_composition_analysis_files/figure-html/fat-mass-barplot-1.png) 

```r
unique.composition.data <- distinct(subset(composition.data, age>145), animal.MouseID)
```

The data were normally distributed (p>0.6111494) and had equal variance via a Levene's test (p=0.1816593).  Therefore via a Student's *t*-test, the p-value was 0.0150579.  There was a 10.609858% increase in fat mass.

# Total Lean Mass


```r
ymax = max(lean.mass.summary$mean + lean.mass.summary$se)
plot <- with(lean.mass.summary, barplot(mean,
                   las=1,
                   ylab ="Total Lean Mass (g)",
                   names.arg=Treatment,
                   ylim = c(0,ymax)))
superpose.eb(plot, lean.mass.summary$mean, lean.mass.summary$se)
```

![](body_composition_analysis_files/figure-html/lean-mass-barplot-1.png) 

The data were normally distributed (p>0.3627422) and had equal variance via a Levene's test (p=0.3664842).  Therefore via a Student's *t*-test, the p-value was 2.2278298\times 10^{-4}.  There was a 10.7632494% increase in lean mass.

# Percent Fat Mass

```r
ymax = max(fat.pct.summary$mean + fat.pct.summary$se)
plot <- with(fat.pct.summary, barplot(mean,
                   las=1,
                   ylab ="Percent Body Fat",
                   names.arg=Treatment,
                   ylim = c(0,ymax)))

superpose.eb(plot, fat.pct.summary$mean, fat.pct.summary$se)
```

![](body_composition_analysis_files/figure-html/fat-pct-barplot-1.png) 


The data were normally distributed (p>0.6152608) and had **unequal variance** via a Levene's test (p=0.0241466).  Therefore via a Welch's *t*-test, the p-value was 0.9699659.

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
## [1] xtable_1.7-4  car_2.0-21    dplyr_0.3.0.2 reshape2_1.4 
## 
## loaded via a namespace (and not attached):
##  [1] assertthat_0.1   DBI_0.3.1        digest_0.6.4     evaluate_0.5.5  
##  [5] formatR_1.0      htmltools_0.2.6  knitr_1.8        lazyeval_0.1.9  
##  [9] magrittr_1.0.1   MASS_7.3-35      nnet_7.3-8       parallel_3.1.1  
## [13] plyr_1.8.1       Rcpp_0.11.3      rmarkdown_0.3.10 stringr_0.6.2   
## [17] tools_3.1.1      yaml_2.1.13
```
