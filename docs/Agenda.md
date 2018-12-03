---
title: "Agenda"
author: "Yohan Min"
date: "November 30, 2018"
output:
  html_document:
    keep_md: yes
    preserve_yaml: yes
    toc: yes
    toc_float: yes
  pdf_document:
    toc: yes
  word_document:
    toc: yes
---


## Risk per full-time workers
![Top 3 risks are related to solar installation on the roof](./Figs/PTD.jpg)

## Ideal design for safety from the interviews
* Roof pitch: lower than 5/12 – 7/12 `to work easy - fall`
* Roof material: composition `not to be slippery - fall`
* Roof structure: no obstruction `not to be interrupted - trip, complexity`
* Roof condition: accessories pre-installed `to reduce the scope - complexity`
* Anchor point: pre-installed `to be efficient - fall`
* Access: low height, `enough space for easier access - fall`
* Additional: setback, snow guard, guardrail `fall`
* Electrical: micro inverter, conduit pre-run, reserving spaces, `electric shock, complexity`


## Solar installation trend in Seattle



<img src="Agenda_files/figure-html/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

## Solar installation trend by contractors

<img src="Agenda_files/figure-html/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />


## Cumulative solar installation per census track

<img src="Agenda_files/figure-html/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />


## Residential household in Seattle

<img src="Agenda_files/figure-html/unnamed-chunk-4-1.svg" style="display: block; margin: auto;" />


## Residential solar potential (MWh) in Seattle

<img src="Agenda_files/figure-html/unnamed-chunk-5-1.svg" style="display: block; margin: auto;" />

## Residential solar potential (MWh/ household) in Seattle

<img src="Agenda_files/figure-html/unnamed-chunk-6-1.svg" style="display: block; margin: auto;" />


## Histograms of multiple variables

<img src="Agenda_files/figure-html/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

## Boxplot for overall potential solar

<img src="Agenda_files/figure-html/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />


## Cor plot

<img src="Agenda_files/figure-html/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />


## Regression


```
##
## Call:
## lm(formula = sol_instl ~ hu_med_val + hu_ex_1000, data = regr[-c(1)])
##
## Residuals:
##     Min      1Q  Median      3Q     Max
## -8.4964 -1.8188 -0.4623  1.1230 16.2559
##
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept) -1.984e+00  8.119e-01  -2.444   0.0159 *
## hu_med_val   3.905e-06  1.954e-06   1.998   0.0478 *
## hu_ex_1000   1.768e+01  1.594e+00  11.094   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 2.875 on 128 degrees of freedom
## Multiple R-squared:  0.6136,	Adjusted R-squared:  0.6076
## F-statistic: 101.6 on 2 and 128 DF,  p-value: < 2.2e-16
```

## Residual from the OLS
<center>

![OLS residual mapping](./Figs/OLS-resid.png){width=450px}

</center>

## Geographically weighted regression (GWR)
<center>

![Residual mapping for GWR](./Figs/GWR-resid.png){width=450px}

</center>

## Geographically weighted impact
<center>

![Impact of housing median value](./Figs/coef1.png){width=450px}

</center>

<center>

![impact of housing cost over $1k/ month](./Figs/coef2.png){width=450px}

</center>

<center>

![Solar installation hotspot](./Figs/hotspot.png){width=450px}

</center>

<center>

![Solar installation outlier](./Figs/outlier.png){width=450px}

</center>


## Factor analysis (Parallel screen)

<img src="Agenda_files/figure-html/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

```
## Parallel analysis suggests that the number of factors =  3  and the number of components =  NA
```

## Factor analysis (Plot)

<img src="Agenda_files/figure-html/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

## Factor analysis (Diagram)

<img src="Agenda_files/figure-html/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

## Factor correlation for solar installation


```
## [1] 131   3
```

<img src="Agenda_files/figure-html/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

## Factor regression

```
##
## Call:
## lm(formula = regr[[14]] ~ dat[, 1] + dat[, 2] + dat[, 3])
##
## Residuals:
##     Min      1Q  Median      3Q     Max
## -8.4394 -1.6916 -0.5028  1.0860 16.2534
##
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   5.0904     0.2531  20.112   <2e-16 ***
## dat[, 1]      3.3447     0.3443   9.715   <2e-16 ***
## dat[, 2]      0.6351     0.3127   2.031   0.0443 *
## dat[, 3]      0.3905     0.3147   1.241   0.2169
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 2.897 on 127 degrees of freedom
## Multiple R-squared:  0.6106,	Adjusted R-squared:  0.6014
## F-statistic: 66.39 on 3 and 127 DF,  p-value: < 2.2e-16
```

## Cluster analysis

```
##
##  1  2  3
## 55 32 44
```

<img src="Agenda_files/figure-html/unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

## Cluster within cluster sum of squares (WCSS)


```
## [1] 177.3648
```

<img src="Agenda_files/figure-html/unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

## Optimal cluster

<img src="Agenda_files/figure-html/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

## Cluster plot
<img src="Agenda_files/figure-html/unnamed-chunk-19-1.png" style="display: block; margin: auto;" /><img src="Agenda_files/figure-html/unnamed-chunk-19-2.png" style="display: block; margin: auto;" />

## 3D plot


![](./Figs/3D.jpg)

## Cluster with boxplot

<img src="Agenda_files/figure-html/unnamed-chunk-21-1.png" style="display: block; margin: auto;" />

## Cluster plot with smooth

<img src="Agenda_files/figure-html/unnamed-chunk-22-1.png" style="display: block; margin: auto;" />

## Data analysis 1

<img src="Agenda_files/figure-html/unnamed-chunk-23-1.png" style="display: block; margin: auto;" />


## Data analysis 2
<img src="Agenda_files/figure-html/unnamed-chunk-24-1.png" style="display: block; margin: auto;" />

## Data analysis 3
<img src="Agenda_files/figure-html/unnamed-chunk-25-1.png" style="display: block; margin: auto;" />

<center>

![Clustered census track](./Figs/cluster.png){width=450px}

</center>
