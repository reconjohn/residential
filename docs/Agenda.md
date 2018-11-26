---
title: "Agenda"
author: "Yohan Min"
date: "November 26, 2018"
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



## Energy code

### Title 24 vs. IECC
연방정부 기준인 IECC 를 적어도 만족시키는 주정부 energy 기준이 있어야 하는데, CA 의 경우 2016 Building Energy Efficiency Standards Title 24, Part 6 이 2015 IECC 를 넘어서는 결과를 보여주고 있다. 

### 2018 IECC 
가장 최근 버전으로 - APPENDIX RA SOLAR-READY PROVISIONS—DETACHED ONE- AND TWOFAMILY DWELLINGS, MULTIPLE SINGLE-FAMILY DWELLINGS (TOWNHOUSES) - solar ready 에 대한 조건이 있다. 

### 2019 Residential Compliance Manual 
CA 에서 나온 것으로, PV 설치를 강제하고 있고, 예외에 한에서 역시 solar ready 를 강제하고 있다. Seattle 이 경우 2017 residential code 가 해당. Residential 의 경우 solar ready 는 residential code 에 있는데, commercial 의 경우 solar ready 는 building code 가 아닌 energy code 에 있다. 참고로, single family 와 low rise multifamily 는 residential code (특별 building code 로 보면 될 듯), 그 외는 building code. 그리고 solar 를 직접 설치 했을 때는 NEC 의 electrical code 와 관계한 electrical permit 을 받아야 한다. 

> The California Energy Code, part 6 of the California Building Standards Code which is title 24 of the California Code of Regulations, also titled The Energy Efficiency Standards for Residential and Nonresidential Buildings, were created by the California Building Standards Commission in 1978 in response to a legislative mandate to reduce California's energy consumption. The standards are updated periodically by the California Energy Commission to allow consideration and possible incorporation of new energy efficiency technologies and methods. The California Energy Code (CEC) contains energy conservation standards applicable to most residential and nonresidential buildings throughout California, including schools. - solar ready, residential compliance manual.

> There are 3 different kinds of building codes: private sector, federal sector, and international. The private sector codes are associated with state and local jurisdiction. States and local jurisdictions have different energy codes that they follow based on climate, geography, and many other contributing factors. The two primary baseline codes for the private sector are the International Energy Conservation Code (IECC), and the ANSI/ASHRAE/IESNA Standard 90.1 energy standard for Buildings Except Low-Rise Residential Buildings (ASHRAE 90.1).[4] States and local governments adopt and enforce these energy codes. The standards are published by national organizations such as ASHRAE. The International Code Council (ICC) develops the codes and standards used to construct residential and commercial buildings, including homes and schools.[5] Within the ICC is the IECC which is a subset of the ICC. The IECC is a model energy code, but it is written in mandatory, enforceable language, so that state and local jurisdictions can easily adopt the model as their energy code.[6] The IECC references several ASHRAE Standards, in particular the ASHRAE 90.1 for commercial building construction.

## OSHA

* Roof slope: OSHA defines a low-slope roof as a roof having a slope of less than or equal to 4 inches of vertical rise for every 12 inches horizontal length (4:12) (1926.500(b)—definitions). This is important because the OSHA definition is used as a basis for implementing low-slope fall-protection measures, such as warningline
systems and safety monitors.

* Ladder: angle 75 degree, one-quarter the working length of the ladder (a 1:4 ratio) (29 CFR 1926.1053(b)(5)(i)). 3 rungs (1 ft apart) above the roof, The side rails of the ladder generally must extend at least 3 feet above the upper landing surface that the worker is trying to access (29 CFR 1926.1053(b)(1)).

* Anchor: OSHA standard regarding anchorages can be found in 29 CFR 1926.502(d)(15)

## Risk per full-time workers 
![Top 3 risks are related to solar installation on the roof](./Figs/PTD.jpg)

## Ideal design for safety from the interviews 
* Roof pitch: lower than 5/12 – 7/12 to work easy, fall
* Roof material: composition not to be slippery, fall
* Roof structure: no obstruction no to be interrupted, trip, complexity
* Roof condition: accessories pre-installed to reduce the scope, complexity
* Anchor point: pre-installed to be efficient, fall
* Access: low height, enough space for easier access, fall
* Additional: setback, snow guard, guardrail to prevent fall
* Electrical: micro inverter, conduit pre-run, reserving spaces, electric shock, complexity


## Solar installation trend in Seattle



![](Agenda_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

## Solar installation trend by contractors 

![](Agenda_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


## Cumulative solar installation per census track

![](Agenda_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## Residential solar potential (MWh) in Seattle

![](Agenda_files/figure-html/unnamed-chunk-4-1.svg)<!-- -->

## Residential solar potential (MWh/ household) in Seattle

![](Agenda_files/figure-html/unnamed-chunk-5-1.svg)<!-- -->


## Histograms of multiple variables 

![](Agenda_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## Cor plot

![](Agenda_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


## Regression


```
## 
## Call:
## lm(formula = sol_instl ~ hu_rnt + hu_med_val + black, data = fin[-c(1, 
##     2, 6, 7, 8, 9)])
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.0301 -1.7673 -0.4132  1.1725 14.2792 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.415e+01  1.770e+00   7.997 6.80e-13 ***
## hu_rnt      -1.901e+01  1.692e+00 -11.236  < 2e-16 ***
## hu_med_val   8.420e-06  1.953e-06   4.311 3.23e-05 ***
## black        7.972e+00  3.218e+00   2.477   0.0146 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.859 on 127 degrees of freedom
## Multiple R-squared:  0.6207,	Adjusted R-squared:  0.6117 
## F-statistic: 69.26 on 3 and 127 DF,  p-value: < 2.2e-16
```

## Residual from the OLS 

![Residual mapping](./Figs/Residual.jpg)

## Geographically weighted regression (GWR)
![Residual mapping for GWR](./Figs/Residual-GWR.jpg)

## Geographically weighted impact 
![Rental proportion](./Figs/Rental-w.jpg)

![House median value](./Figs/H-value-w.jpg)

![Black people proportion](./Figs/Black-w.jpg)

## Factor analysis (Parallel screen) 

![](Agenda_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```
## Parallel analysis suggests that the number of factors =  4  and the number of components =  NA
```

## Factor analysis (Plot)

![](Agenda_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

## Factor analysis (Diagram)

![](Agenda_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

## Factor correlation for solar installation

![](Agenda_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

## Factor regression 

```
## 
## Call:
## lm(formula = fin[[16]] ~ dat[, 1] + dat[, 2] + dat[, 3])
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.2962 -2.0322 -0.5817  1.5500 17.9535 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   5.0904     0.2919  17.441  < 2e-16 ***
## dat[, 1]      2.2605     0.3361   6.726 5.34e-10 ***
## dat[, 2]     -1.1489     0.3472  -3.309  0.00122 ** 
## dat[, 3]     -0.9088     0.3001  -3.028  0.00298 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.34 on 127 degrees of freedom
## Multiple R-squared:  0.4823,	Adjusted R-squared:   0.47 
## F-statistic: 39.44 on 3 and 127 DF,  p-value: < 2.2e-16
```

## Kmeans

```
##          ML2        ML3        ML1
## 1 -0.6885333  0.4305238 -0.1563880
## 2 -0.4289458  0.8822497  2.5790644
## 3  0.8218458 -0.6197500 -0.2747965
```

```
## 
##  1  2  3 
## 63 10 58
```

![](Agenda_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

## Cluster within cluster sum of squares (WCSS)


```
## [1] 251.739
```

![](Agenda_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

## Cluster plot
![](Agenda_files/figure-html/unnamed-chunk-16-1.png)<!-- -->![](Agenda_files/figure-html/unnamed-chunk-16-2.png)<!-- -->

## 3D plot


![](./Figs/3D.jpg)

