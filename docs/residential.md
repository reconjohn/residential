---
title: "Characteristics of Residential Solar in Seattle"
author: "Yohan Min"
date: "Dec 5, 2018"
output:
  html_document:
    keep_md: yes
    preserve_yaml: yes
    toc: yes
    toc_float: yes
---




## Solar installation trend in Seattle

> Why is there a drop in residential solar installation in Seattle from 2016? The study area is Seattle and the unit of analysis is census track.



<img src="residential_files/figure-html/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

## Solar installation trend by contractors

<img src="residential_files/figure-html/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />


## Residential households in Seattle

<img src="residential_files/figure-html/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />


## Residential solar potential (MWh/ household) in Seattle

<img src="residential_files/figure-html/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

## Residential solar potential (MWh) in Seattle

> There are two noticeable groups in social characteristics with solar potential (MWh): A group of high income, single family housing owners and a group of low income, multifamily housing renters. These two groups have comparatively higher potential for solar electricity generation.

<img src="residential_files/figure-html/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

## Boxplot for overall potential solar

<img src="residential_files/figure-html/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

## Histograms of multiple variables

> Variables are collected through several datasets including `National Renewable Energy Laboratory (NREL) REPLICA 2018`, `American Community Survey (ACS) 2011 - 2015`, `the Department of Housing and Urban Development (HUD) 2017` and `City of Seattle open data portal`.

<img src="residential_files/figure-html/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

## Cor plot

> The proportion of solar installation per housing unit is the dependent variable (`sol_instl`) per census track. Rest of variables are as follows.

> * `hu_own`: a proportion of owner-occupied housing units
> * `hu_blt1970`: a proportion of housing units built before 1970
> * `hu_no_mor`: a proportion of housing units without a mortgage
> * `hu_med_val`: median value of owner-occupied housing units 
> * `hu_ex_1000`: a proportion of owner-occupied units with housing costs greater than $1000/month
> * `edu`: a proportion of over 25 year old population with college degree and above
> * `wh_race`: a proportion of Caucasian population
> * `hh_med_income`: household median income
> * `hh_gini_index`: household GINI Index of income inequality
> * `lihtc`: low income tax credit qualification (T/F)
> * `hh_high_sf_own`: a proportion of households of high income, single family housing owners
> * `hu_mwh`: solar energy potential (MWh) per housing unit in a census track

> The dependent variable (`sol_instl`) is correlated to all the variables except for the household GINI index and LITHC qualification.

<img src="residential_files/figure-html/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />


## Regression

> By exploratory regression analyses, the best model with the highest **R-squared (0.61)** and lowest **AIC (653.69)** was chosen with the two variables, `hu_med_val` and `hu_ex_1000` through OLS.

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">sol instl</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Predictors</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-1.98</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-3.58&nbsp;&ndash;&nbsp;-0.39</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.016</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">hu med val</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.00&nbsp;&ndash;&nbsp;0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.048</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">hu ex 1000</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">17.68</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">14.56&nbsp;&ndash;&nbsp;20.80</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">131</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">R<sup>2</sup> / adjusted R<sup>2</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">0.614 / 0.608</td>
</tr>

</table>

## Residual from the OLS

> The residual plot shows it is biased and clustered indicating the model is not catching well the variability of the dependent variable.

<center>

![OLS residual mapping](./Figs/OLS-resid.png){width=450px}

</center>

## Geographically weighted regression (GWR)

> Another method named Geographically Weighted Regression (GWR) was performed with the outcomes of **R-squared (0.76)** and **AIC (625.54)**, which are better than the previous OLS model. Residual map shows random pattern as confirmed by auto correlation analysis with **Moran's index of 0.028** and **z-score of 0.75**.

<center>

![Residual mapping for GWR](./Figs/GWR-resid.png){width=450px}

</center>

## Geographically weighted impact

> This model tells that the median value of owner-occupied housing units predicts residential solar installation more in the area of darker red as below. These areas are more sensitive to the median value of owner-occupied housing units with respect to the residential solar installation.

<center>

![Impact of housing median value](./Figs/coef1.png){width=450px}

</center>

> For the variable, total number of owner-occupied units with housing costs greater than $1000/month, the more sensitive areas are presented as darker red in the map below.

<center>

![impact of housing cost over $1k/ month](./Figs/coef2.png){width=450px}

</center>

## Hotspot analysis of residential solar installation

> Each point represents the house unit with residential solar system installed on its building since 2003. By aggregating these points to the census track they are located in, hotspot areas and outliers were identified as below.

<center>

![Solar installation hotspot](./Figs/hotspot.png){width=450px}

</center>

<center>

![Solar installation outlier](./Figs/outlier.png){width=450px}

</center>


## Factor analysis (Parallel screen)

> It would be useful to include all the variables for a model rather than selecting only two variables. With a factor analysis, all the variables would be used for the model identification. The parallel screen confirms 2 or 3 factors would be appropriate.

<img src="residential_files/figure-html/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

```
## Parallel analysis suggests that the number of factors =  3  and the number of components =  NA
```

## Factor analysis (Plot)

> The 3 factors partially explain each variable depending on the latent characteristics.

<img src="residential_files/figure-html/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

## Factor analysis (Diagram)

> The first factor (`ML1`) is more related to the higher housing stability (homeownership), the 2nd factor (`ML2`) to the higher economic status, and the 3rd factor (`ML3`) is more related to the higher income inequality.

<img src="residential_files/figure-html/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

## Factor correlation for solar installation

> The most loaded, `ML1` is positively correlated with the solar installation variable.

<img src="residential_files/figure-html/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

## Factor regression

> The value of R-squared of the factor regression is the same as the previous OLS **(0.61)**.

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">regrs[[14]]</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Predictors</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">5.09</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">4.59&nbsp;&ndash;&nbsp;5.59</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">dat[,1]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">3.34</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">2.67&nbsp;&ndash;&nbsp;4.02</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">dat[,2]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.64</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.02&nbsp;&ndash;&nbsp;1.25</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.044</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">dat[,3]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.39</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.23&nbsp;&ndash;&nbsp;1.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.217</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">131</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">R<sup>2</sup> / adjusted R<sup>2</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">0.611 / 0.601</td>
</tr>

</table>


## Cluster within cluster sum of squares (WCSS)

> A cluster analysis was done to further study the featured census tracks. It shows clustering three would be appropriate.

<img src="residential_files/figure-html/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

<img src="residential_files/figure-html/unnamed-chunk-16-1.png" style="display: block; margin: auto;" />


## Cluster analysis

> Based on the 3 factors, 3 clusters are presented in colors.

<img src="residential_files/figure-html/unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

## 3D plot


![](./Figs/3D.jpg)

## 3 clusters in Seattle

> Each census track in Seattle was identified with the 3 clusters.

<center>

![Clustered census track](./Figs/cluster.png){width=450px}

</center>


## Cluster with boxplot

> Each cluster shows unique features. Green groups comparatively have less housing stability and economic status while higher income inequality. Light blue groups are relatively opposite to the green groups. Light red groups keep their position in the middle of these 2 groups.

<img src="residential_files/figure-html/unnamed-chunk-19-1.png" style="display: block; margin: auto;" />

## Cluster plot with smooth

<img src="residential_files/figure-html/unnamed-chunk-20-1.png" style="display: block; margin: auto;" />

## Residential solar installation pattern in terms of clustering groups

> Residential solar installation is exactly showing the same pattern of the 1st factor (`ML1`), the housing stability in clustering, which confirms the factor, `ML1` has a predictive power for the residential solar installation. 

<img src="residential_files/figure-html/unnamed-chunk-21-1.png" style="display: block; margin: auto;" />


## Residential solar installation in Low Income Tax Credit

> It indicates that lower solar installation proprotion and lower proportion of owner-occupied units with housing costs greater than $1000/month match the pattern of the certified LIHTC census tracks (TRUE).

<img src="residential_files/figure-html/unnamed-chunk-22-1.png" style="display: block; margin: auto;" />

## Residential solar installation in housing unit median value

> Higher solar installation is correlated with higher housing unit median value. Cluster #3, #1 and #2 in order are more likely to have higher solar installation.


<img src="residential_files/figure-html/unnamed-chunk-23-1.png" style="display: block; margin: auto;" />


## Cumulative solar installation per census track

> Time series analysis will help to understand the spatial-temporal pattern of residential solar installation in Seattle. Interestingly, one census track is noticeably high in installation over the period.

<img src="residential_files/figure-html/unnamed-chunk-24-1.png" style="display: block; margin: auto;" />

## Residential solar installation trend in Seattle

> Two different patterns of cumulative installation and annual new installation, are mapped in each census track in Seattle. It shows the pattern of cumulative number of installation is different from the annual new installation in that every year has a different installation trend depending on census tracks. In addition, hotspot outlier analysis was performed for the annual new installation map.


<center>

![Spatial-temporal installation in Seattle](./Figs/temporal-hotspot.png){width=650px}

</center>

## Spatial-temporal hotspot analysis

> The difference between cumulative number of installation hotspot and annual new installation hotspot shows the emerging area of increasing installation pattern in residential solar. It is noticeable that West Seattle recently increases the installation while Ballad used to the one, then went slow, currently increases also. Columbia city presents that it has been and currently goes slow while the toal amount installation is still the highest in Seattle. It's found that these areas are all clustered as light red (#1) and light blue (#3).

<center>

![Hotspot for spatial-temporal cumulative installation in Seattle](./Figs/temporal-hotspot1.png){width=650px}

</center>

<center>

![Hotspot for spatial-temporal annual installation in Seattle](./Figs/temporal-hotspot2.png){width=650px}

</center>

## Takeaways

> Residential solar installation follows patterns of

> * Housing median value
> * The number of owner-occupied house with higher housing cost

> Cluster and factor analysis captures solar installation pattern (Cluster #1 and #3) in terms of

> * **Higher housing stability**
> * **Higher economic status**
> * **Lower income inequality**

> Temporal pattern was also captured such that three noticeable areas were identified

> * **Ballad** – used to be a hotspot area, then went slow, currently seems increasing in solar installation
> * **West Seattle** – a new emerging area in solar installation
> * **Columbia city** – has been and currently goes slow. The toal amount installation is still the highest in Seattle. 
