#' ---
#' title: "Statistical Methods for Spatial Data: Residential Solar in Seattle"
#' author: "Yohan Min"
#' output:
#'   pdf_document:
#'     latex_engine: xelatex
#' fontsize: 11pt
#' header-includes:
#'   - \usepackage{fontspec}
#' ---
#'
#' \fontfamily{cmr}
#' \fontsize{11}{16}
#' \fontseries{b}
#' \selectfont

#+ r setup, include=FALSE, echo= F
knitr::opts_chunk$set(echo=FALSE, warning=F, message=F, fig.align="center", 
                      fig.height= 3)
par(mar=c(4,4,1,0)) # sets the bottom, left, top and right margins respectively

library(SpatialEpi)
library(maps)
library(shapefiles)
library(maptools)
library(RColorBrewer)
library(tidyverse)
library(spdep)
library(INLA)
library(rgdal)
library(pander)
library(DCluster)
library(geoR)
library(sp)
library(spatstat)
library(splancs)
library(GWmodel)
library(psych)
library (cluster)
library(reshape)
library(reshape2)
library(som)
library(GPArotation)
library(corrplot)
library(GGally)
library(faraway)
library(sjPlot)
library(lattice)
library("scatterplot3d")
library(rgl)

load("../data/derived/reg.Rdata")
load("../data/derived/dv.Rdata")
load("../data/derived/inla.Rdata")
# View(regrs)

seattle <- readOGR(dsn = "../data/raw", layer= "2010_US_Census_Tracts")
# plot(seattle)
# names(seattle)
# View(seattle)
# View(seattle@data)
# head(seattle@regrs)
# nb.map <- poly2nb(seattle)
# nb2INLA("seattle.graph", nb.map)
regrs$clu <- kme$cluster
seattle@data <- seattle@data %>% 
  left_join(regrs, by = c("GEOID10" = "geoid"))

#' # 1. INTRODUCTION 
#' 
#' Residential solar installations have rapidly increased in recent years with advancement of clean energy policies and associated incentives such as tax credits. While numerous studies have been performed on various aspects of the policies designed to support solar installations, there is still a dearth of studies aimed at investigating the impact of such policies on the social equity. Two unanswered questions have emerged: (1) were there certain communities inadvertently left out from incentive opportunities? and (2) do those current policies help the social equity?
#' 
#' # 2. DATA DESCRIPTION 
#' 
#' To answer these questions, the present study performs a spatial analysis of the distribution of solar panel installed-buildings (solar house hereafter) and socioeconomic characteristics based on census track. In particular, this study aims to explore the patterns of residential (single family and multifamily) solar installations by examining electrical permits that were issued between 2003 and 2018 in Seattle, WA. The density of point data of solar house and its related `G estimate` show a clustered pattern across Seattle. 
#'  
so <- as.data.frame(install[,c(4,3)])
# plot(so,pch = 4, cex = 0.6)
solar <- as.ppp(so, W= owin(c(-122.45, -122.20), c(47.49, 47.74)))
# plot(solar)
den <- as(as(density(solar, 0.004), "SpatialGridDataFrame"), "SpatialPixelsDataFrame")
spplot(den)
# scatterplot3d(cbind(den$s1, den$s2, den$v), color="red", pch="7", type = "l", xlab = "x", ylab = "y", zlab = "density")
# plot3d(cbind(den$s1, den$s2, den$v), col = rainbow(7), type = "p", size= 1.5)

#' ![Density plot](3d.png)

#+ echo= F, include=F, warning=F, message=F
evn <- envelope(solar, fun= Gest, nrank= 2, nsim= 99)

#+ echo= F, warning=F, message=F
plot(evn, main= "G estimate for clustering", legend= F, xlab= "Distance(d)", ylab= "G(d)")

#' After aggregating the point data to the related census track, the examined data entails the socioeconomic and housing characteristics based on the American Community Survey of the census. The number of solar installation in each census track is the dependent variable (`SMR_s`). Rest of variables are as follows.

#' * `hu_own`: the proportion of owner-occupied housing units
#' * `single_unit`: the proportion of single unit housings (single family houses)
#' * `hu_no_mor`: the proportion of owner-occupied housing units without a mortgage
#' * `hu_med_val`: the normalized median value of owner-occupied housing units 
#' * `hu_ex_1000`: the proportion of owner-occupied units with housing costs greater than $1000/month
#' * `hh_med_income`: the normalized household median income
#' * `hh_gini_index`: the household GINI Index of income inequality
#' * `high_income`: the proportion of high income households
#' * `SMR_s`: the ratio of solar installation to the expected number of installation in terms of the number of the residential housing units of the given census track.
#' 
source("dv_table.R")
#' 
#' # 3. METHODS
#' 
#' The expected rate of solar house per each census track is estimated based on the proportion of the total number of housing unit and its solar house number. Here SMR, the rate of solar house in a census track is defined by the number of solar house over the expected number of solar house given the estimated proportion, which is the total number of solar house over the total number of housing unit in Seattle. 
#'
coul = colorRampPalette(rev(brewer.pal(11, "RdBu")))(18)
uni = colorRampPalette(brewer.pal(9, "YlOrBr"))(30)
#'
#' $$ SMR_i = \frac{Y_i}{E_i} $$
#' 
spplot(seattle, "SMR_s", col.regions=uni, main="SMR")
#' 
#' Moran's I detects global clustering in the data with the very small `p-value` confirming the solar house rate across the census track is clustered. Using `SatScan method`, area clustering detection finds Northwest Seattle and Northeast Seattle. 
#' $$
#' \begin{aligned}
#' Y_i \sim \mbox{Poisson}(E_i \mbox{e}^{\beta_{0}})
#' \end{aligned}
#' $$
#' 
seattle_nb <- poly2nb(seattle)
col.w <- nb2listw(seattle_nb, style= "W", zero.policy= T)
pois <- glm(n_s ~ 1, offset= log(solar_E), data= seattle@data, family= poisson)
poisres <- residuals(pois, type= "pearson")
moran.test(poisres, col.w) %>% pander()
seattle@data$poisres <- poisres
spplot(seattle, "poisres", col.regions = coul, main= 'Residuals of poission model')

par(mfrow= c(1,3))
centroids <- coordinates(seattle)
colnames(centroids) <- c("x", "y")
rownames(centroids) <- 1:135

pop.upper.bound <- 0.2
n.simulations <- 999
alpha.level <- 0.05

Kpoisson <- kulldorff(centroids,seattle@data$n_s,seattle@data$hu,
                      expected.cases=seattle@data$solar_E, 
                      pop.upper.bound, n.simulations, alpha.level, plot=T)
Kcluster <- Kpoisson$most.likely.cluster$location.IDs.included

plot(seattle, axes= T)
plot(seattle[Kcluster,], add=T, col= "red")
title("Most likely cluster")

K2cluster <- Kpoisson$secondary.clusters[[1]]$location.IDs.included
plot(seattle, axes = TRUE)
plot(seattle[K2cluster,], add = TRUE, col = "red")
title("Second Most Likely Cluster")
#'
#' # 4. RESULTS 
#' Factor analysis was performed to reduce dimension of variables as there are variables representing similar social characteristics, mostly correlated each other. 
#' 
#' #+ echo= F, warning=F, message=F
par(mfrow= c(1,1))
corrplot(cor(regrs_p), method = "ellipse")
#'
#' Factor analysis identifys the similar variables with respect to the housing unit structure (single/ multi-family house unit), house statbility or homeownership (rent/ owns), econimic status (income level and housing value) and inequality index. Housing unit structure shows the similar trend of homeownership while housing value, the proportion of high income class, and household income follow the similar pattern. Solar house rate and each factor after the dimension reduction show strong correlations each case. Generalized log-linear model was fitted to the data.
#' 
fct <- seattle@data %>% 
  dplyr::select(single_unit, hu_own, hu_no_mor, hu_med_val, high_income, hh_med_income, hh_gini_index)
fa <- fa(fct,nfactors=3,rotate="promax",fm="ml")
fa.diagram(fa,simple=T)
factor.plot(fa, labels=rownames(fa$loadings))
dat <- as.data.frame(fa$scores)

fn_fa_plt(log(seattle@data[["SMR_s"]]), dat)
seattle@data <- cbind(seattle@data, dat)

faglm <- glm(n_s ~ offset(log(solar_E)) + ML1 + ML2, data= seattle, family= "poisson") 
faglm %>% pander()
# summary(faglm)
#'
#' The proportion of single family house unit per census track presents the proportion of owner-occupied house well while house median value per census track represents other economic status variables such as the proportion of high income household and household median income. In fact the generalized log-linear has more variance explanation power than the previous model from factor analysis. Howerver, Moran's I test shows this model still has residuals clustered. 
#' 
xglm <- glm(n_s ~ offset(log(solar_E)) + single_unit + hu_med_val, 
            data= seattle@data, family= "poisson") 
xglm %>% pander()
# summary(xglm)
glmres <- residuals(xglm, type= "pearson")
seattle@data$glmres <- glmres
spplot(seattle, "glmres", col.regions = coul, 
       main= 'Residuals of poission \n with covariates model')

moran.test(glmres, col.w) %>% pander()
#'
#' INLA model will address the spatial dependencies and independent variance across the census tracks. Prior parameters were set such that 50% chance that the proportion of the spatial variance, $\phi$ is greater 0.5 and 1% chance that the total residual standard deviation is greater than 0.3. 
#' $$
#' \begin{aligned}
#' Y_i |\beta_{0},S_i,\epsilon_i & \sim_{ind} \mbox{Poisson}(E_i \mbox{e}^{\beta_{0}+\beta_{1}X_1+\beta_{2}X_2} \mbox{e}^{S_i + \epsilon_i}),\\ 
#' \epsilon_i | \sigma_\epsilon^{2} & \sim_{iid} \mbox{N}(0,\sigma_\epsilon^{2}),\\ 
#' S_1,...,S_n | \sigma_s^{2} & \sim ~~~ \mbox{ICAR}(\sigma_s^{2}). 
#' \end{aligned} 
#' $$
#' 
seattle@data$ID <- 1:dim(seattle@data)[1]
x_inla <- inla(n_s ~ 1 + I(single_unit) + I(hu_med_val) +
                 f(ID, model="bym2", graph="../data/raw/seattle.graph", scale.model=T, constr=T,
                   hyper=list(phi=list(prior="pc", param=c(0.5, 0.5), initial=1),
                              prec=list(prior="pc.prec", param=c(0.3,0.01), initial=5))),
               data=seattle@data, family="poisson", E=solar_E, control.predictor=list(compute=TRUE))
# summary(x_inla)
# x_inla$summary.random 
# x_inla$summary.linear.predictor

beta = x_inla$summary.fixed[c(1, 3:5)]
sigma_sp <- 1/sqrt(x_inla$summary.hyperpar)[1, -c(2,6)]
rownames(sigma_sp) = "Total SD"
phi = x_inla$summary.hyperpar[2, -c(2,6)] 
rbind(beta, sigma_sp, phi) %>% pander()

#' This model says that 30% increase in the percentage in single family housing proportion may incur `r round(exp(beta[2,3]*0.3),2)` times increase in solar installation. Furthermore, 30% increase in the percentage in economic status may incur `r round(exp(beta[3,3]*0.3),2)` times increase in solar installation. Even though spatial dependency is considered, this model still shows clustering pattern on its residuals after eliminating the covariates. 
#' 
Diff <- x_inla$summary.random$ID[1:135, 2] - x_inla$summary.random$ID[136:270, 2]
REsnonspat <- exp(Diff)
REsspat <- exp(x_inla$summary.random$ID[136:270, 5])
seattle@data$REsnonspat <- REsnonspat
seattle@data$REsspat <- REsspat
seattle@data$REstot <- exp(x_inla$summary.random$ID[1:135, 5])

spplot(seattle, "REsnonspat", col.regions = uni, main="Non-spatial random effects (BYM2)")
spplot(seattle, "REsspat", col.regions = uni, main="Spatial random effects (BYM2)")
spplot(seattle, "REstot", col.regions = uni, main="Total random effects (BYM2)")

seattle@data$x_inla <- 
  exp(x_inla$summary.linear.predictor[4][,1] - x_inla$summary.random$ID[1:135, 5])
spplot(seattle, "x_inla", col.regions = uni, main="Covariate RRs (BYM2)")

seattle@data$RR_inla <- 
  exp(x_inla$summary.linear.predictor[4][,1])
spplot(seattle, "RR_inla", col.regions = uni, main="Lognormal-Spatial RRs (BYM2)")

par(mfrow=c(1,1))
plot(seattle@data$SMR_s, seattle@data$RR_inla, xlab="SMRs", ylab="Lognormal-Spatial RRs")
abline(0, 1, col="red")
#'
#' K-means cluster analysis will indicate a group of census tracks with the similar characteristics and help us identify the correlation between the concerned covariates and the dependent variable, solar house rate. Three groups are categorized to the census tracks in Seattle. The clustering pattern is evident with respect to the covariates, house unit median value and the proportion of single family house unit. 
#' 
par(mfrow= c(1,1))
set.seed(5099)
dat1 <- seattle@data %>% 
  dplyr::select(single_unit, hu_med_val)
kme <- kmeans(dat1, center=3)
clusplot(dat1, kme$cluster, color=TRUE,shade=TRUE, labels=5, lines=0)
dat1$lg_SMR <- log(seattle@data$SMR_s)
dat1$clu <- as.factor(kme$cluster)

#+ echo= F, warning=F, message=F
ggpairs(dat1, mapping=aes(color=clu))+ 
  theme_bw()

# View(seattle@data)
seattle@data$clu <- as.factor(kme$cluster)
spplot(seattle, "clu", col.regions = c('indianred2', 'limegreen', 'dodgerblue2'), main="Cluster")
#'
#' Geographically Weighted Regression (GWR) finally confirms that the residuals have less chance to be clustered with insignificant `p-value` of Moran's I in 0.05 significance level. This model is even higher in `R-squared` value compared to the previous models. GWR model entails consideration of spatial dependence in a local level by changing the coefficients of covariates without involvement of spatial term to the model. Below figures shows the variance of coefficient values across the census tracks. The intensity of each map of covariates indicates the sensitivity of the concerned covariate in terms of the rate of solar house. Single family house proportion impacks more on the central Seattle while North and South Seattle is more sensitive to the house median value when it comes to solar panel installation on the residential house. 
#' 
#+ echo= F, include=F, warning=F, message=F
seattle@data$n_s[seattle@data$n_s == 0.5] <- 1 # function error: for approximation 
bw.ans <- bw.ggwr(n_s ~ single_unit + hu_med_val + offset(log(solar_E)), data= seattle, family= "poisson", approach= "CV", kernel= "bisquare")

#+ echo= F, include=F, warning=F, message=F
gwr.res <- ggwr.basic(n_s ~ single_unit + hu_med_val + offset(log(solar_E)), data= seattle, bw= bw.ans, family= "poisson", kernel= "bisquare")

#+ echo= F, warning=F, message=F
# gwr.res
moran.test(gwr.res$SDF@data$residual, col.w) %>% pander()
seattle@data$gwr <- gwr.res$SDF$residual
spplot(seattle, "gwr", col.regions = coul, main="GWR residuals with covariates\n (0.086 P value of Moran's I)")

#+ echo= F, warning=F, message=F
par(mfrow= c(1,2))
hist(gwr.res$SDF$single_unit, main= "Single family")
abline(v= xglm$coef[2], col= "red")

hist(gwr.res$SDF$hu_med_val, main= "Home median value")
abline(v= xglm$coef[3], col= "red")

spplot(gwr.res$SDF, "single_unit", col.regions = coul, 
       at=as.numeric(quantile(gwr.res$SDF$single_unit,probs=seq(0,10,1)/10)), 
       main="Single family sensitivity")

spplot(gwr.res$SDF, "hu_med_val", col.regions = coul, 
       at=as.numeric(quantile(gwr.res$SDF$hu_med_val,probs=seq(0,10,1)/10)), 
       main="House median value sensitivity")
#'
#' The previous analyses reveals that (1) residential solar installations are mostly correlated to housing stability (single family house unit and homeownership) and economic status (income level and house value) (2) income inequality is less likely correlated. In addition, cluster and geographically weighted regression model with respect to the residential solar installations were analyzed to find areas in Seattle more influenced by each social characteristic. As a result, three noticeable groups were identified and areas in Seattle where residential solar installations are sensitive to economic status and housing stability. 
#' 
#' # 5. DISCUSSION 
#' 
#' The study results will support policy makers to develop a policy that better supports underserved communities under limited resources (e.g., those who rent and have no finance to install solar) by offering equitable incentive distribution and access to clean energy.
#' 


