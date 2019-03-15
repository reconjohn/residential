#' ---
#' title: "Statistical Methods for Spatial Data: Residential Solar in Seattle"
#' author: "Yohan Min"
#' output:
#'   html_document:
#'     keep_md: yes
#'   pdf_document:
#'     latex_engine: xelatex
#' fontsize: 11pt
#' header-includes:
#'   - \usepackage{fontspec}
#'   - \usepackage{float}
#'   - \floatplacement{figure}{H}
#' ---
#'
#' \fontfamily{cmr}
#' \fontsize{11}{16}
#' \fontseries{b}
#' \selectfont

#+ r setup, include=FALSE, echo= F
knitr::opts_chunk$set(echo=FALSE, warning=F, message=F, fig.align= "center", fig.show= "hold", fig.height= 3)
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
library(png)
library(grid)
library(gridExtra)

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
#' To answer these questions, the present study performs a spatial analysis of the distribution of solar panel installed-buildings (solar house hereafter) and socioeconomic characteristics based on census track in Seattle. In particular, this study aims to explore the patterns of residential (single family and multifamily) solar installations by examining several data sources.
#'
#' # 2. DATA DESCRIPTION
#'
#' City of Seattle open data portal has electrical permits that were issued between 2003 and 2018 in Seattle, WA. Electrical permits are required to have when residential houses want to install solar systems on their house properties. Intensive data mining techniques made it possible to identify residential solar installation permits among the data sets. The data includes latitude, longitude, when the solar system is installed, and solar contractor who installed the system. Mapping the points of solar houses in the region can verify a certain pattern in installation. Residential housing units can be comparatively considered to be evenly present across the region for the purpose of exploratory data analysis although they are not equally distributed. The density of point data of solar house and its related `G estimate` show a clustered pattern across Seattle.
#'
so <- as.data.frame(install[,c(4,3)])
# plot(so,pch = 4, cex = 0.6)
solar <- as.ppp(so, W= owin(c(-122.45, -122.20), c(47.49, 47.74)))
# plot(solar)

#+ echo= F, include=F, warning=F, message=F
evn <- envelope(solar, fun= Gest, nrank= 2, nsim= 99)

#+ echo= F, warning=F, message=F, fig.cap = "G estimate for spatial dependency", fig.height= 3
plot(evn, main="", legend= F, xlab= "Distance(d)", ylab= "G(d)")

#' ![Density plot of residential solar installation in Seattle](3d.png){width=500px}

# scatterplot3d(cbind(den$s1, den$s2, den$v), color="red", pch="7", type = "l", xlab = "x", ylab = "y", zlab = "density")
# plot3d(cbind(den$s1, den$s2, den$v), col = rainbow(7), type = "p", size= 1.5)

#' After the point data was aggregated to the related census track, the data was examined in terms of the socioeconomic and housing characteristics based on the American Community Survey of the census (2011 - 2015 ACS 5-Year estimates). The rate of the residential solar installation in each census track is the dependent variable (`SMR_s`) in this study. Rest of variables are as follows.
#'
#' * `hu_own`: the proportion of owner-occupied housing units
#' * `single_unit`: the proportion of single unit housings (single family houses)
#' * `hu_no_mor`: the proportion of owner-occupied housing units without a mortgage
#' * `hu_med_val`: the normalized median value of owner-occupied housing units
#' * `hu_ex_1000`: the proportion of owner-occupied units with housing costs greater than $1000/month
#' * `hh_med_income`: the normalized household median income
#' * `hh_gini_index`: the household GINI Index of income inequality
#' * `high_income`: the proportion of high income households
#' * `SMR_s`: the ratio of solar installation to the expected number of installations in regard to the total number of the residential housing units of the given census track.
#'
#+ echo= F, warning=F, message=F, fig.cap = "Histograms of variables"
par(mar=c(1.9,1.9,1,1))
fhist <- regrs[, c(3,4,6,7,8,13,14,17,24)]
par(mfrow=c(3,3))
for(i in 1:length(fhist)){
      hist(fhist[[i]], main= paste(names(fhist)[i]),
           xlab= names(fhist)[i], col="gold")
      abline(v = median(fhist[[i]]), col="red", lwd=4)
      text(median(fhist[[i]]), 7, round(median(fhist[[i]]),2), col = "blue")
    }
#'
#' # 3. METHODS
#'
#' The expected rate of solar house per each census track is estimated based on the total number of housing units in Seattle and its solar house numbers. Here the term, Standardized Mortality Ratio (SMR) can be considered to be the rate of solar house in a census track in this study. It is defined by the number of solar houses over the expected number of solar houses given the estimated proportion, which is the total number of solar houses over the total number of housing units in Seattle. SMR shows a pattern of clustering similar to the pattern of the previous solar house point data.
#'
#'
coul = colorRampPalette(rev(brewer.pal(11, "RdBu")))(18)
uni = colorRampPalette(brewer.pal(9, "YlOrBr"))(30)
#'
#' $$ SMR_i = \frac{Y_i}{E_i} $$
#'
#' The solar house rate is assumed to be associated with `poisson count model` considering its rare proportion with respect to the denominator, the total housing units in a census track in addition to fact that the number of solar houses is count data. The residuals after fitting the poisson model shows clustering with the similar pattern of SMR distribution across census tracks as shown in the figures below. This indicates that there is strong evidence of spatial dependency among the regions in Seattle.
#' $$
#' \begin{aligned}
#' Y_i \sim \mbox{Poisson}(E_i \mbox{e}^{\beta_{0}})
#' \end{aligned}
#' $$
#'
par(mar=c(5,5,1,1))
seattle_nb <- poly2nb(seattle)
col.w <- nb2listw(seattle_nb, style= "W", zero.policy= T)
pois <- glm(n_s ~ 1, offset= log(solar_E), data= seattle@data, family= poisson)
poisres <- residuals(pois, type= "pearson")
seattle@data$poisres <- poisres

#+ echo= F, warning=F, message=F, fig.cap = "SMR and residuals of poisson model"
p1 <- spplot(seattle, "SMR_s", col.regions=uni, main="SMR in Seattle")
p2 <- spplot(seattle, "poisres", col.regions = coul, main= 'Residuals of poission model')
grid.arrange(p1, p2, ncol = 2)
#'
#' Moran's I test detects global clustering in the distribution pattern of the rate of solar houses with very small `p-value`. This confirms that the solar house rate across census tracks is clustered. Using `SatScan method`, area clustering detection shows Northwest Seattle and Northeast Seattle as the clustered regions. This clustering trend can be alleviated with the appropriate covariates with the similar characteristics. In this regard, socioeconomic and housing characteristics will be examined to identify the most proper covariates.
#'
#+ echo= F, warning=F, message=F, fig.cap = "StaScan clustering detection"
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
#'Having verified that there is geographical pattern of the solar house rate, there might be related or shared factors in socioeconomic characteristics in the same region. Housing, economy, social inequality variables show correlations pairwise in the figure below.
#'
#+ echo= F, warning=F, message=F, fig.cap= "Covariates correlation plot", fig.height= 4
par(mfrow= c(1,1))
corrplot(cor(regrs_p), method = "ellipse")
#'
#' Dimension reduction of covariates will be performed by factor analysis in consideration of avoiding multicollinearity. These generated factors will fit models to estimate the solar house rate. Residuals will be checked afterwards to see if there is still a clustering pattern, which indicates that the model can't address the spatial dependency. Furthermore, a few variables which represent each factor the most, will be chosen for the model fit compared to the factors from dimension reduction. It is because factors could possibly keep overall noises by including unnecessary covariates, which are less related to the solar house rate. Poisson lognormal spatial model, spatially using `BYM2` method, will be tested to address the residual clustering in addition to `K-means` clustering analysis, which identifies similar regions in terms of socioeconomic patterns of census tracks. Note that `K-means` clustering will not take into account of the solar house rate to define the Euclidean distance among data points so that the categorized census tracks are able to be compared with the solar house rate pattern to figure out the relationship between covariates and solar house rate. Finally Geographically Weighted Regression (GWR) will address the local variation of coefficients of covariates by taking into account of the local spatial dependency.
#'
#' # 4. RESULTS
#'
#' ## 4.1. Factor analysis
#'
#' Factor analysis was performed to reduce dimension of variables as there are variables representing similar characteristics, mostly correlated each other. It identifies the similar variables with respect to housing unit structure (single/ multi-family house unit), house stability (homeownership in rent/ owns), economic status (income level and housing value), and inequality index. Housing unit structure shows the similar trend of homeownership while housing median value, proportion of high income class, and household median income follow the similar pattern representing economic status. Solar house rate and each factor after the dimension reduction show strong correlations. Generalized log-linear model was fitted to the data.
#'
#'
fct <- seattle@data %>%
  dplyr::select(single_unit, hu_own, hu_no_mor, hu_med_val, high_income, hh_med_income, hh_gini_index)
fa <- fa(fct,nfactors=3,rotate="promax",fm="ml")

#+ echo= F, warning=F, message=F, fig.cap= "Factor diagram", fig.height= 2
fa.diagram(fa,simple=T)

dat <- as.data.frame(fa$scores)

#+ echo= F, warning=F, message=F, fig.cap= "SMR plots in factors"
par(mfrow=c(1,3))
ML_ord = c("ML1", "ML2", "ML3")
for(i in seq_along(dat)){
  plot(dat[,i], log(seattle@data$SMR_s), xlab = ML_ord[i],
       ylab = expression("Log of solar installation ("~log(Y[i]/E[i])~")"))
  abline(lm(log(seattle@data$SMR_s) ~ dat[,i]), col = "red")
}

seattle@data <- cbind(seattle@data, dat)
faglm <- glm(n_s ~ ML1 + ML2 + offset(log(solar_E)), data= seattle, family= "poisson")
faglm %>% pander()

xglm <- glm(n_s ~ single_unit + hu_med_val + offset(log(solar_E)), data= seattle, family= "poisson")
glmres <- residuals(xglm, "pearson")
seattle@data$glmres <- glmres

#'
#' ## 4.2. Integrated Nested Laplace Approximations (INLA) model
#'
#' It is obvious that the variables can be divided into three categories: (1) housing stability, mostly the proportion of owner occupied single family houses, (2) economic status such as income level and house value, and (3) income inequality. Solar installation rate seems to be mainly correlated to the housing stability and economic status in this data. A few selected covariates could fit a model better than factors as factors includes all the unrelated covariates to solar house rate in this study. The most representing covariates, single family house proportion (housing stability) and house median value (economic status) are selected for the further analyses. It is confirmed that the same generalized loglinear model fits better with the covariates than factors.
#'
#' Integrated Nested Laplace Approximations (INLA) model takes into account of spatial dependencies and lognormal independent variance across census tracks. This model was set with priors such that 50% chance that the proportion of the spatial variance, $\phi$ is greater 0.5 and 1% chance that the total residual standard deviation is greater than 0.3. The result of the model fit confirms large variance is due to the spatial factor with $\phi$ of 0.96 in median.

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

rbind(beta, sigma_sp, phi) %>% pander("Fitting INLA model: n_s ~ offset(log(solar_E)) + single_unit + hu_med_val")

#' This model says that 30% increase in the percentage in single family housing proportion may incur `r round(exp(beta[2,3]*0.3),2)` times increase in solar installation. Furthermore, 30% increase in the percentage in economic status may incur `r round(exp(beta[3,3]*0.3),2)` times increase in solar installation. Even though this model considers the spatial dependency, the residuals show a clustering pattern after eliminating the covariate terms from the fitted values.
#'
Diff <- x_inla$summary.random$ID[1:135, 2] - x_inla$summary.random$ID[136:270, 2]
REsnonspat <- exp(Diff)
REsspat <- exp(x_inla$summary.random$ID[136:270, 5])
seattle@data$REsnonspat <- REsnonspat
seattle@data$REsspat <- REsspat
seattle@data$REstot <- exp(x_inla$summary.random$ID[1:135, 5])

seattle@data$x_inla <-
  exp(x_inla$summary.linear.predictor[4][,1] - x_inla$summary.random$ID[1:135, 5])

seattle@data$RR_inla <-
  exp(x_inla$summary.linear.predictor[4][,1])
#'
#' ## 4.3. K-means clustering analysis
#'
#' K-means cluster analysis indicates a group of census tracks with the similar characteristics and help to identify the correlations between concerned covariates and the dependent variable, solar house rate. Three groups are categorized to the census tracks in Seattle. The clustering pattern is evident with respect to the covariates, house median value and the proportion of single family house units.
#'
par(mfrow= c(1,1))
set.seed(5099)
dat1 <- seattle@data %>%
  dplyr::select(single_unit, hu_med_val)
kme <- kmeans(dat1, center=3)
dat1$lg_SMR <- log(seattle@data$SMR_s)
dat1$clu <- as.factor(kme$cluster)

#+ echo= F, warning=F, message=F, fig.cap= "Covariate distributions with clustering"
ggpairs(dat1, mapping=aes(color=clu))+
  theme_bw()

# View(seattle@data)
seattle@data$clu <- as.factor(kme$cluster)
#'
#' ## 4.4. Geographically Weighted Regression
#'
#' Geographically Weighted Regression (GWR) finally confirms that the residuals have less chance to be clustered with insignificant `p-value` of Moran's I in 0.05 significance level. This model is even higher in `R-squared` value compared to the previous models. GWR model entails consideration of spatial dependence in a local level by changing the coefficients of covariates without involvement of explicit spatial term to the model. Below figures show the variance of coefficient values across the census tracks. The intensity of each map of covariates indicates the sensitivity of the concerned covariate in terms of the rate of solar houses. Single family house rate impacts more on the central Seattle area while North and South Seattle are more sensitive to the house median value with respect to solar panel installation on the residential houses.
#'
#+ echo= F, include=F, warning=F, message=F
seattle@data$n_s[seattle@data$n_s == 0.5] <- 1 # function error: for approximation
bw.ans <- bw.ggwr(n_s ~ single_unit + hu_med_val + offset(log(solar_E)), data= seattle, family= "poisson", approach= "CV", kernel= "bisquare")

#+ echo= F, include=F, warning=F, message=F
gwr.res <- ggwr.basic(n_s ~ single_unit + hu_med_val + offset(log(solar_E)), data= seattle, bw= bw.ans, family= "poisson", kernel= "bisquare")

#+ echo= F, warning=F, message=F
# gwr.res
seattle@data$gwr <- gwr.res$SDF$residual

#+ echo= F, warning=F, message=F, fig.cap= "Clustering and GWR residuals", fig.height= 2.5
c1 <- spplot(seattle, "clu", col.regions = c('indianred2', 'limegreen', 'dodgerblue2'), main="Cluster")
c2 <- spplot(seattle, "gwr", col.regions = coul, main="GWR residuals with covariates\n (0.086 P value of Moran's I)")
grid.arrange(c1, c2, ncol = 2)

#+ echo= F, warning=F, message=F, fig.cap= 'Coefficient variation of covariates', fig.height=2.5
par(mfrow= c(1,2))
hist(gwr.res$SDF$single_unit, main= "Single family", xlab= "")
abline(v= xglm$coef[2], col= "red")

hist(gwr.res$SDF$hu_med_val, main= "Home median value", xlab= "")
abline(v= xglm$coef[3], col= "red")

#+ echo= F, warning=F, message=F, fig.cap= 'GWR different impact of covariates in Seattle'
s1 <- spplot(gwr.res$SDF, "single_unit", col.regions = coul,
             at=as.numeric(quantile(gwr.res$SDF$single_unit,probs=seq(0,10,1)/10)),
             main="Single family sensitivity")

s2 <- spplot(gwr.res$SDF, "hu_med_val", col.regions = coul,
             at=as.numeric(quantile(gwr.res$SDF$hu_med_val,probs=seq(0,10,1)/10)),
             main="House median value sensitivity")
grid.arrange(s1, s2, ncol = 2)
#'
#'
#' # 5. DISCUSSION AND CONCLUSION
#'
#' The previous analyses reveal that (1) residential solar installations are mostly correlated to housing stability (single family house unit and homeownership) and economic status (income level and house value) (2) income inequality is less likely correlated. The results answer the questions that there are certain communities left out from using renewable energy due to the lack of resources (i.e., houses and finance). Since these communities can't join the programs with incentives such as 30% income federal tax credits, it is necessary to address the issue by coming up with policies such that encouraging these communities to take advantage of the clean energy as well. In addition, cluster and geographically weighted regression model with respect to the residential solar installations were analyzed to find areas in Seattle more influenced by each characteristic. As a result, three categorized groups in terms of housing stability and economic status were identified as well as areas in Seattle where residential solar installations are sensitive to the economic status and the housing stability.
#'
#' The study results will support policy makers to develop a policy that better supports underserved communities under limited resources (e.g., those who rent houses and have no finance to install solar systems) by offering equitable incentive distribution and access to clean energy.
#'
#' \pagebreak
#'
#' # Appendix
#'
par(mfrow=c(1,1))
factor.plot(fa, labels=rownames(fa$loadings))

den <- as(as(density(solar, 0.004), "SpatialGridDataFrame"), "SpatialPixelsDataFrame")
#+ echo= F, warning=F, message=F, fig.height=3.5
plot(den)
title("Solar house density in Seattle")

moran.test(poisres, col.w)  %>% pander("Residuals of poisson model without covariates")

xglm %>% pander()

moran.test(glmres, col.w) %>% pander("Residuals of poission model with covariates")

#+ echo= F, warning=F, message=F, fig.height=3
m1 <- spplot(seattle, "glmres", col.regions = coul,
       main= 'Residuals of poission \n with covariates model')
m2 <- spplot(seattle, "REsnonspat", col.regions = uni, main="Non-spatial random \n effects (BYM2)")
m3 <- spplot(seattle, "REsspat", col.regions = uni, main="Spatial random effects (BYM2)")
m4 <- spplot(seattle, "REstot", col.regions = uni, main="Total random effects (BYM2)")
m5 <- spplot(seattle, "x_inla", col.regions = uni, main="Covariate RRs (BYM2)")
m6 <- spplot(seattle, "RR_inla", col.regions = uni, main="Lognormal-Spatial RRs (BYM2)")
grid.arrange(m1, m2, ncol = 2)
grid.arrange(m3, m4, ncol = 2)
grid.arrange(m5, m6, ncol = 2)

plot(seattle@data$SMR_s, seattle@data$RR_inla, xlab="SMRs", ylab="Lognormal-Spatial RRs")
abline(0, 1, col="red")

moran.test(gwr.res$SDF@data$residual, col.w) %>% pander("GWR residual residuals")
