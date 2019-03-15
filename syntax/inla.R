#' ---
#' title: "Statistical Methods for Spatial Data: INLA"
#' author: "Yohan Min"
#' output:
#'   html_document:
#'     keep_md: yes
#'   pdf_document:
#'     latex_engine: xelatex
#' fontsize: 12pt
#' header-includes:
#'   - \usepackage{fontspec}
#' ---
#'
#' \fontfamily{cmr}
#' \fontsize{12}{18}
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
library(broom)
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

#' # 1. Seattle solar installation Poisson model

regrs$clu <- kme$cluster
seattle@data <- seattle@data %>% 
  left_join(regrs, by = c("GEOID10" = "geoid"))

coul = colorRampPalette(brewer.pal(9, "YlOrBr"))(30)
# colorRampPalette(rev(brewer.pal(11, "RdBu")))(30)

#+ echo= F, warning=F, message=F
spplot(seattle, "n_s", col.regions=coul, main="Observed count (Y)")

spplot(seattle, "solar_E", col.regions=coul, main="Expected count (E)")

#' $$ SMR_i = \frac{Y_i}{E_i} $$

spplot(seattle, "SMR_s", col.regions=coul, main="SMR")

#' # 2. Poisson-Lognormal-Spatial model
#' 
#' $$
#' \begin{aligned}
#' Y_i |\beta_{0},S_i,\epsilon_i & \sim_{ind} \mbox{Poisson}(E_i \mbox{e}^{\beta_{0}+\beta_{1}X_1+\beta_{2}X_2} \mbox{e}^{S_i + \epsilon_i}),\\ 
#' \epsilon_i | \sigma_\epsilon^{2} & \sim_{iid} \mbox{N}(0,\sigma_\epsilon^{2}),\\ 
#' S_1,...,S_n | \sigma_s^{2} & \sim ~~~ \mbox{ICAR}(\sigma_s^{2}). 
#' \end{aligned} 
#' $$
#' 
# 
# seattle@data$ID <- 1:dim(seattle@data)[1]
# seattle@data$ID2 <- 1:dim(seattle@data)[1]
# lognorm_sp <- inla(n_s ~ 1 + I(single_unit) + I(hu_med_val) + I(hu_ex_1000) +
#                      f(ID, model="iid") +
#                      f(ID2, model="besag",graph="../data/raw/seattle.graph"), data=seattle@data,
#                    family="poisson",E=solar_E, control.predictor=list(compute=TRUE))
# summary(lognorm_sp)
# lognorm_sp$summary.random 
# lognorm_sp$summary.linear.predictor
# 
# beta = lognorm_sp$summary.fixed[c(1, 3:5)]
# sigma_sp <- 1/sqrt(lognorm_sp$summary.hyperpar)[, -c(2,6)]
# rownames(sigma_sp) = c("SD for IID", "SD for spatial")
# rbind(beta, sigma_sp) %>% pander()

#' 50% chance that the proportion of the spatial variance, $\phi$ is greater 0.5 and 1% chance that the total residual standard deviation is greater than 0.3. 

lgnorm_sp <- inla(n_s ~ 1 + I(single_unit) + I(hu_med_val) + I(hu_ex_1000) + I(hu_own) + I(hu_no_mor) + I(hh_med_income) + I(hh_gini_index) + I(high_income) +
                    f(ID, model="bym2", graph="../data/raw/seattle.graph", scale.model=T, constr=T,
                      hyper=list(phi=list(prior="pc", param=c(0.5, 0.5), initial=1),
                                 prec=list(prior="pc.prec", param=c(0.3,0.01), initial=5))),
                  data=seattle@data, family="poisson", E=solar_E, control.predictor=list(compute=TRUE))
# summary(lgnorm_sp)
# lgnorm_sp$summary.random 
# lgnorm_sp$summary.linear.predictor

beta1 = lgnorm_sp$summary.fixed[c(1, 3:5)]
sigma_sp1 <- 1/sqrt(lgnorm_sp$summary.hyperpar)[1, -c(2,6)]
rownames(sigma_sp1) = "Total SD"
phi1 = lgnorm_sp$summary.hyperpar[2, -c(2,6)] 
#' ## BYM2 Model
rbind(beta1, sigma_sp1, phi1) %>% pander()

#' * `r exp(beta1[2,3]*0.5)` - 50% increase in the percentage in single unit may incur the amount increase in solar installation. 
#' * `r exp(beta1[3,3]*0.5)` - 50% increase in the percentage in house unit median value may incur the amount increase in solar installation. 
#' * `r exp(beta1[4,3]*0.5)` - 50% increase in the percentage in homeownership with more than $1,000 expenditure may incur the amount increase in solar installation. 
#'
#' ## (b) Relative risk estimates and comparison

Diff <- lgnorm_sp$summary.random$ID[1:135, 2] - lgnorm_sp$summary.random$ID[136:270, 2]
REsnonspat <- exp(Diff)
REsspat <- exp(lgnorm_sp$summary.random$ID[136:270, 5])
seattle@data$REsnonspat <- REsnonspat
seattle@data$REsspat <- REsspat
seattle@data$lnormsp_RE <- exp(lgnorm_sp$summary.random$ID[1:135, 5])

spplot(seattle, "REsnonspat", col.regions = coul, main="Non-spatial random effects(BYM2)")
spplot(seattle, "REsspat", col.regions = coul, main="Spatial random effects(BYM2)")
spplot(seattle, "lnormsp_RE", col.regions = coul, main="Total random effects or residual(BYM2)")
#' > Can I consider the total random effects to be residuals? 

seattle@data$lnormsp_RR <- exp(lgnorm_sp$summary.linear.predictor[4])[,1]
spplot(seattle, "lnormsp_RR", col.regions = coul, main="Lognormal-Spatial RRs(BYM2)")

par(mfrow=c(1,1))
plot(seattle@data$SMR_s, seattle@data$lnormsp_RR, xlab="SMRs", ylab="Lognormal-Spatial RRs")
abline(0, 1, col="red")

#' # 3. Clustering 
#' 
#' $$
#' \begin{aligned}
#' Y_i \sim \mbox{Poisson}(E_i \mbox{e}^{\beta_{0}})
#' \end{aligned}
#' $$
#' 
#' ## (a) Clustering of the residual of the poisson model using Moran's

seattle_nb <- poly2nb(seattle)
col.w <- nb2listw(seattle_nb, style= "W", zero.policy= T)
pois <- glm(n_s ~ 1, offset= log(solar_E), data= seattle@data, family= poisson)
seattleres <- residuals(pois, type= "pearson")
moran.test(seattleres, col.w) %>% pander()
#' > How can I get residulas from INLA for the moran's I test? 

seattle@data$res <- seattleres

#' ## (b) Clustering of the residual using Geary's 
geary.test(seattleres, col.w) %>% pander()

#' ## (c) Cluster detection based on $Y_i$ count 

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

#' # 4. Point data analysis

# View(install)

install$CompletedDate <- as.numeric(install$CompletedDate)

#+ echo= F, include=F, warning=F, message=F
sol <- as.geodata(install, coords.col = 4:3, data.col = 2)

summary(sol)

#+ echo= F, warning=F, message=F, fig.height= 5
plot(sol)

# cloudsol <- variog(sol,option="cloud")
# plot(cloudsol,ylab="Semi-variance",xlab="Distance",col="grey",cex=.3)
# title("Cloud semi-variogram")

#+ echo= F, include=F, warning=F, message=F
bin <- variog(sol, uvec=seq(0,0.1,0.001),trend=~sol$data)

par(mfrow= c(1,1))
#+ echo= F, warning=F, message=F
plot(bin,ylab="Semi-variance",xlab="Distance",cex=.5,col="blue")
title("Binned semi-variogram detrended")
#' > how to project to the different coordinates? how to address the very small variance cos I can't test any regressions on it such as MLE.

#' ## (b) Monte Carlo intervals 
#' 
#+ echo= F, include=F, warning=F, message=F
sol.env <- variog.mc.env(sol,obj=bin)

#+ echo= F, warning=F, message=F
plot(bin,env=sol.env,xlab="Distance",ylab="Semi-variance")
title("Monte Carlo envelopes")


#' ## 5. Regression - Moran's test 

va_glm_re <- glm(n_s ~ single_unit + hu_med_val + hu_ex_1000 + offset(log(solar_E)),
                 data= seattle@data, family= "poisson")

va_lm_re <- lm(SMR_s ~ single_unit + hu_med_val + hu_ex_1000,
               data= seattle@data)

lm.morantest(va_lm_re, col.w, alternative= "two.sided") %>% pander() # showing spatial dependence 
lm.morantest(va_glm_re, col.w, alternative= "two.sided") %>% pander() 

seattleres <- residuals(va_lm_re, type= "pearson")
seattlegres <- residuals(va_glm_re, type= "pearson")
moran.test(seattleres, col.w) %>% pander()
moran.test(seattlegres, col.w) %>% pander()
seattle@data$lmres <- seattleres
seattle@data$glmres <- seattlegres

#' ## 6. Spatial regression - Moran's test 
#' 

#' ### lagsar
spmod <- lagsarlm(SMR_s ~ single_unit + hu_med_val + hu_ex_1000, data= seattle@data, col.w)
summary(spmod)
moran.test(residuals(spmod, type= "pearson"), col.w) %>% pander()

#' ### stsls
sp2mod <- stsls(SMR_s ~ single_unit + hu_med_val + hu_ex_1000, data= seattle@data, col.w)
summary(sp2mod)
impacts(spmod, listw= col.w)
moran.test(residuals(sp2mod, type= "pearson"), col.w) %>% pander()

seattle@data$ols.res <- resid(va_lm_re)
seattle@data$sar.res <- resid(spmod)
seattle@data$sar2.res <- resid(sp2mod)

#' ### errorsar
err <- errorsarlm(SMR_s ~ single_unit + hu_med_val + hu_ex_1000, data= seattle@data, col.w)
summary(err) 
moran.test(residuals(err, type= "pearson"), col.w) %>% pander()
seattle@data$err.res <- resid(err)

#' ### GWR
#+ echo= F, include=F, warning=F, message=F
bw.ans <- bw.gwr(SMR_s ~ single_unit + hu_med_val + hu_ex_1000, data= seattle, approach= "CV", kernel= "bisquare")
#+ echo= F, include=F, warning=F, message=F
gwr.res <- gwr.basic(SMR_s ~ single_unit + hu_med_val + hu_ex_1000, data= seattle, bw= bw.ans, kernel= "bisquare")

#+ echo= F, warning=F, message=F
gwr.res
moran.test(gwr.res$SDF@data$residual, col.w) %>% pander()
seattle@data$gwr <- gwr.res$SDF@data$residual

spplot(seattle, "res", col.regions = coul, main="Residuals of poisson model\n (very small P value of Moran's I)")
spplot(seattle, "lmres", col.regions = coul, main="Residuals of OLS model with covariates\n (very small P value of Moran's I)")
spplot(seattle, "glmres", col.regions = coul, main="Residuals of Poission model with covariates\n (very small P value of Moran's I)")
spplot(seattle, "sar.res", col.regions = coul, main="Residuals of spatial lagsar regression with covariates\n (0.09887 P value of Moran's I)")
spplot(seattle, "sar2.res", col.regions = coul, main="Residuals of spatial stsls regression with covariates\n (0.04 P value of Moran's I)")
spplot(seattle, "err.res", col.regions = coul, main="Residuals of spatial errorsar regression with covariates\n (0.50 P value of Moran's I)")
spplot(seattle, "gwr", col.regions = coul, main="GWR residuals with covariates\n (0.06 P value of Moran's I)")

#+ echo= F, warning=F, message=F
par(mfrow= c(1,3))
hist(gwr.res$SDF$single_unit, main= "Single family")
abline(v= va_lm_re$coef[2], col= "red")

hist(gwr.res$SDF$hu_med_val, main= "Home median value")
abline(v= va_lm_re$coef[3], col= "red")

hist(gwr.res$SDF$hu_ex_1000, main= "Homewonership with higher ex")
abline(v= va_lm_re$coef[4], col= "red")

spplot(gwr.res$SDF, "single_unit", col.regions = coul, 
       at=as.numeric(quantile(gwr.res$SDF$single_unit,probs=seq(0,10,1)/10)), 
       main="Single family sensitivity")

spplot(gwr.res$SDF, "hu_med_val", col.regions = coul, 
       at=as.numeric(quantile(gwr.res$SDF$hu_med_val,probs=seq(0,10,1)/10)), 
       main="House median value sensitivity")

spplot(gwr.res$SDF, "hu_ex_1000", col.regions = coul, 
       at=as.numeric(quantile(gwr.res$SDF$hu_ex_1000,probs=seq(0,10,1)/10)), 
       main="Homeownership sensitivity")

#' ## 7. Analysis in detail
par(mfrow= c(1,1))
corrplot(cor(regrs_p), method = "ellipse")

#' ### Factor analysis

#+ echo= F, warning=F, message=F
fct <- seattle@data %>% 
  dplyr::select(single_unit, hu_own, hu_no_mor, hu_med_val, high_income, hh_med_income, hh_gini_index)
fa <- fa(fct,nfactors=3,rotate="promax",fm="ml")
fa.parallel(fct,fa="fa",n.iter=100)
fa.diagram(fa,simple=T)

factor.plot(fa, labels=rownames(fa$loadings))
dat <- as.data.frame(fa$scores)

fn_fa_plt(seattle@data[["SMR_s"]], dat)

seattle@data <- cbind(seattle@data, dat)

#' 50% chance that the proportion of the spatial variance, $\phi$ is greater 0.5 and 1% chance that the total residual standard deviation is greater than 0.3. 

fa_sp <- inla(n_s ~ 1 + I(ML1) + I(ML2) + I(ML3) +
                    f(ID, model="bym2", graph="../data/raw/seattle.graph", scale.model=T, constr=T,
                      hyper=list(phi=list(prior="pc", param=c(0.5, 0.5), initial=1),
                                 prec=list(prior="pc.prec", param=c(0.3,0.01), initial=5))),
                  data=seattle@data, family="poisson", E=solar_E, control.predictor=list(compute=TRUE))
# summary(fa_sp)
# fa_sp$summary.random 
# fa_sp$summary.linear.predictor

beta2 = fa_sp$summary.fixed[c(1, 3:5)]
sigma_sp2 <- 1/sqrt(fa_sp$summary.hyperpar)[1, -c(2,6)]
rownames(sigma_sp2) = "Total SD"
phi2 = fa_sp$summary.hyperpar[2, -c(2,6)] 
#' ## BYM2 Model
rbind(beta2, sigma_sp2, phi2) %>% pander()

#' * `r exp(beta2[2,3]*0.5)` - 50% increase in the percentage in single family homeonw factor may incur the amount increase in solar installation. 
#' * `r exp(beta2[3,3]*0.5)` - 50% increase in the percentage in economic status may incur the amount increase in solar installation. 
#' * `r exp(beta2[4,3]*0.5)` - 50% increase in the percentage in the area inequality may incur the amount increase in solar installation.
#' 
#' 

#' ### regression 

falm <- lm(seattle@data[["SMR_s"]] ~ dat[,1] + dat[,2])
falm %>% tidy() %>% pander()

faglm <- glm(seattle@data[["n_s"]] ~ offset(log(seattle@data[["solar_E"]])) + dat[,1] + 
      dat[,2], family= "poisson") 
faglm %>% pander()

moran.test(residuals(falm, type= "pearson"), col.w) %>% pander()
moran.test(residuals(faglm, type= "pearson"), col.w) %>% pander()
seattle@data$falm <- residuals(falm, type= "pearson")
seattle@data$faglm <- residuals(faglm, type= "pearson")
spplot(seattle, "falm", col.regions = coul, main="Factor linear residuals \n (small P value of Moran's I)")
spplot(seattle, "faglm", col.regions = coul, main="Factor poisson residuals \n (small P value of Moran's I)")

#' > How can I use lagsar, stsls, errorsar and gwr for poisson model? 
#'
#' ### cluster analysis
par(mfrow= c(1,1))
set.seed(5099)
kme <- kmeans(dat,center=3)
clusplot(dat, kme$cluster, color=TRUE,shade=TRUE, labels=5, lines=0)
g_perf_re 

#' ### regression 
lm(regrs[["SMR_s"]] ~ single_unit + hu_med_val + hu_ex_1000, data= regrs) %>% tidy() %>% pander()

min <- glm(n_s ~ single_unit + hu_med_val + hu_no_mor + offset(log(solar_E)), data= regrs, family= "poisson") 
summary(min)

# View(seattle@data)
seattle@data$clu <- as.factor(kme$cluster)
spplot(seattle, "clu", col.regions = c('indianred2', 'limegreen', 'dodgerblue2'), main="Cluster")

#+ echo= F, warning=F, message=F
fn_tot_plt(seattle@data, dat, "SMR_s")
fn_lm_plt(var_s, "SMR_s", dat, seattle@data)

save(lognorm_sp, lgnorm_sp, Kpoisson, Kcluster, coul, sol, va_glm_re, va_lm_re, seattle,
     file = "../data/derived/inla.Rdata")

