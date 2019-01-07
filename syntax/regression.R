library(tidyverse) # includes all the packages below 
# library(readr)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(tibble)
# library(forcats)
# library(stringr)
library(lubridate)
library(pander)
library(psych)
library (cluster)
library(reshape)
library(reshape2)
library(som)
library(GPArotation)
library(corrplot)
library(GGally)
# library(MASS)
library(faraway)
library(sjPlot)

## MASS and dplyr clash each other for select()
load("./data/derived/ts.Rdata")
load("./data/derived/ets.Rdata")

po <- ts %>% 
  filter(year == "2018-01-01") %>% 
  select(geoid, sum)

poe <- temp_spatiale %>% 
  group_by(geoid) %>% 
  summarise(tot = max(sum))


regrs <- regr %>% 
  left_join(po, by = "geoid") %>% 
  left_join(poe, by = "geoid") %>% 
  mutate(solar = sum * 100/ hu,
         ev = tot * 100/ hu) %>% 
  select(-sum, -hu, -tot)

regrs$solar[is.na(regrs$solar)] <- 0
regrs$ev[is.na(regrs$ev)] <- 0 

# View(regrs)

### cor plot
regrs_p <- regrs %>% 
  select(-geoid, -solar, -lihtc, -L_HOOD,-hh_low_mf_rent, -non_us, -hu_blt1979, - hu_mwh,
         -edu, -wh_race, -af_race)

# View(regrs_p)
# str(regrs_p)
corrplot(cor(regrs_p), method = "ellipse")

### parallel plot
fct <- regrs%>% 
  select(-geoid, -solar, -lihtc, -L_HOOD,-hh_low_mf_rent, -non_us, -hu_blt1979, - hu_mwh,
         -edu, -wh_race, -af_race)

fa.parallel(fct,fa="fa",n.iter=100)


## FA analysis
fa <- fa(fct,nfactors=3,rotate="promax",fm="ml")

factor.plot(fa, labels=rownames(fa$loadings))
fa.diagram(fa,simple=T)

dat <- as.data.frame(fa$scores)
# dim(dat)

par(mfrow=c(1,2))
plot(dat[,1], regrs[["solar"]], xlab = "ML2", ylab = "Solar installation")
abline(lm(regrs[["solar"]] ~ dat[,1]), col = "red")

plot(dat[,2], regrs[["solar"]], xlab = "ML1", ylab = "Solar installation")
abline(lm(regrs[["solar"]] ~ dat[,2]), col = "red")

fa_lm_re <- lm(regrs[["solar"]] ~ dat[,1] + dat[,2] + dat[,3]) %>% 
  tab_model()


# ## Leverage plot, Residual plot, and Half normal plot of square-root of Cook's D
# par(mfrow=c(1,3))
# reg <- lm(regrs[[14]] ~ dat[,1] + dat[,2] + dat[,3])
# plot(hatvalues(reg), ylab="leverage",main="Leverage plot", ylim=c(0,.05))
# p=3;n=length(regrs[[14]]);
# abline(h=2*((p+1)/n),lwd=0.5,col=2)
# 
# sigma = summary(reg)$sigma
# min_rstudent = min(rstudent(reg))
# max_rstudent = max(rstudent(reg))
# plot(rstudent(reg), ylim = c(min(-5, min_rstudent), max(5, max_rstudent)),
#      ylab="rstudent",main="rstudent residual plot")
# abline(h=c(-3, 3),lwd=0.5,col=2)
# 
# halfnorm(sqrt(cooks.distance(reg)), xlim=c(0,5), nlab = 4,
#          ylab=("square root Cook's D"), main="Half normal plots of square-root of Cook's D")
# 
# ## normality plot
# par(mfrow=c(1,2))
# qqnorm(rstudent(reg), datax = TRUE,
#        xlab = "normal quantile",
#        ylab = "rstudent", 
#        main = "normal probability plot for rstudent")
# qqline(rstudent(reg), datax=TRUE, col = 2)
# d <- density(rstudent(reg), adjust = 1, na.rm = TRUE)
# plot(d, type = "n", xlim=c(-5,5), main="KDE for rstudent residuals and N(0,1) density") 
# polygon(d, col = "wheat")
# z = seq(from=-5,to=5,by=.01)
# lines(z,dnorm(z), lty=2,lwd=3,col="red")
# 
# ## variance plot 
# par(mfrow=c(1,1))
# plot(fitted(reg), abs(rstudent(reg)),
#      xlab = "fitted values", ylab = "abs(rstudent)",
#      main = "Absolute residual plot with a smoother")
# smoother = loess(abs(rstudent(reg)) ~ fitted(reg))
# ord = order(fitted(reg)) # why do we need this line? 
# lines(fitted(reg)[ord], fitted(smoother)[ord], col="red", lwd=2)
# 
# par(mfrow=c(1,3))
# preds = list(dat[,1], dat[,2], dat[,3])
# for (p in 1:3) {
#   X = preds[[p]]
#   plot(X, rstudent(reg),
#        ylab = "rstudent")
#   smoother = loess(rstudent(reg) ~ X); ord = order(X)
#   lines(X[ord], fitted(smoother)[ord], col="red", lwd=2)
# }


## cluster 
set.seed(5099)
kme <- kmeans(dat,center=3)
# summary(kmeans) # slots in kmeans object
# kmeans$centers # centroids of clusters
# table(kme$cluster) #number of samples in each cluster
# Group1 <- kmeans$centers[1,]
# Group2 <- kmeans$centers[2,]
# plot(Group1,Group2,type="n")
# text(Group1,Group2, labels=colnames(dat))
# pairs(dat, col=kme$cluster)
clusplot(dat, kme$cluster, color=TRUE,shade=TRUE, labels=5, lines=0)

## 1. optimal cluster number
sum(kme$withinss)
wss <- (nrow(dat)-1)*sum(apply(dat,2,var))
for (i in 2:9) wss[i] <- sum(kmeans(dat,centers=i)$withinss)
plot(1:9, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

## 2. optimal cluster number
performance=c()
for (i in rep(1:100,times=30)) {
  clust=kmeans(dat,i)
  performance=c(performance,1-clust$tot.withinss/clust$totss)
}
perf_df=data.frame(metrics=performance,number_of_center=rep(1:100,times=30))
g_perf_re <- ggplot(perf_df,aes(x=number_of_center,y=metrics)) +
  geom_point(alpha=0.2) +
  geom_vline(xintercept = 3,color='red')

# library(rgl)
# plot3d(fa$scores, col = kmeans$cluster)

dat1 <- dat
dat1$Sol <- regrs[["solar"]]
dat1$Cluster=as.factor(kme$cluster)
g_pair_re <- ggpairs(dat1, mapping=aes(color=Cluster))+ 
  theme_bw()

g_pair1_re <- ggpairs(dat1, columns = 1:4, 
        aes(color=Cluster, alpha=0.4), 
        title="Scatterplot Matrix",
        upper=list(continuous="density", combo="box"),
        lower=list(continuous="smooth", combo="dot")) +
  theme_light() +
  theme(plot.title=element_text(size=10))+ 
  theme_bw()

c_reg <- regrs
c_reg$cluster <- as.factor(kme$cluster)
View(c_reg)

## regrsession 
# reg <- lm(solar ~ .,regrs[-c(1,14)])
# MASS::stepAIC(reg)
# summary(lm(formula = solar ~ hu_ex_1000 + ev, data = regrs[-c(1, 14)]))

va_lm_re <- lm(formula = solar ~ hu_ex_1000 + ev, data = regrs[-c(1, 14)]) %>% 
  tab_model()

## data analysis
g_sol_re <- c_reg %>% 
  ggplot(aes(x = cluster, y = solar, color = cluster)) +
  geom_boxplot() +
  ggtitle("Solar installation pattern per cluster")+ 
  theme_bw()

g_ef1_re <- c_reg %>% 
  ggplot(aes(x = hu_ex_1000, y = solar, color = cluster)) +
  geom_point(alpha = 0.4)+
  geom_smooth(span = 0.9)+ 
  theme_bw()

g_ef2_re <- c_reg %>% 
  ggplot(aes(x = ev, y = solar, color = cluster)) +
  geom_point(alpha = 0.4)+
  geom_smooth(span = 0.9)+ 
  theme_bw()

save(regrs, regrs_p, fct, fa, dat, dat1, kme, va_lm_re, fa_lm_re, 
     wss, g_perf_re, g_pair_re, g_pair1_re, g_sol_re, g_ef1_re, g_ef2_re, 
     file = "./data/derived/reg.Rdata")

load("./data/derived/reg.Rdata")
