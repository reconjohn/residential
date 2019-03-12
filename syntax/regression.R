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
load("./data/derived/iv.Rdata")

po <- ts %>% 
  filter(year == "2018-01-01") %>% 
  select(geoid, sum) %>% 
  mutate(n_s = sum) %>% 
  select(-sum)

poe <- temp_spatiale %>% 
  group_by(geoid) %>% 
  summarise(n_e = max(sum)) 

N = sum(regr$hu) # total number of hh in Seattle
P = sum(po$n_s, na.rm = T)/ N # total number of solar/ N
Pe = sum(poe$n_e, na.rm = T)/ N # total number of EV/ N

regrs <- regr %>% 
  left_join(po, by = "geoid") %>% 
  left_join(poe, by = "geoid") %>%
  mutate(n_s = ifelse(is.na(n_s), 0.5, n_s),
         solar_E = ifelse(is.na(n_s), hu*P+0.5, hu*P),
         SMR_s = n_s/ solar_E,
         se_SMR_s = sqrt(SMR_s/ solar_E),
         EV_E = ifelse(is.na(n_e), hu*Pe+0.5, hu*Pe),
         n_e = ifelse(is.na(n_e), 0.5, n_e),
         SMR_e = n_e/ EV_E,
         se_SMR_e = sqrt(SMR_e/ EV_E))

# View(regrs)
plot(regrs$se_SMR_s, regrs$SMR_s)
plot(regrs$se_SMR_e, regrs$SMR_e)

## cor plot
regrs_p <- regrs %>% 
  select(-geoid, -solar_E, -EV_E, -lihtc, -L_HOOD, -hh_low_mf_rent, -non_us, 
         -hu_no_mor, -hu_blt1979, - hu_mwh, -edu, -wh_race, -af_race, -hu, -hh_high_sf_own, 
         -n_s, -n_e, -se_SMR_s, -se_SMR_e, -SMR_e)

# View(regrs_p)
# str(regrs_p)
corrplot(cor(regrs_p), method = "ellipse")


## Parallel plot
fct <- regrs%>% 
  select(-geoid, -SMR_s, -SMR_e, -solar_E, -EV_E, -lihtc, -L_HOOD, -hh_low_mf_rent, -hh_high_sf_own,-non_us, -hu_ex_1000, -hu_blt1979, -hu_mwh, -edu, -wh_race, -af_race,-hu, -n_s, -n_e, -se_SMR_s, -se_SMR_e)

fa.parallel(fct,fa="fa",n.iter=100)


## FA analysis
fa <- fa(fct,nfactors=3,rotate="promax",fm="ml")

factor.plot(fa, labels=rownames(fa$loadings))
fa.diagram(fa,simple=T)

dat <- as.data.frame(fa$scores)
# dim(dat)


##### Solar 
# SMR: vector
# dat: FA score dataframe

fn_fa_plt <- function(SMR, dat){
  par(mfrow=c(1,3))
  ML_ord = c("ML1", "ML2", "ML3")
  if (str_detect(deparse(substitute(SMR)), "SMR_s")){
    for(i in seq_along(dat)){
      plot(dat[,i], SMR, xlab = ML_ord[i], 
           ylab = expression("Solar installation ("~Y[i]/E[i]~")"))
      abline(lm(SMR ~ dat[,i]), col = "red")
      }
    } else {
      for(i in seq_along(dat)){
        plot(dat[,i], SMR, xlab = ML_ord[i], 
             ylab = expression("EV charger installation ("~Y[i]/E[i]~")"))
        abline(lm(SMR ~ dat[,i]), col = "red")
    }
  }
}

# fn_fa_plt(regrs[["SMR_s"]], dat)

## FA regression
fa_lm_re <- lm(regrs[["SMR_s"]] ~ dat[,1] + dat[,2]) %>% 
  tab_model()

fa_glm_re <- glm(regrs[["n_s"]] ~ offset(log(regrs[["solar_E"]])) + dat[,1] + 
                   dat[,2], family= "poisson") %>% 
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

## Regrsession 
# names(regrs)
# reg <- lm(SMR_s ~ hu + hu_own + single_unit+hu_blt1979+hu_no_mor+hu_med_val+hu_ex_1000+edu+wh_race+af_race+non_us+hh_med_income+hh_gini_index+lihtc+high_income, data= regrs)
# MASS::stepAIC(reg)
# reg <- lm(SMR_s ~ single_unit + hu_med_val + hu_ex_1000, data = regrs)
reg <- glm(n_s ~ single_unit + hu_med_val + hu_ex_1000 + offset(log(solar_E)), data= regrs, family= "poisson")
# summary(reg)

va_glm_re <- glm(n_s ~ single_unit + hu_med_val + hu_ex_1000 + offset(log(solar_E)),
                 data= regrs[-c(1,2,15)], family= "poisson") %>% 
  tab_model()

## Plot
fn_tot_plt <- function(regrs, dat, SMR){
  set.seed(5099)
  kme <- kmeans(dat,center=3)
  dat1 <- dat
  dat1$SMR <- regrs[[SMR]]
  dat1$cluster=as.factor(kme$cluster)
  ef <- list()
  for(i in 1:(length(names(dat1))-2)){
    ef[[i]] <- dat1 %>% 
      ggplot(aes(x = !!sym(names(dat1)[i]), y = SMR, color = cluster)) +
      geom_point(alpha = 0.4)+
      geom_smooth(span = 0.9)+ 
      theme_bw()
  }
  
  pair <- ggpairs(dat1, mapping=aes(color=cluster))+ 
    theme_bw()
  
  pair1 <- ggpairs(dat1, columns = 1:4, 
                        aes(color=cluster, alpha=0.4), 
                        title="Scatterplot Matrix",
                        upper=list(continuous="density", combo="box"),
                        lower=list(continuous="smooth", combo="dot")) +
    theme_light() +
    theme(plot.title=element_text(size=10))+ 
    theme_bw()

  return(list(ef, pair, pair1))
}
# fn_tot_plt(regrs, dat, "SMR_s")

fn_lm_plt <- function(var, SMR, dat, regrs){
  set.seed(5099)
  kme <- kmeans(dat,center=3)
  c_reg <- regrs
  c_reg$cluster <- as.factor(kme$cluster)
  
  clu <- c_reg %>% 
    ggplot(aes(x = cluster, y = !!sym(SMR), color = cluster)) +
    geom_boxplot() +
    ggtitle("Installation pattern per cluster")+ 
    theme_bw()
  
  ef <- list()
  for(i in seq_along(var)){
    ef[[i]] <- c_reg %>% 
      ggplot(aes(x = !!sym(var[i]), y = !!sym(SMR), color = cluster)) +
      geom_point(alpha = 0.4)+
      geom_smooth(span = 0.9)+ 
      theme_bw()
  }
  return(list(clu, ef))
}

var_s <- c("single_unit", "hu_med_val", "hu_ex_1000")
# fn_lm_plt(var_s, "SMR_s", dat, regrs)


##### EV
# fn_fa_plt(regrs[["SMR_e"]], dat)

## FA regression
fa_lme_re <- lm(regrs[["SMR_e"]] ~ dat[,1] + dat[,2] + dat[,3]) %>% 
  tab_model()

fa_glme_re <- glm(regrs[["n_e"]] ~ offset(log(regrs[["EV_E"]])) + dat[,1] + 
                   dat[,2] + dat[,3], family= "poisson") %>% 
  tab_model()

## Plots
# fn_tot_plt(regrs, dat, "SMR_e")
var_e <- c("single_unit","hh_med_income", "hh_gini_index")
# fn_lm_plt(var_e, "SMR_e", dat, regrs)

## Regrsession 
# names(regrs)
# rege <- lm(SMR_e ~ hu + hu_own + single_unit+hu_blt1979+hu_no_mor+hu_med_val+hu_ex_1000+edu+wh_race+af_race+non_us+hh_med_income+hh_gini_index+lihtc+high_income, data= regrs)
# MASS::stepAIC(rege)
# rege <- lm(SMR_e ~ single_unit + hh_med_income + hh_gini_index, data = regrs)
rege <- glm(n_e ~ single_unit + hh_med_income + hh_gini_index + offset(log(EV_E)),
            data= regrs[-c(1,2,15)], family= "poisson")
# summary(rege)

va_glme_re <- glm(n_e ~ single_unit + hh_med_income + hh_gini_index + offset(log(EV_E)),
                  data= regrs[-c(1,2,15)], family= "poisson") %>% 
  tab_model()


save(regrs, regrs_p, fct, fa, dat, kme, g_perf_re, fa_lm_re, fa_glm_re, va_glm_re,
     va_glme_re, fa_lme_re, fa_glme_re, fn_fa_plt, fn_tot_plt, fn_lm_plt, var_e, var_s,
     file = "./data/derived/reg.Rdata")

load("./data/derived/reg.Rdata")
