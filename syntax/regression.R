library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(pander)
library(stringr)
library(psych)
library (cluster)
library(reshape)
library(reshape2)
library(som)
library(GPArotation)
library(corrplot)
library(tibble)
library(MASS)

load("./data/drived/wrk.Rdata")
View(fin)
str(fin)

### cor plot
fct <- fin %>% 
  select(-geoid, -hu, -pop_total, -hu_blt2010, -`MWh/hh_low_mf_own`, -lihtc_qualified)

View(fct)
str(fct)
corrplot(cor(fct), method = "ellipse")

### parallel plot
fct <- fin %>% 
  select(-geoid, -hu, -pop_total, -hu_blt2010, -`MWh/hh_low_mf_own`, -lihtc_qualified, -sol_instl)

fa.parallel(fct,fa="fa",n.iter=50)

## FA analysis
fa <- fa(fct,nfactors=3,rotate="promax",fm="ml")

factor.plot(fa, labels=rownames(fa$loadings))
fa.diagram(fa,simple=T)

dat <- fa$scores
dim(dat)
dim(fin)
plot(dat[,1], fin[[16]], xlab = "The 1st factor", ylab = "Solar installation")
summary(lm(fin[[16]] ~ dat[,1] + dat[,2] + dat[,3]))

reg <- lm(fin[[16]] ~ dat[,1] + dat[,2] + dat[,3])
stepAIC(reg)


kmeans <- kmeans(dat,center=3)
summary(kmeans) # slots in kmeans object
kmeans$centers # centroids of clusters
table(kmeans$cluster) #number of samples in each cluster
Group1 <- kmeans$centers[1,]
Group2 <- kmeans$centers[2,]
plot(Group1,Group2,type="n")
text(Group1,Group2, labels=colnames(dat))
pairs(dat, col=kmeans$cluster)
clusplot(dat, kmeans$cluster, color=TRUE,shade=TRUE, labels=5, lines=0)

set.seed(5099)
sum(kmeans(dat,center=2)$withinss)
wss <- (nrow(dat)-1)*sum(apply(dat,2,var))
for (i in 2:9) wss[i] <- sum(kmeans(dat,centers=i)$withinss)
plot(1:9, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

library(rgl)
plot3d(fa$scores, col = kmeans$cluster)


reg <- lm(sol_instl ~ .,fin[-c(1,2,6,7,8,9)])
stepAIC(reg)

summary(lm(formula = sol_instl ~ hu_rnt + hu_med_val + 
             black, data = fin[-c(1, 2, 6, 7, 8, 9)]))
