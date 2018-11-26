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
library(ggplot2)
library(som)
library(GPArotation)
library(corrplot)
library(tibble)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(lmtest)
library(MASS)

potential <- read_csv(file = "./data/drived/potential.csv")

View(potential)
str(potential)

fct <- potential %>% 
  select(-geoid, -hu, -pop_total, -hu_blt2010, -`MWh/hh_low_mf_own`, -lihtc_qualified, -sol_instl)

View(fct)
str(fct)
corrplot(cor(fct), method = "ellipse")

fa.parallel(fct,fa="fa",n.iter=50)
fa <- fa(fct,nfactors=3,rotate="promax",fm="ml")
fa

factor.plot(fa, labels=rownames(fa$loadings))
fa.diagram(fa,simple=T)
dat <- fa$scores
dim(dat)

dim(potential)
plot(dat[,1], potential[[16]])

summary(lm(potential[[16]] ~ dat[,1] + dat[,2] + dat[,3]))


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


reg <- lm(sol_instl ~ .,potential[-c(1,2,6,7,8,9)])
stepAIC(reg)

summary(lm(formula = sol_instl ~ hu_rnt + hu_med_val + 
             black, data = potential[-c(1, 2, 6, 7, 8, 9)]))

