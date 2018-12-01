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
# library(MASS)
library(GGally)

## MASS and dplyr clash each other for select()

load("./data/derived/prj1.Rdata")
load("./data/derived/prj3.Rdata")

### cor plot
regr_p <- regr %>% 
  select(-geoid, -lihtc)

View(regr_p)
str(regr_p)
corrplot(cor(regr_p), method = "ellipse")

### parallel plot
fct <- regr%>% 
  select(-geoid, -sol_instl, -lihtc)

fa.parallel(fct,fa="fa",n.iter=50)


## FA analysis
fa <- fa(fct,nfactors=3,rotate="promax",fm="ml")

factor.plot(fa, labels=rownames(fa$loadings))
fa.diagram(fa,simple=T)

dat <- fa$scores
dim(dat)
plot(dat[,1], regr[[14]], xlab = "The 1st factor", ylab = "Solar installation")
summary(lm(regr[[14]] ~ dat[,1] + dat[,2] + dat[,3]))

reg <- lm(regr[[14]] ~ dat[,1] + dat[,2] + dat[,3])
# stepAIC(reg)

## cluster 
set.seed(5099)
kme <- kmeans(dat,center=3)
summary(kmeans) # slots in kmeans object
kmeans$centers # centroids of clusters
table(kme$cluster) #number of samples in each cluster
Group1 <- kmeans$centers[1,]
Group2 <- kmeans$centers[2,]
plot(Group1,Group2,type="n")
text(Group1,Group2, labels=colnames(dat))
pairs(dat, col=kme$cluster)
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
ggplot(perf_df,aes(x=number_of_center,y=metrics)) +
  geom_point(alpha=0.2) +
  geom_vline(xintercept = 3,color='red')

library(rgl)
plot3d(fa$scores, col = kmeans$cluster)

## It shows "green" is in the less benefit situation. "Red" is medium wealthy while lower inequility. "Blue" is the most wealthy and higher inequilty. 

dat <- as.data.frame(dat)
dat$cluster=as.factor(kme$cluster)
ggpairs(dat, mapping=aes(color=cluster))+ 
  theme_bw()

ggpairs(dat, columns = 1:3, 
        aes(color=cluster, alpha=0.4), 
        title="Scatterplot Matrix",
        upper=list(continuous="density", combo="box"),
        lower=list(continuous="smooth", combo="dot")) +
  theme_light() +
  theme(plot.title=element_text(size=10))+ 
  theme_bw()

c_reg <- regr
c_reg$cluster <- as.factor(kme$cluster)
View(c_reg)

## regression 
reg <- lm(sol_instl ~ .,regr[-c(1)])
# stepAIC(reg)

summary(lm(formula = sol_instl ~ hu_med_val + hu_ex_1000,
           data = regr[-c(1)]))

## data analysis
c_reg %>% 
  ggplot(aes(x = cluster, y = sol_instl, color = cluster)) +
  geom_boxplot() +
  ggtitle("Solar installation pattern per cluster")+ 
  theme_bw()

c_reg %>% 
  ggplot(aes(x = hu_ex_1000, y = sol_instl, color = cluster, size = lihtc)) +
  geom_point() +
  ggtitle("Solar installation pattern per cluster")+ 
  theme_bw()

c_reg %>% 
ggplot(aes(x = hu_med_val, y = sol_instl, color = cluster)) +
  geom_point(alpha = 0.4)+
  geom_smooth(span = 0.9)+ 
  theme_bw()
