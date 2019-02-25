library(readr)
library(ggplot2)
library(rstanarm)
library(lme4)
lgd<-read_csv("~/Lost_grove/Facebook_Spending.csv")
lgd$Spent[is.na(lgd$Spent)] <- 0
lgd$`Co-Hosts`[is.na(lgd$`Co-Hosts`)] <- 0
lgd2<-lgd[c(2:6,19:22)]
names(lgd2)[2:9]<-c("Going","Interested","Views","Reached","Type","Co_hosts","Spent","Sales")
#lgd2<-lgd2[-5]
lgd2<-na.omit(lgd2)
lgd2$multi_host<-as.logical(ifelse(lgd2$Co_hosts == 0,0,1))
lgd2<-lgd2[1:106,]
lgd2$paid<-as.logical(ifelse(lgd2$Spent == 0,0,1))
#try to remove outlier
lgd2<-lgd2[-17,]
#for a gamma distributionwith log-link use exp to interpret parameters
fit<-glm(Sales~Interested,family=Gamma(link="log") ,data=lgd2 )
summary(fit)
coef(fit)
fit<-glm(Sales~Spent+Reached+Type,family=Gamma(link="log") ,data=lgd2)
loo(fit)
AIC(fit)
#The gamma model appears to be better
summary(fit)
plot(Sales~Reached,data=lgd2)
ggplot(data=lgd2,aes(x=Views, y=Sales))+
  scale_x_log10()+
geom_point()+
geom_smooth()+theme_classic()
summary(fit)

ggplot(data=na.omit(lgd2),aes(x=multi_host,y=Sales))+geom_boxplot()+theme_classic()
ggplot(data=lgd2,aes(x=Type,y=Sales))+geom_boxplot()+theme_classic()

