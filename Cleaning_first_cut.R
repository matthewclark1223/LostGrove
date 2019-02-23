library(readr)
library(ggplot2)
lgd<-read_csv("~/Lost_grove/Facebook_Spending.csv")
lgd$Spent[is.na(lgd$Spent)] <- 0
lgd$`Co-Hosts`[is.na(lgd$`Co-Hosts`)] <- 0
lgd2<-lgd[c(2:6,19:22)]
names(lgd2)[2:9]<-c("Going","Interested","Views","Reached","Type","Co_hosts","Spent","Sales")
lgd2<-lgd2[-5]
lgd2<-na.omit(lgd2)
fit<-glm(Sales~Spent+Views, data=lgd2 )
summary(fit)
plot(Sales~Spent,data=lgd2)
ggplot(data=lgd2,aes(x=Views, y=Sales))+
  scale_x_log10()+
geom_point()+
geom_smooth(method=lm)+theme_classic()
2+2