install.packages("corrplot")
install.packages("GGally")
library(corrplot)
library(RColorBrewer)
library(GGally)
library(tidyverse)
library(gridExtra)
library(dslabs)
library(magrittr)
library(dplyr)
library(ggplot2)
library(plyr)
library(MASS)
library(nnet)
library(plyr)
library(corrplot)

#Read file and take a look
data<-read.csv(file="googleplaystore.csv")
names(data)
head(data)
str(data)

# Exploratory analysis using graph
# Histogram of Rating
ggplot((data),aes(Rating))+geom_histogram()

#Correlation Table
data.clean<-data%>%
  mutate(Reviews=as.numeric(Reviews),Price=as.numeric(Price),Size=as.numeric(Size),Day.Since.Last.Update=as.numeric(Day.Since.Last.Update))
data.clean1<-data.clean[,c(2,3,6,8,9)]
data.clean2<-na.omit(data.clean1)
M<-cor(data.clean2)
corrplot(M)

#Rating against category
ggplot(data,aes(Category,Rating,fill=Category))+
  geom_boxplot()+theme(axis.text.x = element_text(angle=90))+
  theme(axis.text.x=element_blank(),
        axis.title=element_text(size=14,face="bold"))

#Rating against Content Rating
ggplot(data,aes(Content.Rating,Rating,fill=Content.Rating))+
  geom_boxplot()+theme(
    axis.text.x = element_text(angle=60))+
  theme(legend.position="none",
    axis.title.x = element_text(color="black", size=18, face="bold"),
    axis.title.y = element_text(color="black", size=18, face="bold"),
    axis.text.x = element_text(face = "bold", color = "black", 
                               size = 12, angle = 0))
      
#Rating against Installs
ggplot(data,aes(Installs,Rating,fill=Installs))+
  geom_boxplot()+theme(axis.text.x = element_text(angle=90))+
  theme(legend.position="none",
        axis.title.x = element_text(color="black", size=18, face="bold"),
        axis.title.y = element_text(color="black", size=18, face="bold"),
        axis.text.x = element_text(face = "bold", color = "black", 
                                   size = 15, angle = 90))

#Rating against Price
ggplot(data,aes(Price,Rating))+
  geom_point()+
  stat_smooth(method="lm",se = FALSE)+
  theme(axis.title.x = element_text(color="black", size=18, face="bold"),
        axis.title.y = element_text(color="black", size=18, face="bold"))

#Rating against Reviews
ggplot(data,aes(Reviews,Rating))+
  geom_point()+
  stat_smooth(method="lm",se = FALSE)+
  theme(axis.title.x = element_text(color="black", size=18, face="bold"),
        axis.title.y = element_text(color="black", size=18, face="bold"))

#Rating against log(reviews)
ggplot(data,aes(log(Reviews),Rating))+
  geom_point()+
  stat_smooth(method="lm",se = FALSE)+
  theme(axis.title.x = element_text(color="black", size=18, face="bold"),
        axis.title.y = element_text(color="black", size=18, face="bold"))

#Rating against Day Since Last Update
ggplot(data,aes(Day.Since.Last.Update,Rating))+
  geom_point()+
  stat_smooth(method="lm",se = FALSE)+
  theme(axis.title.x = element_text(color="black", size=18, face="bold"),
        axis.title.y = element_text(color="black", size=18, face="bold"))

#Rating against Size
ggplot(data,aes(Size,Rating))+xlim(1,100)+
  geom_point()+
  stat_smooth(method="lm",se = FALSE)+
  theme(axis.title.x = element_text(color="black", size=18, face="bold"),
        axis.title.y = element_text(color="black", size=18, face="bold"))



#Naive Regression
lm1=lm(Rating~Category+Installs+Content.Rating+Price+Size+Reviews+Day.Since.Last.Update,data=data)
summary(lm1)
AIC(lm1)

#Log Regression
lm2=lm(Rating~Category+Installs+Content.Rating+Price+Size+log(Reviews)+Day.Since.Last.Update,data=data)
summary(lm2)
AIC(lm2)

#Backward Selection
lm3=lm(Rating~Category+Installs+Content.Rating+Price+Size+log(Reviews)+Day.Since.Last.Update,data=data)
summary(lm3)
AIC(lm3)

# Observe Content Rating has high P value, Drop it
lm4=lm(Rating~Category+Installs+Price+Size+log(Reviews)+Day.Since.Last.Update,data=data)
summary(lm4)
AIC(lm4)

# Observe price has high P value, Drop it
lm5=lm(Rating~Category+Installs+Size+log(Reviews)+Day.Since.Last.Update,data=data)
summary(lm5)
AIC(lm5)
#All variables are significant, so we stop

#Automatic Selection
fit.nothing<-lm(Rating~1,data=data)
summary(fit.nothing)
fit.all<-lm(Rating~Category+Installs+Content.Rating+Price+Size+log(Reviews)+Day.Since.Last.Update,data=data)
summary(fit.all)

step(fit.all,direction  = 'backward', scope =formula('fit.nothing'))
lm6=step(fit.all,direction  = 'backward', scope =formula('fit.nothing'))
summary(lm6)
AIC(lm6)

lm7<-lm(Rating~as.factor(Category)+as.factor(Installs)+Size+log(Reviews)+Day.Since.Last.Update,data=data)
summary(lm7)


