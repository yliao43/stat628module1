rm(list=ls())
library(dplyr)
library(data.table)
library(ggplot2)
setwd("E:/UWM/STAT 628")
body=fread("Bodyfat.csv")
summary(body[,-c(1,3,7)])
ggplot(data=body)+
  geom_point(mapping = aes(x=IDNO,y=BODYFAT),position = "jitter",col="red",pch=18)+
  geom_hline(yintercept = 3,col="blue")+
  geom_text(x = 187,y = 0,label="182",size=3)+
  geom_text(x=177,y=1.9,label="172",size=3)

#1. data clean
##(1). adiposity(bmi) calculation inspection
test1=body %>% select(WEIGHT,HEIGHT,ADIPOSITY)
TEST1=test1 %>% mutate(adi=round(WEIGHT*703/HEIGHT^2,1),diff=adi-ADIPOSITY)
TEST1$index=1:252
ggplot(data=TEST1)+
  geom_point(mapping = aes(x=index,y=diff),col="red",pch=18)+
  geom_hline(yintercept = 0,col="blue")+
  geom_text(x = 46,y = 135.7,label="42",size=3)+
  geom_text(x=167,y=3,label="163",size=3)+
  geom_text(x=225,y=-2.8,label="221",size=3)
### patient #42 is obviously wrong data, and #163 and #221 may also be wrong data due to the wrong bmi calculation.
##(2). bodyfat fill up
test2=body %>% select(BODYFAT,DENSITY)
TEST2=test2 %>% mutate(bodyf=round(495/DENSITY-450,1),diff=BODYFAT-bodyf)
TEST2$index=1:252
ggplot(data=TEST2)+
  geom_point(mapping = aes(x=index,y=diff),col="red",pch=18)+
  geom_hline(yintercept = 0,col="blue")+
  geom_text(x = 100,y = 16.9,label="96",size=3)+
  geom_text(x=52,y=-7.7,label="48",size=3)+
  geom_text(x=80,y=4.2,label="76",size=3)
TEST2[c(96,48,76),]
body[c(96,48,76),]
### the difference may come from the instrumental error, but #96, #48, #76 may be wrong calculation, and #182 has
### 0 body fat which should be filled up.
##(3). other weird indicator
d=c(body[,8],body[,9],body[,10],body[,11],body[,12],body[,13],body[,14],body[,15],body[,16],body[,17])
type=c(rep("neck",252),rep("chest",252),rep("abdomen",252),rep("hip",252),rep("thigh",252),rep("knee",252),rep("ankle",252),rep("biceps",252),rep("forearm",252),rep("wrist",252))
type2=c(rep(1:10,times=1,each=252))
type2=as.numeric(type2)
d =unlist(d)
names(d)=NULL
dd=data.frame(circumference=as.numeric(d),type=type)
ddd=data.frame(type=as.numeric(type2),circumference=as.numeric(d))
ggplot(dd, aes(type,circumference,  fill=type)) +
  geom_boxplot() + facet_grid(.~type, scales = "free_x")
ggplot(data=ddd)+
  geom_jitter(mapping=aes(ddd[,1],ddd[,2],color=type))+
  labs(colour="Part")+
  labs(title="circumsference of different parts")+
  labs(x="different parts")+
  labs(y="circumference")+
  theme(axis.text.x = element_text(angle=45))

##(NECK %>% order(decreasing = T))[1]
##(CHEST %>% order(decreasing = T))[1:2]
##(ABDOMEN %>% order(decreasing = T))[1:3]
##(HIP %>% order(decreasing = T))[1:2]
##(KNEE %>% order(decreasing = T))[1]
##(THIGH %>% order(decreasing = T))[1:3]
##(ANKLE %>% order(decreasing = T))[1:3]
##(BICEPS %>% order(decreasing = T))[1]
##(FOREARM %>% order(decreasing = F))[1:3]
##(WRIST %>% order(decreasing = T))[1:3]
body[39,] ### outliers in neck,chest,abdomen,hip,knee,thigh,ankle,wrist,biceps
### this is an extra-obese guy, correct, but may has low contribution to the model.
body[41,] ### outliers in chest,abdomen,hip,wrist
### still an extra-obese guy, correct, may has low contribution too.
body[216,] ### abdomen
body[169,] ### thigh
body[152,] ### thigh,forearms
body[86,] ### ankle
body[31,] ### ankle
### this data is weird, this guy has normal body, 172cm tall, 75kg weight, but his ankle has circumference 33.7cm, how
### could a guy has his thigh and knee thiner than average, but has an extra-big ankle?
body[175,] ### forearms
###this guy is also weird, he is 182cm tall, 103kg weight, but his forearms has circumference only 21cm.
body[226,] ### forearms
body[45,] ### forearms
body[252,] ### wrist
##### From this step, we found 2 low contribution guy #39 and #41, and 2 weird data, #86 has extra-big ankle and #175
##### has extra-thin forearm.
strange=body[c(42,163,221,96,48,76,182,172,39,41,86,175),]
strange$REMARK=c("bmi:rewrite height","bmi:wrong cal","bmi:wrong cal","bodyf:wrongcal","bodyf:wrongcal","bodyf:wrongcal",
                 "bodyf:fillup","bodyfat:wrong","extrafat","extrafat","bigankle","thinforearm")
strange

