library(dplyr)
bodyfat = read.csv("BodyFat.csv")
colnames(bodyfat)
bodyfat = bodyfat[,-1]
#bodyfat = bodyfat %>% mutate(siri=495/DENSITY-450)
#head(bodyfat$siri)
#View(bodyfat)
#str(bodyfat)
bodyfat = bodyfat[-c(42,163,221,96,48,76,182,39,41,86,175,172),]
dim(bodyfat)

### STEPWISE MODEL SELECTION
model1 = lm(BODYFAT~.,data=subset(bodyfat,select =- DENSITY))

### AIC CRITERIA
model1.aic = step(model1,k=2)
summary(model1.aic)

### MSE
fit=predict(model1.aic,data=bodyfat)
sum((fit-bodyfat$BODYFAT)^2)/240

### BIC CRITERIA
model1.bic = step(model1,k=log(240))
summary(model1.bic)

### MSE
fit=predict(model1.bic,data=bodyfat)
sum((fit-bodyfat$BODYFAT)^2)/240


### Diagnosis
pairs(BODYFAT~WEIGHT + ABDOMEN + WRIST, data=bodyfat) #look at the trend

# identify outliers
par(mfrow = c(2,1))
leverage = hat(model.matrix(model1.bic))
plot(leverage,type="p",pch=23,bg="red",cex=1.2,
     xlab="Index (Each Observation)",ylab="Pii",main="Influence Values (Pii)")

cook = cooks.distance(model1.bic)
plot(cook,type="p",pch=23,bg="red",cex=1.2,
     xlab="Index (Each Observation)",ylab="Cook's Distance",main="Influence Values (Cook's Distance)")
bodyfat[cook>0.04,]



### residual plot
par(mfrow=c(2,2)) 
r = rstudent(model1.bic) 
plot(bodyfat$WEIGHT, r,ylab = 'standard residuals',type="p",pch=23,bg="red",cex=1) 
plot(bodyfat$ABDOMEN, r,ylab = 'standard residuals',type="p",pch=23,bg="red",cex=1)
plot(bodyfat$WRIST,r,ylab = 'standard residuals',type="p",pch=23,bg="red",cex=1)
plot(model1.bic$fitted, r,ylab = 'standard residuals',type="p",pch=23,bg="red",cex=1) 

### QQplot
qqnorm(r,type="p",pch=23,bg="red",cex=1) 
abline(a=0,b=1,col="black",lwd=3)
hist(model1.bic$res) 

### robustness
#### bootstrap method 
pvalues = matrix()
series = seq(1,240,1)
for (i in 1:8) {
  index = c((30*i-29):(30*i))
  sampledata = bodyfat[-index,]
  model = lm(BODYFAT~WEIGHT+ABDOMEN+WRIST,data=sampledata)
  pvalues = cbind(pvalues,as.data.frame(summary(model)$coefficients[,4]))
}
(pvalues[,-1])
write.csv(pvalues[,-1],file = 'pvalues.csv')

#### variants permutation
#### normality test
for (i in 1:ncol(bodyfat)) {
  p_value <- apply(bodyfat,2,shapiro.test)[[i]]$p.value
  print(paste(names(bodyfat)[i],p_value,sep="    "))
}

#### spearman correlation
colnames(bodyfat)
bf = bodyfat[,c(4,5,7,9,10,11,12,13,14,15,16)]
cor(bf,method = 'spearman')

#### subtitute WRIST with NECK, NECK is highly correlated with WRIST given by spearman test
submodel3 = lm(BODYFAT~WEIGHT+ABDOMEN+NECK,data=bodyfat)
summary(submodel3)