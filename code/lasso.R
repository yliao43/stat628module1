#LASSO regression
library(glmnet)
body1=bodyfat
body1 %>% str
X=body1[,c(4:6,8:17)]
Y=body1$BODYFAT
X=as.matrix(X)
fit=glmnet(X,Y)
plot(fit)
model.cv=cv.glmnet(X,Y,type.measure = "mse")
lambda=model.cv$lambda.1se
coef(fit,lambda)@x
pre.lasso=coef(fit,lambda)@x[1]+coef(fit,lambda)@x[2]*body1$AGE+coef(fit,lambda)@x[3]*body1$HEIGHT+coef(fit,lambda)@x[4]*body1$ABDOMEN+coef(fit,lambda)@x[5]*body1$WRIST
mean((pre.lasso-body1$BODYFAT)^2)