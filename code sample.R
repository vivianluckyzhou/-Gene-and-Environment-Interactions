dat <- read.table("C:/Users/hzhou/Desktop/Acumen,LLC/578/data.csv ", header=TRUE, sep=",")
dat <- dat[complete.cases(dat),]  # remove rows with missing value
str(dat)  # 1364 obs. of 32 variables

#cor(dat$Y,dat[,]) #cor between Y and all other variables
cor(dat[,-1]) #cor between every col and row, variables are independent
#which(cor(dat[,-1])!= 1 & abs(cor(dat[,-1]))>0.1)  # return 'integer(0)'

##########################################################################
fit1 <- lm(Y~.,data=dat)
summary(fit1)
pvalue1 <-NULL
pvalue1 <-c(pvalue1,summary(fit1)$coefficient[,4])
pvalue1[which(pvalue1<0.01)]  
# E1, E6, G9 significant

fit2 <- lm(Y~.^2,data=dat)
summary(fit2)
pvalue2 <-NULL
pvalue2 <-c(pvalue2,summary(fit2)$coefficient[,4])
pvalue2[which(pvalue2<0.01)] 
# E3, E1:E6, E3:G17, E6:G16 significant

##########################################################################
dat1 <- dat[c(1,2,4,7,16,23,24)]
str(dat1)  # Y, E1, E3, E6, G9, G16, G17 

### stepwise regression ###
null <- lm(Y~1,data=dat1) #????????????,????????????
full <- lm(Y~.^4,data=dat1) 
step(full,scope=list(upper=null),data=dat1,direction='both',k=log(1364))
# Y ~ E1 + E6 + G9 + G16 + E1:E6 + E6:G16

fit3 <- lm(Y ~ E1 + E6 + G9 + G16 + E1:E6 + E6:G16,data=dat1)
summary(fit3) 
# G9, E1:E6 significant
# Adjusted R-squared = 0.4532
BIC(fit3) # 73050.24

fit4 <- lm(Y ~ G9 + E1:E6 -1,data=dat1)
summary(fit4) 
# Adjusted R-squared = 0.6765
BIC(fit4) # 73027.52

### stepwise regression ###
step(null,scope=list(upper=full),data=dat1,direction='both',k=log(1364))
# Y ~ E6 + E1 + G9 + E6:E1

fit5 <- lm(Y ~ E6 + E1 + G9 + E6:E1,data=dat1)
summary(fit5) 
# G9, E6:E1 signiificant  
# Adjusted R-squared = 0.4509
BIC(fit5) # 73043.32

fit6 <- lm(Y ~ G9 + E6:E1 -1,data=dat1)
summary(fit6)
# Adjusted R-squared = 0.6765
BIC(fit6) # 73027.52

### lasso ### #????????????,??????
library(glmnet)
f1 <- as.formula(Y~ .)
x1 <- model.matrix(f1, dat)
cvfit1 <- cv.glmnet(x1, dat$Y)
c1 <- coef(cvfit1, s = "lambda.1se") #E1, E6, G9

f2 <- as.formula(Y~ .^2)
x2 <- model.matrix(f2, dat)
cvfit2 <- cv.glmnet(x2, dat$Y)
c2 <- coef(cvfit2, s = "lambda.1se") #G9, E1:E6, E1:G9, E6:G9

f3 <- as.formula(Y~ .^3)
x3 <- model.matrix(f3, dat)
cvfit3 <- cv.glmnet(x3, dat$Y)
c3 <- coef(cvfit3, s = "lambda.1se") #G9, E1:E6, E1:G9, E1:E6:G9
sink('c3.txt')
c3
sink(NULL)

f4 <- as.formula(Y~ .^4)
x4 <- model.matrix(f4, dat)
cvfit4 <- cv.glmnet(x4, dat$Y)
c4 <- coef(cvfit4, s = "lambda.1se") #G9, E1:E6, E1:G9, E1:E6:G9
sink('c4.txt')
c4
sink(NULL)

fit7 <- lm(Y ~ E1 + E6 + G9 + E1:E6 + E1:G9 + E6:G9 + E1:E6:G9,data=dat1)
summary(fit7)
# G9, E1:E6 signiificant  
# Adjusted R-squared = 0.4507
BIC(fit7) # 73062.45

##########################################################################
### check assumptions ### ##plot residual, check if the residual is normal distribution
par(mfrow=c(2,2))
plot(fit4)

library(car)
# test of independence #??????residual????????????, residual???????????????model???????????????????????????
durbinWatsonTest(fit4)  #p-value = 0.266 
# test of normality
shapiro.test(fit4$residuals)  #p-value = 0.2232
# test of homoscedasticity
ncvTest(fit4)  #p-value = 0.4089509 

