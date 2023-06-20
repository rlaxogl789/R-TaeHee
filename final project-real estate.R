#loop 만드는 법 
for(i in 1:5) {
  print(paste("This is step", i))
}

rm(list = ls()) #remove all variables/data present in the workspace

#
realestate <- read.csv("Real estate.csv", header=TRUE)
head(realestate)
str(realestate)

par(mfrow=c(1,1))
attach(realestate)
#Summary of each variables 
summary(X2.house.age)
summary(X3.distance.to.the.nearest.MRT.station)
summary(X4.number.of.convenience.stores)
summary(X4.number.of.convenience.stores)
summary(X5.latitude)
summary(X6.longitude)
summary(Y.house.price.of.unit.area)
sd(X5.latitude)
sd(X6.longitude)
#let's look at the scatter plot 
pairs(realestate[,c(3,4,5,8)])

# which one seems to have linear trend 
plot(realestate$X2.house.age,realestate$Y.house.price.of.unit.area)
plot(realestate$X3.distance.to.the.nearest.MRT.station,realestate$Y.house.price.of.unit.area)
plot(realestate$X4.number.of.convenience.stores,realestate$Y.house.price.of.unit.area)

names(realestate)

pairs(realestate[,c(3,4,5,8)])

library(GGally)
ggpairs(realestate[,c(3,4,5,8)])

attach(realestate)

#simplify the column names 
library(dplyr)
realestate <- realestate %>% 
  rename("X2" = "X2.house.age")
realestate <- realestate %>% 
  rename("X3"= "X3.distance.to.the.nearest.MRT.station",
         "X4"= "X4.number.of.convenience.stores",
         "X1" = "X1.transaction.date",
         "X5" = "X5.latitude",
         "X6" = "X6.longitude")
realestate <- realestate %>% 
  rename("Y" = "Y.house.price.of.unit.area")

names(realestate)
attach(realestate)

par(mfrow=c(1,3))
plot(X2,Y)
abline(lsfit(X2,Y),col=2)
plot(X3,Y)
abline(lsfit(X3,Y),col=2)
plot(X4,Y)
abline(lsfit(X4,Y),col=2)
# Which one has a linear trend? (8.1 11pg~)
#All three cases seem to have a linear trend. 


#draw multiple linear regression line 
m1=lm(Y~X2+X3+X4, data=realestate)
summary(m1)
par(mfrow=c(1,1))
anova(m1)
#diagnostic test for the model 
par(mfrow=c(2,2))
plot(m1)

#leverage points 
nrow(data_new)
n=414 
#hii>2*4/414
2*4/414

#erase high leverage points 
hatvalues() 
which(hatvalues(m1)>0.01932367)
which(hatvalues(m1)<=0.01932367)
subset(realestate, hatvalues(m1)<=0.01932367)
eraseleverage <- subset(realestate, hatvalues(m1)<=0.01932367 )
eraseleverage

4/412
# Analyze eraseleverage
m2=lm(Y~X2+X3+X4, data=eraseleverage)
summary(m2)
#diagnostic test for the model 
par(mfrow=c(2,2))
plot(m2)
#in terms of cook's distance, both m1 and m2 are fine. 
#This is also quite good.


#Added variable plots 
library(car)
par(mfrow=c(1,3))
avPlot(m1,variable="X2",ask=FALSE)
avPlot(m1,variable="X3",ask=FALSE)
avPlot(m1,variable="X4",ask=FALSE)

vif(m1)
#It seems there is a less multicollinearity issue. 

# Now let's do the log transformation 


#log transformation for y and x3  
log_realestate = transform(realestate, lY=log(Y), lX3 = log(X3))
m3 = lm(lY~X2+lX3+X4, data=log_realestate)
summary(m3)
par(mfrow=c(2,2))
plot(m3)
#This is also quite good, but seems less fitted than m1&m2


#let's remove values with 0
subset(realestate, X4>0)
realestate[apply(realestate,1, function(x) all(x!= 0)),]
without0 <- realestate[apply(realestate,1, function(x) all(x!= 0)),]
without0
log_without0 = transform(without0, lY=log(Y),lX2=log(X2),lX4=log(X4), lX3 = log(X3))
m4 = lm(lY~lX2+lX3+lX4, data=log_without0)
summary(m4)
plot(m4)
#This is also quite good, but seems less fitted than m1&m2


#Let's figure out which variable needs transformation. 
#inverseResponsePlot transformation (transform the response variable only)
library(car)
inverseResponsePlot(m1, key=TRUE)
par(mfrow=c(1,1))
# lamda of 0.16 provides the closest fit to the data

#log transformation to the y 
log_withonlyy = transform(realestate, lY=log(Y))
m5 = lm(lY~X2+X3+X4, data=log_withonlyy)
summary(m5)
par(mfrow=c(2,2))
plot(m5)



#using power transform code
require(car)
library(car)
summary(powerTransform(cbind(Y,X2,X3,X4)~1))

#another transformation function : using car package 
(bc.realestate <- powerTransform(m1))
str(bc.realestate, max.level=1)
print(bc.realestate$lambda)
summary(bc.realestate)


#Now we need to transform the response variable accordingly
BCTransform <- function(y, lambda=0) {
  if (lambda == 0L) { log(y) }
  else { (y^lambda - 1) / lambda }
}
BCTransformInverse <- function(yt, lambda=0) {
  if (lambda == 0L) { exp(yt) }
  else { exp(log(1 + lambda * yt)/lambda) }
}
Y.bc <- BCTransform(Y, bc.power)
#Re-run the linear model with this transformation:
par(mfrow=c(2,2))
summary(m1.bc <- lm(Y.bc ~X2+X3+X4, data=realestate))
plot(m1.bc)
#This is also quite good.



#histogram before log transformation 
summary(Y)
hist(Y, breaks=12); rug(Y)
Y.bc <- bcPower(Y, lambda=bc.realestate$lambda)
#histogram after log  transformation 
hist(Y.bc, breaks=12); rug(Y.bc)
par(mfrow=c(1,2))
# quite similar, so does not matter whether we transform the reponse variable or not. 


#fitted values and house price 
par(mfrow=c(1,1))
plot(m1.bc$fitted.values,Y.bc,xlab="Fitted Values",ylab=expression(Y.bc))
abline(lsfit(m1.bc$fitted.values,Y.bc),col=2)
#plot of Y.bc against fitted values seems good

# Now let's use variable selection to obtain a final model and remove redundancy in the full model. 
m1=lm(Y~X2+X3+X4, data=realestate)
#backwards elimination using p-values 
summary(m1)
m2=lm(Y~X3+X4, data=realestate)
summary(m2)
m3=lm(Y~X3, data=realestate)
summary(m3)


#backward elimination using AIC
backAIC<-step(m1, direction="backward", data=realestate)
#Consequently, including all three variables seems to be the most significant.


#backward elimination usic BIC 
backBIC <-step(m1, direction="backward", data=realestate, k=log(414))
nrow(realestate)
#This also shows that including all three variables seems to be the most significant.


#Forward selection using AIC 
mint<-lm(Y~1, data=realestate)
summary(mint)
forwardAIC <- step(mint, scope=list(lower=~1, upper=~	Y ~ X2 + X3	 + X4),direction="forward", data=realestate) 
#This also shows that including all three variables seems to be the most significant.

#Forward selection using BIC 
# You should have 1 variable subset 
forwardBIC <- step(mint, scope=list(lower=~1, upper=~	Y ~ X2 + X3	 + X4),direction="forward", data=realestate, k=log(414)) 
#This also shows that including all three variables seems to be the most significant.

#another option 
library(leaps)
b1<-regsubsets(Y~X2+X3+X4, data=realestate)
b2<-regsubsets(thickness ~ food +temp +seastar.density + waves + human.use, data=mussels, method="forward")
b3<-regsubsets(thickness ~ food +temp +seastar.density + waves + human.use, data=mussels, method = "backward")
rs<-summary(b1)
par(mfrow=c(1,2))
plot(1:3, rs$bic, xlab = "Subset Size", ylab="BIC")
subsets(b1, statistic=c("bic"))

#another option 
X <- cbind(X2,X3,X4)
b <- regsubsets(as.matrix(X), Y)
rs <-summary(b)
par(mfrow=c(1,2))
plot(1:3, rs$adjr2, xlab = "SubsetSize", ylab="Adjusted R-squred")
subsets(b, statistic = c("adjr2"))
subsets()


#draw reduced model 
m.ols.reduced <- lm(Y~ X3+X4, data=realestate)
anova(m.ols.reduced, m1 )
# we reject the null hypothesis, so the full model better fits the data. 
