#ISDS Assignment #1

install.packages("PPforest")
library(PPforest)
data(fishcatch)
is.data.frame(fishcatch)
dim(fishcatch)
head(fishcatch)
colnames(fishcatch)
any(is.na(fishcatch))

weight<-fishcatch$weight
length1<-fishcatch$length1
length2<-fishcatch$length2
length3<-fishcatch$length3
height<-fishcatch$height
width<-fishcatch$width
type<-fishcatch$Type

pairs(fishcatch)
dim(fishcatch)

plot(fishcatch$length1, fishcatch$weight)
plot(fishcatch$length2, fishcatch$weight)
plot(fishcatch$length3, fishcatch$weight)
plot(fishcatch$height, fishcatch$weight)
plot(fishcatch$width, fishcatch$weight)
plot(fishcatch$type, fishcatch$weight)

cor(length1, weight)
cor(length2, weight)
cor(length3, weight)

model_length1<-lm(weight ~ length1)
model_length2<-lm(weight ~ length2)
model_length3<-lm(weight ~ length3)

coef1<-c(coef(model_length1))
coef2<-c(coef(model_length2))
coef3<-c(coef(model_length3))

abline(coef1)
abline(coef2)
abline(coef3)

summary(model_length1)
summary((model_length2))
summary(model_length3)

confint(model_length1, level=0.95)
confint(model_length2, level=0.95)
confint(model_length3, level=0.95)

#newdata= data.frame(length1=10)
#predict(model_length1,newdata=newdata, interval="confidence",  level = 0.95)
#predict(model_length1,data.frame(lstat=10), interval="prediction",  level = 0.95)


#multiple
model_all<-lm(weight~.-weight, data=fishcatch)
model_lengths<-lm(weight ~ length1+length2+length3, data=fishcatch)
summary(model_lengths)
summary(model_all)
car::vif(model_lengths)
car::vif(model_all)

#the model including all indep. variables actually has a very slightly higher adj.r2 than the lengths model
#before collinearity adjustments

#all 3 lengths (obviously) are highly collinear. Length 3 has the lowest collinearity and the best Pearson correlation with weight anyway
#so I chose that to include in my model

model1<-lm(weight~.-length1 -length2, data=fishcatch)
summary(model1)
car::vif(model1)

#collinearity on Type is still very high

model2<-lm(weight~.-length1-length2-Type, data=fishcatch)
summary(model2)
car::vif(model2)

#low collinearity on all 3 indep. var. for model2
#adjusted r2 is now a lot lower

model3<-lm(weight~length3+log(height)+width)
summary(model3)
car::vif(model3)

#better, now to account for the left-skew in the pearson correlation between length3 and weight

model4<-lm(weight~I(length3^2)+width+height)
summary(model4)
vif(model4)

model5<-lm(weight~length3+height+width)

model6<-lm(weight~length3)

model7<-lm(weight~length3 + type)

model8<-lm(weight~I(length3^2)+width+height+resid4)

model9<-lm(weight~length3+width+height+resid4)

summary(model8)
car::vif(model8)

summary(model9)
car::vif(model9)

resid4<-resid(model4)
#i think this is the best model i've had so far
#predictions
install.packages("Metrics")
library("Metrics")
install.packages("caret")
library("caret")
predictions<-predict(model4, fishcatch, interval="prediction",  level = 0.95)
conf_predictions<-predict(model4, fishcatch, interval="confidence",  level = 0.95)
data.frame(rmse=rmse(predictions, weight), R2 = R2(predictions, weight))

#assumptions
#linear relationship between the dep. v. and all indep. v.
#all indep. var. are approx normally distributed
#no multicollinearity - v.i.
#homoscedasticity - fixed by adding residuals of previous model
#normality

resid8<-resid(model8)
resid4<-resid(model4)
ks.test(resid8, pnorm)

plot(resid4)
plot(resid8)
hist(length3)
hist(weight)
hist(width)
hist(height)

par(mfrow=c(2,2))
plot(model4)
plot(model5)
plot(model6)
plot(model1)
plot(model2)
plot(model3)
plot(model7)
plot(model8)
plot(model9)

adjrsq_model1<-summary(model1)$adj.r.squared
adjrsq_model2<-summary(model2)$adj.r.squared
adjrsq_model3<-summary(model3)$adj.r.squared
adjrsq_model4<-summary(model4)$adj.r.squared
adjrsq_model5<-summary(model5)$adj.r.squared
adjrsq_model6<-summary(model6)$adj.r.squared
adjrsq_model7<-summary(model7)$adj.r.squared
adjrsq_model8<-summary(model8)$adj.r.squared
adjrsq_model9<-summary(model9)$adj.r.squared

data.frame(adjrsq_model1, adjrsq_model2,adjrsq_model3,adjrsq_model4,adjrsq_model5,adjrsq_model6,adjrsq_model7,adjrsq_model8,adjrsq_model9)


       
           
           
           
           
#rel low RMSE given the scale of the weight variable
#actually kind oif not? maybe inclue anyway and just show that it's an interesting element


#fix heteroscedasticity
#check other checks
#write up

predictions<-predict(model8, fishcatch, interval="prediction",  level = 0.95)
conf_predictions<-predict(model8, fishcatch, interval="confidence",  level = 0.95)
data.frame(rmse=rmse(predictions, weight), R2 = R2(predictions, weight))

summary(model8)
car::vif(model8)
