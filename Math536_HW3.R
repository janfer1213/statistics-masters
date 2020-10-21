#Math 536 HW 3
setwd("/Users/macbookpro/Documents/Janice/Cal-State Fullerton/Spring 2020")
colon = read.csv("colon2017.csv",h = T)
attach(colon)

#Load packages
library(rpart)
library(adabag)
library(randomForest)
library(ROCR)
library(gbm)
library(caret)
install.packages("formattable")
library(formattable)


#Take out unnecessary variables
colon2017 = colon[,-c(1:4,6,7,16,17,20)]
detach(colon)
attach(colon2017)

#Fix data set
colon2017[,4][colon2017[,4] == "White"] <- "W"
colon2017[,4][colon2017[,4] == "white"] <- "W"
colon2017[127,4] <- "W"
colon2017[,4] = droplevels(colon2017[,4])
Race = colon2017[,4]
View(colon2017)

#Convert variables to factor

cols = c("Gender","Tobacco","DM","CAD.PAD","Cancer","Anastamotic.Leak")
colon2017[cols] <- lapply(colon2017[cols], factor)
#check if they are factors
sapply(colon2017, class)

#Initializing levels
levels(Tobacco) <- c(0,1)
levels(DM) <- c(0,1)
levels(CAD.PAD) <- c(0,1)
levels(Cancer) <- c(0,1)
levels(colon2017$Anastamotic.Leak) <- c("No","Yes") 
#To make it clear whether there was a anastomic leak

#Explore Data set
names(colon2017)
dim(colon2017)
summary(colon2017)
pairs(colon2017)
cor(colon2017[,c(2,3,9,10)])
par(mfrow=c(2,2))
hist(BMI, col = 'red')
hist(Age, col = 'orange')
hist(Albumin..g.dL., col = 'green')
hist(Operative.Length, col = 'blue')

#Problem 1: 
#Articulate the risks of anastomotic leaking following a colectomy associated with BMI.  
#In addition to treating BMI numerically, you may want to also consider discretizing BMI.  

#General Logistic Model on all variables
model = glm(Anastamotic.Leak~.,data = colon2017, family="binomial")
summary(model)
coef_est = as.matrix(summary(model)$coefficients[,1], nrow=11)
std_err = as.matrix(summary(model)$coefficients[,2], nrow=11)
pval = as.matrix(summary(model)$coefficients[,4], nrow=11)
data.frame(coef_est, std_err, pval)

#BMI, Age, Albumin, Operative Length seem to be statistically significant
##Albumin highest significance: this means that 
#On average, a one unit increase in Albumin results in a 
#1.36235 decrease in the log odds.

exp(-1.36235)
#In other words, #On average, a one unit increase in Albumin results in a 
#25.6% decrease in odds of risk of anastamotic leaking

#For BMI: 
exp(0.09486)
#On average, a one unit increase in BMI results in a 9.95% increase in odds of risk of anastamotic leaking

#For Age:
exp(0.08436674)

#Confidence Intervals
p.Alb.low = exp(-1.36235-1.96*0.36698)
p.Alb.high = exp(-1.36235+1.96*0.36698)
c(p.Alb.low, p.Alb.high)

#Interpreting slopes of a log reg model
#Assuming it is normally distributed: with a big enough sample size 
#We're 95% confident that
#on average, Holding all else constant
#a one unit increase in Albumin results in a 
#12.47 to 52.57% decrease in odds of risk of anastamotic leaking.

p.BMI.low = exp(0.09486-1.96*0.03227)
p.BMI.high = exp(0.09486+1.96*0.03227)
c(p.BMI.low, p.BMI.high)

#Interpreting slopes of a log reg model
#Assuming it is normally distributed: with a big enough sample size 
#We're 95% confident that
#on average, Holding all else constant
#a one unit increase in BMI results in a 
#3.21 to 17.13% increase in odds of risk of anastamotic leaking.

p.Age.low = exp(0.08436674-1.96*0.02646191)
p.Age.high = exp(0.08436674+1.96*0.02646191)
c(p.Age.low, p.Age.high)

#Interpreting slopes of a log reg model
#Assuming it is normally distributed: with a big enough sample size 
#We're 95% confident that
#on average, Holding all else constant
#a one unit increase in Age results in a 
#3.3 to 14.59% increase in odds of risk of anastamotic leaking.

#Let's try Cross-validation: Test and train for log-likelihood, contigency tables, ROC curve
k = 10
n = nrow(colon2017)
index = sample(1:n,n,replace=F)
rows = k
cols = floor(n/k)
samp.matrix = matrix(index[1:(cols*rows)],nrow=rows)

lglik = rep(0,k)
overall.pred = c()
overall.y = c()
overall.phat = c()
overall.y1 = c()

for (i in 1:k){
  test = colon2017[samp.matrix[i,],]
  train = colon2017[-samp.matrix[i,],]
  Anastamotic.Leak.test = test[,11]
  model.fit.train = glm(Anastamotic.Leak ~ ., data = train, family = binomial)
  log.odds = predict.glm(model.fit.train, newdata=test, family = "binomial",type = "response")
  p.hat = exp(log.odds)/(1+exp(log.odds))
  levels(test$Anastamotic.Leak) <- c(0,1) 
  y = as.numeric(test$Anastamotic.Leak)
  lglik[i] = sum(y*log(p.hat) + (1-y)*log(1-p.hat))
  #Table
  lg.pred = rep(0,nrow(test))
  pred = predict(model.fit.train, test, type = "response")
  lg.pred[pred > .5] = 1
  
  #Contigency table  
  cont_table = table(Prediction = lg.pred, Actual = test$Anastamotic.Leak)
  print(cont_table)
  y = as.character(test$Anastamotic.Leak)
  lg.pred = as.character(lg.pred)
  overall.pred = c(overall.pred, lg.pred)
  overall.y = c(overall.y, y)
  overall.phat = c(overall.phat, p.hat)
  overall.y1 = c(overall.y1, test$Anastamotic.Leak)
}
print(lglik)
print(sum(lglik))
print(mean(lglik))
table(overall.pred, overall.y)
mean(overall.pred==overall.y)
mean(overall.pred!=overall.y)

#Plotting ROC curve
pred_logreg = prediction(overall.phat, overall.y1)
perf_logreg = performance(pred_logreg,'tpr', 'fpr')

plot(perf_logreg, main = "ROC for Logistic Regression", colorize = T)
auc_lg = performance(pred_logreg, 'auc')



###Discretizing Data into BMI interval (low, medium, high) 

library(arules)
B <- discretize(BMI,method = "interval", breaks = 3, labels = c("low", "medium","high")) 

BMI_disc <- factor(cut(BMI,c(16.57158,30.10677,43.64196,57.17715)),labels = c("low","medium","high"))
modele_disc <- glm(Anastamotic.Leak ~ BMI_disc + Gender + Race + Tobacco + DM + Cancer + Age + Albumin..g.dL.+ Operative.Length + CAD.PAD, data = colon2017, family = "binomial")

#Albumin, Age, BMI is less significant based on level of BMI
#To interpret BMI: we see that BMI_dischigh is more significant from 43.64196 to 57.17715
#High levels of BMI have exp(1.93081) higher odds than low levels of BMI for risk of AL
#comparing high to medium: exp(1.93081) - exp(0.36024) higher odds than medium levels of BMI

##Discretize also in underweight, normal weight, overweight, obesity
##BMI Categories:
#Underweight = <18.5
#Normal weight = 18.5–24.9
#Overweight = 25–29.9
#Obesity = BMI of 30 or greater
BMI_obese <- factor(cut(BMI,c(16.57,18.5,24.9,29.9,57.18)),labels = c("Underweight","Normal","Overweight","Obese"))
modele_obese <- glm(Anastamotic.Leak ~ BMI_obese + Gender + Race + Tobacco + DM + Cancer + Age + Albumin..g.dL.+ Operative.Length + CAD.PAD, data = colon2017, family = "binomial")
##No statistical significance for BMI here -> will not use

#Confidence Intervals
p.Alb.low = exp(-1.167545-1.96*0.35282)
p.Alb.high = exp(-1.16754+1.96*0.35282)
c(p.Alb.low, p.Alb.high)

p.BMIh.low = exp(1.93081-1.96*0.82203)
p.BMIh.high = exp(1.93081+1.96*0.82203)
c(p.BMIh.low, p.BMIh.high)

#Interpreting slopes of a log reg model
#Assuming it is normally distributed: with a big enough sample size 
#We're 95% confident that
#on average, Holding all else constant
#a one unit increase in BMI results in a 
#3.21 to 17.13% increase in odds of risk of anastamotic leaking.

p.Age.low = exp(0.07229-1.96*0.02509)
p.Age.high = exp(0.07229+1.96*0.02509)
c(p.Age.low, p.Age.high)

#Interpreting slopes of a log reg model
#Assuming it is normally distributed: with a big enough sample size 
#We're 95% confident that
#on average, Holding all else constant
#a one unit increase in Age results in a 
#3.3 to 14.59% increase in odds of risk of anastamotic leaking.

colon2017_noB = colon2017[,-c(2)]
colon2017_disc = cbind(colon2017_noB, BMI_disc)

#Cross-validation: Test and train for log-likelihood
k = 10
n = nrow(colon2017_disc)
index = sample(1:n,n,replace=F)
rows = k
cols = floor(n/k)
samp.matrix = matrix(index[1:(cols*rows)],nrow=rows)

lglik = rep(0,k)
overall.pred = c()
overall.y = c()
overall.phat = c()
overall.y1 = c()

for (i in 1:k){
  test = colon2017_disc[samp.matrix[i,],]
  train = colon2017_disc[-samp.matrix[i,],]
  Anastamotic.Leak.test = test[,11]
  BMI_disc = as.numeric(BMI_disc)
  model.fit.train = glm(Anastamotic.Leak ~ BMI_disc + Gender + Race + Tobacco + DM + Cancer + Age + Albumin..g.dL.+ Operative.Length + CAD.PAD, data = train, family = binomial)
  log.odds = predict.glm(model.fit.train, newdata=test, family = "binomial",type = "response")
  p.hat = exp(log.odds)/(1+exp(log.odds))
  levels(test$Anastamotic.Leak) <- c(0,1) 
  y = as.numeric(test$Anastamotic.Leak)
  lglik[i] = sum(y*log(p.hat) + (1-y)*log(1-p.hat))
  #Table
  lg.pred = rep(0,nrow(test))
  pred = predict(model.fit.train, test, type = "response")
  lg.pred[pred > .5] = 1
  
  #Contigency table  
  cont_table = table(Prediction = lg.pred, Actual = test$Anastamotic.Leak)
  print(cont_table)
  y = as.character(test$Anastamotic.Leak)
  lg.pred = as.character(lg.pred)
  overall.pred = c(overall.pred, lg.pred)
  overall.y = c(overall.y, y)
  overall.phat = c(overall.phat, p.hat)
  overall.y1 = c(overall.y1, test$Anastamotic.Leak)
}
table(overall.pred, overall.y)
mean(overall.pred==overall.y)
mean(overall.pred!=overall.y)
print(lglik)
print(sum(lglik))
print(mean(lglik))

#Plotting ROC curve
pred_logregD = prediction(overall.phat, overall.y1)
perf_logregD = performance(pred_logregD,'tpr', 'fpr')

plot(perf_logregD, main = "ROC for Logistic Regression Discretized", colorize = T)
auc_lgD = performance(pred_logregD, 'auc')

##Random Forest
rf.model = randomForest(Anastamotic.Leak ~., data = colon2017, ntree=2000, mtry=4, control = rpart.control(minsplit=10, cp=.1))
names(rf.model)
rf.model$votes
rf.model$err.rate
rf.model$confusion
rf.model$importance
plot(rf.model)

prediction.rf = rf.model$predicted
table(prediction.rf,Anastamotic.Leak)

#Cross-validation: Test and train for log-likelihood
k = 10
n = nrow(colon2017)
index = sample(1:n,n,replace=F)
rows = k
cols = floor(n/k)
samp.matrix = matrix(index[1:(cols*rows)],nrow=rows)

lglik.rf = rep(0,k)
overall.pred.rf = c()
overall.y.rf = c()
overall.p.rf = rbind()
overall.y1.rf = c()

for (i in 1:k){
  test = colon2017[samp.matrix[i,],]
  train = colon2017[-samp.matrix[i,],]
  
  #model
  rf.model = randomForest(Anastamotic.Leak ~., data = train, ntree=2000, mtry=4, control = rpart.control(minsplit=10, cp=.1))
  pred.tree = as.character(predict(rf.model, newdata = test[,1:10], type="class"))
  print(table(pred.tree, test$Anastamotic.Leak))
  y = as.character(test$Anastamotic.Leak)
  overall.pred.rf = c(overall.pred.rf, pred.tree)
  overall.y.rf = c(overall.y.rf, y)
  levels(test$Anastamotic.Leak) <- c(0,1) 
  y.i = as.numeric(test$Anastamotic.Leak)
  p  = as.numeric(predict(rf.model, newdata = test[,1:10], type="prob"))
  p1 = predict(rf.model, newdata = test[,1:10], type="prob")
  prob = exp(p) / (1+exp(p))
  lglik.rf[i] = sum(y.i*log(prob) + (1-y.i)*log(1-prob))
  overall.p.rf = rbind(overall.p.rf, p1)
  overall.y1.rf = c(overall.y1.rf, test$Anastamotic.Leak)
}
print(lglik.rf)
print(sum(lglik.rf))
table(overall.pred.rf, overall.y.rf)
mean(overall.pred.rf==overall.y.rf)

#Notice the new argument for predict.  I've set type='prob'
test.forest = predict(rf.model, newdata = test[,1:10], type="prob")
forestpred = prediction(test.forest[,2], test$Anastamotic.Leak)
forestperf = performance(forestpred, 'tpr', 'fpr')
plot(forestperf,main = "ROC for Random Forest", colorize=T)

forestpred = prediction(overall.p.rf[,2], overall.y1.rf)
forestperf = performance(forestpred, 'tpr', 'fpr')
plot(forestperf,main = "ROC for Random Forest", colorize=T)
auc_rf = performance(forestpred, 'auc')
auc_rf

plot(perf_logreg, col=5, main = "ROC: Logistic Regression vs Random Forest")
plot(perf_logregD, col=2, add = TRUE)
plot(forestperf, col=3, add=TRUE)
legend(0.2, 0.3, c('Logistic Regresstion Num.','Logistic Regresstion Disc.', 'Random Forest'), fill = c("5","2","3"), cex=0.8)

#Obtaining AUC for Logistic Regression vs Random Forest
performance(pred_logreg, 'auc')
performance(pred_logregD, 'auc')
performance(forestpred, 'auc')



#Problem 3
#.  The first is for Arizona Robbins, a 35 year old white female 
#who doesn't use tobacco, doesn't have diabetes, doesn't have CAD or PAD, doesn't have cancer, 
#has a post-operative albumin level of 4.2 and whose operation length took 90 minutes.  
log_reg = glm(Anastamotic.Leak ~., data = colon2017, family = "binomial")
pred.data.AR = data.frame(BMI = 16:50, Race = "W", DM = "0",Cancer = "0", Age = 35, Gender = "Female", Tobacco = "0", CAD.PAD = "0", Albumin..g.dL. = 4.2, Operative.Length = 0.0625)
Arizona_Robbins = predict.glm(log_reg, newdata = pred.data.AR, type = "response")
p.AR = exp(Arizona_Robbins) / (1+exp(Arizona_Robbins))
plot(16:50, p.AR, type = "l", xlab = 'BMI',ylab = 'Probabilities',main = "Risk of Anastatomic Leaking based on BMI for Arizona Robbins")

#The second is a for Richard Webber, a 62 African American male who uses tobacco and has diabetes 
#and whom had an albumin level of 2.8 following a 210 minute operation
pred.data.RW = data.frame(BMI = 16:60, Race = "AA", DM = "1",Cancer = "0", Age = 62, Gender = "Male", Tobacco = "1", CAD.PAD = "0", Albumin..g.dL. = 2.8, Operative.Length = 0.14583)
Richard_Webber = predict.glm(log_reg, newdata = pred.data.RW, type = "response")
p.RW = exp(Richard_Webber) / (1+exp(Richard_Webber))
plot(16:60, p.RW, type = "l",xlab = 'BMI',ylab = 'Probabilities',main = "Risk of Anastatomic Leaking based on BMI for Richard Webber")


n = nrow(colon2017)
p.star = matrix(0,2000,35)
for (j in 1:2000){
  #sampling x's
  BS.index = sample(1:n,n,replace=T)
  BS.x1 = colon2017$Gender[BS.index] #keep it together as rows ] if there is multicollinearity in 
  BS.x2 = colon2017$BMI[BS.index] #original data, i need that to exist in my bootstrapped data
  BS.x3 = colon2017$Age[BS.index]
  BS.x4 = colon2017$Race[BS.index]
  BS.x5 = colon2017$Tobacco[BS.index]
  BS.x6 = colon2017$DM[BS.index]
  BS.x7 = colon2017$CAD.PAD[BS.index]
  BS.x8 = colon2017$Cancer[BS.index]
  BS.x9 = colon2017$Albumin..g.dL.[BS.index]
  BS.x10 = colon2017$Operative.Length[BS.index]
  
  #Plug Bs.x's into orig. model
  BS.p = predict.glm(log_reg,newdata = data.frame(Gender=BS.x1,BMI=BS.x2,Age = BS.x3,Race = BS.x4, Tobacco = BS.x5, DM = BS.x6, CAD.PAD = BS.x7,Cancer = BS.x8, Albumin..g.dL. = BS.x9, Operative.Length = BS.x10),type = "response")
  
  #Create BS.y by sampling from c(1,0) with appropriate probability 
  BS.y = rep(0,n)
  
  for (i in 1:n){
    BS.y[i] = sample(c(1,0),1,prob=c(BS.p[i],1-BS.p[i]))
  }
  BS.model = glm(BS.y~BS.x1+BS.x2+BS.x3+BS.x4+BS.x5+BS.x6+BS.x7+BS.x8+BS.x9+BS.x10,family = "binomial")
  p.star[j,] = predict.glm(BS.model,newdata=data.frame(BS.x1="Female",BS.x2=16:50,BS.x3=35,BS.x4="W", BS.x5="0", BS.x6="0", BS.x7="0",BS.x8="0",BS.x9=4.2, BS.x10 = 0.0625),type="response")
}

low.p = rep(0,35)
high.p = rep(0,35)
p.low = rep(0,35)
p.high = rep(0,35)
for(i in 1:35){
  low.p[i] = sort(p.star[,i])[50]
  high.p[i] = sort(p.star[,i])[1950]
  p.low[i] = exp(low.p[i]) / (1+exp(low.p[i]))
  p.high[i] = exp(high.p[i]) / (1+exp(high.p[i]))
}
plot(16:50, p.AR, type = "l", xlab = 'BMI',ylab = 'Probabilities',main = "Risk of Anastatomic Leaking based on BMI for Arizona Robbins",ylim = c(.495,.515))
lines(16:50, p.low, lty = 2, col = "Red")
lines(16:50, p.high, lty = 2, col = "Red")

n = nrow(colon2017)
p.star = matrix(0,2000,45)
for (j in 1:2000){
  #sampling x's
  BS.index = sample(1:n,n,replace=T)
  BS.x1 = colon2017$Gender[BS.index] #keep it together as rows ] if there is multicollinearity in 
  BS.x2 = colon2017$BMI[BS.index] #original data, i need that to exist in my bootstrapped data
  BS.x3 = colon2017$Age[BS.index]
  BS.x4 = colon2017$Race[BS.index]
  BS.x5 = colon2017$Tobacco[BS.index]
  BS.x6 = colon2017$DM[BS.index]
  BS.x7 = colon2017$CAD.PAD[BS.index]
  BS.x8 = colon2017$Cancer[BS.index]
  BS.x9 = colon2017$Albumin..g.dL.[BS.index]
  BS.x10 = colon2017$Operative.Length[BS.index]
  
  #Plug Bs.x's into orig. model
  BS.p = predict.glm(log_reg,newdata = data.frame(Gender=BS.x1,BMI=BS.x2,Age = BS.x3,Race = BS.x4, Tobacco = BS.x5, DM = BS.x6, CAD.PAD = BS.x7,Cancer = BS.x8, Albumin..g.dL. = BS.x9, Operative.Length = BS.x10),type = "response")
  
  #Create BS.y by sampling from c(1,0) with appropriate probability 
  BS.y = rep(0,n)
  
  for (i in 1:n){
    BS.y[i] = sample(c(1,0),1,prob=c(BS.p[i],1-BS.p[i]))
  }
  BS.model = glm(BS.y~BS.x1+BS.x2+BS.x3+BS.x4+BS.x5+BS.x6+BS.x7+BS.x8+BS.x9+BS.x10,family = "binomial")
  p.star[j,] = predict.glm(BS.model,newdata=data.frame(BS.x1="Male",BS.x2=16:60,BS.x3=62,BS.x4="AA", BS.x5="1", BS.x6="1", BS.x7="0",BS.x8="0",BS.x9=2.8, BS.x10 = 0.14583),type="response")
}
low.p = rep(0,45)
high.p = rep(0,45)
p.low = rep(0,45)
p.high = rep(0,45)
for(i in 1:45){
  low.p[i] = sort(p.star[,i])[50]
  high.p[i] = sort(p.star[,i])[1950]
  p.low[i] = exp(low.p[i]) / (1+exp(low.p[i]))
  p.high[i] = exp(high.p[i]) / (1+exp(high.p[i]))
}
plot(16:60, p.RW, type = "l", xlab = 'BMI',ylab = 'Probabilities',main = "Risk of Anastatomic Leaking based on BMI for Richard Webber",ylim = c(.495,.75))
lines(16:60, p.low, lty = 2, col = "Blue")
lines(16:60, p.high, lty = 2, col = "Blue")

