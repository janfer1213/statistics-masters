setwd("/Users/macbookpro/Documents/Janice/Cal-State Fullerton/Spring 2020")
bball = read.csv("baseball.csv",h = T)
summary(bball)

bball = na.omit(bball)

#remove X and Pitcher variable
bball = bball[,-c(1,4)]

library(rpart)
library(adabag)
library(randomForest)
library(ROCR)
library(gbm)
library(caret)

#Exploratory data analysis
names(bball)
dim(bball)
summary(bball)

cor(bball[,c(2,4:9)])  #low correlation between variables

attach(bball)
par(mfrow=c(2,2))
hist(balls, col = "orange")
hist(strikes, col = "yellow")
hist(plate_x, col = "green")
hist(plate_z, col = "blue")

dev.off()
par(mfrow=c(3,1))
hist(release_speed, col = "red")
hist(home_score, col = "purple")
hist(away_score, col = "#CC0066")

factor_var = lapply(bball[c("pitch_type","description")], table)
pitch_type_tab = factor_var$pitch_type

dev.off()
barplot(factor_var$pitch_type, col = rainbow(10), xlab = "Types of Pitches", ylab = "Number of Pitches", main = "Number of Pitches by Type")
#Majority FF: Four-Seam Fastballs (7605)
#CH	Changeup
#CU	Curveball
#EP	Eephus
#FC	Cut Fastball (Cutter)
#FF Four-Seam Fastballs
#FO	Pitch Out
#FS	Sinking Fastball / Split-Fingered (Splitter)
#FT	Two-seam Fastball
#KC	Knuckle-curve
#SI	Sinker
#SL	Slider

legend("topright", legend = levels(pitch_type), fill = rainbow(10), ncol = 2,cex = 0.75)
barplot(factor_var$description, col = c("blue","red"), main = "Results of Not Swinging at the Ball: Ball vs. Strike", xlab = "Ball or Strike", ylab = "Number of Balls or Strikes")

##Need to define our population

#Looking at location x,z where there is a clear strike or ball
plot(bball$plate_x[bball$description=="ball"], bball$plate_z[bball$description=="ball"], col = 3, pch = "o", cex=.5) 
points(bball$plate_x[bball$description!="ball"], bball$plate_z[bball$description!="ball"], col = "#CC0033", pch = "x", cex=.5) 

bball1 = bball[!(bball$plate_x > -.75 & bball$plate_x <.75 & bball$plate_z > 1.7 & bball$plate_z < 3.2),]
bball2 = bball1[bball1$plate_x > -1 & bball1$plate_x < 1 & bball1$plate_z > 1.4 & bball1$plate_z < 3.5,]
dim(bball)
dim(bball1)
dim(bball2)
length(bball2$description[bball2$description=="ball"])/length(bball2$description)
plot(bball2$plate_x, bball2$plate_z, col = "#009999", pch = "o", cex=.5, xlab = "plate_x", ylab = "plate_z", main = "Coordinates: Balls vs. Strikes")
points(bball2$plate_x[bball2$description!="ball"], bball2$plate_z[bball2$description!="ball"], col = "#CC0033", pch = "x", cex=.5)

#Let's define Levels for description variable
levels(bball2$description) <- list("1" = "called_strike", "0" = "ball")
##Analysis 1
#1.	I’ve always believed that umpires are more likely to call a fringe pitch as 
#a strike if there are already 3 balls on the batter.  
#Assess whether or not the count (number of strikes and number of balls) impacts 
#the probability of a strike being called if all pitch conditions are comparable.

##Define our Cases:
bball2[ , "cases"] = ""

#Less than 3 Balls or Do not have any 3 Balls
bball2[bball2$balls<3,]$cases= "Not3"

#3 Balls and 2 Strikes
bball2[bball2$balls==3 & bball2$strikes==2,]$cases="3B&2S"

#3 Balls less than 2 Strikes
bball2[bball2$balls==3 & bball2$strikes<2,]$cases="3B&LTS"

##Define Euclidean Distance from the Center 
bball2$distance = sqrt((bball2$plate_x - 0)^2 + (bball2$plate_z - 2.45)^2)

##Model: Logistic Regression
logreg = glm(description ~ pitch_type + release_speed + balls + strikes + plate_x + plate_z + home_score + away_score + cases + distance, data = bball2, family = "binomial")
summary(logreg)
#release_speed, balls, strikes, plate_z, distance, statistically significant
#None of our cases statistically significant -> Might need more data to 
pval = as.matrix(summary(logreg)$coefficients[,4])
##Interpretation of Our Coefficients
#Distance
exp(7.226277)

CI.d.low = exp(7.226277-1.96*(1.954746*exp(-129)))
CI.d.high = exp(7.226277+1.96*(1.954746*exp(-129)))
c(CI.d.low, CI.d.high)

#Release Speed
exp(0.030030)

#Interpreting slopes of a log reg model
#On average, a one unit increase in release_speed results in a 
#3.05% increases in odds of the pitch getting called a strike.

CI.rs.low = exp(0.030030-1.96*0.012541)
CI.rs.high = exp(0.030030+1.96*0.012541)
c(CI.rs.low, CI.rs.high)

#Interpreting slopes of a log reg model
#Assuming it is normally distributed: with a big enough sample size 
#We're 95% confident that
#on average, holding all else constant
#a one unit increase in the release speed results in a 
#0.55 to 5.61% increase in odds of the pitch getting called a strike.

#Balls
exp(-0.173838)
#On average, a one unit increase in balls results in a 
#84.04% decrease in odds of the pitch getting called a strike.

CI.balls.low = exp(-0.173838-1.96*0.057206)
CI.balls.high = exp(-0.173838+1.96*0.057206)
c(CI.balls.low, CI.balls.high)

#Interpreting slopes of a log reg model
#Assuming it is normally distributed: with a big enough sample size 
#We're 95% confident that
#on average, holding all else constant
#a one unit increase in balls results in a 
#75.13 to 94.02% decrease in odds of the pitch getting called a strike.

#Strikes
exp(0.415640)
#On average, a one unit increase in strikes results in a 
#51.53% increase in odds of the pitch getting called a strike.

CI.strike.low = exp(0.415640-1.96*0.057498)
CI.strike.high = exp(0.415640+1.96*0.057498)
c(CI.strike.low, CI.strike.high)

#Interpreting slopes of a log reg model
#Assuming it is normally distributed: with a big enough sample size 
#We're 95% confident that
#on average, holding all else constant
#a one unit increase in strikes results in a 
#35.38 to 69.61% increase in odds of the pitch getting called a strike.

#Plate_z
exp(0.284531)
#On average, a one unit increase in plate_z (The vertical coordinate of the ball 
#as it passes over the plate) results in a 
#32.91% increase in odds of the pitch getting called a strike.

CI.pz.low = exp(0.284531-1.96*0.051804)
CI.pz.high = exp(0.284531+1.96*0.051804)
c(CI.pz.low, CI.pz.high)

#Interpreting slopes of a log reg model
#Assuming it is normally distributed: with a big enough sample size 
#We're 95% confident that
#on average, holding all else constant
#a one unit increase in plate_z results in a 
#20.08 to 47.12% increase in odds of the pitch getting called a strike.

#Interpreting the cases: 
#cases3B&LTS
#For categorical: Slope for Case 3 Balls & Less than 2 Strikes is 0.406295 
#with s.e. = 0.285401

exp(0.406295-1.96*0.285401)
exp(0.406295+1.96*0.285401)

#if response is prob you get pitch called a strike or not (ball)

#We are 95% confident that on average and
#holding all else constant
#having the case when there are 3 balls and less than 2 strikes as opposed to 
#having 3 balls and 2 strikes, there is
#an 85.81% decrease to 163% increase in the pitch getting called a strike.

#Slope for Case less than 3 balls is 0.055885 
#with s.e. = 0.258785
exp(0.055885-1.96*0.258785)
exp(0.055885+1.96*0.258785)

#We are 95% confident that on average and
#holding all else constant
#having the case when there are less than 3 balls as opposed to 
#having 3 balls and 2 strikes, there is
#a 63.68% decrease to 75.61% increase in the pitch getting called a strike.


library(car)
residualPlots(logreg)

##Cross Validation
#Let's try Cross-validation: Test and train for log-likelihood, contigency tables, ROC curve
k = 10
n = nrow(bball2)
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
  test = bball2[samp.matrix[i,],]
  train = bball2[-samp.matrix[i,],]
  Description.test = test[,3]
  model.fit.train = glm(description ~ pitch_type + release_speed + balls + strikes + plate_x + plate_z + home_score + away_score + cases, data = train, family = "binomial")
  log.odds = predict.glm(model.fit.train, newdata=test, family = "binomial",type = "response")
  p.hat = exp(log.odds)/(1+exp(log.odds))
  levels(test$description) <- list("1" = "called_strike", "0" = "ball") 
  y = as.numeric(test$description)
  lglik[i] = sum(y*log(p.hat) + (1-y)*log(1-p.hat))
  #Table
  lg.pred = rep(0,nrow(test))
  pred = predict(model.fit.train, test, type = "response")
  lg.pred[pred > .5] = 1
  
  #Contigency table  
  cont_table = table(Prediction = lg.pred, Actual = test$description)
  print(cont_table)
  y = as.character(test$description)
  lg.pred = as.character(lg.pred)
  overall.pred = c(overall.pred, lg.pred)
  overall.y = c(overall.y, y)
  overall.phat = c(overall.phat, p.hat)
  overall.y1 = c(overall.y1, test$description)
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
auc_lg = performance(pred_logreg, 'auc') #0.5779176

##Random Forest
# rf.model = randomForest(description ~ pitch_type + release_speed + balls + strikes + plate_x + plate_z + home_score + away_score + cases + distance, data = bball2, ntree=2000, mtry=4, control = rpart.control(minsplit=10, cp=.1))
# names(rf.model)
# rf.model$votes
# rf.model$err.rate
# rf.model$confusion
# rf.model$importance
# plot(rf.model)
# 
# prediction.rf = rf.model$predicted
# table(prediction.rf,Anastamotic.Leak)
# 
# #Cross-validation: Test and train for log-likelihood
# k = 10
# n = nrow(colon2017)
# index = sample(1:n,n,replace=F)
# rows = k
# cols = floor(n/k)
# samp.matrix = matrix(index[1:(cols*rows)],nrow=rows)
# 
# lglik.rf = rep(0,k)
# overall.pred.rf = c()
# overall.y.rf = c()
# overall.p.rf = rbind()
# overall.y1.rf = c()
# 
# for (i in 1:k){
#   test = colon2017[samp.matrix[i,],]
#   train = colon2017[-samp.matrix[i,],]
#   
#   #model
#   rf.model = randomForest(Anastamotic.Leak ~., data = train, ntree=2000, mtry=4, control = rpart.control(minsplit=10, cp=.1))
#   pred.tree = as.character(predict(rf.model, newdata = test[,1:10], type="class"))
#   print(table(pred.tree, test$Anastamotic.Leak))
#   y = as.character(test$Anastamotic.Leak)
#   overall.pred.rf = c(overall.pred.rf, pred.tree)
#   overall.y.rf = c(overall.y.rf, y)
#   levels(test$Anastamotic.Leak) <- c(0,1) 
#   y.i = as.numeric(test$Anastamotic.Leak)
#   p  = as.numeric(predict(rf.model, newdata = test[,1:10], type="prob"))
#   p1 = predict(rf.model, newdata = test[,1:10], type="prob")
#   prob = exp(p) / (1+exp(p))
#   lglik.rf[i] = sum(y.i*log(prob) + (1-y.i)*log(1-prob))
#   overall.p.rf = rbind(overall.p.rf, p1)
#   overall.y1.rf = c(overall.y1.rf, test$Anastamotic.Leak)
# }
# print(lglik.rf)
# print(sum(lglik.rf))
# table(overall.pred.rf, overall.y.rf)
# mean(overall.pred.rf==overall.y.rf)

#Let's observe an example 
logreg = glm(description ~ pitch_type + release_speed + balls + strikes + plate_x + plate_z + home_score + away_score + cases + distance, data = bball2, family = "binomial")
pred.data1 = data.frame(strikes = 0:2, pitch_type = "FF", release_speed = 90.20, balls = 0, plate_x = 0.1171, plate_z = 2.135, home_score = 1, away_score = 2, cases = "Not3", distance = 1.0015)
pitch1 = predict.glm(logreg, newdata = pred.data1, type = "response")
p1 = exp(pitch1) / (1+exp(pitch1))
plot(0:2, p1, type = "l", xlab = 'Strikes',ylab = 'Probabilities',main = "Case 1: Odds of Calling Strike over a Ball")


n = nrow(bball2)
p.star = matrix(0,2000,3)
for (j in 1:2000){
  #sampling x's
  BS.index = sample(1:n,n,replace=T)
  BS.x1 = bball2$pitch_type[BS.index] #keep it together as rows ] if there is multicollinearity in 
  BS.x2 = bball2$release_speed[BS.index] #original data, i need that to exist in my bootstrapped data
  BS.x3 = bball2$balls[BS.index]
  BS.x4 = bball2$strikes[BS.index]
  BS.x5 = bball2$plate_x[BS.index]
  BS.x6 = bball2$plate_z[BS.index]
  BS.x7 = bball2$home_score[BS.index]
  BS.x8 = bball2$away_score[BS.index]
  BS.x9 = bball2$cases[BS.index]
  BS.x10 = bball2$distance[BS.index]
  
  #Plug Bs.x's into orig. model
  BS.p = predict.glm(logreg,newdata = data.frame(pitch_type = BS.x1,release_speed = BS.x2,balls = BS.x3, strikes = BS.x4, plate_x = BS.x5, 
                                                 plate_z = BS.x6,home_score = BS.x7, away_score = BS.x8, 
                                                 cases = BS.x9, distance = BS.x10),type = "response")
  
  #Create BS.y by sampling from c(1,0) with appropriate probability 
  BS.y = rep(0,n)
  
  for (i in 1:n){
    BS.y[i] = sample(c(1,0),1,prob=c(BS.p[i],1-BS.p[i]))
  }
  BS.model = glm(BS.y~BS.x1+BS.x2+BS.x3+BS.x4+BS.x5+BS.x6+BS.x7+BS.x8+BS.x9+BS.x10,family = "binomial")
  p.star[j,] = predict.glm(BS.model,newdata=data.frame(BS.x1 = "FF", BS.x2 = 90.20, BS.x3 = 0, BS.x4 = 0:2, BS.x5 = 0.1171, BS.x6 = 2.135, 
                                                       BS.x7 = 1, BS.x8 = 2, BS.x9 = "Not3", BS.x10 = 1.0015),type="response")
}

low.p = rep(0,3)
high.p = rep(0,3)
p.low = rep(0,3)
p.high = rep(0,3)
for(i in 1:3){
  low.p[i] = sort(p.star[,i])[50]
  high.p[i] = sort(p.star[,i])[1950]
  p.low[i] = exp(low.p[i]) / (1+exp(low.p[i]))
  p.high[i] = exp(high.p[i]) / (1+exp(high.p[i]))
}
plot(0:2, p1, type = "l", xlab = 'Strikes',ylab = 'Probabilities',main = "Case 1: Odds of Calling Strike over a Ball", ylim = c(.57,.645))
lines(0:2, p.low, lty = 2, col = "Red")
lines(0:2, p.high, lty = 2, col = "Red")
p.low
p.high

#Example 2
pred.data2 = data.frame(strikes = 0:2, pitch_type = "SL", release_speed = 84.40, balls = 3, plate_x = -0.7539, plate_z = 1.597, home_score = 0, away_score = 0, cases = "3B&LTS", distance = 0.9098)
pitch2 = predict.glm(logreg, newdata = pred.data2, type = "response")
p2 = exp(pitch2) / (1+exp(pitch2))
plot(0:2, p2, type = "l", xlab = 'Strikes',ylab = 'Probabilities',main = "Case 2: Odds of Calling Strike over a Ball")


n = nrow(bball2)
p.star = matrix(0,2000,3)
for (j in 1:2000){
  #sampling x's
  BS.index = sample(1:n,n,replace=T)
  BS.x1 = bball2$pitch_type[BS.index] #keep it together as rows ] if there is multicollinearity in 
  BS.x2 = bball2$release_speed[BS.index] #original data, i need that to exist in my bootstrapped data
  BS.x3 = bball2$balls[BS.index]
  BS.x4 = bball2$strikes[BS.index]
  BS.x5 = bball2$plate_x[BS.index]
  BS.x6 = bball2$plate_z[BS.index]
  BS.x7 = bball2$home_score[BS.index]
  BS.x8 = bball2$away_score[BS.index]
  BS.x9 = bball2$cases[BS.index]
  BS.x10 = bball2$distance[BS.index]
  
  #Plug Bs.x's into orig. model
  BS.p = predict.glm(logreg,newdata = data.frame(pitch_type = BS.x1,release_speed = BS.x2,balls = BS.x3, strikes = BS.x4, plate_x = BS.x5, 
                                                 plate_z = BS.x6,home_score = BS.x7, away_score = BS.x8, 
                                                 cases = BS.x9, distance = BS.x10),type = "response")
  
  #Create BS.y by sampling from c(1,0) with appropriate probability 
  BS.y = rep(0,n)
  
  for (i in 1:n){
    BS.y[i] = sample(c(1,0),1,prob=c(BS.p[i],1-BS.p[i]))
  }
  BS.model = glm(BS.y~BS.x1+BS.x2+BS.x3+BS.x4+BS.x5+BS.x6+BS.x7+BS.x8+BS.x9+BS.x10,family = "binomial")
  p.star[j,] = predict.glm(BS.model,newdata=data.frame(BS.x1 = "SL", BS.x2 = 84.40, BS.x3 = 3, BS.x4 = 0:2, BS.x5 = -0.7539, BS.x6 = 1.597, BS.x7 = 0, 
                                                       BS.x8 = 0, BS.x9 = "3B&LTS", BS.x10 = 0.9098),type="response")
}

low.p2 = rep(0,3)
high.p2 = rep(0,3)
p2.low = rep(0,3)
p2.high = rep(0,3)
for(i in 1:3){
  low.p2[i] = sort(p.star[,i])[50]
  high.p2[i] = sort(p.star[,i])[1950]
  p2.low[i] = exp(low.p[i]) / (1+exp(low.p[i]))
  p2.high[i] = exp(high.p[i]) / (1+exp(high.p[i]))
}
plot(0:2, p2, type = "l", xlab = 'Strikes',ylab = 'Probabilities',main = "Case 2: Odds of Calling Strike over a Ball", ylim = c())
lines(0:2, p2.low, lty = 2, col = "Blue")
lines(0:2, p2.high, lty = 2, col = "Blue")
p2.low
p2.high


#Trying Analysis 2
#2.	Some pitchers (and catchers by extension) seem to have a wider strike range, meaning 
#that they get more strikes called in similar situations than other pitchers.  
#Mine the data to assess the validity of this ‘conventional’ baseball wisdom.  
#Are there a few pitchers in particular that stand out?  
#If you choose this analysis you might want to truncate your dataset to only include 
#pitchers that have thrown more than 20 pitches
# pitch <- table(bball$pitcher)
# pitch2 <- table(bball2$pitcher)
# pitcher = as.data.frame(pitch)
# pitcher2 = as.data.frame(pitch2)
# pitch20 = pitcher2$Var1[which(pitcher2$Freq <= 20)]
# 
# #Obtain total rows that have the values of pitcher from pitch20
# rows = c()
# total_rows = c()
# for (i in 1:54){
#   p = as.vector(pitch20)[i]
#   rows = which(bball$pitcher==p, arr.ind=TRUE)
#   total_rows = c(total_rows, rows)
# }
# #Remove rows (total_rows) that include the pitcher that did not throw more than 20 pitches
# bball = bball[-total_rows,]
# 
# 
# #check if pitcher in dataset is factor
# is.factor(bball$pitcher) #false
# 
# #make pitcher variable in dataset factor
# 
# bball[c("pitcher")] <- lapply(bball[c("pitcher")], factor)
# #check which are factors in dataset
# sapply(bball, class)
# 
# 
# #remove X variable
# bball = bball[,-c(1)]
# 
# #Exploratory data analysis
# names(bball)
# dim(bball)
# summary(bball) 
# 
# cor(bball[,c(2,5:10)])  #low correlation between variables
# pairs(bball[,c(2,5:10)]) #no clear relationship between numerical variables
# summary(bball)
# 
# attach(bball)
# hist(release_speed)
# hist(balls)
# hist(strikes)
# hist(plate_x)
# hist(plate_z)
# hist(home_score)
# hist(away_score)
# 
# factor_var = lapply(bball_1[c("pitch_type","pitcher","description")], table)
# pitch_type_tab = factor_var$pitch_type
# 
# barplot(factor_var$pitch_type, col = rainbow(10))
# legend("topright", legend = levels(pitch_type), fill = rainbow(10), ncol = 2,cex = 0.75)
# barplot(factor_var$description, col = c("blue","red"), main = "Results of Not Swinging at the Ball: Ball vs. Strike", xlab = "Ball or Strike", ylab = "Number of Balls or Strikes")
# legend("topright", legend = levels(description), fill = c("blue","red"),cex = 0.75)


