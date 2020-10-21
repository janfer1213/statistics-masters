#Math 537 Exam 2
#Problem 1
#Download the Apple.txt dataset, which includes the data which compares apple trees from 
#different rootstocks.  Eight trees (observations) are measured from six different 
#rootstocks.  The variables of interest are:
##	y_1 = Trunk girth at 4 years of age (m/10)
## y_2 = Extension growth at 4 years (m)
## y_3 = Trunk girth at 15 years (m/10)
## y_4 = Weight of the tree above ground at 15 years (lb*1000)

setwd("/Users/macbookpro/Documents/Janice/Cal-State Fullerton/Summer 2020")
apple = read.table("Apple.txt",h = T)
head(apple)
apple[c("Rootstock")] = lapply(apple[c("Rootstock")], factor)
levels(apple$Rootstock) = c(1,2,3,4,5,6)

#Explore Data set
attach(apple)
names(apple)
dim(apple)
#Check class of all variables
sapply(apple, class)
summary(apple)
pairs(apple)
##appears to be some positive correlation between variables y1 and y2
##appears to be some positive correlation between variables y3 and y4
cor(apple[,2:5])
##high positive correlation of 88% between y1 and y2
##high positive correlation between y3 and y4
library(ggplot2)
library(ggpubr)
A <- ggplot(apple, aes(x=y1))+
  geom_histogram(binwidth=0.05,color="#990000", fill="#FF6666")+
  labs(title="Distribution of Trunk girth at 4 years of age",x="Trunk Girth (m/10)", y = "Count")+
  theme(text = element_text(size=10))
B <- ggplot(apple, aes(x=y2))+
  geom_histogram(binwidth=0.3,color="#FF6600", fill="#FF9966")+
  labs(title="Distribution of Extension growth at 4 years",x="Extension Growth (m)", y = "Count")+
  theme(text = element_text(size=10))
C <- ggplot(apple, aes(x=y3))+
  geom_histogram(binwidth=0.2,color="#FFCC00", fill="#FFFF00")+
  labs(title="Distribution of Trunk girth at 15 years",x="Trunk Girth (m/10)", y = "Count")+
  theme(text = element_text(size=10))
D <- ggplot(apple, aes(x=y4))+
  geom_histogram(binwidth=0.07,color="#669900", fill="#CCFF99")+
  labs(title="Distribution of Weight of the tree above ground at 15 years",x="Weight (lb*1000)", y = "Count")+
  theme(text = element_text(size=10))
ggarrange(A,B,C,D, labels = c("A", "B", "C", "D"),ncol = 2, nrow = 2)

#Part a-c
#(a) Do you think if the mean vector of the 4 variables above is the same across the 6 rootstocks?  
#Write an appropriate hypothesis to test this and provide a detailed statistical analysis to 
#test your null hypothesis.  Interpret your findings in the context of the problem.

#(b) Analyze the residuals from (a).  Do the usual MANOVA assumptions appear to be satisfied.  
#You don’t need to do a formal test for Independence, but casually discuss.

#(c) Calculate the p-value from parts a.) using a bootstrap approach.  
#Briefly discuss the strengths and weaknesses of the three approaches you’ve considered (MANOVA, Bootstrap).

model.a = manova(cbind(y1,y2,y3,y4)~Rootstock,data=apple)
summary(model.a)

#Built in R test
#Part A
summary.manova(model.a,test="Wilks")
lambda.obs.a = summary.manova(model.a,test="Wilks")$stats[3]
#p-value is very small thus we reject H0 and conclude there exists treatment effects 

#Part B
head(model.a$residuals)
resi = as.data.frame(model.a$residuals)
library(ggplot2)
library(ggpubr)
one <- ggplot(resi, aes(x=resi[,1]))+
  geom_histogram(binwidth=0.04,color="#990000", fill="#FF6666")+
  labs(title="Distribution of Residuals for Trunk girth at 4 years of age",x="Residuals for y1", y = "Count")+
  theme(text = element_text(size=8))
two <- ggplot(resi, aes(x=resi[,2]))+
  geom_histogram(binwidth=0.2,color="#FF6600", fill="#FF9966")+
  labs(title="Distribution of Residuals for Extension growth at 4 years",x="Residuals for y2", y = "Count")+
  theme(text = element_text(size=8))
three <- ggplot(resi, aes(x=resi[,3]))+
  geom_histogram(binwidth=0.1,color="#FFCC00", fill="#FFFF00")+
  labs(title="Distribution of Residuals for Trunk girth at 15 years",x="Residuals for y3", y = "Count")+
  theme(text = element_text(size=8))
four <- ggplot(resi, aes(x=resi[,4]))+
  geom_histogram(binwidth=0.09,color="#669900", fill="#CCFF99")+
  labs(title="Distribution of Residuals for Weight of the tree above ground at 15 years",x="Residuals for y4", y = "Count")+
  theme(text = element_text(size=8))
ggarrange(one,two,three,four, labels = c("y1", "y2", "y3", "y4"),ncol = 2, nrow = 2)

install.packages("RVAideMemoire")
library(RVAideMemoire)
mqqnorm(resi[-c(25, 42),])
mshapiro.test(resi[-c(42),])

#Part C
#Bootstrap test
n = nrow(apple)

BS.lambda = rep(0,2000)
for(i in 1:2000){
  BS.apple = apple
  BS.index = sample(1:n,n,replace=T)
  BS.apple[,2:5] = apple[BS.index,2:5]
  BS.model = manova(cbind(y1,y2,y3,y4)~Rootstock,data=BS.apple)
  BS.lambda[i] = summary.manova(BS.model,test="Wilks")$stats[3]
}
ggplot(as.data.frame(BS.lambda), aes(x=BS.lambda)) + geom_histogram(binwidth=0.06,color="#663366", fill="#CC66CC")+
  labs(title="Distribution of Bootstrapped Wilks Lambdas",x="Wilks Lambdas", y = "Frequency")+
  theme(text = element_text(size=8))
hist(BS.lambda, xlim = c(0.1,0.9))
lines(c(lambda.obs.a,lambda.obs.a),c(0,10000),col=2,lty=2)
pval = length(BS.lambda[BS.lambda<lambda.obs.a])/length(BS.lambda)

##Box Test Covariance Test
sample_1 = as.matrix(apple[apple$Rootstock=="1",2:5])
sample_2 = as.matrix(apple[apple$Rootstock=="2",2:5])
sample_3 = as.matrix(apple[apple$Rootstock=="3",2:5])
sample_4 = as.matrix(apple[apple$Rootstock=="4",2:5])
sample_5 = as.matrix(apple[apple$Rootstock=="5",2:5])
sample_6 = as.matrix(apple[apple$Rootstock=="6",2:5])
x.bar.1 = matrix(c(mean(sample_1[,1]),mean(sample_1[,2]),mean(sample_1[,3]),mean(sample_1[,4])),ncol=1)
x.bar.2 = matrix(c(mean(sample_2[,1]),mean(sample_2[,2]),mean(sample_2[,3]),mean(sample_2[,4])),ncol=1)
x.bar.3 = matrix(c(mean(sample_3[,1]),mean(sample_3[,2]),mean(sample_3[,3]),mean(sample_3[,4])),ncol=1)
x.bar.4 = matrix(c(mean(sample_4[,1]),mean(sample_4[,2]),mean(sample_4[,3]),mean(sample_4[,4])),ncol=1)
x.bar.5 = matrix(c(mean(sample_5[,1]),mean(sample_5[,2]),mean(sample_5[,3]),mean(sample_5[,4])),ncol=1)
x.bar.6 = matrix(c(mean(sample_6[,1]),mean(sample_6[,2]),mean(sample_6[,3]),mean(sample_6[,4])),ncol=1)

S1 = cov(sample_1)
S2 = cov(sample_2)
S3 = cov(sample_3)
S4 = cov(sample_4)
S5 = cov(sample_5)
S6 = cov(sample_6)

nl = nrow(sample_1) ##same for all samples
p = ncol(sample_1)

Sp = (nl-1)*(S1+S2+S3+S4+S5+S6)/(6*nl-6)

M = (6*nl-6)*log(det(Sp))-((nl-1)*(log(det(S1)) + log(det(S2)) + log(det(S3)) + log(det(S4)) + log(det(S5)) + log(det(S6))))
#large M statistic
g=6
U = (6/(nl-1) - (1/(6*nl-6)) ) * ((2*(p^2) + 3*p - 1)/(6*(p+1)*(g-1)))

C = M*(1-U)
d = (p*(p+1)*(g-1))/2
pval.BT = 1-pchisq(C,d) #large p-value

LL = ( det(S1)^((nl-1)/2) * det(S2)^((nl-1)/2) * det(S3)^((nl-1)/2) * det(S4)^((nl-1)/2) * det(S5)^((nl-1)/2) * det(S6)^((nl-1)/2) ) / (det(Sp)^21)
#small likelihood

#We fail to reject null hypothesis that all covariance matrices of sub pops are equal. 
#Therefore we have reason to believe that the individual covariance matrices are not
#expected to differ too much and also do not differ too much from the covariance matrix

#Using F distribution since it does not meet conditions nl > 20 and p, g <= 5
C2 = ( ((p-1)*(p+2)) / (6*(g-1)) )*( (4/((nl-1)^2)) - (1/((n-g)^2)) )
d2 = (d+2) / (abs(C2 - (C^2)))
#F- since C2 < C^2
a_ = d2 / (1-C + (2/d2))
F_ = (d2 * M) / (d*(a_ - M))
pval.fBT = 1-pf(F_, d,d2) #fail to reject 

#Problem 2
#The DrivFace database contains image sequences of subjects while driving in real scenarios.  
#It is composed of 606 samples of 640,480 pixels each, acquired over different days from 
#4 drivers (2 women and 2 men) with several facial features like glasses and beard.

#A set of labels assigning each image into 3 possible gaze direction classes are given 
#(variable 4 in the dataset:  lr- looking right, f – looking front, and lf-looking left).  
#Variable 5 gives the angle associated with the head direction 
#(lr: -45 to 30, f: -15 to 15, lf: 30-45).  
#Consider these response variables, the first three variables are just identifying 
#information. 

#Variables 6-9 contain information on face position (x,y,w,h) are features for location, width and height.

#Variables 10-11 are x and y coordinates for the right eye position

#Variables 12-13 are x and y coordinates for the left eye position

#Variables 14-15 are x and y coordinates for the nose position

#Variables 15-17 are x and y positions for the right corner of the mouth

#Variables 18-19 are x and y positions for the left corner of the mouth

dp = read.table("drivPoints.txt",h = T,sep = ",")
head(dp)
dp = dp[,-c(1,2,3)]
sapply(dp, class) #all treated as integers
#change response variable to factor
col = c("label")
dp[col] <- lapply(dp[col], factor)
levels(dp$label) = c(1,2,3)
sapply(dp, class)
summary(dp)
attach(dp)
names(dp)
dim(dp)
pairs(dp)
cor(dp[,2:16])
##high correlation: 0.9358 between xRE and xF, 0.99649461 between yF and yRE, 0.99139508 between yN and yF

#Part A
#Perform a full PCA analysis of the data using variables 6-19 to predict either variable 4 
#or 5.  How many principal components did you settle on and why?  
#Please interpret some of the loading coefficients for a few of your most prominent components.  
pca = prcomp(dp[,3:16], center=T,scale. = T)
pca
names(pca)
pca$center
pca$scale
plot(pca,type="l", main = "PCA", xlab = "Number of Principal Components")

dp.label = dp[,1]
dp.pca = prcomp(dp[,3:16],center=TRUE,scale. = TRUE)
names(dp.pca)
head(dp.pca$x)
dp.pca$rotation
plot(dp.pca,type="l",main = "Variance of Principal Components") 

pr.var=pca$sdev^2 #variance explained by each principal component
pve=pr.var/sum(pr.var) #proportion of variance explained by each principal component
#pr.var divided by total variance explained by all principal components
pve
#46.88% of variance explained by first component
#39.67% of variance explained by 2nd component (86.55% explained)
#7.84% of variance explained by 3rd component (94.39% explained)
plot(pve, main = "Proportion of Variance of the Principal Components", xlab="Principal Component", ylab="Proportion of Variance Explained ",type='b')
##only need 2 (or 3) PC - have majority of the variance explained just using the first 2 components 



#A really fun plot for looking at how all of our predictors map onto the first two principal components. There is a biplot package in R but the gg plot version is so much nicer. It's not on CRAN so I've uploaded the source code into our course code files.
#Download packages plyr, scales, grid, ggplot2
source("ggbiplot.R")
g = ggbiplot(dp.pca, obs.scale = 1, var.scale = 1, 
             groups = dp.label, ellipse = TRUE,
             circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
g

#Part B
#Perform a Factor analysis of the data using variables 6-19.  
#How many underlying features do you think account for the observed variables, 
#which features are loaded from the same factors?  
#Why can’t you use Factor Analysis to predict variables 4 or 5 like Principal Component Analysis?
#We're first going to do some exploratory factor analysis, then we'll play around with some confirmatory factor analysis.  First step is to isolate the predictors.

X = dp[,-c(1,2)]
fact.model = factanal(X,factors=3,rotation="varimax")
fact.model
##Factor 1: we see is loading heavily/creating all of the y coordinate data
###this means all the y coordinates of the face could be derived from a single underlying source
##Factor 2: we see is loading heavily/creating all of the x coordinate data
##Factor 3: not much it is loading heavily on: mainly 'hF'(height of face) (independent - derived from own single source)
names(fact.model)
fact.model$loadings
loadings = fact.model$loadings[,1:2]
plot(loadings,type="n")
text(loadings,labels=names(X),cex=.7)
lines(c(-2,2),c(0,0),lty=2,lwd=.7)
lines(c(0,0),c(-2,2),lty=2,lwd=.7)

fact.model = factanal(X,factors=4,rotation="varimax")
fact.model

#Added factor: loading heavily on wF (width of the face)
names(fact.model)
fact.model$loadings
loadings = fact.model$loadings[,1:2]
plot(loadings,type="n")
text(loadings,labels=names(X),cex=.7)
lines(c(-2,2),c(0,0),lty=2,lwd=.7)
lines(c(0,0),c(-2,2),lty=2,lwd=.7)

library(psy) ##trouble loading nfactors package 
scree.plot(fact.model$correlation)
