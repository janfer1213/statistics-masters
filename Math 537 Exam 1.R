setwd("/Users/macbookpro/Documents/Janice/Cal-State Fullerton/Summer 2020")
genderwage = read.csv("genderwage.csv",h = T)
summary(genderwage)

#Part a
#Develop a statistical hypothesis test using a mean vector (no paired
#t-tests or simple linear regression models!) for whether or not you think that
#non-management males and females have the same average salary.

attach(genderwage)
plot(female,male,xlim=c(min(female),max(female)),ylim=c(min(male),max(male)))

n = length(male)
p=2

x.bar = mean(female)
y.bar = mean(male)

#set up vectors and matrices for x.bar.vec, mu0.vec and sigma.xbar.mat

x.bar.vec = matrix(c(x.bar,y.bar),2,1)
mu0.vec = matrix(c(20.47,20.47),2,1) #value found using plot in part b
S.x.bar = matrix(var(genderwage),ncol=2)/n
T2 = t(x.bar.vec-mu0.vec)%*%solve(S.x.bar)%*%(x.bar.vec-mu0.vec)
m = (n-p)/((n-1)*p)
pval = 1-pf(m*T2,p,n-p)

#Part b
#Plot your data along with a 95% confdence region for where you believe
#the true average salary vector for males and females in the manufacturing sector
#is. Add your null hypothesis as a line to this plot. (If you're struggling with
#part a. I'd start with part b.)

#Plotting Challenge

points(x.bar,y.bar,pch=19,cex=1.8,col=2) #sample mean 

eig = eigen(S.x.bar) #eigenvalues of cov matrix

lam1 = eig$'values'[1]
lam2 = eig$'values'[2]

v1 = eig$vectors[,1]
v2 = eig$vectors[,2]

dist = (p*(n-1)/(n-p))*qf(.95,p,n-p) 
#cut off for the inner 95% of observations for F distribution

a1 = v1*sqrt(lam1)*sqrt(dist) #c*sqrt(\lambda_i)*eigenvector 
#c is sqrt(dist) #not needed
a2 = v2*sqrt(lam2)*sqrt(dist)


lines(c(x.bar,x.bar+a1[1]),c(y.bar,y.bar+a1[2]),lwd=1.5,col=2)
lines(c(x.bar,x.bar+a2[1]),c(y.bar,y.bar+a2[2]),lwd=1.5,col=2)

#Zooming into Plot
plot(female,male,xlim=c(20,24),ylim=c(23,27))
points(x.bar,y.bar,pch=19,cex=1.8,col=2)

lines(c(x.bar,x.bar+a1[1]),c(y.bar,y.bar+a1[2]),lwd=3,col=2)
lines(c(x.bar,x.bar+a2[1]),c(y.bar,y.bar+a2[2]),lwd=3,col=2)


d1 = sqrt(sum(a1*a1))
d2 = sqrt(sum(a2*a2))

#####Find theta to rotate ellipse.
theta = acos(v1[1])

x.seq1 = seq(-d1,d1,by=.001)
x.seq2 = seq(d1,-d1,by=-.001)

y.seq1 = d2*sqrt(1-(x.seq1/d1)^2)
y.seq2 = -d2*sqrt(1-(x.seq2/d1)^2)


old.x = c(x.seq2,x.seq1)
old.y = c(y.seq2,y.seq1)

lines(old.x+x.bar,old.y+y.bar)

n1 = length(old.x)

#R insists the range for atan is between -pi/2 and pi/2.  
old.theta = atan(old.y/old.x)
plot(old.theta,type="l")

old.theta[round(1+n1/4):round(3*n1/4)] = old.theta[round(1+n1/4):round(3*n1/4)]-pi 
old.theta[round(1+3*n1/4):n1] = old.theta[round(1+3*n1/4):n1]-2*pi
plot(old.theta,type="l")

new.theta = old.theta+theta
l = sqrt(old.x^2 + old.y^2)
new.x = cos(new.theta)*l
new.y = sin(new.theta)*l

plot(female,male,xlim=c(18,26),ylim=c(20,28), main = "Female vs Male Salary")
points(x.bar,y.bar,pch=19,cex=1.8,col=2)

lines(c(x.bar,x.bar+a1[1]),c(y.bar,y.bar+a1[2]),lwd=3,col=2)
lines(c(x.bar,x.bar+a2[1]),c(y.bar,y.bar+a2[2]),lwd=3,col=2)

lines(x.bar+new.x,y.bar+new.y,col=2,lwd=3)

##Find mu.0
#Want value of mu.0 where we obtain the smallest MD from the actual mean (x.bar.vec) 
#to a point of the y = x 
abline(0,1,col = "pink", lwd=3) #use values 19.5-23 to see shortest distance

x.bar.vec = matrix(c(x.bar,y.bar),2,1)
S.x.bar = matrix(var(genderwage),ncol=2)/n

mu0.vec = matrix(c(0,0),2,1)

x = seq(19.5,23,length.out=1000)
T2 = rep(0,1000)
for (i in 1:1000){
  T2[i] = t(x.bar.vec-matrix(c(x[i],x[i]),2,1))%*%solve(S.x.bar)%*%(x.bar.vec-matrix(c(x[i],x[i]),2,1))
}
x[which.min(T2)] #20.47 is the best value for mu0 to get maximizing pvalue

segments(x.bar, y.bar, 20.47, 20.47, col = "orange",lty = 4,lwd=1) #shortest distance
legend(21,27.8, legend=c("y = x", "Shortest MD to Center"), col=c("pink", "orange"), lty=1:4, lwd=3:1, cex=0.8)

#Plotting null hypothesis
abline(0,1,col = "pink", lwd=3)
points(20.47,20.47,pch=19,cex=1.8,col="magenta")
text(20.47,20.47,labels="H0: mu = (20.47, 20.47)")
legend(21,27.8, legend=c("Null Hypothesis Line"), col="pink", lty=1, lwd=3, cex=0.8)

##Part c
#Separating the males and females, and a marginal confidence interval for
#the males and a marginal confidence interval for the females.

#Marginal 
t = qt(.975, n-1)
s.x = sd(female) / sqrt(n)
CI.low.x = x.bar - t*s.x
CI.high.x = x.bar + t*s.x
c(CI.low.x, CI.high.x)

s.y = sd(male) / sqrt(n)
CI.low.y = y.bar - t*s.y
CI.high.y = y.bar + t*s.y
c(CI.low.y, CI.high.y)

lines(c(CI.low.x, CI.high.x), c(CI.low.y, CI.low.y), lty = 2, col = 4)
lines(c(CI.low.x, CI.high.x), c(CI.high.y, CI.high.y), lty = 2, col = 4)
lines(c(CI.low.x, CI.low.x), c(CI.low.y, CI.high.y), lty = 2, col = 4)
lines(c(CI.high.x, CI.high.x), c(CI.low.y, CI.high.y), lty = 2, col = 4)

#Part d
#Find simultaneous confidence intervals for the males and females in this dataset.

#Projected

f = qf(.95,p,n-p)
c = sqrt((p/(n-p))*((n-1))*f)
xa1 = x.bar - c*s.x
xb1 = x.bar + c*s.x

ya1 = y.bar - c*s.y
yb1 = y.bar + c*s.y

lines(c(xa1, xb1), c(ya1, ya1), lty = 2, col = 3)
lines(c(xa1, xb1), c(yb1, yb1), lty = 2, col = 3)
lines(c(xa1, xa1), c(ya1, yb1), lty = 2, col = 3)
lines(c(xb1, xb1), c(ya1, yb1), lty = 2, col = 3)

#Bonferroni 
tp = qt(1-.05/(2*p),n-1)
p.low.x = x.bar - tp*s.x
p.high.x = x.bar + tp*s.x
c(p.low.x, p.high.x)

p.low.y = y.bar - tp*s.y
p.high.y = y.bar + tp*s.y
c(p.low.y, p.high.y)

lines(c(p.low.x, p.high.x), c(p.low.y, p.low.y), lty = 2, col = 5, lwd = 2)
lines(c(p.low.x, p.high.x), c(p.high.y, p.high.y), lty = 2, col = 5, lwd = 2)
lines(c(p.low.x, p.low.x), c(p.low.y, p.high.y), lty = 2, col = 5, lwd = 2)
lines(c(p.high.x, p.high.x), c(p.low.y, p.high.y), lty = 2, col = 5, lwd = 2)


#Part e 
#Mahalet and Rachel are having an epic feud right now. Mahalet insists
#that this data came from a bivariate normal distribution with \mu = (22.5, 24.5)
#and \Sigma=[12 8;8 12], while Rachel is extremely confdent that this data came
#from a bivariate normal distribution with \mu = (21.5, 26) and \Sigma = [9 8;8 16]
#Based on the data, which side are you on, Rachels or Mahalets?
m.mu = matrix(c(22.5,24.5),2,1)
r.mu = matrix(c(21.5, 26), 2, 1)
m.Sig = matrix(c(12,8,8,12),2,2)
r.Sig = matrix(c(9,8,8,16),2,2)

library(mvtnorm)
m.ll = dmvnorm(genderwage, mean = m.mu, sigma = m.Sig, log = T)
r.ll = dmvnorm(genderwage, mean = r.mu, sigma = r.Sig, log = T)
mah.ll = sum(m.ll)
rach.ll = sum(r.ll)
like.ratio = exp(rach.ll-mah.ll)
like.ratio = 0.53
c = -2*log(like.ratio)
c = 9.53
1-pchisq(c,2)

#since LR is greater than 1,


#Part f
#Similarly to how you did on HW1 Problem 5. Load both of your variables
#onto the strongest eigen vector (the one with the largest eigen value). Pass \mu_0
#that you used for part a, through the same eigen rotation (lets call it v0).
#Perform a simple t-test for whether or not the mean of your new information
#super-loaded variable could reasonably be equal to v0. Compare your p-values
#from this new univariate test to the p-value from your bivariate test in part a.)

eig = eigen(S.x.bar)
gamma = eig$vectors
lambda = eig$values
lamb1ev1 = lambda[1]*gamma[,1]
new.x = as.matrix(genderwage)%*%gamma[,1]
mu0 = matrix(c(20.47,20.47),ncol=2)
new.mu0 = mu0%*%gamma[,1]
new.mean = mean(new.x)
new.se = sd(new.x)
t = (new.mean - new.mu0)/(new.se/sqrt(n))
t.pval = 2*pt(-abs(t),n-1)

(.95)^2
