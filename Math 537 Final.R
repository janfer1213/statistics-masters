##Math 537 Final
setwd("/Users/macbookpro/Documents/Janice/Cal-State Fullerton/Math 437 Lab")
data = read.csv("College.csv",h = T)
College = data[,-c(1,15)]
exclusivity = 100*(College$Apps - College$Accept)/(College$Apps) + 100*(College$Enroll/College$Accept)
College = data.frame(cbind(College,exclusivity))

#Cross Validation: Splitting into Training and Test data sets
set.seed(2020)
train = sample(1:nrow(College),.75*nrow(College))
test = -train
Coll.train = College[train,]
Coll.test = College[test,]

#i.	A simple linear model using least squares and all of the variables.
lm.fit = lm(exclusivity ~ Private + Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+Room.Board+Books+Personal   
            +PhD+S.F.Ratio+perc.alumni+Expend+Grad.Rate, data = Coll.train)
summary(lm.fit)
lm.pred = predict(lm.fit, Coll.test)
lmSSR = mean((lm.pred - Coll.test$exclusivity)^2)
lmSSR

#ii.	A ridge regression model with lambda = best.lam.ridge*
library(glmnet)
grid = 10^seq(4,-2,length=100)
train.mat = model.matrix(exclusivity~Private+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+Room.Board+Books+Personal   
                         +PhD+S.F.Ratio+perc.alumni+Expend+Grad.Rate, data = Coll.train)[,-1]
##removing intercept column
test.mat = model.matrix(exclusivity~Private+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+Room.Board+Books+Personal   
                        +PhD+S.F.Ratio+perc.alumni+Expend+Grad.Rate, data=Coll.test)[,-1]
set.seed(2020)
cv.ridge = cv.glmnet(train.mat, Coll.train[, "exclusivity"], alpha=0, nfolds = 10, lambda=grid)
plot(cv.ridge, main = "Cross Validation for Ridge")
best.lam.ridge = cv.ridge$lambda.min
best.lam.ridge
ridge.mod = glmnet(train.mat, Coll.train[, "exclusivity"], alpha=0, nfolds = 10, lambda=grid)
summary.ridge = predict(ridge.mod,type="coefficients",s=best.lam.ridge)[1:15,]
data.frame(Variables=c("Intercept","Private","Top10perc","Top25perc","F.Undergrad","P.Undergrad","Outstate","Room.Board","Books","Personal","PhD","S.F.Ratio","perc.alumni","Expend","Grad.Rate") 
           ,Coefficent_Estimates = as.vector(summary.ridge))
ridge.pred = predict(ridge.mod, newx=test.mat, s=best.lam.ridge)
ridgSSR = mean((Coll.test$exclusivity - ridge.pred)^2)
ridgSSR

#iii.	A lasso regression model with lambda = best.lam.lasso*
set.seed(2020)
cv.lasso = cv.glmnet(train.mat, Coll.train[, "exclusivity"], alpha=1, nfolds = 10,
                      lambda=grid) 
plot(cv.lasso, main = "Cross Validation for Lasso")
best.lam.lasso = cv.lasso$lambda.min
best.lam.lasso
lasso.mod = glmnet(train.mat, Coll.train[, "exclusivity"], alpha=1, nfolds = 10, lambda=grid)
summary.lasso = predict(lasso.mod,type="coefficients",s=best.lam.lasso)[1:15,]
data.frame(Variables=c("Intercept","Private","Top10perc","Top25perc","F.Undergrad","P.Undergrad","Outstate","Room.Board","Books","Personal","PhD","S.F.Ratio","perc.alumni","Expend","Grad.Rate"),Coefficent_Estimates = as.vector(summary.lasso))
lasso.pred = predict(lasso.mod, newx=test.mat, s=best.lam.lasso)
lassSSR= mean((Coll.test[, "exclusivity"] - lasso.pred)^2)
lassSSR

#iv.	An elastic.net regression model with lambda1 = lambda2 = best.lam.elasticnet*
set.seed(2020)
cv.elasticnet = cv.glmnet(train.mat, Coll.train[, "exclusivity"], alpha=0.5, nfolds = 10, lambda=grid)
plot(cv.elasticnet, main = "Cross Validation for Elastic Net")
best.lam.elasticnet = cv.elasticnet$lambda.min
best.lam.elasticnet
elasticnet.mod = glmnet(train.mat, Coll.train[, "exclusivity"], alpha=0.5, nfolds = 10, lambda=grid)
summary.en = predict(elasticnet.mod,type="coefficients",s=best.lam.lasso)[1:15,]
data.frame(Variables=c("Intercept","Private","Top10perc","Top25perc","F.Undergrad","P.Undergrad","Outstate","Room.Board","Books","Personal","PhD","S.F.Ratio","perc.alumni","Expend","Grad.Rate"),Coefficent_Estimates = as.vector(summary.en))
elasticnet.pred = predict(elasticnet.mod, newx=test.mat, s=best.lam.elasticnet)
elasticnetSSR = mean((Coll.test$exclusivity - elasticnet.pred)^2)
elasticnetSSR

#v.	PCR.  Be sure to document and or discuss the number of components you selected and why.
library(pls)
set.seed(2020)
pcr.fit = pcr(exclusivity~Private+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+Room.Board+Books+Personal   
              +PhD+S.F.Ratio+perc.alumni+Expend+Grad.Rate, data=Coll.train, scale = T,validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP",main = "Validation Plot for PCR")
msep.pcr = MSEP(pcr.fit)
which.min(msep.pcr$val[1,1, ])
abline(v = 13, col = "red") #choose 13, 14 is just original least squares
pcr.pred = predict(pcr.fit, Coll.test, ncomp=14)
pcrSSR = mean((Coll.test[, "exclusivity"] - pcr.pred)^2)
pcrSSR

#vi.	A PLSR.  Be sure to document and or discuss the number of components you selected and why.
set.seed(2020)
pls.fit = plsr(exclusivity~Private+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+Room.Board+Books+Personal   
               +PhD+S.F.Ratio+perc.alumni+Expend+Grad.Rate, data=Coll.train, scale=T, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP", main = "Validation Plot for PLS")
msep.pls = MSEP(pls.fit)
which.min(msep.pls$val[1,1, ])
abline(v = 8, col = "blue")
pls.pred=predict(pls.fit,Coll.test,ncomp=8)
plsSSR = mean((pls.pred-Coll.test$exclusivity)^2)
plsSSR

#discuss why you think the model that was most competitive won out over the others? 
test.avg = mean(Coll.test[, "exclusivity"])
test.exclusivity = Coll.test[, "exclusivity"]
lm.test.r2 = 1 - (lmSSR /mean((test.exclusivity - test.avg)^2))
ridge.test.r2 = 1 - (ridgSSR /mean((test.exclusivity - test.avg)^2))
lasso.test.r2 = 1 - (lassSSR /mean((test.exclusivity - test.avg)^2))
elasticnet.test.r2 = 1 - (elasticnetSSR /mean((test.exclusivity - test.avg)^2))
pcr.test.r2 = 1 - (pcrSSR /mean((test.exclusivity - test.avg)^2))
pls.test.r2 = 1 - (plsSSR /mean((test.exclusivity - test.avg)^2))
SSRdf <- data.frame(Model=c("OLS", "Ridge", "Lasso","E-Net" ,"PCR", "PLS"), SSR = c(lmSSR, ridgSSR, lassSSR, elasticnetSSR, pcrSSR, plsSSR))
SSRdf
df <- data.frame(Model=c("OLS", "Ridge", "Lasso","E-Net" ,"PCR", "PLS"), TestRSquared = c(lm.test.r2, ridge.test.r2, lasso.test.r2, elasticnet.test.r2, pcr.test.r2, pls.test.r2))
df
p<-ggplot(SSRdf, aes(x=Model, y=SSR, fill=Model)) + geom_bar(stat="identity") + labs(title="Test SSR for all Models",x="Model", y = "SSR") +theme_minimal() 
p
s<-ggplot(df, aes(x=Model, y=TestRSquared, fill=Model)) + geom_bar(stat="identity") + labs(title="Test R-Squared for all Models",x="Model", y = "Test R-Squared") +theme_minimal() + scale_fill_brewer(palette="Spectral") 
s
##R squared not very high and seems pretty much the same throughout
##seems like original model may not be overfitting to begin with
##highest R-squared  for PLS model
