#____________________________________________________________________________________________
#                           Georgetown University
#                           Department of Economics
#                           Advanced Data Analysis
#                           prof. Franco Peracchi
#                                 May, 2019
#____________________________________________________________________________________________

#********************************************************************************************
#********************************************************************************************
# 
#   POVERTY IMPUTATION AT THE MICRO LEVEL USING MICRO DATA FROM THE VIETNAM LSMS
#
#********************************************************************************************
#********************************************************************************************

#____________________________________________________________________________________________
# Authors: Lucia Ukalovic (lu53@georgetown.edu)
#          Zeyang Dong    (zd92@georgetown.edu)
#          Junhe Zhang    (jz581@georgetown.edu)
#____________________________________________________________________________________________

rm(list=ls())
#Install the necessary packages, set the working directory and import the dataset
install.packages("haven")
install.packages("rlang")
library(haven)
install.packages("glmnet")
library(glmnet)
install.packages("psych")
library(psych)
install.packages("mclust")
library(mclust)
install.packages("cluster")
library(cluster)
install.packages("flexclust")
library(flexclust)
install.packages("dplyr")
library(dplyr)
setwd("C:/Users/lucia/Documents/Georgetown University/Courses/Advanced Data Analysis/")
table5=read_dta("table5.dta")
attach(table5)

#____________________________________________________________________________________________
#Generate the interaction terms - detailed description can be found in section 2 of our paper
#____________________________________________________________________________________________
# a) Focus interaction terms #

table5$feth=female*ethmin
table5$fecol=female*col

# b) Auxiliary interaction terms #
# 
table5$ageurban=age*urban
table5$agecolurban=age*col*urban

table5$wkcol=workd*col
table5$wkagecol=workd*age*col

table5$refcar=ref*car
table5$airrefcar=airc*ref*car

wt=wall*toilet           
table5$wallto30  =ifelse(wt==30, 1, 0)  
table5$wallto36  =ifelse(wt==36, 1, 0) 

#_____________________________________________________________________________________________
# Split the dataset into the training set (2010) and test set (2012)
#_____________________________________________________________________________________________
leave = names(table5) %in% c("year", "strata", "com", "pline")
new_data=table5[!leave]
training_set=new_data[1:9211, ]
test_set=new_data[9212: dim(new_data)[1], ]

#---------------------------------------------------------------------------------------------

#*********************************************************************************************
#4.1. SUPERVISED LEARNING
#*********************************************************************************************

#_____________________________________________________________________________________________
#     4.1.1.: Focus-Auxiliary Model Regression
#_____________________________________________________________________________________________
# Step 1: Run the regression of lnpcex on all the covariates on the training set
focaux_reg=lm(lnpcex~.-hhszwt, weights = training_set$hhszwt, data = training_set)
# Step 2: Keep the residuals
focaux_rsd=residuals(focaux_reg, type = "response")
# Set the random seed
set.seed(12345)
# Step 3: Draw the residuals
focaux_rsd_s=sample(focaux_rsd, 9324, replace = TRUE)
# Step 4: Predict this regression on the test set
focaux_reg_pred=predict(focaux_reg, newdata = test_set)
# Step 5: Impute log(consumption) for 2012 by adding predictions and residuals from focaux_reg
imp_lnpcex=focaux_reg_pred+focaux_rsd_s
# Step 6: Identify the household as poor if the imputed log(consumption) for that household is 
#         below the poverty line 
poor1=ifelse(imp_lnpcex>table5$pline[9212:18535], 0,1)
#_____________________________________________________________________________________________
# Step 7: Find the imputed poverty rate
mean(poor1)  #Imputed poverty rate is 16.39%
#_____________________________________________________________________________________________
# Step 8: Compute RMSE
se.focaux_reg=sqrt((sum(focaux_rsd^2))/(dim(training_set)[1]-2))
mean(se.focaux_reg)  # RMSE=0.3668
#_____________________________________________________________________________________________
# Step 9: Report the focaux_reg summary (Appendix)
summary(focaux_reg)
#*********************************************************************************************


#_____________________________________________________________________________________________
#     4.1.2. Ridge regression with penalized auxiliary variables
#_____________________________________________________________________________________________
# No penalties on the estimated coefficients of focus variables
penalty=rep(1, dim(training_set)[2]-1)
penalty[c(1,2,3,5,6,23,24,25,26,30,31)]=0

set.seed(12345)
ridge.model=model.matrix(lnpcex~. -hhszwt, data=training_set)[, -1]
ridge=cv.glmnet(ridge.model, training_set$lnpcex, weights=training_set$hhszwt, alpha=0, penalty.factor=penalty)

# ---------------------------------------------------------------------------
# Training set for covariates
lv=names(training_set) %in% c("lnpcex", "hhszwt")
train.mat=training_set[!lv]
int=rep(1, dim(training_set)[1])
train.mat=data.frame(int, train.mat)
tr.mat=data.matrix(train.mat)

# Test set for covariates
test.mat=test_set[!lv]
int=rep(1, dim(test_set)[1])
test.mat=data.frame(int, test.mat)
ts.mat=data.matrix(test.mat)

# ---------------------------------------------------------------------------
# Ridge
set.seed(12345)
fn.ridge = glmnet(ridge.model, training_set$lnpcex, weights=training_set$hhszwt, alpha=0, penalty.factor=penalty)

lam=c(ridge$lambda, 0) # lambda

imp.ridge=rep(NA, length(lam))
#fitimp.ridge=rep(NA, length(lam))
se.ridge=rep(NA, length(lam))

# For each lambda calculate coefficients for training set
for (i in 1:length(lam)) {
  coef10 = predict(fn.ridge, s=lam[i],type="coefficients")
  coef10=matrix(coef10)	
  
  est10=tr.mat %*% coef10                                # estimating log(consumption)
  rsd10=training_set$lnpcex-est10                        # estimating residuals 
  
  se.ridge[i]=sqrt((sum(rsd10^2))/(dim(training_set)[1]-2)) # standard error
  
  rsd12=sample(rsd10, dim(test_set)[1], replace=TRUE) # drawn residuals from empirical distribution)
  pred12=ts.mat %*% coef10                              # predicting log(consumption for 2012)
  plncom=pred12+rsd12                                      # imputed log(consumprion) for 2012 (empirical distribution)
  
  poor=ifelse(plncom> 8.966484, 0, 1)              # define poor or not poor, here poverty line is the same for both year
  #poor.fit=ifelse(pred12> 8.966484, 0, 1)
  #fitimp.ridge[i]=mean(poor.fit)
  imp.ridge[i]=mean(poor)                               # calculating poverty rate
  remove(pred10, rsd10, m ,sd, pred12, plncom, poor, rsd12)
}

# ---------------------------------------------------------------------------
#### Calculating cvm where lambda =0

coef10 = predict(fn.ridge, s=0,type="coefficients") 
coef10=matrix(coef10)	
est10=tr.mat %*% coef10                  #estimating log(consumption)
rsd10=training_set$lnpcex-est10                  # estimating residuals 
mrsd=mean((rsd10-mean(rsd10))^2)   # mean squard error where lambda=0
remove(coef10,est10, rsd10)

cvm=c(ridge$cvm, mrsd)

###### Figure ########
par(mfrow=c(2,3))

# Trace of the imputed poverty rate 

plot(lam[70:length(lam)], imp.ridge[70:length(lam)], ylim=c(0.15,0.21), type="b",
     col="red3", xlab="lambda", ylab="Imputed poverty rate (2012)")
abline(h=0.1689, lty=2, col="blue3", lwd=1.5)
abline(v=0, lty=2, lwd=0.75)                                     # OLS
abline(v=lam[which.min(ridge$cvm)], lty=2, lwd=0.75) # min CV error
abline(v=lam[73], lty=2, lwd=0.75)                            # chosen by ridge


plot(lam[46:69], imp.ridge[46:69], ylim=c(0.15,0.21), type="b",
     col="red3", xlab="lambda", ylab="Imputed poverty rate (2012)",
     main="Trace of imputed poverty rate")
abline(h=0.1689, lty=2, col="blue3", lwd=1.5)
abline(v=lam[58], lty=2, lwd=0.75)      # chosen by true poverty rate


plot(lam[25:45], imp.ridge[25:45], ylim=c(0.15,0.21), type="b",
     col="red3", xlab="lambda", ylab="Imputed poverty rate")
abline(h=0.1689, lty=2, col="blue3", lwd=1.5)


# Mean squared error over lambda
plot(lam[70:length(lam)], cvm[70:length(lam)], ylim=c(0.13,0.27), pch=20,
     col="red3", xlab="lambda", ylab="Mean-Squared error")   
abline(v=0, lty=2, lwd=0.75)                                       # ols
mtext("39", side=3, line=0, at=c(0, 0.3), col="red3")         
abline(v=lam[which.min(ridge$cvm)], lty=2, lwd=0.75)   # min CV error
mtext("39", side=3, line=0, at=c(lam[which.min(ridge$cvm)], 0.3), col="red3")        
abline(v=lam[73], lty=2, lwd=0.75)                              # chosen by ridge
mtext("39", side=3, line=0, at=c(0.187, 0.3), col="red3")


plot(lam[46:69], cvm[46:69], ylim=c(0.13,0.27), pch=20,
     col="red3", xlab="lambda", ylab="Mean-Squared error", 
     main="Mean-Squared error vs. lambda")      
abline(v=lam[58], lty=2, lwd=0.75)                             # chosen by true poverty rate
mtext("39", side=3, line=0, at=c(lam[58], 0.3), col="red3")


plot(ridge$lambda[25:45],ridge$cvm[25:45], ylim=c(0.13,0.27), pch=20,
     col="red3", xlab="lambda", ylab="Mean-Squared error")

# ---------------------------------------------------------------------------
# Print lambda and the associated imputed poverty rate 
# **** When lambda=0, the ridge is consistent with OLS ****
lam[length(lam)]         #lambda
cvm[length(lam)]        #MSE
imp.ridge[length(lam)] #imputed poverty rate for 2012

# **** Where the mean squared error is minimum ****
ridge$lambda[which.min(ridge$lambda)]  #lambda
ridge$cvm[which.min(ridge$lambda)]      #MSE
imp.ridge[which.min(ridge$lambda)]       #imputed poverty rate for 2012

# **** Lambda is selected by ridge ****
lam[73]          #lambda
cvm[73]         #MSE
imp.ridge[73] #imputed poverty rate for 2012

# **** Chosen lambda according to the true poverty rate 2012 ****
coef.ridge = predict(fn.ridge, s=ridge$lambda[58],type="coefficients") 
imp.ridge[58] #imputed poverty rate for 2012
lam[58]        #lambda
cvm[58]       #MSE
se.ridge[58]  #standard error

#*********************************************************************************************

#*********************************************************************************************
#4.2. UNSUPERVISED LEARNING
#*********************************************************************************************
#_____________________________________________________________________________________________
#     k-means Clustering
#_____________________________________________________________________________________________
unch=names(table5)  %in% c("female", "ethmin","pri","lws", "ups","col") # unchanged variables
dt=table5[unch]

poor=ifelse(lnpcex> pline, 0, 1)

# training set vs. test set
train=dt[1: 9211, ]
test=dt[9212: dim(dt)[1], ]
#####################
train.se=rep(NA, 9)
between=rep(NA,9)
within=rep(NA,9)
fit=rep(NA, 9)
tss=rep(NA,9)


for (i  in 2:10) {
  # k-means clustering
  set.seed(12345)
  
  km=kmeans(train, i, iter.max=20, nstart=20)
  group.train=km$cluster
  
  
  train=data.frame(train, poor[1: 9211], group.train)
  train.mat=describeBy (train$poor, train$group.train, mat=TRUE)
  
  
  train.se[i-1]=(t(train.mat[ ,4]/9211)%*%sqrt(km$withinss/km$size))/sqrt(9211) # standard error
  between=km$betweenss
  within[i-1]=km$tot.withinss/9211
  
  fit[i-1]=as.numeric(km$betweenss/km$totss)
  tss[i-1]=as.numeric(km$totss)
  
  # clean data
  train=select(train, -group.train)
  train=select(train, -poor.1.9211.)
  
  remove(km, group.train, train.mat)
}

set.seed(12345)
km=kmeans(train, 10, iter.max=20, nstart=20)
group.inf=cbind(km$size, km$centers)
colnames(group.inf)[1]="size"
group.inf

# group information

#####################
par(mfrow=c(1,1))
plot.new()
with.bet= data.frame(n=seq(2,10), withinss=within, fit=fit)
par(mar=c(2,5,2,5))
with(with.bet, plot(n, withinss, type="b", lwd=1.5,
                    xlab="Number of clusters",
                    ylab="Within groups MSE", 
                    main="Within MSE & betweenSS/totalSS ")
)
abline(h=with.bet$withinss[which.min(with.bet$within)]+1.96*train.se[which.min(with.bet$withinss)], lty=2, lwd=1.5, col="blue")
points(which.min(with.bet$withinss)+1,with.bet$withinss[which.min(with.bet$withinss)], col="red", cex=1.5, pch=20)

par(new=T)                        
with(with.bet, plot(n, fit, type="b", col="red3",
                    ylab=NA, xlab=NA, axes=F))
abline(v=which.max(with.bet$fit)+1, lty=2, lwd=1.5, col="blue")
axis(side=4)
mtext(side=4, line=3, 'betweenSS/totalSS')
legend("top", legend=c("within MSE","betweenSS/totalSS"), 
       lty=c(1,1), col=c("black", "red3"), cex=0.5)


##########Figure 2#############
#gap=clusGap(train, kmeans, 10, B = 50, verbose = interactive())
#plot.new()
#plot(gap, type="l", main="the number of clusters via gap statistics", 
#xlab="number of clusters", ylab="gap")
#abline(v=3, col="blue", lty=2)
#gap.mat=as.data.frame(gap$Tab)

######################################

# k=3
unch=names(table5)  %in% c("female", "ethmin","pri","lws", "ups","col") # unchanged variables
dt=table5[unch]

poor=ifelse(lnpcex> pline, 0, 1)

# training set vs. test set
train=dt[1: 9211, ]
test=dt[9212: dim(dt)[1], ]
#####################
set.seed(12345)
k=3
cl=kcca(train, k, kccaFamily("kmeans"))

pred.test =predict(cl, newdata=test)
pred.test=replace(pred.test, pred.test==1, 0)
pred.test=replace(pred.test, pred.test==3, 1)
pred.test=replace(pred.test, pred.test==0, 3)


test=data.frame(test, poor[9212: 18535], pred.test)
test.mat=describeBy (test$poor, test$pred.test, mat=TRUE)

set.seed(12345)
# k-means clustering
km=kmeans(train, k, iter.max=20, nstart=20)

group.inf3=cbind(km$size, km$centers)
colnames(group.inf3)[1]="size"
group.inf3 # group information

group.train=km$cluster

train=data.frame(train, poor[1: 9211], group.train)
train.mat=describeBy (train$poor, train$group.train, mat=TRUE)
# generate a new mat
opt=matrix(NA, nrow=k, ncol=7)
name.opt=c("group", "observartions(2010)", "fraction of poverty(2010)", "fraction of observation(2010)", 
           "observartions(2012)", "fraction of poverty(2012)", "fraction of observation(2012)")
colnames(opt)=name.opt

opt[, 1]=seq(1:k)
opt[, 2]=train.mat[, 4] #observation
opt[, 3]=train.mat[, 5] # mean
opt[, 4]=train.mat[, 4]/9211 # weight
opt[, 5]=test.mat[, 4]
opt[, 6]=test.mat[, 5] #mean
opt[, 7]=test.mat[, 4]/9324 #weight


p10=t(opt[, 4]) %*%opt[,3]
p10
imp12=t(opt[, 7]) %*%opt[,3] # imputed poverty rate
imp12
p12=t(opt[, 7]) %*%opt[,6]
p12

opt.mat3=as.data.frame(opt)


#_____________________________________________________________________________________________________
# clustering and regression with benchmark model 3
#_____________________________________________________________________________________________________
# ----------------------------------------------------------------------------
## function

fn.poverty=function(k, train, test) {
  
  sub.imp=rep(NA, 3)
  coef=matrix(NA, nrow=dim(train)[2]-3, ncol=3)
  sub.se=rep(NA, 3)
  
  # for each predicted group
  for (j in 1:k) {
    sub.train=subset(train, train$group.train==j, select=c(urban:lnpcex)) #subset of trainning set
    sub.test=subset(test, test$pred.test==j, select=c(urban:lnpcex)) #subset of test set
    
    fn.sub=lm(lnpcex~.-hhszwt, weights=hhszwt, data=sub.train)
    coef[, j]=fn.sub$coefficients
    rsd.sub1=fn.sub$residuals                                         #residuals
    sub.se[j]=sqrt((sum(rsd.sub1^2))/(dim(sub.train)[1]-2))  #standard error for regression
    
    rsd.draw=sample(rsd.sub1, dim(test)[1], replace=TRUE) #empirical error
    fix.fnp=predict(fn.sub, newdata=sub.test)                    # prediction of log(consumption) for 2012 
    pred=fix.fnp+rsd.draw                                              # imputed log(consumption) for 2012
    
    poor.es=ifelse(pred>8.966, 0 , 1)                               #identify poor
    
    sub.imp[j]=mean(poor.es)                                         #imputed poverty rate for 2012 
    row.name=c("Intercept", names(sub.train[-31]))
    remove(sub.train, sub.test, fn.sub, rsd.sub1, m , sd, rsd.draw, fix.fnp, pred, poor.es)
  }
  
  poverty.rate=t(sub.imp)%*%opt[, 7] # imputed overall poverty rate
  
  rownames(coef)=row.name[-3]
  col.name=c("Group1","Group2","Group3")
  colnames(coef)=col.name
  return(list(sub.imp, poverty.rate, coef, sub.se))
}
# -----------------------------------------------------------------------------

leave=names(table5) %in% c("year", "strata", "com", "pline","feth","fecol", "ageurban", "agecolurban",
                           "wkcol","wkagecol","compcell","refcar","airrefcar","wallto30","wallto36")
dt=table5[!leave]

# training set vs. test set
train=dt[1: 9211, ]
train=data.frame(train, poor[1: 9211], group.train )

test=dt[9212: dim(dt)[1], ]
test=data.frame(test, poor[9212: 18535], pred.test)

set.seed(12345)

m3=fn.poverty(k,train,test)
m3.gimp= unlist(m3[1]) # imputed poverty rate for each group
m3.gimp

m3.se=unlist(m3[4]) # standard error
m3.se

m3.imp=unlist(m3[2]) #overall imputed poverty rate
m3.imp

m3.coef=as.data.frame(m3[3])

# ********************************************************************************
# clustering and regression with focus-auxiliary model
# ********************************************************************************
# ----------------------------------------
## function
fn.poverty2=function(k, train, test) {
  
  sub.imp=rep(NA, 3)
  coef=matrix(NA, nrow=dim(train)[2]-3, ncol=3)
  sub.se=rep(NA, 3)
  for (j in 1:k) {
    sub.train=subset(train, train$group.train==j, select=c(urban:wallto36)) #subset
    sub.test=subset(test, test$pred.test==j, select=c(urban:wallto36)) #subset
    
    fn.sub=lm(lnpcex~.-hhszwt, weights=hhszwt, data=sub.train)
    coef[, j]=fn.sub$coefficients
    rsd.sub1=fn.sub$residuals  #residuals
    sub.se[j]=sqrt((sum(rsd.sub1^2))/(dim(sub.train)[1]-2))
    
    rsd.draw=sample(rsd.sub1, dim(test)[1], replace=TRUE) #empirical error
    fix.fnp=predict(fn.sub, newdata=sub.test) # predicted fixed number 
    pred=fix.fnp+rsd.draw
    
    poor.es=ifelse(pred>8.966, 0 , 1)
    
    sub.imp[j]=mean(poor.es)
    row.name=c("Intercept", names(sub.train[-31]))
    remove(sub.train, sub.test, fn.sub, rsd.sub1, m , sd, rsd.draw, fix.fnp, pred, poor.es)
  }
  
  poverty.rate=t(sub.imp)%*%opt[, 7] # imputed poverty rate
  
  rownames(coef)=row.name[-3]
  col.name=c("Group1","Group2","Group3")
  colnames(coef)=col.name
  return(list(sub.imp, poverty.rate, coef, sub.se))
}
# --------------------------------

leave=names(table5) %in% c("year", "strata", "com", "pline")
dt=table5[!leave]

# training set vs. test set
train=dt[1: 9211, ]
train=data.frame(train, poor[1: 9211], group.train )

test=dt[9212: dim(dt)[1], ]
test=data.frame(test, poor[9212: 18535], pred.test)
set.seed(12345)

mb=fn.poverty2(k,train,test)
mb.gimp= unlist(mb[1]) # imputed poverty rate for each group
mb.gimp

fau.se=unlist(mb[4])
fau.se

mb.imp=unlist(mb[2]) #overall imputed poverty rate
mb.imp

mb.coef=as.data.frame(mb[3])


# ********************************************************************************
# clustering and ridge regression 
# ********************************************************************************
# ----------------------------------
# function 
fn.gridge=function(j){ # group number
  sub.train=subset(train, train$group.train==j, select=c(urban:wallto36)) #subset
  sub.test=subset(test, test$pred.test==j, select=c(urban:wallto36)) #subset
  
  #####################
  # ridge regression with penalty on auxiliary variables #
  penalty=rep(1, dim(sub.train)[2]-1)
  penalty[c(1,2,3,5,6,23,24,25,26,30,31)]=0
  
  set.seed(12345)
  ridge.model=model.matrix(lnpcex~. -hhszwt, data=sub.train)[, -1]
  ridge=cv.glmnet(ridge.model, sub.train$lnpcex, weights=sub.train$hhszwt, alpha=0, penalty.factor=penalty)
  plot(ridge)
  ##############################
  lv=names(sub.train) %in% c("lnpcex", "hhszwt")
  train.mat=sub.train[!lv]
  int=rep(1, dim(sub.train)[1])
  train.mat=data.frame(int, train.mat)
  tr.mat=data.matrix(train.mat)
  
  
  test.mat=sub.test[!lv]
  int=rep(1, dim(sub.test)[1])
  test.mat=data.frame(int, test.mat)
  ts.mat=data.matrix(test.mat)
  
  #ridge
  set.seed(12345)
  fn.ridge = glmnet(ridge.model, sub.train$lnpcex, weights=sub.train$hhszwt, alpha=0, penalty.factor=penalty)
  
  lam=c(ridge$lambda, 0) # lambda
  
  imp.ridge=rep(NA, length(lam))
  fitimp.ridge=rep(NA, length(lam))
  se.ridge=rep(NA, length(lam))
  
  for (i in 1:length(lam)) {
    #for each lambda calculating coefficients for training set
    coef10 = predict(fn.ridge, s=lam[i],type="coefficients") 
    coef10=matrix(coef10)	
    
    est10=tr.mat %*% coef10                     #estimating log(consumption)
    rsd10=sub.train$lnpcex-est10               # estimating residuals 
    
    se.ridge[i]=sqrt((sum(rsd10^2))/(dim(sub.train)[1]-2))
    
    rsd12=sample(rsd10, dim(sub.test)[1], replace=TRUE) # drawn residuals from empirical distribution)
    pred12=ts.mat %*% coef10              # predicting log(consumption for 2012)
    plncom=pred12+rsd12                     # imputed log(consumprion) for 2012 (empirical distribution)
    
    poor=ifelse(plncom> 8.966484, 0, 1)  # define poor or not poor, here poverty line are same in both year
    poor.fit=ifelse(pred12> 8.966484, 0, 1)
    fitimp.ridge[i]=mean(poor.fit)
    imp.ridge[i]=mean(poor)                           # calculating poverty rate
    remove(pred10, rsd10, m ,sd, pred12, plncom, poor, rsd12)
  }
  
  ########################################
  ####calculating cvm where lambda =0
  
  coef10 = predict(fn.ridge, s=0,type="coefficients") 
  coef10=matrix(coef10)	
  est10=tr.mat %*% coef10                     #estimating log(consumption)
  rsd10=sub.train$lnpcex-est10               # estimating residuals 
  mrsd=mean((rsd10-mean(rsd10))^2)      # mean squard error where lambda=0
  remove(coef10,est10, rsd10)
  
  
  cvm=c(ridge$cvm, mrsd)
  
  return(list(lam,imp.ridge,se.ridge,cvm))
  #list(lambda, imputed poverty rate, standard error, mse)
}
# --------------------------------------------
# for group 1
g1=fn.gridge(1)
g1.lam=unlist(g1[1]) #lambda
g1.imp=unlist(g1[2]) #imputed poverty rate
g1.se=unlist(g1[3]) #standard error
g1.cvm=unlist(g1[4])
# lambda chosen by ridge lam[73]=0.20158143
# based on true poverty rate for 2012 lam[95]

# for group 2
g2=fn.gridge(2)
g2.lam=unlist(g2[1]) #lambda
g2.imp=unlist(g2[2]) #imputed poverty rate
g2.se=unlist(g2[3]) #standard error
g2.cvm=unlist(g2[4])
# lambda chosen by ridge lam[69]
# based on true poverty rate for 2012 lam[9]

# for group 3
g3=fn.gridge(3)
g3.lam=unlist(g3[1]) #lambda
g3.imp=unlist(g3[2]) #imputed poverty rate
g3.se=unlist(g3[3]) #standard error
g3.cvm=unlist(g3[4])

# lambda chosen by ridge lam[66]
# based on true poverty rate for 2012 lam[91]

mridge=matrix(NA, nrow=k, ncol=8)
name.mridge=c("group","ridge", "ridge(l)", "ridge[se]","tpr (2012)", "tpr(2012)[l]", "tpr(2012)[se]",
              "frac of obs. 2012" )
colnames(mridge)=name.mridge

mridge[,1]=seq(1:k)

mridge[1,2]=g1.lam[71]
mridge[1,3]=g1.imp[71]
mridge[1,4]=g1.se[71]

mridge[2,2]=g2.lam[68]
mridge[2,3]=g2.imp[68]
mridge[2,4]=g2.se[68]

mridge[3,2]=g3.lam[67]
mridge[3,3]=g3.imp[67]
mridge[3,4]=g3.se[67]


mridge[1,5]=g1.lam[95]
mridge[1,6]=g1.imp[95]
mridge[1,7]=g1.se[95]

mridge[2,5]=g2.lam[21]
mridge[2,6]=g2.imp[21]
mridge[2,7]=g2.se[21]

mridge[3,5]=g3.lam[88]
mridge[3,6]=g3.imp[88]
mridge[3,7]=g3.se[88]

mridge[,8]=opt[,7]

imp.gr=t(mridge[ ,3]) %*% mridge[ ,8]
imp.gr
imp.gtpr=t(mridge[ ,6])%*% mridge[ ,8]
imp.gtpr

#*********************************************************************************************
#*********************************************************************************************


#____________________________________________________________________________________________


#*********************************************************************************************
# PROBABILITY APPROACH
#*********************************************************************************************
attach(table5)
dataset = table5
#*********************************************************************************************
# Generating Interaction Terms
#_____________________________________________________________________________________________
# Set quadratic term of age for probability model II
agesq = age^2
table5$agesq = agesq

# Focus interaction terms
table5$feth=female*ethmin
table5$fecol=female*col

# Auxiliary interaction terms
table5$ageurban=age*urban
table5$agecolurban=age*col*urban
table5$wkcol=workd*col
table5$wkagecol=workd*age*col
table5$compcell=comp*mphone
table5$airrefcar=airc*ref*car
table5$refcar=ref*car
wt=wall*toilet           
table5$wallto30  =ifelse(wt==30, 1, 0)  
table5$wallto36  =ifelse(wt==36, 1, 0)

dataset = table5

# Set trainning and test set
train=dataset[1: 9211, ]
test=dataset[9212: dim(dataset)[1], ]

# Generate binary variable of poverty
attach(train)
poor <- as.numeric(lnpcex < pline)

leave=names(train) %in% c("year", "strata", "com", "pline", "lnpcex","hhszwt")
dt1=train[!leave]

leave=names(test) %in% c("year", "strata", "com", "pline", "lnpcex","hhszwt")
test=test[!leave]

x= model.matrix(poor~.,dt1)
y= poor
#********************************************************************************************
# Estimating LPM,Logit and Probit with benchmark model and do predictions
#____________________________________________________________________________________________


suppressMessages(library(lmtest))
model.base <- (poor~urban+hhsize+age+workd+female+ethmin+car+motob+bicy+dphone+mphone+dvd+tivi
             +comp+ref+airc+wash+efan+lnarea+wall+water+toilet+pri+lws+ups+col+age0to14sh
             +age15to24sh+age25to59sh)
res.logit <- glm(model.base,data=dt1,family=binomial(link="logit"))


suppressMessages(library(lmtest))
model.base <- (poor~urban+hhsize+age+workd+female+ethmin+car+motob+bicy+dphone+mphone+dvd+tivi
              +comp+ref+airc+wash+efan+lnarea+wall+water+toilet+pri+lws+ups+col+age0to14sh
              +age15to24sh+age25to59sh)
res.probit <- glm(model.base,data=dt1,family=binomial(link="probit"))

prob_pred1 = predict(res.logit,type = 'response',newdata = test)
prob_pred2 = predict(res.probit,type = 'response',newdata = test)

# Making predictions using LPM with a Gaussian error term generated by LPM(there are also other 
# alternatives like empirical distribution)
lpm= lm(poor~urban+hhsize+age+workd+female+ethmin+car+motob+bicy+dphone+mphone+dvd+tivi+comp
             +ref+airc+wash+efan+lnarea+wall+water+toilet+pri+lws+ups+col+age0to14sh+age15to24sh
             +age25to59sh,data=dt1)
prob_pred3 = predict(lpm,type = 'response',newdata = test)+rnorm(1,mean = 0,sd = 0.305)
poor_pred3 = subset(prob_pred3,prob_pred3>0.5)

# Prediction of poverty in benchmark model
poor_pred1= mean(prob_pred1)
poor_pred2= mean(prob_pred2)
poor_pred3= mean(prob_pred3)

# SE for the prediction
sd(prob_pred1)
sd(prob_pred2)
sd(prob_pred3)

# LASSO for LPM in model II and prediction
cv.out1=cv.glmnet(x,y,alpha=1)
plot(cv.out1)
coef(cv.out1)
lpm1= lm(poor~hhsize+age+ethmin+motob+dphone+mphone+dvd+ref+efan+lnarea+wall+water+toilet
         +age0to14sh+age25to59sh,data=dt1)

residuallpm1=resid(lpm1)
sd(residuallpm1)
prob_pred3.3 = predict(lpm1,type = 'response',newdata = test)+rnorm(1,mean = 0,sd = 0.306)
poor_pred3.3 = subset(prob_pred3.3,prob_pred3.3>0.5)

length(poor_pred3.3)

poor_pred3.3= mean(prob_pred3.3)
sd(prob_pred3.3)

# Logit LASSO and its prediction
logitlasso1 = cv.glmnet(x, y,family=c("binomial"),
                        offset=NULL, alpha = 1, nlambda = 100,
                        lambda=NULL,
                        standardize = TRUE, intercept=TRUE, thresh = 1e-07,  
                        lower.limits=-Inf, upper.limits=Inf, maxit=100000,
                        type.logistic=c("Newton"), 
                        standardize.response=FALSE, type.multinomial=c("ungrouped"))
coef(logitlasso1)
plot(logitlasso1)

suppressMessages(library(lmtest))
model.base <- (poor~urban+hhsize+ethmin+motob+dphone+mphone+dvd+comp+ref+efan+wall+water
            +toilet+age0to14sh+age25to59sh)
res.logit2 <- glm(model.base,data=dt1,family=binomial(link="logit"))

prob_pred1.2 = predict(res.logit2,type = 'response',newdata = test)
poor_pred1.2 = subset(prob_pred1.2,prob_pred1.2>0.5)

length(poor_pred1.2)
poor_pred1.3 = mean(prob_pred1.2)

sd(prob_pred1.2)

#Probit LASSO and its predictions
probitlasso1 = cv.glmnet(x, y,family=c("gaussian"),
                         offset=NULL, alpha = 1, nlambda = 100,
                         lambda=NULL,
                         standardize = TRUE, intercept=TRUE, thresh = 1e-07,  
                         lower.limits=-Inf, upper.limits=Inf, maxit=100000,
                         type.gaussian=c("naive"),
                         standardize.response=FALSE, type.multinomial=c("ungrouped"))

coef(probitlasso1)
plot(probitlasso1)

suppressMessages(library(lmtest))
model.base <- (poor~hhsize+ethmin+motob+dphone+mphone+dvd+ref+efan+lnarea+wall+water+toilet
             +age0to14sh+age25to59sh)
res.probit2 <- glm(model.base,data=dt1,family=binomial(link="probit"))

prob_pred2.2 = predict(res.probit2,type = 'response',newdata = test)

poor_pred2.2 = subset(prob_pred2.2,prob_pred2.2>0.5)
length(poor_pred2.2)
poor_pred2.3 = mean(prob_pred2.2)
sd(prob_pred2.2)

#LOGIT,Probit and LPM LASSO for model III
lpm3= lm(poor~hhsize+ethmin+motob+dphone+mphone+dvd+ref+efan+lnarea+wall+water+toilet+age0to14sh
              +age25to59sh+feth,data=dt1)
residuallpm3=resid(lpm3)
sd(residuallpm3)

prob_pred3.3 = predict(lpm3,type = 'response',newdata = test)+rnorm(1,mean = 0,sd = 0.305)
poor_pred3.3 = subset(prob_pred3.3,prob_pred3.3>0.5)
length(poor_pred3.3)

poor_pred3.4 = mean(prob_pred3.3)

sd(prob_pred3.3)

##Logit LASSS and prediction

logitlasso2 = cv.glmnet(x, y,family=c("binomial"),
                        offset=NULL, alpha = 1, nlambda = 100,
                        lambda=NULL,
                        standardize = TRUE, intercept=TRUE, thresh = 1e-07,  
                        lower.limits=-Inf, upper.limits=Inf, maxit=100000,
                        type.logistic=c("Newton"),
                        standardize.response=FALSE, type.multinomial=c("ungrouped"))

coef(logitlasso2)
plot(logitlasso2)

suppressMessages(library(lmtest))
model.base1 <- (poor~urban+hhsize +ethmin +motob +dphone+mphone+dvd +comp+ref +wash+efan +lnarea+wall+water
               +toilet +age0to14sh +age25to59sh+feth+wallto30)
res.logit3 <- glm(model.base1,data=dt1,family=binomial(link="logit"))

prob_pred1.4 = predict(res.logit3,type = 'response',newdata = test)
poor_pred1.4 = subset(prob_pred1.4,prob_pred1.4>0.5)

length(poor_pred1.4)
poor_pred1.5 = mean(prob_pred1.4)

sd(prob_pred1.4)

# Probit LASSO and prediction
probitlasso2 = cv.glmnet(x, y,family=c("gaussian"),
                         offset=NULL, alpha = 1, nlambda = 100,
                         lambda=NULL,
                         standardize = TRUE, intercept=TRUE, thresh = 1e-07,  
                         lower.limits=-Inf, upper.limits=Inf, maxit=100000,
                         type.gaussian=c("naive"),
                         standardize.response=FALSE, type.multinomial=c("ungrouped"))

coef(probitlasso2)
plot(probitlasso2)

suppressMessages(library(lmtest))
model.base <- poor~hhsize+ethmin+motob+dphone+mphone+dvd+ref+efan+lnarea+wall+water+toilet+age0to14sh+age25to59sh
res.probit3 <- glm(model.base,data=dt1,family=binomial(link="probit"))

prob_pred2.4 = predict(res.probit3,type = 'response',newdata = test)
poor_pred2.4 = subset(prob_pred2.4,prob_pred2.4>0.5)

length(poor_pred2.4)
poor_pred2.5 = mean(prob_pred2.4)

sd(prob_pred2.4)


# Logit,Probit,LPM LASSO and predictions for focus-auxiliary model

penalty = rep(1,dim(dt1)[2]-1)
penalty[c(1,2,3,5,6,23,24,25,31,32)]=0

cv.out1=cv.glmnet(x,y,alpha=1)

lpm4= lm(poor~hhsize+female+age+pri+lws+ups+col+urban+ethmin+motob+dphone+mphone+dvd+ref+efan
              +lnarea+wall+water+toilet+age0to14sh+age25to59sh+feth+fecol,data=dt1)
prob_pred3.4 = predict(lpm3,type = 'response',newdata = test)++rnorm(1,mean = 0,sd = 0.305)
poor_pred3.4 = subset(prob_pred3.4,prob_pred3.4>0.5)
length(poor_pred3.4)

poor_pred3.5 = mean(prob_pred3.4)

sd(prob_pred3.4)

residuallpm4=resid(lpm4)
summary(residuallpm4)
var(residuallpm4)

logitlassok = cv.glmnet(x, y,family=c("binomial"),
                        offset=NULL, alpha = 1, nlambda = 100,
                        lambda=NULL,
                        standardize = TRUE, intercept=TRUE, thresh = 1e-07,  
                        lower.limits=-Inf, upper.limits=Inf, maxit=100000,
                        type.logistic=c("Newton"),
                        standardize.response=FALSE, type.multinomial=c("ungrouped"))

suppressMessages(library(lmtest))
model.base1 <- (poor~urban+hhsize+age+female+pri+lws+ups+col +ethmin +motob +dphone+mphone+dvd
              +comp+ref +wash+efan +lnarea+wall+water+toilet +age0to14sh +age25to59sh+feth+wallto30++fecol)
res.logit4 <- glm(model.base1,data=dt1,family=binomial(link="logit"))

prob_pred1.6 = predict(res.logit4,type = 'response',newdata = test)
poor_pred1.6 = subset(prob_pred1.6,prob_pred1.6>0.5)

length(poor_pred1.6)
poor_pred1.7 = mean(prob_pred1.6)

sd(prob_pred1.6)

probitlassok = cv.glmnet(x, y,family=c("gaussian"),
                         offset=NULL, alpha = 1, nlambda = 100,
                         lambda=NULL,
                         standardize = TRUE, intercept=TRUE, thresh = 1e-07,  
                         lower.limits=-Inf, upper.limits=Inf, maxit=100000,
                         type.gaussian=c("naive"),
                         standardize.response=FALSE, type.multinomial=c("ungrouped"))

suppressMessages(library(lmtest))
model.base <- (poor~hhsize+urban+age+female+pri+lws+ups+col+ethmin+motob+dphone+mphone+dvd+ref+efan
             +lnarea+wall+water+toilet+age0to14sh+age25to59sh+feth+fecol)
res.probit4 <- glm(model.base,data=dt1,family=binomial(link="probit"))

prob_pred2.5 = predict(res.probit4,type = 'response',newdata = test)
poor_pred2.6 = subset(prob_pred2.5,prob_pred2.5>0.5)

length(poor_pred2.6)
poor_pred2.7 = mean(prob_pred2.5)

sd(prob_pred2.5)

# Kernel density for focus-auxiliary model predictions

d1.3 <- density(prob_pred1.6)
d2.3 <- density(prob_pred2.5)
d3.3 <- density(prob_pred3.4)

plot(d1.3,
     main = "Kernel Density",col = "red")
lines(d2.3,col = "green")
lines(d3.3)
legend("topright",
       c("LPM","Logit","Probit"),
       fill=c("black","red","green")
)


# End
