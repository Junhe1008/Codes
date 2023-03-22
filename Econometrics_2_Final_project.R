rm(list=ls())
setwd("D:/2021-22 sem2 M1 APE/Econometrics 2 Time series/Homework/PSE-metrics2-projects")
install.packages("ggplot2")
install.packages("forecast")
install.packages("urca")
install.packages("parallel")
install.packages("tidyverse")
library(forecast)
library(ggplot2)
library(urca)
library(vars)
library(stargazer)
library(foreign)
library(readr)
library(tidyverse)
library(broom)

#0) import data
hwdata = read.delim("dsge1_data.csv", sep=",", header=TRUE)
head(hwdata)
hwdata = ts(hwdata[,-1], start=c(1983,1), frequency=4)
summary(hwdata)
hwdata = window(hwdata, end=c(2002,4))

############# 1. Univariate Analysis ##############
# 1) plot each time series
infl = hwdata[,"infl"]
int = hwdata[,"int"]
plot(infl) ## plot U.S inflation rate
plot(int) ## plot U.S nominal interest rate

#2）Unit root tests for each time series
## selection of optimal lags for each time series using information criteria
VARselect(int, lag.max=8) ## 2 lags for U.S nominal interest rate with all criteria
VARselect(infl, lag.max=8) ## disagreement, maybe 3 lags or 1 lag
stargazer(VARselect(int, lag.max=8), title="Information criteria results of U.S nominal interest rate")
stargazer(VARselect(infl, lag.max=8), title="Information criteria results of U.S inflation")




## UR test for U.S inflation
### ADF test (Augmented Dickey-Fuller test)
summary(ur.df(infl, type="drift",lags = 3)) #choosing lag=3, we reject Ho at 5% ---> no UR in infl
summary(ur.df(infl, type= "drift",lags = 1)) #choosing lag=1, we reject Ho at 1% ---> no UR in infl


### PP test (Phillips-Perron)
summary(ur.pp(infl, model="constant", type="Z-tau", use.lag=3)) # reject Ho (UR) at 1% ---> stationarized TS 
summary(ur.pp(infl, model="constant", type="Z-tau", use.lag=1)) # reject Ho (UR) at 1% ---> stationarized TS

### ERS tests (Elliott-Rothenberg-Stock)
summary(ur.ers(infl, model="constant", type="DF-GLS", lag.max=3)) ## reject Ho (UR) at 10% ----> stationarized TS, but the result is not so strong
summary(ur.ers(infl, model="constant", type="DF-GLS", lag.max=1)) ## reject Ho (UR) at 5% ---> stationarized TS

### KPSS test (Kwiatkowski-Phillips-Schmidt-Shin)
#--------------------
### the null hypothesis is I(0), i.e., stationarity
summary(ur.kpss(infl, type="mu", lags="long")) ## do not reject Ho(stationarity around a constant) at 5% ----> stationarized TS



## UR test for U.S nominal interest rate
dint=diff(int)
### ADF test (Augmented Dickey-Fuller test)
summary(ur.df(int, type="trend", lags = 2)) ## we reject the H0 and H01 but we cannot still make sure if rh0=1
summary(ur.df(dint, type="drift", lags = 1)) ## reject H0---->no UR in dint, so int is I(1)

### PP test (Phillips-Perron)
summary(ur.pp(int, model="trend", type="Z-tau", use.lag=2)) ## do not reject Ho (UR)
summary(ur.pp(dint, model="constant", type="Z-tau", use.lag=1)) ## Reject Ho at 1% ----> stationarized TS

### ERS tests (Elliott-Rothenberg-Stock)
summary(ur.ers(int, model="trend", type="DF-GLS", lag.max=2)) ## do not reject Ho(UR)
summary(ur.ers(dint, model="constant", type="DF-GLS", lag.max=1)) ## reject Ho at 1% ---> stationarized

### KPSS test (Kwiatkowski-Phillips-Schmidt-Shin)
summary(ur.kpss(int, type="tau", lags="long")) # do not reject H0 (stationarity around a trend) even at 10% ---> stationarized TS
summary(ur.kpss(dint, type="mu", lags="long")) ## do not reject Ho (stationarity around a constant), even at 10% ---> stationarized TS

plot(dint)


# 3) Identification of the ARMA process for U.S nominal interest rate
## plot acf and Pacf to determine the upper bound of p and q
par(mfrow=c(1,2))
ACF <- Acf(int, lag.max = 40) # the upper bound of q ----> qu=7
PACF <- Pacf(int, lag.max = 40) # the upper bound of p ---> pu=2


## Pure AR test
ar2int <- Arima(int,order = c(2,0,0))
summary(ar2int) # significantly p=2

### WN residuals
Box.test(ar2int$residuals, lag = 40, type = "Ljung-Box") ## do not reject H0, hence residuals are WN
tsdiag(ar2int, gof.lag = 40) ## check robustness

### normal distribution test
par(mfrow=c(1,1))
hist(ar2int$residuals, breaks = 20)
shapiro.test(ar2int$residuals) # reject H0 and residuals are not normally distributed

##---------------------------------------
## pure MA test
ma7int <- Arima(int,order = c(0,0,7))
summary(ma7int) # significantly q=7

### WN residuals
Box.test(ma7int$residuals, lag = 40, type = "Ljung-Box") ## do not reject H0, hence residuals are Wn
tsdiag(ar2int, gof.lag = 40) ## check robustness

### normal distribution test
hist(ma7int$residuals, breaks = 20) ## visualization of distribution
shapiro.test(ma7int$residuals) # reject H0 and hence residuals are not normally distributed


##---------------------------------------
## ARMA(p,q) test
arma27int <- Arima(int,order = c(2,0,7))
summary(arma27int) #the lag 3,4,5,7 of MA components are not significant at 5% under t-test.
arma26int <- Arima(int,order = c(2,0,6))
summary(arma26int) #the lag 3,4,5 of MA components are not significant at 5% under t-test.
arma25int <- Arima(int,order = c(2,0,5))
summary(arma25int) #the lag 3,4,5 of MA components are not significant at 5% under t-test.
arma24int <- Arima(int,order = c(2,0,4))
summary(arma24int) #the lag 3,4 of MA components are not significant at 5% under t-test.
arma23int <- Arima(int,order = c(2,0,3))
summary(arma23int) #the MA components are not significant at 5% under t-test.
arma22int <- Arima(int,order = c(2,0,2))
summary(arma22int) #the MA components are not significant at 5% under t-test.
arma21int <- Arima(int,order = c(2,0,1))
summary(arma21int)#the MA components are not significant at 5% under t-test.

## WN residuals
Box.test(arma27int$residuals, lag = 40, type = "Ljung-Box") #do not reject Ho, hence residuals are WN.
tsdiag(arma27int, gof.lag = 40) ## check robustness

## normal distribution test
hist(arma27int$residuals, breaks = 20)
shapiro.test(arma27int$residuals) # reject H0 and hence residuals are not normally distributed


## Choice of the best model (minimizing the criterion value) using BIC
ar2int$bic
ma7int$bic
arma27int$bic


## AR(2) is the best


# 4) Forecasts: in-sample; out-sample and an example of simulated out-of-sample
# for the U.S nominal interest rate

## In-sample forecasting (=predictions)
fit_int = ar2int$fitted # fitted values of the U.S nominal interest rate
plot(cbind(int,fit_int), plot.type="s", col=c("black","red")) 

## out-of-sample forecasting
forecast_int <- forecast(object = int, model = ar2int) # use the full sample to estimate the model
plot(forecast_int) # The forecasts and The forecast intervals

## simulated out-of-sample forecasting : from 1999 Q1
int99q1 <- window(int, end = c(1999, 1))
ar2int_99q1 <- Arima(int99q1, order=c(2,0,0)) # re-estimate the model for this subsample!
sim_forecast_int_99q1 <- forecast(ar2int_99q1)
plot(sim_forecast_int_99q1)
lines(int, col = "black") ## compare the forecasts and the realizations



#=========================================
# Part II: Multivariate Analysis

# Q1

vse = VARselect(hwdata,lag.max = 4)  # Information criterion for lag selection
stargazer(vse)

# Q2

VAR2 <- VAR(hwdata,p=2)  #Estimate the VAR model of lag 2
summary(VAR2)

stargazer(VAR2[["varresult"]])

serial.test(VAR2, lags.pt=15, type="PT.asymptotic")  #Testing the serial correlation of residuals

normality.test(VAR2) #Testing the normality of residuals


# Q3

VAR2_1 <- VAR(hwdata,p=2,type ="none")

causality(VAR2_1,cause ="int",boot = TRUE,boot.runs = 1000)
causality(VAR2_1,cause ="infl",boot = TRUE,boot.runs = 1000) #Testing the Granger Causality and Instantaneous Cansality



# Q4

fv1 = ts(fitted(VAR2), end=c(2002,1), frequency=4)
plot(int);lines(fv1[,"int"], col="red")
plot(infl);lines(fv1[,"infl"], col="blue")   #Generating In-sample fit

forecast_VAR2_out = forecast(VAR2, h=8)
plot(forecast_VAR2_out)    #Generating 2-year out-of-sample forecasting

hwdata_1995 = window(hwdata, end=c(1995,4)) #Simulate at the time point of 1995 4th season
VAR2_1995 = VAR(hwdata_1995, p=2)
forecast_VAR2 = forecast(VAR2_1995, h=20)
plot(forecast_VAR2)



plot(forecast_VAR2$forecast$int); lines(hwdata[,"int"])
plot(forecast_VAR2$forecast$infl); lines(hwdata[,"infl"]) #Simulated out-of-sample results



# Q5

hwdata_ordered_for_oirf = hwdata[,c("infl","int")]
VAR2_ordered_for_oirf = VAR(hwdata_ordered_for_oirf, p=2)  ## Reorder variables and estimate a VAR used for IRFS

oirf = irf(VAR2_ordered_for_oirf, ortho="TRUE", n.ahead=5*12, boot=TRUE, ci=0.95, runs=1000)  ## Orthogonized IRFs
plot(oirf)         

##  IRF of U.S inflation from interest rate shock
dev.off()
impulse = "int"; response = "infl" 
plot.ts(cbind(oirf$irf[[impulse]][,response], 
              oirf$Lower[[impulse]][,response], 
              oirf$Upper[[impulse]][,response]),
        plot.type="s", col=c("black","red","red"), 
        lty=c(1,2,2), xlab="", ylab="")
abline(h=0, col="red") 

##  IRF of U.S nominal interest rate from interest rate shock
impulse = "int"; response = "int" 
plot.ts(cbind(oirf$irf[[impulse]][,response], 
              oirf$Lower[[impulse]][,response], 
              oirf$Upper[[impulse]][,response]),
        plot.type="s", col=c("black","red","red"), 
        lty=c(1,2,2), xlab="", ylab="")
abline(h=0, col="red")

dev.off()
oirf_cum = irf(VAR2_ordered_for_oirf, ortho="TRUE", cumulative=TRUE, n.ahead=5*12, boot=TRUE, ci=0.95, runs=1000) #Cumulative IRFs
plot(oirf_cum)  ## Cumulative Orthogonalized IRFs (only show the IRF of inflation shock

## cumulative OIRF when the impulse variable is U.S nominal interest rate
dev.off()
impulse = "int"; response = "infl"
plot.ts(cbind(oirf_cum$irf[[impulse]][,response], 
              oirf_cum$Lower[[impulse]][,response], 
              oirf_cum$Upper[[impulse]][,response]),
        plot.type="s", col=c("black","red","red"), 
        lty=c(1,2,2), xlab="", ylab="")
abline(h=0, col="red")

impulse = "int"; response = "int"
plot.ts(cbind(oirf_cum$irf[[impulse]][,response], 
              oirf_cum$Lower[[impulse]][,response], 
              oirf_cum$Upper[[impulse]][,response]),
        plot.type="s", col=c("black","red","red"), 
        lty=c(1,2,2), xlab="", ylab="")
abline(h=0, col="red")

