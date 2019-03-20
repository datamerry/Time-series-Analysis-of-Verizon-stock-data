
setwd("F:/Sindhu/UCD/SPRING 2018/Time Series/Final_project/DS")
vz.daily<-read.csv("VZ.csv",header=T, sep=",")
vz.daily
vz.daily.adj= vz.daily$Adj.Close
hist(vz.daily.adj)
plot.ts(vz.daily.adj)
acf(vz.daily.adj)
pacf(vz.daily.adj)

vz.simplereturns=diff(vz.daily.adj)/ vz.daily.adj[-length(vz.daily.adj)]
vz.returns = log(1+vz.simplereturns)
mean_vz.returns = mean(vz.returns)
vz_center_returns = vz.returns - mean_vz.returns
hist(vz_center_returns)


plot.ts(vz_center_returns)
acf(vz_center_returns) #cutoff at lag 2
pacf(vz_center_returns) #cutoff at lag 2

acf(vz_center_returns^2)
pacf(vz_center_returns^2)

Box.test(vz_center_returns)

Box.test(vz_center_returns^2) #p-value < alpha, data is not independent

library(fBasics)
basicStats(vz_center_returns) 
library(forecast)
tsdisplay(vz_center_returns) # Looks stationary with outliers.


#T- Test
t.test(vz_center_returns) #True mean is equal to 0, p-value > alpha

#Jarque bera normality test
normalTest(vz_center_returns,method="jb") #p-value < alpha, data is not normally distributed

library(fUnitRoots)
adfTest(vz_center_returns)  #p value < alpha, reject null hypothesis - data is stationary





#question 3
fit1 <- auto.arima(vz_center_returns) # No differences! That looks odd.
summary(fit1) 



m1=arima(vz_center_returns ,order=c(1,0,0))
m1 #aic = -14958.08

m2=arima(vz_center_returns ,order=c(1,0,1))
m2 #aic = -14964.66

m3=arima(vz_center_returns ,order=c(1,0,2))
m3 #aic = -14976.25

m4=arima(vz_center_returns ,order=c(2,0,0))
m4 #aic = -14976.7

m5=arima(vz_center_returns ,order=c(2,0,1))
m5 #aic = -14976.28

m6=arima(vz_center_returns ,order=c(2,0,2))
m6 #aic = -14974.29

m7=arima(vz_center_returns ,order=c(3,0,0))
m7 #aic = -14975.94

m8=arima(vz_center_returns ,order=c(3,0,1))
m8 #NANS produced

m9=arima(vz_center_returns ,order=c(3,0,2))
m9 #aic = -14972.34

m10=arima(vz_center_returns ,order=c(3,0,3))
m10 #aic = -14977.69

m11=arima(vz_center_returns ,order=c(4,0,0))
m11 #aic = -14974.74

m12=arima(vz_center_returns ,order=c(4,0,1))
m12 #aic = -14972.79

m13=arima(vz_center_returns ,order=c(4,0,2))
m13 #aic = -14987.67

m14=arima(vz_center_returns ,order=c(5,0,0))
m14 #aic = -14972.91



library(fGarch)
gf1 = garchFit(formula = ~arma(4, 2) + garch(1, 1), data = vz_center_returns, 
         trace = F) 
summary(gf1) #-6.076057

gf2 = garchFit(formula = ~arma(2, 0) + garch(1, 1), data = vz_center_returns, 
               trace = F) 
summary(gf2) #-6.076885 #ar not significant

gf3 = garchFit(formula = ~arma(1, 2) + garch(1, 1), data = vz_center_returns, 
               trace = F) 
summary(gf3) #-6.076550 #ma2 not significant

gf4 = garchFit(formula = ~arma(2, 1) + garch(1, 1), data = vz_center_returns, 
               trace = F) 
summary(gf4) #-6.076543 #ar2 not significant

gf5 = garchFit(formula = ~arma(1, 0) + garch(1, 1), data = vz_center_returns, 
               trace = F) 
summary(gf5) #-6.077179 #ar1 not significant


library(caTools) 

set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(vz_center_returns,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train1 =subset(vz_center_returns,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
length(train1)
test1=subset(vz_center_returns, sample==FALSE)
length(test1)
length(vz_center_returns)


tgf1 = garchFit(formula = ~garch(1, 1), data = train1, 
                cond.dist = c("std"), trace = F) 

summary(tgf1) #arch & garch coefficient significant aic = -6.115894


tgf2 = garchFit(formula =  ~garch(2, 0), data = train1, 
               trace = F) 
summary(tgf2) #aic = -5.989952

tgf3 = garchFit(formula =  ~garch(1, 1), data = train1, 
               trace = F) 

summary(tgf3)  #aic = -6.069638 coefficient significant aic = -6.115894


sresi=residuals(tgf1,standardize=T) # Obtain standardized residuals
sigma.t=volatility(tgf1)  # obtain the fitted volatility sigma_t.

library(forecast)
tsdisplay(residuals(tgf1))

library(F)

Box.test(residuals(tgf1)) #p-value < alpha, series is independent

#predictions

predict(tgf1,newdata=test1,10,plot=TRUE)





