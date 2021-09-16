###########################################################################
#		Advanced Econometrics                                                 #
#   Spring semester                                                       #
#   Alberto Delgado Lopez, Elbrus Gasimov
#   University of Warsaw, Faculty of Economic Sciences                    #
#                                                                         #
#                                                                         #
#                                                                         #
#                           Final Poject                                  #
#                   Time series models – ARDL models                      #
#                                                                         #
###########################################################################


#One main hypothesis: 2022 death cases will significantly decrease
#Secondary hypothesises: 
#- ‘Cases or Deaths’ growth is highly related with ethnicity. (‘Ethnicity’ not time dependent)
#- 'Cases or Deaths' growth is higly related with age group.

#Libraries 
library(lmtest)
library(fBasics)
library(urca)
library(xts)
library(forecast)
library(quantmod)
# load needed packages
library(xts)
library(fBasics)
library(urca)
source("function_testdf2.R")
source("function_import_data_into_xts.R")
# libraries
requiredPackages = c("lmtest","fBasics","urca","xts","forecast", "quantmod", "reshape2")

for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE)}



#Lets load the datasets, analyse the data, do a little bit of cleaning and reorganize variables.

#firstfile

covid <- read.csv("main_case.csv", # name of the file
                header = T,  	# if first row doesn't contain variable names: header=F'
                sep = ",", 	  # sign used as columns separator
                dec = ".")	  # sign used as decimal place separator
head(covid)
tail(covid)

summary(covid)
str(covid)

covid$date <- as.Date(covid$date, "%Y-%m-%d")

covid<-covid[,c(1,6)]
covid<-na.omit(covid)
covid<-aggregate(covid[,-1], by = list(covid$date), FUN = sum)
names(covid) <- c("date","deaths")

head(covid)
tail(covid)
summary(covid)
str(covid)



#Let's create the xts objects
head(covid)
covid <- xts(covid[,-1], order.by=covid$date)
names(covid) <- c("deaths")
plot(covid)

plot(as.zoo(covid))

diffcovid <- diff.xts(covid)
plot(diffcovid)

diffcovid$deaths <- tsclean(diffcovid$deaths)
plot(diffcovid)


df.test <- ur.df(diffcovid$deaths,
                 type = c('none'),
                 lags = 0)

summary((df.test))

resids_ <- df.test@testreg$residuals
bgtest(resids_~1, order = 1)
bgtest(resids_~1, order = 2)
bgtest(resids_~1, order = 3)
bgtest(resids_~1, order = 4)
bgtest(resids_~1, order = 5)

# to be sure that conlusions may be drawn we need to check whether there is 
# no autocorrelation in residuals

#adf.test performance

adf.test<- ur.df(diffcovid$deaths,
                 type = c('none'),
                 lags = 5)
summary(adf.test)

resids_ <- adf.test@testreg$residuals
bgtest(resids_~1, order = 1)
bgtest(resids_~1, order = 2)
bgtest(resids_~1, order = 3)
bgtest(resids_~1, order = 4)
bgtest(resids_~1, order = 5)


testdf2(variable = diffcovid$deaths,
        test.type = 'nc',
        max.augmentations = 5,
        max.order = 5)

#as we reject the H0 we comfirm that your variable is Stationary! 

#lets see some other alternative tests

pp.test.d <-ur.pp(diffcovid$deaths,
                  type =  c("Z-tau"),
                  model = c("constant"))

summary(pp.test.d)

#pp test here comfirms that our variable is stationary. Generaly, the result of df and pp 
#tests should be the same just like ours to be in the safe side
#howver if the results are different then we contine testing with the 3rd test
#KPSS test has a different H0 which is ts is stationary 

kpps.test <- ur.kpss(diffcovid$deaths,
                     type = c('mu'))
summary(kpps.test)

#as a result we get that we fail to reject H0 which is our variable is stationary
# our 5 percent rejecttion intervals starts from 0.463 till plus infinite
#our t statistics value is 0.3022 means it is out of the rejection zone!!

# The Box-Jenkins procedure
#Initial identification of parameters p and q:
#we need to see if the residuals are autocorrelated or not! if not then
#we have a good model we can use the model to calculate the forecast, 
#however if residuals are autocorrelated then we need to increase p and q values
#and estimate the  model again

#lets see ACF and PACF non- stationary variable

#ACF
par(mfrow = c(2,1))
acf(diffcovid$deaths, 
    lwd = 5, # line width
    col = "dark green", # color
    main = "ACF for Death")

#PACF

pacf(diffcovid$deaths, 
     lwd = 5, # line width
     col = "dark green", # color
     main = "PACF for Death")

#As we have daily data,then my approuch we be to see if we have any pattern repeating itsel
#as i can see somehow there is a weekly pattern, which makes feel choosing p = 5 and q = 5
#We have done all steps and it is time to check our model for this we will start with creating
#the model and we will use Arima() ## as it is better than arima(), because it allows us to
#include constant term to the varibale

arima1 <- Arima(covid$deaths,
                order = c(5,1,5),
                include.constant = TRUE)

summary(arima1)

#this output is not exaclty what we want so lets check coeftest() function,
#from the lmtest package to test the significance of model parameters.

coeftest(arima1)

#from the results we can see that 5th lag from AR and 1st lag from MA
#is statisticly significiant
#now lets check if the model is valid!

# are residuals of arima111 model white noise? 
# resid() function applied to model results returns residuals

par(mfrow = c(2,1))  
acf(resid(arima1), 
    lag.max = 36,
    ylim = c(-0.1,0.1), 
    lwd = 5, col = "dark green",
    na.action = na.pass)
pacf(resid(arima1), 
     lag.max = 36, 
     lwd = 5, col = "dark green",
     na.action = na.pass)
par(mfrow = c(1,1))

#from the plot it is shown that we have autocorrelation of residuals 
#however there is a better tool to check autocorrelation which is 
#Ljung-Box test for max 10 lags

Box.test(resid(arima1),
         type = 'Ljung-Box',
         lag = 10)

#H0 for Ljung-Box is that, Residuals are fine, means there is not auto correlation!
#we used lag = 10 means, there is no autocorrelation - order = 1, order = 2.....order = 10
#as our p value is 0.9932 we fail to reject H0 which means our model is valid!
#however to obtan more percise model i can restrict my model further by droping
#statisticly insiginificant models and then we can compare which one is better model

arima1_1 <- Arima(covid$deaths,
                    order = c(5,1,5),
                    fixed = c(0,0,0,0,NA,  # vector of the same
                              NA,0,0,0,0,NA), # length as the total number of parameters
                    include.constant = TRUE  # last is for the intercept (if included)
)   

summary(arima1_1)
coeftest(arima1_1)


Box.test(resid(arima1_1),
         type = 'Ljung-Box',
         lag = 10)

#now as we have 2 models and 2 of them seem valid then lets compare them
#for this we will do AIC and BIC

#AIC

AIC(arima1, arima1_1)   

#BIC

BIC(arima1, arima1_1)

#Furthermore there is also a way to create the best model
#for this we can use auto.arima() function
#lets try and then we can compare all the models 

auto_arima.AIC <- auto.arima(covid$deaths,
                             d = 1,             # parameter d of ARIMA model
                             max.p = 10,        # Maximum value of p
                             max.q = 10,        # Maximum value of q
                             max.order = 14,    # maximum p+q
                             start.p = 1,       # Starting value of p in stepwise procedure
                             start.q = 1,       # Starting value of q in stepwise procedure
                             ic = "aic",        # Information criterion to be used in model selection.
                             stepwise = FALSE,  # if FALSE considers all models
                             allowdrift = TRUE, # include a constant
                             trace = TRUE)      # show summary of all models considered

# the result might be surprising

coeftest(auto_arima.AIC)

#okay we have arima1 and arima1_1 models so we can compare them
#for comparing the model we use AIC abd BIC

AIC(auto_arima.AIC)
# AIC better than for the best manually selected model

BIC(auto_arima.AIC)
# BIC worse than for the best manually selected model

# Ljung-Box test
Box.test(resid(auto_arima.AIC),
         type = "Ljung-Box", lag = 10)

#########################
regressionMetrics <- function(real, predicted) {
    MSE <- mean((real - predicted)^2) # Mean Squera Error
    RMSE <- sqrt(MSE) # Root Mean Square Error
    MAE <- mean(abs(real - predicted)) # Mean Absolute Error
    MAPE <- mean(abs(real - predicted)/abs(real)) # Mean Absolute Percentage Error
    MedAE <- median(abs(real - predicted)) # Median Absolute Error
    TSS <- sum((real - mean(real))^2) # Total Sum of Squares
    RSS <- sum((predicted - real)^2)  # Explained Sum of Squares
    R2 <- 1 - RSS/TSS
    
    result <- data.frame(MSE, RMSE, MAE, MAPE, MedAE, R2)
    return(result)
}

regressionMetrics(fit.r[19:416], fit.m[19:416])

fit.r <- covid$deaths  # real values - last 31 observations, last observation iis zero thats why i dint take it
fit.m <- xts(fitted(arima1),order.by=index(covid))# forecast

fit_forecast <- data.frame(fit.r[19:416], fit.m[19:416])
head(fit_forecast)
names(fit_forecast) <- c("real","model")

colors_ <- c("black", "red")
plot.xts(merge(fit.r, fit.m), plot.type="single",
         col = colors_,   # colors for subsequent lines
         ylab = "Daily number of deaths", # axes labels
         main = "Real data Vs Model data") # title above the plot
addLegend("topleft", names(fit_forecast), text.col = colors_)


##################



#time to start ppreparing for the forecast


#########################################
# lets see last 25% observations
tail(covid, 104)


covid.sample <- covid["/2021-01-07", 1]

tail(covid.sample)


#FORECAST for death - model arima515

# estimate the model on shorter sample

arima515s <- Arima(covid.sample$deaths,  # variable
                   order = c(5,1,5),   # (p,d,q) parameters
                   include.constant = T)

arima515s

# lets make a prediction
forecast515  =  forecast(arima515s, # model for prediction
                         h = 104) # how many periods outside the sample

# lets see the result
forecast515

# the forecasts are indexed with a observation number, 
# not a date!

class(forecast515$mean)
# it is a ts object, not xts


# it includes:
# Point Forecast - predicted values
forecast515$mean

# 80% and 95% confidence intervals
forecast515$lower
forecast515$upper

# if we want to easily put together both real data
# and the forecast on the plot, we have to convert
# both to ts or both to xts objects

class(covid)

tail(covid, 104) # indexed by date

# ts() function does easily convert xts to ts object

class(ts(covid))

tail(ts(covid), 104) # indexed by observation number

# lets plot the figure with the forecast

# original data
plot(ts(covid[,1]), main = "104 day forecast of death",xlim = c(0,418), ylim = c(0, 3000))
# line at the end of a sample period (start of the forecast)
# observation number 388
abline(v = 313, lty = 2, col = "gray")
# line for the forecast
lines(forecast515$mean, col = "red", lwd = 2)
# line for the 95% confidence interval
# indexed by observation numbers from the forecast
lines(313:416,forecast515$lower[,2], col = "red", lty = 3)
lines(313:416,forecast515$upper[,2], col = "red", lty = 3)

##############################################################################
#VERY BAD FORECASTING it tends to the long termn value


# lets see few last observations
tail(covid, 31)

# we cut last 31 observations (since 2021-03-22 )
# and keep them aside as the out-of-sample
# to perform a forecasting exercise
# and have a possibility to assess it

covid.sample <- covid["/2021-03-22", 1]

tail(covid.sample)


#FORECAST for death - model arima515

# estimate the model on shorter sample

arima515s <- Arima(covid.sample$deaths,  # variable
                   order = c(5,1,5),   # (p,d,q) parameters
                   include.constant = T)

arima515s

# lets make a prediction
forecast515  =  forecast(arima515s, # model for prediction
                         h = 31) # how many periods outside the sample

# lets see the result
forecast515

# the forecasts are indexed with a observation number, 
# not a date!

class(forecast515$mean)
# it is a ts object, not xts


# it includes:
# Point Forecast - predicted values
forecast515$mean

# 80% and 95% confidence intervals
forecast515$lower
forecast515$upper

# if we want to easily put together both real data
# and the forecast on the plot, we have to convert
# both to ts or both to xts objects

class(covid)

tail(covid, 31) # indexed by date

# ts() function does easily convert xts to ts object

class(ts(covid))

tail(ts(covid), 31) # indexed by observation number

# lets plot the figure with the forecast

# original data
plot(ts(covid$deaths), main = "31 day forecast of death")
# line at the end of a sample period (start of the forecast)
# observation number 388
abline(v = 388, lty = 2, col = "gray")
# line for the forecast
lines(forecast515$mean, col = "red", lwd = 2)
# line for the 95% confidence interval
# indexed by observation numbers from the forecast
lines(388:418,forecast515$lower[,2], col = "red", lty = 3)
lines(388:418,forecast515$upper[,2], col = "red", lty = 3)


# lets see its zoom
plot(ts(covid[,1]), main = "31 day forecast of death",
     xlim = c(388,418), ylim = c(0, 500))
abline(v = 388, lty = 2, col = "gray")
lines(forecast515$mean, col = "red", lwd = 2)
lines(388:418,forecast515$lower[,2], col = "red", lty = 3)
lines(388:418,forecast515$upper[,2], col = "red", lty = 3)


# checking forecast quality 

# for simplicity of the following formulas
# lets define two new objects:

death.r <- tail(covid$death[-417], 31)  # real values - last 31 observations, last observation iis zero thats why i dint take it
death.f <- as.numeric(forecast515$mean) # forecast

death_forecast <- data.frame(death.r, death.f)



# lets add the basis for different measures of the forecast error
death_forecast$mae  =  abs(death.r - death.f)
death_forecast$mse  =  (death.r - death.f)^2
death_forecast$mape  =  abs((death.r - death.f)/death.r)
death_forecast$amape  =  abs((death.r - death.f)/(death.r+death.f))

# and calculate its averages
colMeans(death_forecast[,3:6])

# NOTICE!!!! Model which performs best in the sample 
#	will NOT NECESSARILY be best for forecasting.
#	One should compare forecasts for several models. 
