############################################################################
############################################################################
##                        Elbrus Gasimov                                  ## 
##                        Applied Finance                                 ##
##                        Homework_01                                     ##
############################################################################
############################################################################

# libraries

library(xts)
library(data.table)
library(pryr)
library(xts)
library(quantmod)
library(tseries) 
library(chron)
library(TTR)
library(caTools)
library(lubridate)
library(dplyr)
library(lattice)
library(grDevices)
# Import quotations for three different companies (eg. MSFT, KO, PEP)
# from yahoo finance. Merge together all close prices into one xts
# object and plot them on one graph.


quot_msft <- getSymbols("MSFT", 
                        from = "2005-01-01",
                        to = "2021-10-24",
                        auto.assign = FALSE) 
quot_KO <- getSymbols("KO", 
                      from = "2005-01-01",
                      to = "2021-10-24",
                      auto.assign = FALSE) 
quot_PEP <- getSymbols("PEP", 
                       from = "2005-01-01",
                       to = "2021-10-24",
                       auto.assign = FALSE) 

all_qs <- merge(quot_msft$MSFT.Close, quot_KO$KO.Close, quot_PEP$PEP.Close)
str(all_qs) # 'xts' object

# Chaning close price to only  company names

names(all_qs) <- c('Microsoft','Coca Cola','Pepsi')

plot(all_qs,
     main = "Close price of Microsoft, Coca Cola and Pepsi 2005-2021",
     col = RColorBrewer::brewer.pal(3, 'Accent'),
     major.ticks = "years",
     grid.ticks.on = "years",
     grid.ticks.lty = 2,
     cex = .8,
     multi.panel = 3,
     yaxis.same = FALSE)

############################################################################

# Sample data from truefx for EURGBP
# here sample for 1 week 22-29.09.2017

# Importing the data

EURGBP <- fread("http://wne.uw.edu.pl/pwojcik/hfd/EURGBP-2017-09.csv", stringsAsFactors = F)

# Assign column names: symbol, date, time, bid, ask.

# CASE 1

case_01 <- EURGBP

# Since data and time are stored in one columns, thus we should first separate 
# those two into two different columns to comply with the 5 columns listed in the 
# description:

case_01$date <- substr(case_01$V2,1,8)
case_01$time <- substr(case_01$V2,10,nchar(case_01$V2[1]))

case_01$V2 <- NULL # We're done with V2!
names(case_01) <- c('symbol', 'bid', 'ask','date','time') # Assigning names
case_01 <- case_01[,c(1,4,5,2,3)] # Reordering
head(case_01)

# CASE 2

# Assuming there exists 4 columns (datetime as one column):
names(EURGBP) <- c('symbol','datetime','bid', 'ask')
# Converting to xts.
str(EURGBP) # datetime stored as character 

EURGBP <- data.frame(EURGBP)

EURGBP$datetime <- strptime(EURGBP$datetime,
                            format = "%Y%m%d %H:%M:%S", 
                            tz = "GMT")

EURGBP_xts <- xts(EURGBP[,c(-1,-2)],
                 EURGBP$datetime,
                 tzone = "GMT")
str(EURGBP_xts)

# Compare the size of a data.frame and xts object.
object_size(EURGBP) # 74.4 MB
object_size(EURGBP_xts) # 27.9 MB

# Play with different plots of the data.
plot(EURGBP_xts,
     main = "Quotations of EURO and GBP",
     multi.panel = 2,
     major.ticks = "days", 
     grid.ticks.on = "days",
     grid.ticks.lty = 3,
     legend.loc = "bottomright",
     yaxis.same = T)

plot(EURGBP_xts,
     main = "Quotations of EURO and GBP",
     multi.panel = 1, # Two separate graphs
     col = c("green", "blue"),
     major.ticks = "days", 
     grid.ticks.on = "days",
     grid.ticks.lty = 2,
     legend.loc = "bottomleft",
     yaxis.same = T)

############################################################################

# Aggregation:
# - 15 sec data
# - 3 min data
# - 2 hourly data

sec_15.bid <- to_period(EURGBP_xts$bid, period = 'seconds', k = 15)
sec_15.ask <- to_period(EURGBP_xts$ask, period = 'seconds', k = 15)

min_03.bid <- to.minutes3(EURGBP_xts$bid)
min_03.ask <- to.minutes3(EURGBP_xts$ask)

hour_02.bid <- to.minutes(EURGBP_xts$bid, k = 120)
hour_02.ask <- to.minutes(EURGBP_xts$ask, k = 120)

############################################################################

# Sample file from Bossa (PKOBP)
PKOBP <- fread("http://wne.uw.edu.pl/pwojcik/hfd/PKOBP.prn",stringsAsFactors = F)
str(PKOBP)

# renameing the columns: ticker, null, date, time, open, high, low, 
#                   close, volume, null2
names(PKOBP) <- c('ticker','null', 'date', 'time', 'open', 'high', 
                  'low', 'close', 'volume','null2')

# create a correct date-time index and convert to xts
# We need to combine two columns (date and time) with each other
# To do so first we need to change the num type of the columns to chr
# The time for 9 AM only has 5 digits, let's fix it
PKOBP$time <- ifelse(nchar(PKOBP$time)==5,paste0(0,PKOBP$time),PKOBP$time)

PKOBP <- transform(PKOBP, datetime = paste(as.character(PKOBP$date),
                                           as.character(PKOBP$time), sep = " "))
PKOBP[,c(3,4)] <- NULL # We're done with date and time separately!        
PKOBP <- data.frame(PKOBP)
PKOBP$datetime <- strptime(PKOBP$datetime,
                           format = "%Y%m%d %H%M%S", 
                           tz = "GMT")
PKOBP_xts <- xts(PKOBP[,c(-1,-2,-8,-9)], 
                 PKOBP$datetime, 
                 tzone = "GMT")

# Play with different plots of the data.
matrix(1:6, nrow = 3, ncol = 2)

# Size comparison
object.size(PKOBP) # 3839760 bytes
object.size(PKOBP.xts) # 2003136 bytes


# Separation 
plot(PKOBP_xts,
     multi.panel = 1,
     main = "Quotations for PKO, the best bank in Poland", 
     col = c("#E41A1C", "#377EB8", "#4DAF4A", 
             "#984EA3", "#FFEA00"),
     grid.ticks.lty = 3,
     legend.loc = "bottomright",
     yaxis.same = FALSE,
     cex = 0.35)

## Layout() to display multiple plost in one 

layout(matrix(1:5, nrow = 5, ncol = 1))
plot(PKOBP_xts,
     multi.panel = 1,
     main = "Quotations for PKO, the best bank in Poland", 
     col = c("#E41A1C", "#377EB8", "#4DAF4A", 
             "#984EA3", "#FFEA00"),
     grid.ticks.lty = 3,
     legend.loc = "bottomright",
     yaxis.same = FALSE,
     cex = 0.35)


# Resetting the graphics 

dev.off() 
