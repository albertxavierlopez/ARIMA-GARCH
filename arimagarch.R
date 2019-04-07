# GARCH + ARMA MODEL
# https://www.quantstart.com/articles/ARIMA-GARCH-Trading-Strategy-on-the-SP500-Stock-Market-Index-Using-R


# Import the necessary libraries

library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)

# Obtain the S&P500 returns and truncate the NA value   
# BTC-USD
# VHGEX

getSymbols("BTC-USD", from="2014-03-01")

btcReturns = diff(log(btc$`BTC-USD.Close`))
btcReturns[as.character(head(index(btcReturns),1))] = 0

library(tidyverse)
prices<-read.csv("bitprices.csv", header = TRUE, sep=";")  
prices$Date<-as.Date(prices$Date,format="%d/%m/%Y")
prices<-prices[order(prices$Date),]

library(xts)
btcReturns<- xts(prices[,5], order.by=as.Date(prices[,1], "%m/%d/%Y"))
btcReturns = diff(log(btcReturns))
btcReturns[as.character(head(index(btcReturns),1))] = 0



# Create the forecasts vector to store the predictions

windowLength = 500
foreLength = length(btcReturns) - windowLength
forecasts <- vector(mode="character", length=foreLength)
# Model to compute the best ARIMA(p,d,q) model + GARCH(1,1)

    t = Sys.time()
    for (d in 0:foreLength) {
      # Obtain the S&P500 rolling window for this day
      btcReturnsOffset = btcReturns[(1+d):(windowLength+d)]
      
      # Fit the ARIMA model
      final.aic <- Inf
      final.order <- c(0,0,0)
      for (p in 0:5) for (q in 0:5) {
        if ( p == 0 && q == 0) {
          next
        }
        
        arimaFit = tryCatch( arima(btcReturnsOffset, order=c(p, 0, q)),   # d=0 since we already applied differences to prices
                             error=function( err ) FALSE,
                             warning=function( err ) FALSE )
        
        if( !is.logical( arimaFit ) ) {
          current.aic <- AIC(arimaFit)
          if (current.aic < final.aic) {
            final.aic <- current.aic
            final.order <- c(p, 0, q)
            final.arima <- arima(btcReturnsOffset, order=final.order)
          }
        } else {
          next
        }
      }
      
      # Specify and fit the GARCH model
      spec = ugarchspec(
        variance.model=list(garchOrder=c(1,1)),
        mean.model=list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
        distribution.model="sged"   # skewed generalized error distribution
      )
      fit = tryCatch(
        ugarchfit(
          spec, btcReturnsOffset, solver = 'hybrid'     # which tries different solvers in order to increase the likelihood of convergence
        ), error=function(e) e, warning=function(w) w
      )
      
      # If the GARCH model does not converge, set the direction to "long" else
      # choose the correct forecast direction based on the returns prediction
      # Output the results to the screen and the forecasts vector
      
      if(is(fit, "warning")) {
        forecasts[d+1] = paste(index(btcReturnsOffset[windowLength]), 1, sep=",")
        print(paste(index(btcReturnsOffset[windowLength]), 1, sep=","))
      } else {
        fore = ugarchforecast(fit, n.ahead=1)
        ind = fore@forecast$seriesFor
        forecasts[d+1] = paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")
        print(paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")) 
      }
    }
    print(paste0(format(Sys.time()-t,units='hours')))

    

# Output the CSV file to "forecasts.csv"
setwd("C:/Users/a.lopez.barrantes/Desktop/R scripts")
write.csv(forecasts, file="forecasts_btc_3.csv", row.names=FALSE)

############ python mod ("forecasts.csv")###############

# Input the Python-refined CSV file
btcArimaGarch = as.xts( 
  read.zoo(
    file="forecasts_new_btc_3.csv", format="%Y-%m-%d", header=F, sep=","
  )
)

# Create the ARIMA+GARCH returns
btcIntersect = merge( btcArimaGarch[,1], btcReturns, all=F )
btcArimaGarchReturns = btcIntersect[,1] * btcIntersect[,2]

# Create the backtests for ARIMA+GARCH and Buy & Hold
btcArimaGarchCurve = log( cumprod( 1 + btcArimaGarchReturns ) )
btcBuyHoldCurve = log( cumprod( 1 + btcIntersect[,2] ) )
btcCombinedCurve = merge( btcArimaGarchCurve, btcBuyHoldCurve, all=F )

# Plot the equity curves
xyplot( 
  btcCombinedCurve,
  superpose=T,
  col=c("darkred", "darkblue"),
  lwd=2,
  key=list( 
    text=list(
      c("ARIMA+GARCH", "Buy & Hold")
    ),
    lines=list(
      lwd=2, col=c("darkred", "darkblue")
    )
  )
)

