library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(devtools)
library(BatchGetSymbols)


## Second attempt at Whole SP500
first.date <- Sys.Date()-5479
last.date <- Sys.Date()
freq.data <- 'monthly'

df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$tickers

#This line takes awhile to run 
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)

## I'd like to combine this line with BatchGetSymbols above so that it doesn't fetch all the data, just these columns. 
bunch_of_tickers <- as.data.frame(l.out$df.tickers) %>% 
  select(price.adjusted, ref.date, ticker) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select_if(~ !any(is.na(.))) 

## Checking to see if ref.date is recognized as dates. 
class(bunch_of_tickers$ref.date)

print(l.out$df.control)
print(l.out$df.tickers)


##### "Melting" the SP500 data
# Computing the correlation matrix (and deleting the date column for now)
bunch_of_tickers$ref.date <- NULL
bunch_of_tickers$ref.date_2 <- NULL
cormat <- round(cor(bunch_of_tickers),2)


# "melting" the data 
melted_cormat <- melt(cormat)


