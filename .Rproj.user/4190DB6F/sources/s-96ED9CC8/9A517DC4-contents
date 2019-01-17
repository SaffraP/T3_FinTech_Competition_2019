### The sole purpose of this document is to get clean data to create the interactive historical correlation graph. 
## Dates from 10 years ago. 
## Breaking the data up by year, finding the correlation between each year, adding a year column, and joining the data back together

################################
library(tidyverse)
library(BatchGetSymbols)
library(reshape2) # Used to melt the data


## This section gets the data

first.date <- Sys.Date() - 3668
last.date <- Sys.Date()
freq.data <- 'monthly'

tickers <- c('GILD','UNP','MCD','HPQ', 'MMM', 'CSCO', 'SLB', 'AMGN', 'BA', 'COP', 'CMCSA', 'BMY', 'VZ', 'T', 'PEP', 'MCD', 'PFE', 'ABT', 'ORCL', 'DIS')

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()


## This section breaks the data up into subsets by year, finds the correlation, and creates a year column.


dat_2009 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2009')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2009),2)
# "melting" the data for the heat map
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
#upper_tri
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Adding a column with the year
dat_09 <- melted_cormat %>% 
  cbind(year = "2009")



dat_2010 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2010')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2010),2)
# "melting" the data for the heat map
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
#upper_tri
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Adding a column with the year
dat_10 <- melted_cormat %>% 
  cbind(year = "2010")



dat_2011 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2011')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2011),2)
# "melting" the data for the heat map
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
#upper_tri
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Adding a column with the year
dat_11 <- melted_cormat %>% 
  cbind(year = "2011")



dat_2012 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2012')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2012),2)
# "melting" the data for the heat map
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
#upper_tri
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Adding a column with the year
dat_12 <- melted_cormat %>% 
  cbind(year = "2012")




dat_2013 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2013')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2013),2)
# "melting" the data for the heat map
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
#upper_tri
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Adding a column with the year
dat_13 <- melted_cormat %>% 
  cbind(year = "2013")



dat_2014 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2014')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2014),2)
# "melting" the data for the heat map
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
#upper_tri
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Adding a column with the year
dat_14 <- melted_cormat %>% 
  cbind(year = "2014")



dat_2015 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2015')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2015),2)
# "melting" the data for the heat map
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
#upper_tri
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Adding a column with the year
dat_15 <- melted_cormat %>% 
  cbind(year = "2015")



dat_2016 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2016')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2016),2)
# "melting" the data for the heat map
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
#upper_tri
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Adding a column with the year
dat_16 <- melted_cormat %>% 
  cbind(year = "2016")



dat_2017 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2017')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2017),2)
# "melting" the data for the heat map
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
#upper_tri
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Adding a column with the year
dat_17 <- melted_cormat %>% 
  cbind(year = "2017")



dat_2018 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2018')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2018),2)
# "melting" the data for the heat map
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
#upper_tri
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Adding a column with the year
dat_18 <- melted_cormat %>% 
  cbind(year = "2018")


## This section combines all the subsets back together into one dataset.

lit_dat <- bind_rows(dat_18, dat_17, dat_16, dat_15, dat_14, dat_13, dat_12, dat_11, dat_10, dat_09)

