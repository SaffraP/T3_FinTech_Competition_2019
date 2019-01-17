# The sole purpose of this script is to create a finished heatmap containing as many stocks as reasonably possible. 

library(tidyverse)
library(BatchGetSymbols)
library(reshape2) # Used to melt the data


## This section gets the data

first.date <- Sys.Date() - 3668
last.date <- Sys.Date()
freq.data <- 'monthly'

tickers <- c('GILD','UNP','MCD','HPQ', 'MMM', 'CSCO', 'SLB', 'AMGN', 'BA', 'COP', 'CMCSA', 'BMY', 'VZ', 'T', 'PEP', 'MCD', 'PFE', 'ABT', 'ORCL', 'DIS')
# 'UNH', 'UTX'


l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()


## This section puts the data into a clean format. Each ticker is a column. The dates are from Jan 1, 2014 - Jan 1, 2019.
dat <- as.data.frame(l.out$df.tickers) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)


# Computing the correlation matrix 
cormat <- round(cor(dat),2)

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
upper_tri

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)


# Helper function to reorder the correlation matrix 
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reordered correlation data visualization
# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_bw()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  labs(x = "", y = "") +
  coord_fixed()


print(ggheatmap)