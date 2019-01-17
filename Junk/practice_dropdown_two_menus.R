library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(devtools)
library(BatchGetSymbols)

# Reading in data for each ticker
siri <- read_csv("C:/Users/Saffra/Documents/T3/Data/SIRI.csv")
msft <- read_csv("C:/Users/Saffra/Documents/T3/Data/MSFT.csv")
f <- read_csv("C:/Users/Saffra/Documents/T3/Data/F.csv") 

# Clean out all of the columns except for date and adjusted close. 

siri$Open <- NULL
siri$High <- NULL
siri$Low <- NULL
siri$Close <- NULL
siri$Volume <- NULL

msft$Open <- NULL
msft$High <- NULL
msft$Low <- NULL
msft$Close <- NULL
msft$Volume <- NULL

f$Open <- NULL
f$High <- NULL
f$Low <- NULL
f$Close <- NULL
f$Volume <- NULL


# Renaming adjusted close column to ticker
colnames(siri)[colnames(siri) == 'Adj Close'] <- 'siri'
colnames(msft)[colnames(msft) == 'Adj Close'] <- 'msft'
colnames(f)[colnames(f) == 'Adj Close'] <- 'f'


# Joining the data into one dataset 
combined_dat <- f %>% 
  left_join(siri) %>% 
  left_join(msft)

# Computing the correlation matrix (and deleting the date column for now)
combined_dat$Date <- NULL
#cormat <- round(cor(combined_dat),2)

##################################
library(plotly)

x <- seq(-2 * pi, 2 * pi, length.out = 1000)
df <- data.frame(combined_dat)

p <- plot_ly(df, x = x) %>%
  add_lines(y = combined_dat, name = "A") %>%
  add_lines(y = combined_dat, name = "B", visible = F) %>%
  layout(
    title = "Drop down menus - Styling",
    xaxis = list(domain = c(0.1, 1)),
    yaxis = list(title = "y"),
    updatemenus = list(
      list(
        y = 0.8,
        buttons = list(
          
          list(method = "restyle",
               args = list("line.color", "blue"),
               label = "Blue"),
          
          list(method = "restyle",
               args = list("line.color", "red"),
               label = "Red"))),
      
      list(
        y = 0.7,
        buttons = list(
          list(method = "restyle",
               args = list("visible", list(TRUE, FALSE)),
               label = "Sin"),
          
          list(method = "restyle",
               args = list("visible", list(FALSE, TRUE)),
               label = "Cos")))
    )
  )

print(p)
