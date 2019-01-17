library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(devtools)
library(BatchGetSymbols)


#http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
#http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
#dat <- read_excel("C:/Users/Saffra/Documents/T3/compiled_data.xlsx")


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
#combined_dat$Date <- NULL


###########################################


#https://stackoverflow.com/questions/48096600/can-i-make-a-line-plot-with-a-dropdown-menu-in-r-without-using-shiny-or-plotly

library(plotly)

## Create random data. cols holds the parameter that should be switched
l <- lapply(1:100, function(i) rnorm(100))
df <- as.data.frame(l)
cols <- paste0(letters, 1:100)
colnames(df) <- cols
df[["c"]] <- 1:100

df <- combined_dat

## Add trace directly here, since plotly adds a blank trace otherwise
p <- plot_ly(df,
             type = "scatter",
             mode = "lines",
             x = "Date", 
             y= ~df[[cols[[1]]]], 
             name = cols[[1]])
## Add arbitrary number of traces
## Ignore first col as it has already been added
for (col in cols[-1]) {
  p <- p %>% add_lines(x = ~c, y = df[[col]], name = col, visible = FALSE)
}

p <- p %>%
  layout(
    title = "Dropdown line plot",
    xaxis = list(title = "x"),
    yaxis = list(title = "y"),
    updatemenus = list(
      list(
        y = 0.7,
        ## Add all buttons at once
        buttons = lapply(cols, function(col) {
          list(method="restyle", 
               args = list("visible", cols == col),
               label = col)
        })
      )
    )
  )

print(p)

