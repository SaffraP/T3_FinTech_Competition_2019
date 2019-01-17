df <- data.frame(x = runif(200), y = runif(200), z = runif(200))

unique_col_small <- unique_col %>% 
  select("A_A", "A_AEE", "A_AIV")


p <- plot_ly(df, x = ~x, y = ~y, mode = "markers", name = "A", visible = T) %>%
  layout(
    title = "Drop down menus - Styling",
    xaxis = list(domain = c(0.1, 1)),
    yaxis = list(title = "y"),
    updatemenus = list(
      list(
        y = 0.7,
        buttons = list(
          list(method = "restyle",
               args = list("y", list(df$y)),  # put it in a list
               label = "Show A"),
          list(method = "restyle",
               args = list("y", list(df$z)),  # put it in a list
               label = "Show B")))
    ))
p



#################################################################

library(dygraphs) 
library(ggplot2)
library(tidyquant)
library(timetk)
library(xts)


tickers_today <- c("A", "AEE", "AIV", "AMZN")

tidy <- tq_get(tickers_today, get = "stock.prices", from = "2004-01-02", to = "2019-01-02")

# This isn't working. It's an attempt to just pull a few stocks from case study 9 but it won't let me just select stuff. 
dat <- tq_get(tickers_today, get = "stock.prices", from = "2004-01-02", to = "2019-01-02") %>%
  select(symbol, date, adjusted) %>% 
  spread(key = symbol, adjusted, fill = NA)

xts_dat <- dat %>% 
  tk_xts(select = tickers_today, date_var = date)


##############################################################################


library(plotly)
library(MASS)

covmat <- matrix(c(0.8, 0.4, 0.3, 0.8), nrow = 2, byrow = T)
df <- mvrnorm(n = 10000, c(0,0), Sigma = covmat)
df <- as.data.frame(df)

colnames(df) <- c("x", "y")
p <- plot_ly(df, x = ~x, y = ~y, alpha = 0.3) %>%
  add_markers(marker = list(line = list(color = "black", width = 1))) %>%
  layout(
    title = "Drop down menus - Plot type",
    xaxis = list(domain = c(0.1, 1)),
    yaxis = list(title = "y"),
    updatemenus = list(
      list(
        y = 0.8,
        buttons = list(
          
          list(method = "restyle",
               args = list("type", "scatter"),
               label = "Scatter"),
          
          list(method = "restyle",
               args = list("type", "histogram2d"),
               label = "2D Histogram")))
    ))

print(p)
