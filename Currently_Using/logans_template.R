### The purpose of this section is to get clean data.
## Dates from 10 years ago. 
## Breaking the data up by year, finding the correlation between each year, adding a year column, and joining the data back together

library(tidyverse)
library(BatchGetSymbols)
library(reshape2) # Used to melt the data

## This section gets the data

first.date <- Sys.Date() - 5550
last.date <- Sys.Date()
freq.data <- 'monthly'

tickers <- c('GILD','UNP','MCD','HPQ', 'MMM', 'CSCO', 'SLB', 'AMGN', 'BA', 'COP', 'CMCSA', 'BMY', 'VZ', 'T', 'PEP', 'MCD', 'PFE', 'ABT', 'ORCL', 'DIS', 'AAPL', 'HD')

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()


## This section breaks the data up into subsets by year, finds the correlation, and creates a year column.
dat_2004 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2004')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2004),2)
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
dat_04 <- melted_cormat %>% 
  cbind(year = "2004")




dat_2005 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2005')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2005),2)
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
dat_05 <- melted_cormat %>% 
  cbind(year = "2005")




dat_2006 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2006')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2006),2)
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
dat_06 <- melted_cormat %>% 
  cbind(year = "2006")




dat_2007 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2007')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2007),2)
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
dat_07 <- melted_cormat %>% 
  cbind(year = "2007")




dat_2008 <- as.data.frame(l.out$df.tickers) %>% 
  filter(str_detect(ref.date, '2008')) %>% 
  select(ticker,  ref.date, price.adjusted) %>% 
  spread(key = ticker, price.adjusted, fill = NA) %>% 
  select(-ref.date)

# Computing the correlation matrix 
cormat <- round(cor(dat_2008),2)
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
dat_08 <- melted_cormat %>% 
  cbind(year = "2008")


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

lit_dat <- bind_rows(dat_18, dat_17, dat_16, dat_15, dat_14, dat_13, dat_12, dat_11, dat_10, dat_09, dat_08, dat_07, dat_06, dat_05, dat_04)
lit_dat$ticker_1 <- lit_dat$Var1
lit_dat$Var1 <- NULL
lit_dat$ticker_2 <- lit_dat$Var2
lit_dat$Var2 <- NULL


#
#
#
#
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(readxl)
library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel('Historical Correlation, 2004-2019'),
  
  # Sidebar with a slider input for number of bins 
  ## Buttons
  sidebarPanel(uiOutput('ticker1'),
               uiOutput('ticker2')),
  
  
  # Show a plot of the generated distribution
  ## graphOutput
  mainPanel(
    plotOutput('ticker3')
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$ticker1 <- renderUI({
    selectInput(
      ## The title of the imput (whatever)
      inputId = 'ticker_1',
      label = 'Select First Ticker',
      ## first column
      choices = unique(lit_dat$ticker_1)
    )
  })
  
  output$ticker2 <- renderUI({
    selectInput(
      inputId = 'ticker_2',
      label = 'Select Second Ticker',
      choices = unique(lit_dat$ticker_2)
    )
  })
  
  dataInput <- reactive({
    lit_dat %>% filter(ticker_1 == input$ticker_1 & ticker_2 == input$ticker_2)
  })
  
  ## This is where the graph is created
  ## Change renderTable to renderGraph
  output$ticker3 <- renderPlot({
    dataInput() %>%
      ggplot(aes(x =year, y = value)) +
      geom_point(aes(x =year, y = value)) +
      geom_line(aes(group = ticker_1)) +
      theme_bw() +
      labs(x = "Year", y = "Correlation Score")
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

###################################
#Trying to host to the cloud
library(rsconnect)
# https://shiny.rstudio.com/articles/shinyapps.html
#rsconnect::setAccountInfo(name='saffra', token='token', secret='secret')
rsconnect::deployApp('C:/Users/Saffra/Documents/T3/T3/Currently_Using/logans_template.R')
