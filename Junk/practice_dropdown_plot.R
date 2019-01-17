## This was taken from StackOverflow
#https://stackoverflow.com/questions/48096600/can-i-make-a-line-plot-with-a-dropdown-menu-in-r-without-using-shiny-or-plotly


## Create random data. cols holds the parameter that should be switched
l <- lapply(1:100, function(i) rnorm(100))
df <- as.data.frame(l)
cols <- paste0(letters, 1:100)
colnames(df) <- cols
df[["c"]] <- 1:100

## Add trace directly here, since plotly adds a blank trace otherwise
p <- plot_ly(df,
             type = "scatter",
             mode = "lines",
             x = ~c, 
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
