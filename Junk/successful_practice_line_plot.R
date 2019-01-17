
## Creating a plot with a dropdown menu


## Create random data. cols holds the parameter that should be switched
# 100 columns and 50 rows
l <- lapply(1:100, function(i) rnorm(50))
df <- as.data.frame(l)
# Renaming the column names
cols <- paste0(letters, 1:100)
colnames(df) <- cols
# Does something with the rows
df[["c"]] <- 1:50

## Add trace directly here, since plotly adds a blank trace otherwise
p <- plot_ly(unique_col,
             type = "scatter",
             # R will default mode to markers
             mode = "lines",
             # I think this means "each column"
             x = ~c,
             # I think this means "all the values in the dataframe? Might need to change it to be jsut ~df
             y= ~df[[cols[[1]]]],
             # No idea what this does. 
             name = cols[[1]])
## Add arbitrary number of traces
## Ignore first col as it has already been added
for (col in cols[-1]) {
  p <- p %>% add_lines(x = ~c, y = df[[col]], name = col, visible = FALSE)
}

p <- p %>%
  layout(
    title = "Dropdown line plot",
    xaxis = list(title = "date- hopefully"),
    yaxis = list(title = "correlation score - hopefully"),
    updatemenus = list(
      list(
        y = 0.7,
        ## Add all buttons at once
        buttons = lapply(cols, function(col) {
          list(method="restyle", 
               args = list("visible"),
               label = cols)
        })
      )
    )
  )

print(p)
