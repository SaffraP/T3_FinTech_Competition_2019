---
title: "interactive_correlation"
author: "Saffra Parks"
date: "January 11, 2019"
output: 
  html_document:
    keep_md: true 
---


```r
rm(list=ls())
library(quantmod)
```

```
## Loading required package: xts
```

```
## Loading required package: zoo
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```
## Loading required package: TTR
```

```
## Version 0.4-0 included new data defaults. See ?getSymbols.
```

```r
library(ggplot2)
```

https://lamfo-unb.github.io/2017/07/22/intro-stock-analysis-1/


```r
library(quantmod)
symbols = c("GOOG","MSFT","AAPL")
getSymbols(symbols, from="2004-01-01")
```

```
## 'getSymbols' currently uses auto.assign=TRUE by default, but will
## use auto.assign=FALSE in 0.5-0. You will still be able to use
## 'loadSymbols' to automatically load data. getOption("getSymbols.env")
## and getOption("getSymbols.auto.assign") will still be checked for
## alternate defaults.
## 
## This message is shown once per session and may be disabled by setting 
## options("getSymbols.warning4.0"=FALSE). See ?getSymbols for details.
```

```
## 
## WARNING: There have been significant changes to Yahoo Finance data.
## Please see the Warning section of '?getSymbols.yahoo' for details.
## 
## This message is shown once per session and may be disabled by setting
## options("getSymbols.yahoo.warning"=FALSE).
```

```
## [1] "GOOG" "MSFT" "AAPL"
```

```r
ClosePrices <- lapply(symbols, function(x) Ad(get(x)))
```



```r
pbr <- getSymbols("PBR", src = "yahoo", from = "2013-01-01", to = "2017-06-01", auto.assign = FALSE)
```

