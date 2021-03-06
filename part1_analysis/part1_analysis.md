---
title: "Part 1: optimising simulation time"
author: Krzysztof Janik
date: 05/11/2020
output: github_document
---
This is part 1 out of 3 my analysis. The question that I will try to answer here is: do I have to run a full backtest to see how a strategy performs, or is it enought to use fewer, randomly chosen stocks?   

This would allow me significantly reduce simulation time in part 2 of analysis, and ultimately arrive at a better set of conditions resulting in better performance.  

### Setting up  

The first step is to load neccessary libraries. 

* __DBI__ for interacting with my sql database  
* __ggplot2__ for plotting  

I also use __set.seed()__ to make random parts of the analysis consistent with every re-run.  

Lastly, I set up my database connection parameters.


```r
library(DBI)
library(ggplot2)
set.seed(1)
db <- "lse_db"
db_user <- 'krzysztofjanik' 
db_password <- '####'
db_port <- '5432'
default_connection <- dbConnect(RPostgres::Postgres(), dbname = db, port=db_port, user=db_user,     
                                password=db_password)
```
One more thing before the actual analysis is to check the time period for which the data is available.

```r
query <- "SELECT min(date) FROM prices"
sent <- dbSendQuery(default_connection, query)
min_date <- dbFetch(sent)
dbClearResult(sent)
query <- "SELECT max(date) FROM prices"
sent <- dbSendQuery(default_connection, query)
max_date <- dbFetch(sent)
dbClearResult(sent)
query <- "SELECT distinct(tidm) FROM prices"
sent <- dbSendQuery(default_connection, query)
N <- dbFetch(sent)
dbClearResult(sent)
cat("Start date: ", as.character(min_date[1,]), ". End date: ", as.character(max_date[1,]), ". ",
    "Number of stocks: ", nrow(N),".", sep = "")
```

```
## Start date: 2016-01-04. End date: 2020-09-29. Number of stocks: 2007.
```
So we have nearly 5 years worth of data. I will divide it into train and test datasets. Train will be years 2016,17 and 2018,19,20 will be our test data.

```r
train_start_date <- "2016-01-04"
train_end_date <- "2018-12-31"
test_start_date <- "2019-01-01"
test_end_date <- "2020-09-29"
```
Lastly, I use functions I defined in trading_algorithm.R to help with the backtest.

```r
source("/Users/krzysztofjanik/trading_project/trading_algorithm.R")
```
## What to measure  

I will measure a strategy's performance with a number of indicators. These are:  

* mean return  
* variance of returns  
* total return  
* sharpe ratio  
* max drawdown  
* max drawdown duration  

Because I am using a stoploss in my strategy, and I can't be sure that I will actually be able to execute a trade at that price (as I explained in the readme file), I calculate the indicators with and without the stoploss.  

The following code is used to generate a 'report' for a given strategy. It includes the indicators above, as well as equity curves and dates on which trades happen.

```r
max_drawdowns <- function (accumulation) {
  data <- data.frame(accumulated = accumulation)
  data$high_water <- NA
  for (i in seq(from = 1, to = nrow(data))) {
    data$high_water[i] <- max(data$accumulated[1:i])
  }
  data$drawdown <- 100 * ((data$accumulated - data$high_water) / data$high_water)
  data$drawdown_duration <- NA
  for (i in seq(from = 1, to = nrow(data))) {
    if (data$drawdown[i] < 0) {
      data$drawdown_duration[i] <- data$drawdown_duration[i - 1] + 1
    } else {
      data$drawdown_duration[i] <- 0
    }
  }
  max_drawdown <- min(data$drawdown)
  max_drawdown_duration <- max(data$drawdown_duration)
  to_give <- list(max_drawdown, max_drawdown_duration)
  return(to_give)
}

create_report <- function (strategy) {
  report <- list()
  report$mean_return <- mean(strategy$return)
  report$mean_sreturn <- mean(strategy$return_stoploss)
  report$var_return <- var(strategy$return)
  report$var_sreturn <- var(strategy$return_stoploss)
  report$total_return <- tail(strategy$accumulated, n = 1)
  report$total_sreturn <- tail(strategy$accumulated_stoploss, n = 1)
  report$sharpe <- mean(strategy$return) / sd(strategy$return)
  report$ssharpe <- mean(strategy$return_stoploss) / sd(strategy$return_stoploss)
  report$equity <- strategy$accumulated
  report$sequity <- strategy$accumulated_stoploss
  drawdowns <- max_drawdowns(strategy$accumulated)
  report$max_drawdown <- drawdowns[[1]]
  report$max_drawdown_duration <- drawdowns[[2]]
  sdrawdowns <- max_drawdowns(strategy$accumulated_stoploss)
  report$smax_drawdown <- sdrawdowns[[1]]
  report$smax_drawdown_duration <- sdrawdowns[[2]]
  report$dates <- strategy$date
  return(report)
}
```
Here I create a vector of Ns, for which I generate reports. I also create a report for the full backtest, so I will be able to see whether the numbers converge to the true one.  

To generate this document, all of the code has to run. Because it takes ~2.5 hours to create all the reports, I save them as __part1_data.RData__, and after that I just load it to cut down on rendering time.

```r
if (file.exists("part1_data.RData")) {
  load("part1_data.RData")
} else {
  true_strategy <- strategy_summary(train_start_date, train_end_date, long_window = 50, 
                                    short_window = 10, top_cutoff = 10, bottom_cutoff = -10,
                                    full = TRUE, connection = default_connection, stoploss = -2)
  true_report <- create_report(true_strategy[[1]])
  how_many_tidms <- c(seq(from = 5, to = 200, by = 5), seq(from = 220, to = 700, by = 20),
                      seq(from = 750, to = 1000, by = 50))
  report_dataframe <- data.frame(N = how_many_tidms, mean_return = NA, mean_sreturn = NA,
                                 var_return = NA, var_sreturn = NA, total_return = NA,
                                 total_sreturn = NA, sharpe = NA, ssharpe = NA, max_drawdown = NA, 
                                 max_drawdown_duration = NA, smax_drawdown = NA,
                                 smax_drawdown_duration = NA)
  equity <- list()
  sequity <- list()
  dates <- list()
  for (how_many in how_many_tidms) {
    sample_strategy <- strategy_summary(start_date = train_start_date, end_date = train_end_date, 
                                        long_window = 50, short_window = 10, top_cutoff = 10, 
                                        bottom_cutoff = -10, tidms_size = how_many, 
                                        connection = default_connection, stoploss = -2)
    sample_report <- create_report(sample_strategy[[1]])
    for (statistic in names(sample_report[c(-9, -10, -15)])) {
      report_dataframe[report_dataframe$N == how_many, statistic] <- sample_report[[statistic]]
    }
    equity[[length(equity) + 1]] <- sample_report$equity
    sequity[[length(sequity) + 1]] <- sample_report$sequity
    dates[[length(dates) + 1]] <- sample_report$dates
  }
  save(dates, equity, sequity, report_dataframe, true_report, file = "part1_data.RData")
}
```
## Let's see if this works  

First things first, it's worth having a look at the values we are looking to obtain.

```r
true_report[c(-9,-10,-15)]
```

```
## $mean_return
## [1] 0.5836188
## 
## $mean_sreturn
## [1] 0.7901363
## 
## $var_return
## [1] 15.98629
## 
## $var_sreturn
## [1] 9.400888
## 
## $total_return
## [1] 36.00095
## 
## $total_sreturn
## [1] 192.9218
## 
## $sharpe
## [1] 0.1459673
## 
## $ssharpe
## [1] 0.2577019
## 
## $max_drawdown
## [1] -54.21048
## 
## $max_drawdown_duration
## [1] 120
## 
## $smax_drawdown
## [1] -14.99334
## 
## $smax_drawdown_duration
## [1] 52
```
And at the what the accumulation looks like.  

```r
stop_colour <- "#4285F4"
colour <- "#0F9D58"
ggplot() + 
geom_line(mapping = aes(x = true_report$dates, y = true_report$sequity, colour = stop_colour)) + 
geom_line(mapping = aes(x = true_report$dates, y = true_report$equity, colour = colour)) + 
xlab("Date") + ylab("Value of £1 investment") + ggtitle("Accumulation curve") + 
theme(plot.title = element_text(hjust = 0.5), legend.position = "top") + 
scale_x_date(date_breaks = "3 months", date_labels = "%b %y") + ylim(0, 200) + 
geom_abline(slope = 0, intercept = 1, linetype = 3) + 
scale_color_identity(guide = "legend", labels = c("Without stoploss", "Stoploss of -2%"), name = "") + 
scale_y_continuous(breaks = seq(0 ,200, 10))
```

<img src="Figs/unnamed-chunk-8-1.png" width="100%" height="75%" />

As we can see, adding a stoploss hugely improves performance, particularly later on in the strategy. 

Also, from now on I won't be showing the code used to generate plots, as I don't think it's particularly informative.  

Let's have a look at what happens to accumulation curves if we use fewer N's
<img src="Figs/unnamed-chunk-9-1.png" width="100%" height="75%" /><img src="Figs/unnamed-chunk-9-2.png" width="100%" height="75%" />
In both the graphs the thicker lines are actual performance, with N = 2007. What we can learn from this is that fewer Ns offer drastically different result. This hints that maybe we won't be able to make the approximations we hoped for.  

On the other hand, relatively few combinations of stocks achieved better performance than the full backtest.  

I will now look at other performance measures and see what happens to them as we increase N.
<img src="Figs/unnamed-chunk-10-1.png" width="100%" height="75%" />
The data for daily mean return looks promising, as we increase N, mean returns seem to converge to the true values (the dashed lines).
<img src="Figs/unnamed-chunk-11-1.png" width="100%" height="75%" />
We have the same situation with the variance. The values still converge, although slower.
However, we have to remember that variance is in units of %^2^, and therefore it tends to take a wider range of values.
<img src="Figs/unnamed-chunk-12-1.png" width="100%" height="75%" />
In terms of total return, arguably the most important metric, the values don't seem to converge. On would think that if the mean return converges, so would the total return. I think it is a result of the difference in frequency of trades and different distributions of returns.
<img src="Figs/unnamed-chunk-13-1.png" width="100%" height="75%" />
Unsurprisingly, daily Sharpe ratios converge rather quickly, because they are based on values that themselves converge (mean and standard deviation).
<img src="Figs/unnamed-chunk-14-1.png" width="100%" height="75%" />
The max drawdown also seems to converge, although slowly. An interesting thing to point out about the graph above is that our strategy becomes less volatile with the increase of N.
<img src="Figs/unnamed-chunk-15-1.png" width="100%" height="75%" />
Similarly with max drawdown duration we can see that the use of a stoploss significantly reduces the time needed to recoup any lossess. 

Overall, in the future, when evaluating the strategy, I will focus more on the total return and drawdowns. 
As we saw, similar mean returns can produce drastically different total returns. 
As for the variance, it measures both positive (higher returns) and negative (losses) dispersion. Because I am primarly concerned with possible loss of money, I will focus more on measuring drawdowns.

To conclude, I believe that it is better to run a full backtest every time. The time we could save is outweighed by the loss of accuracy. 
  
  
  
