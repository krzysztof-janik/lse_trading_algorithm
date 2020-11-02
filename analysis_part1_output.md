Analysis part 1
================
Krzysztof Janik
02/11/2020

This is the analysis of my trading strategy. The analysis consists of 3
main parts: 1. Checking whether it’s possible to cut down on simulation
time. 2. Choosing the best set of trading parameters. 3. For the best
performing strategy, choose the best stocks via linear regression.

Firstly, I load the libraries: DBI for database connection, xts for
working with timeseries data, ggplot2 for plots. I also specify my
connection parameters here. The seed is set so that the random elements
can be reproduced.

    library(DBI)
    library(xts)
    set.seed(1)
    db <- "lse_db"
    db_user <- 'krzysztofjanik' 
    db_password <- '####'
    db_port <- '5432'
    default_connection <- dbConnect(RPostgres::Postgres(), dbname = db, port=db_port, user=db_user, password=db_password)

One more thing before the actual analysis is to check the time period
for which the data is available.

    query <- "SELECT min(date) FROM prices"
    sent <- dbSendQuery(default_connection, query)
    minim <- dbFetch(sent)
    dbClearResult(sent)
    query <- "SELECT max(date) FROM prices"
    sent <- dbSendQuery(default_connection, query)
    maxim <- dbFetch(sent)
    dbClearResult(sent)
    cat("Start date: ", as.character(minim[1,]), ". End date: ", as.character(maxim[1,]), ".", sep = "")

    ## Start date: 2016-01-04. End date: 2020-09-29.

So we have nearly 5 years worth of data. I will divide it into train and
test dataset. Train will be years 2016,17 and 18. 2019 and 2020 will be
our test data.

    train_start_date <- "2016-01-04"
    train_end_date <- "2018-12-31"
    test_start_date <- "2019-01-01"
    test_end_date <- "2020-09-29"

Lastly, I use function I defined in trading\_algorithm.R to help with
the backtest. Note: the path is hardcoded, if you want to rerun this
file, modify it.

    source("/Users/krzysztofjanik/trading_project/trading_algorithm.R")

Part 1. Cutting down on the simulation time. Now, the full backtest of
strategy works through \~2000 stocks and takes 15 minutes. I will try to
see whether I can save some time, simulate only a couple hundred stocks
and still get similar performance measure. Firstly I have to desribe my
strategy. The following will measure my performance: mean of
returns,variation of returns,total return,sharpe ratio,equity curve, max
drawdown,max drawdown duration. Since I evaluate a strategy with a
stoploss as well as without one (as a worst-case-scenario), I evaluate
each measure twice.

    max_drawdowns <- function(accumulation){
      data <- data.frame(accumulated = accumulation)
      data$high_water <- NA
      for (i in seq(from = 1, to = nrow(data))){
        data$high_water[i] <- max(data$accumulated[1:i])
      }
      data$drawdown <- 100*((data$accumulated - data$high_water) / data$high_water)
      data$drawdown_duration <- NA
      for (i in seq(from = 1, to = nrow(data))){
        if (data$drawdown[i] < 0){
          data$drawdown_duration[i] <- data$drawdown_duration[i -1] + 1
        }
        else{
          data$drawdown_duration[i] <- 0
        }
      }
      max_drawdown <- min(data$drawdown)
      max_drawdown_duration <- max(data$drawdown_duration)
      to_give <- list(max_drawdown, max_drawdown_duration)
      return(to_give)
    }
    create_report <- function(strategy){
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
