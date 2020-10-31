#xts will be used to facilite implemenenting rolling means, while DBI to connect with
#PostgreSQL database.
library(xts)
library(DBI)


#Here you specify your connection parameters
db <- "lse_db"
db_user <- 'krzysztofjanik' 
db_password <- 'krzysio1'
db_port <- '5432'
#This connection will be used throught the script
default_connection <- dbConnect(RPostgres::Postgres(), dbname = db, port=db_port, user=db_user,
                 password=db_password)


#check_variables function essentially makes sure that the values supplied to
#strategy_summary function make sense.
check_variables <- function(start_date, end_date, long_window, short_window,
                           top_cutoff, bottom_cutoff, full, tidms_size){
  if(as.Date(end_date) <= as.Date(start_date)){
    stop("Start date must be sooner than the end date.")
  }
  if((as.integer(long_window) <= as.integer(short_window)) | short_window < 0){
    stop("Short window must be shorter than the long window")
  }
  if(as.integer(top_cutoff) <= 0 | as.integer(bottom_cutoff) >= 0){
    stop("Top cutoff has to be above zero, and bottom below.")
  }
  if(as.integer(tidms_size) <=0){
    stop("The size of tidms has to be a positive whole number.")
  }
  if (!is.logical(full)){
    stop("Full has to be either TRUE or FALSe")
  }
}


#Here we fetch the data from our database, for a given tidm.
fetch_data <- function(tidm, connection, start, end){
  query <- sprintf("SELECT * FROM prices WHERE tidm = '%s'
                   AND date BETWEEN '%s' AND '%s'", tidm, start, end)
  sent_query <- dbSendQuery(connection, query)
  data <- dbFetch(sent_query)
  dbClearResult(sent_query)
  return(data)
}


#This function returns a list of tidms to use
which_tidms <- function(FULL, N){
  statement <- "SELECT DISTINCT tidm FROM prices"
  query <- dbSendQuery(default_connection, statement)
  tidms <- dbFetch(query)
  dbClearResult(query)
  full_list <- as.data.frame(tidms)$tidm
  if (FULL == TRUE){
    return(full_list)
  }
  else{
    return(sample(full_list, N, replace = FALSE))
  }
}


#Here I compute long and short averages. I briefly turn the dataframe into a
#timeseries, as it facilitates computing rolling means.
compute_means <- function(data, short_window, long_window, tidm){
  data <- xts(data[,2:8],order.by = data$date)
  data$short_mean <- rollapply(data$close, width = short_window, FUN = 'mean',
                               fill = NA)
  data$long_mean <- rollapply(data$close, width = long_window, FUN = "mean",
                              fill = NA)
  index <- index(data)
  data <- as.data.frame(data)
  data$date <- as.Date(index)
  rownames(data) <- seq(nrow(data))
  data$tidm <- tidm
  data$index <- seq(nrow(data))
  return(data)
}


#This function scans our data for trading days when the entry positions for the next
#day were met. Also includes a filter for volume and price.
choose_conditions <- function(data, cutoff_top, cutoff_bottom){
  when_conditions_ok <- data[(data$short_mean > data$long_mean) & 
                               ((data$return >= cutoff_top) | 
                                  (data$return <= cutoff_bottom)) &
                               (data$volume > 10000) & (data$close >= 2),]
  when_conditions_ok <- when_conditions_ok$index
  return(when_conditions_ok)
}


#Here I create a dataframe of trades, where conditions were met
give_conditions <- function(data, conditions_index){
  conditions_ok <- data[is.element(data$index, conditions_index),]
  return(conditions_ok)
}


#Here I create a dataframe of the actual trades
give_trading_days <- function(data, conditions_index){
  trading_index <- conditions_index + 1
  trading_days <- data[is.element(data$index, trading_index),]
  return(trading_days)
}


#This function returns 2 datasets, a conditions dataset, and a dataset with trades
#based on these.
create_datasets <- function(tidm_list, cutoff_top, cutoff_bottom, short_window,
                            long_windom, connection, start, end){
  
  conditions_to_return <- data.frame(open = numeric(), high = numeric(), 
                                     low = numeric(), close = numeric(), 
                                     volume = numeric(), return = numeric(), 
                                     tidm = character(), short_mean = numeric(), 
                                     long_mean = numeric(), index = integer(), 
                                     data = as.Date(character()))
  trades_to_return <- data.frame(open = numeric(), high = numeric(), 
                                 low = numeric(), close = numeric(), 
                                 volume = numeric(), return = numeric(),
                                 tidm = character(), short_mean = numeric(),
                                 long_mean = numeric(), index = integer(),
                                 data = as.Date(character()))
  for (tidm in tidm_list){
    raw_data <- fetch_data(tidm, connection, start, end)
    if (nrow(raw_data) > long_windom){
      data <- compute_means(raw_data, short_window, long_windom, tidm)
      conditions_flags <- choose_conditions(data, cutoff_top, cutoff_bottom)
      if (length(conditions_flags) != 0){
        conditions_data <- give_conditions(data, conditions_flags)
        trades_data <- give_trading_days(data, conditions_flags)
        conditions_to_return <- rbind(conditions_data, conditions_to_return)
        trades_to_return <- rbind(trades_data, trades_to_return)
      }
    }
  }
  final_data <- list(conditions_to_return, trades_to_return)
  return(final_data)
}



#Here I add two columns with returns to my trades data. One is calculated 
#with respect to open and close. The other adds a stop loss.
add_proper_returns <- function(trades_data, stoploss){
  trades_data$return <- 100 * ((trades_data$close / trades_data$open) - 1)
  low <- 100 * ((trades_data$low / trades_data$open) - 1)
  trades_data$return_stoploss <- ifelse(trades_data$return <= stoploss | 
                                          low <= stoploss, stoploss,
                                        trades_data$return)
  return(trades_data)
}


#This function consoldates trades for individual days. Every day there can be a few
#assets worth investing in. This puts them together and works out the return for the
#day
consolidated_trading_dataset <- function(returns_data){
  dates <- as.data.frame(table(returns_data$date))
  dates$Var1 <- as.Date(dates$Var1)
  colnames(dates) <- c("date", "no_assets")
  dates$return <- NA
  dates$return_stoploss <- NA
  for (date_val in dates$date){
    dates[dates$date == date_val, "return"] <- mean(returns_data[returns_data$date 
                                                                 == date_val, "return"])
    dates[dates$date== date_val, "return_stoploss"] <- mean(
      returns_data[returns_data$date == date_val, "return_stoploss"])
  }
  dates$mulitplier <- (dates$return / 100) + 1
  dates$multiplier_stoploss <- (dates$return_stoploss / 100) + 1
  dates$accumulated <- cumprod(dates$mulitplier)
  dates$accumulated_stoploss <- cumprod(dates$multiplier_stoploss)
  return(dates)
}


#This is the main function of the script. By calling it, you generate a "report"
#on a given backtest. It returns a list of daily returns, all the trades performed,
#as well as the conditions trigerring the trades. Also, it takes a while
#(>30 minutes) to run a full backtest, I included the option to run it on N randomly
#selected stocks. This is specified by tidms_size. If you want to run a full backtest,
#use the argument full = T. To specify your connection, use connection details at the
#top of the file
strategy_summary <- function(
  start_date = "2015-12-31",
  end_date = "2017-12-31",
  long_window = 50,
  short_window = 10,
  top_cutoff = 10,
  bottom_cutoff = -10,
  full = FALSE,
  tidms_size = 200,
  connection = default_connection,
  stoploss = -2){
 check_variables(start_date, end_date, long_window, short_window, top_cutoff,
                 bottom_cutoff, full, tidms_size)
  cat("All arguments valid")
  tidms <- which_tidms(full, tidms_size)
  conditions_and_trades <- create_datasets(tidms, top_cutoff, bottom_cutoff,
                                           short_window, long_window, connection,
                                           start_date, end_date)
  trading_data <- add_proper_returns(conditions_and_trades[[2]], stoploss)
  conditions_data <- conditions_and_trades[[1]]
  consolidated <- consolidated_trading_dataset(trading_data)
  to_give_back <- list(consolidated, trading_data, conditions_data)
  return(to_give_back)
}

#analysis <- strategy_summary(end_date = "2020-05-05", full = T, top_cutoff = 6, bottom_cutoff = -6, stoploss = -3)
