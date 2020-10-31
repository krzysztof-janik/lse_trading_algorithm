# lse_trading_algorithm
A trading system generating buy and sell signals for stocks listed on the London Stock Exchange.
The algorithm calculates a short and long rolling average. A buy signal is generated if:
1. Previous day's return (calculated close to close) if above or below a cutoff value (ie. 10% and -7%). This is a simple way to capture volatility.
2. Short rolling average is higher than the long rolling average. This is to capture momentum, or reversal.
I also include a filter that exludes very cheap stocks, as well as those with little volume.

Before assessing the performance of a strategy, here are the assumptions I made:
1. I can enter a position at the day's open and sell at the close, regardless of order size.
2. Stocks are infinitely divisible (I can buy £8.32 worth of stock that trades at £10 and so on)
3. If on a given day I invest in more than one stock, all stocks get equal amount of investment.
4. I am not accounting for transaction costs.
5. I include a stop loss, which assumes I can sell the security at precisely the loss limit.
6. Lastly, the data I have suffers from survivorship bias.

So far the project consists of 3 files, one with data and two scripts. My goal is to add one more data file and an R markdown file where I analyse the strategy and optimise for the best return.

How to use...

The first file you have to run is the python script 'db_creation.py'. Make sure to open the file before running it, as you have to adjust your sql settings.
The file creates a database on your computer in one of 2 ways. You can either opt to download all the data or use the data I supply. Mind that it takes a while for
the download process to finish, and you may lose connection while doing so. I also gave the possibility of adjusting the time horizon for the download.
I only supplied the data file used for download, 'Trading statistics September 2020.xlsx'. I will inlcude the data for setting up the database without the need to download, but I have to work around Githubs max file size limit.

After you set up the database, you can use the R script trading_algorithm.R to run a strategy of your own. You can adjust all the variables (lenghts of rolling averages, stoploss limits etc). This script is primarly a set of functions that I will import in the final analysis part.
