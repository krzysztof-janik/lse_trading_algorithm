# lse_trading_algorithm
A trading system generating buy and sell signals for stocks listed on the London Stock Exchange.
The algorithm calculates a short and long rolling average. A buy signal is generated if:
1. Previous day's return (calculated close to close) is above or below a cutoff value (ie. 10% and -7%). This is a simple way to capture volatility.
2. Short rolling average is higher than the long rolling average. This is to capture momentum, or reversal.
I also include a filter that exludes very cheap stocks, as well as those with little volume.

Before assessing the performance of a strategy, here are the assumptions I made:
1. I can enter a position at the day's open and sell at the close, regardless of order size.
2. Stocks are infinitely divisible (I can buy £8.32 worth of stock that trades at £10 and so on)
3. If on a given day I invest in more than one stock, all stocks get equal amount of investment.
4. I am not accounting for transaction costs.
5. I include a stop loss, which assumes I can sell the security at precisely the loss limit.
6. Lastly, the data I have suffers from survivorship bias.

The project consists of 3 analysis files:
1. Here I try to optimise simulation time. https://github.com/krzysztof-janik/lse_trading_algorithm/blob/main/part1_analysis/part1_analysis.md
2. __To be completed__ This part looks at finding optimal parameters maximising the profitability of my strategy.
3. __To be completed__ The last file seeks to further optimise the strategy by trying to choose only a few stocks each day that have the most potential.

Each part has it's own folder, containing the .md file that you can view, the .Rmd file used to generate the report, and a Figs folder with graphs. 

I also include two files that enable the analysis:
* *db_creation.py* creates a PostgreSQL database with the stockmarket data. You can run it yourself, and either download the data from scratch
(for that you need the *Trading statistics September 2020.xlsx* file) or use the data file *prices.csv* that I supply __not yet, it's over github's file size limit__. Further instructions inside the file.
* *trading_algorithm.R* contains functions used in the 3 analysis files. You can also run it yourself, to generate trades based on the parameters you specify inside the file.

Feel free to reach out to me if you have any questions!
