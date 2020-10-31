#Imported libraries: pandas and numpy for manipulating data; yfinance to
#download stock prices from YahooFinance; time to monitor the download
#process; psycopg2 to create and interact wiht a PostgreSql database.
import pandas as pd
import numpy as np
import yfinance as yf
import time
import psycopg2


#These are default connection parameters, the user needs to configure them
#with own username etc.
user = '####'
password = '####'
host = 'localhost'
port = '5432'
database = 'lse_db'
default_connection = psycopg2.connect(user = user,
                                      password = password,
                                      host = host,
                                      port = port,
                                      database = database)


#This is the path to the file that contains tidms and describes individual stocks.
#The user can download the newest version from: https://www.londonstockexchange.com/reports?tab=equities
#But has to modify the file so that it is formatted like the orginial used here.
trading_statistics_path = "Trading statistics September 2020.xlsx"


#This is the path where prices of stocks are saved, this is used if the user
#chooses to skip the download process.
prices_path = "prices.csv"


#If the user download new data, it will be saved here as a csv.
#I figured that the fastest and easiest way of saving data to sql is to save
#it a csv file first and then upload to the database, hence this path.
new_prices_path = "new_prices.csv"



#Here I extract the tidms from data downloaded from the lse website.
#There is one issue with the data that I am not sure how to resolve.
#There are 2398 securities, but only 2037 unique ISINs. Theoretically, an ISIN
#is unique for every security and should not repeat. I decided to drop the ISIN
#column. I only need TIDMs to download the data and will use them to uniqely
#specify a security in my database.
def extract_tidms (trading_statistics_path):
    raw_file = pd.read_excel(trading_statistics_path)
    raw_file.dropna(inplace = True)
    columns_to_drop = ["Value Traded (£)", "Volume", "Trades",
                   "Average Daily Value Traded (£) YTD",
                   "Average Daily Volume YTD", "Average Daily Trades YTD",
                   "Instrument Name", "ISIN", "LSE Market Segment Code",
                   "LSE Market Sector Code"]
    raw_file.drop(columns=columns_to_drop, inplace=True)
    raw_file.drop_duplicates(inplace=True, subset=["TIDM"], keep='first')
    tidms = np.array(raw_file["TIDM"])
    tidms = tidms + ".L"
    for i in range(len(tidms)):
        tidms[i] = tidms[i].replace("..", ".")
    print(len(tidms), "tidms extracted.")
    return(tidms)


#Here I calculate daily returns on the stock. They are calculated close-to-close
def return_calculator(stock):
    '''Because some of prices are negative, I have to write my own function
    to calulate percenatge returns. It takes a pandas DataFrame, adds a 
    column with returns and returns the modified DataFrame.'''
    close = stock.Close
    diffs = close.diff()
    close = close.abs()
    returns = diffs[1:].div(close[:-1])
    stock["Return"] = 100 * returns
    return(stock)


#This function iterates through my list of tidms, and downloads relevant data.
#It also includes a timer, to measure progress. I pause the code for 1 second
#after each download for fear of yahoo banning my IP adress.
def download_data(tidms, start_date, end_date, where_to_save):
    not_found = []
    prices = pd.DataFrame(columns = ["Open", "High", "Low", "Close", "Volume",
                                     "Return", "TIDM"])
    counter = 0
    start = time.time()
    for stock in tidms:
        data = yf.download(stock, start = start_date, end = end_date,
                           progress = False, auto_adjust = True)
        if data.empty:
            not_found.append(stock)
        else:
            data = return_calculator(data)
            data["TIDM"] = stock
            prices = prices.append(data)
        time.sleep(1)
        counter = counter + 1
        if (counter % 25) == 0:
            end = time.time()
            dur = round(end - start, 4)
            print("It took:", dur,"seconds to process:", counter, "stocks.")
            print("Successful downloads:", counter - len(not_found))
            print("Not found:", len(not_found))
    prices = prices.dropna()
    prices.to_csv(where_to_save)
    print("Data saved to:", where_to_save)
    
    
#This function takes data saved to csv and copies it to a sql database.
def upload_to_sql(where_prices_saved, connection):
    cursor = connection.cursor()
    create_prices_query = '''CREATE TABLE prices
        (date DATE,
         open REAL,
         high REAL,
         low REAL,
         close REAL,
         volume REAL,
         return REAL,
         tidm VARCHAR(10)
         );'''
    cursor.execute(create_prices_query)
    connection.commit()
    add_prices_query = '''COPY prices
        FROM '{}'
        DELIMITER ','
        CSV HEADER
        '''.format(where_prices_saved)
    cursor.execute(add_prices_query)
    connection.commit()
    cursor.close()
    connection.close()
    print("Data uploaded")
    

#This is the file's main function, it creates an SQL database, with the option
#to download new data (it takes >30 minutes to do so, be warned).
def create_database(where_are_prices=prices_path,
                    where_to_save_prices=new_prices_path,
                    where_are_lse_stats=trading_statistics_path,
                    start_date="2015-12-31",
                    end_date = "2020-09-30",
                    connection_parameters = default_connection,
                    download=False):
    if start_date >= end_date:
        return("Start date can't be after the end date.")
    else:
        print("Dates correct")
    if download:
        tidms = extract_tidms(where_are_lse_stats)
        download_data(tidms, start_date, end_date, where_to_save_prices)
        upload_to_sql(where_to_save_prices, connection_parameters)
    else:
        upload_to_sql(where_are_prices, connection_parameters)
    print("Done")
create_database(download=False)








