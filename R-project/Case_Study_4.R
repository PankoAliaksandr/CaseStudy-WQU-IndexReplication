# Task Description:
#==================================================================================================================
# Task 1.
# Download data for last 3 years for the DJIA (Dow Jones Industrial Average) and each of the 30 component stocks.
# Task 2.
# Calculate Monthly returns of the DJIA index and the downloaded stocks over last 3 years

# Task 3.
# Calculate mean and standard deviation of monthly returns for the DJIA index

# Task 4.
# Choose an equal weighted portfolio consisting of any 5,10,15,20,25 random stocks from the DJIA.

# Task 5.
# For each portfolio calculate the mean monthly returns (total 5x) and its standard deviation (total 5x).

# Task 6.
# Calculate tracking errors for each of the portfolios
# i.e. the margin by which the mean and standard deviation of the portfolio returns diverge from those of DJIA

# Task 7.
# Graphically represent the tracking error for 1) returns
# and 2) risk (standard deviation of returns used as a proxy for risk)
# on y-axis against the sample size of portfolio on the x-axis

# Task 8. (question 1)
# What all factors account for the tracking error of the constructed portfolios?

# Task 9. (question 2)
# What is the relationship between tracking error and portfolio sample size?

# Task 10. (question 3)
# What might be the most optimal way to decrease tracking error
# without having to construct a full portfolio matching the entire index

#==================================================================================================================

#Libraries
library(quantmod)
library(XML)
library(RCurl)
library(PerformanceAnalytics)

# FUNCTION: Equally weighted portfolio returns
#-----------------------------------------------------------
GetPortfolioReturns <- function(ts_portfolio_assets_returns){
  assets_returns_matrix <- as.matrix(ts_portfolio_assets_returns)
  returns_vector <- c()
  number_of_assets <- ncol(assets_returns_matrix)
  number_of_periods <- nrow(assets_returns_matrix)
  for(i in 1:number_of_periods){
    period_sum_return <- 0
    for(j in 1:number_of_assets){
      period_sum_return <- period_sum_return + assets_returns_matrix[i,j]
    }
    portfolio_return <- period_sum_return/number_of_assets
    returns_vector <- append(returns_vector,portfolio_return)
  }
  return(returns_vector)
}
#-----------------------------------------------------------

# Task 1. Download Data
#-------------------------------------------------------------------------------------------------------------
#Read Index and Stocks
global_data_frame <- read.csv(file = "d:/DJprices.csv")

#Convert Date into "YYYY-MM-DD" format
global_data_frame$Date <- as.Date(global_data_frame$Date, "%m/%d/%Y")

#Create time series for all data frame
time_series_character <- xts(x = global_data_frame,order.by = global_data_frame$Date )

#Remove "Date" column
time_series_character$Date <- NULL

#Convert character time series into numeric
time_series_numeric <-as.numeric(time_series_character)
attributes(time_series_numeric) <- attributes(time_series_character)

#-------------------------------------------------------------------------------------------------------------


# Task 2. Calculate monthly returns based on monthly prices
monthly_returns <- Return.calculate(prices = time_series_numeric, method = "discrete")
monthly_returns <- monthly_returns[-1,]


# Task 3. Index mean and st.dev.
#-------------------------------------------------------------------------------------------------------------
#Calculate mean of monthly returns for DJIA 
index_returns <- monthly_returns$INDU
index_mean <- mean (index_returns, na.rm = TRUE)

#Calculate index standard deviation
index_stdev <- sd(index_returns, na.rm = TRUE)

#-------------------------------------------------------------------------------------------------------------

# Task 4. Portfolios creation
#-------------------------------------------------------------------------------------------------------------


# Task 4.1  Create stocks symbols vector
#==================================================================================
url <- 'https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average'
tabs <- getURL(url)

# Get all tables from the site
tables <- readHTMLTable(tabs)

# retrieve the necessary table with symbols
companies_symbols_matrix <- as.matrix(tables[[2]]["Symbol"])

# Create stocks symbols vector
companies_symbols_vector <- companies_symbols_matrix[,1]

#==================================================================================


# Task 4.2 Create random combinations of stocks
#==================================================================================

# Prepare 30 stocks returns time series
ts_for_portfolios <- monthly_returns[,-1]    



# I need to simulate at least 50 trials
margin_mean_vector_5_trials_50  <- c()
margin_mean_vector_10_trials_50 <- c()
margin_mean_vector_15_trials_50 <- c()
margin_mean_vector_20_trials_50 <- c()
margin_mean_vector_25_trials_50 <- c()

margin_stdev_vector_5_trials_50  <- c()
margin_stdev_vector_10_trials_50 <- c()
margin_stdev_vector_15_trials_50 <- c()
margin_stdev_vector_20_trials_50 <- c()
margin_stdev_vector_25_trials_50 <- c()

for( i in 1:100){

# Choose only 5 stocks for the first portfolio
portfolio_5_size <- 5
selected_stocks <- sample(ncol(ts_for_portfolios), portfolio_5_size)
ts_portfolio_5_returns <- ts_for_portfolios[ , selected_stocks ] 
monthly_portfolio_5_returns <- GetPortfolioReturns(ts_portfolio_5_returns)

# Choose only 10 stocks for the first portfolio
portfolio_10_size <- 10
selected_stocks <- sample(ncol(ts_for_portfolios), portfolio_10_size)
ts_portfolio_10_returns <- ts_for_portfolios[ , selected_stocks ] 
monthly_portfolio_10_returns <- GetPortfolioReturns(ts_portfolio_10_returns)

# Choose only 15 stocks for the first portfolio
portfolio_15_size <- 15
selected_stocks <- sample(ncol(ts_for_portfolios), portfolio_15_size)
ts_portfolio_15_returns <- ts_for_portfolios[ , selected_stocks ] 
monthly_portfolio_15_returns <- GetPortfolioReturns(ts_portfolio_15_returns)

# Choose only 20 stocks for the first portfolio
portfolio_20_size <- 20
selected_stocks <- sample(ncol(ts_for_portfolios), portfolio_20_size)
ts_portfolio_20_returns <- ts_for_portfolios[ , selected_stocks ] 
monthly_portfolio_20_returns <- GetPortfolioReturns(ts_portfolio_20_returns)

# Choose only 25 stocks for the first portfolio
portfolio_25_size <- 25
selected_stocks <- sample(ncol(ts_for_portfolios), portfolio_25_size)
ts_portfolio_25_returns <- ts_for_portfolios[ , selected_stocks ] 
monthly_portfolio_25_returns <- GetPortfolioReturns(ts_portfolio_25_returns)

#==================================================================================

# Task 5. Calculate portfolios returns mean and st.dev
#----------------------------------------------------------------
portfolio_5_returns_mean   <- mean(monthly_portfolio_5_returns)
portfolio_5_returns_stdev  <- sd(monthly_portfolio_5_returns)
portfolio_10_returns_mean  <- mean(monthly_portfolio_10_returns)
portfolio_10_returns_stdev <- sd(monthly_portfolio_10_returns)
portfolio_15_returns_mean  <- mean(monthly_portfolio_15_returns)
portfolio_15_returns_stdev <- sd(monthly_portfolio_15_returns)
portfolio_20_returns_mean  <- mean(monthly_portfolio_20_returns)
portfolio_20_returns_stdev <- sd(monthly_portfolio_20_returns)
portfolio_25_returns_mean  <- mean(monthly_portfolio_25_returns)
portfolio_25_returns_stdev <- sd(monthly_portfolio_25_returns)
#-----------------------------------------------------------------

# Task 6. Calculate tracking errors
#----------------------------------------------------------------
mean_margin_5  <- abs(index_mean - portfolio_5_returns_mean) 
mean_margin_10 <- abs(index_mean - portfolio_10_returns_mean)
mean_margin_15 <- abs(index_mean - portfolio_15_returns_mean)
mean_margin_20 <- abs(index_mean - portfolio_20_returns_mean)
mean_margin_25 <- abs(index_mean - portfolio_25_returns_mean)

stdev_margin_5  <- abs(index_stdev - portfolio_5_returns_stdev)
stdev_margin_10 <- abs(index_stdev - portfolio_10_returns_stdev)
stdev_margin_15 <- abs(index_stdev - portfolio_15_returns_stdev)
stdev_margin_20 <- abs(index_stdev - portfolio_20_returns_stdev)
stdev_margin_25 <- abs(index_stdev - portfolio_25_returns_stdev)
#----------------------------------------------------------------

margin_mean_vector_5_trials_50[i]  <- mean_margin_5 
margin_mean_vector_10_trials_50[i] <- mean_margin_10 
margin_mean_vector_15_trials_50[i] <- mean_margin_15 
margin_mean_vector_20_trials_50[i] <- mean_margin_20 
margin_mean_vector_25_trials_50[i] <- mean_margin_25 

margin_stdev_vector_5_trials_50[i]  <- stdev_margin_5
margin_stdev_vector_10_trials_50[i] <- stdev_margin_10
margin_stdev_vector_15_trials_50[i] <- stdev_margin_15
margin_stdev_vector_20_trials_50[i] <- stdev_margin_20
margin_stdev_vector_25_trials_50[i] <- stdev_margin_25

}

average_mean_5_of_50_trials  <- mean(margin_mean_vector_5_trials_50)
average_mean_10_of_50_trials <- mean(margin_mean_vector_10_trials_50)
average_mean_15_of_50_trials <- mean(margin_mean_vector_15_trials_50)
average_mean_20_of_50_trials <- mean(margin_mean_vector_20_trials_50)
average_mean_25_of_50_trials <- mean(margin_mean_vector_25_trials_50)

average_stdev_5_of_50_trials  <- mean(margin_stdev_vector_5_trials_50)
average_stdev_10_of_50_trials <- mean(margin_stdev_vector_10_trials_50)
average_stdev_15_of_50_trials <- mean(margin_stdev_vector_15_trials_50)
average_stdev_20_of_50_trials <- mean(margin_stdev_vector_20_trials_50)
average_stdev_25_of_50_trials <- mean(margin_stdev_vector_25_trials_50)

# Task 7. Graphical representation
mean_vector  <- c(average_mean_5_of_50_trials, average_mean_10_of_50_trials, average_mean_15_of_50_trials,
                  average_mean_20_of_50_trials, average_mean_25_of_50_trials)

stdev_vector <- c(average_stdev_5_of_50_trials, average_stdev_10_of_50_trials, average_stdev_15_of_50_trials,
                  average_stdev_20_of_50_trials, average_stdev_25_of_50_trials)

portfolio_size_vector <- c(portfolio_5_size, portfolio_10_size, portfolio_15_size, portfolio_20_size, portfolio_25_size)

max_mean_margin <- max(mean_vector)
max_stdev_margin <- max(stdev_vector)
y_lim <- c(0, max(max_mean_margin, max_stdev_margin))

plot(x = portfolio_size_vector, y = mean_vector, xlab = 'portfolio size', ylab = 'margin',  main = 'tracking errors', col  = 'red', pch = 16, ylim = y_lim)
points(x = portfolio_size_vector, y = stdev_vector, xlab = 'portfolio size', ylab = 'margin',  main = 'tracking errors', col  = 'blue', pch = 16)
legend(x = "topright", c("Mean", "St.Dev"), 
       col = c('red','blue'),
       lwd = c(1, 1))

