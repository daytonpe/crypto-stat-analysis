# CS6313: Statistial Methods for Data Science -S19
# Pat Dayton and AJ Rahendran

# install.packages("plyr")
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("dply")
library(plyr)
library(readr)
library(ggplot2)
library(dplyr)

setwd('/Users/daytonpe/Dropbox/utd/6316_stat_methods_for_ds_akcora/project/src')

# The modulo of the sum of our UTD IDs was 2, so we will be using tronix, omisego, and yocoin

# Load in the Etherium Tokens Information
omg_price_df = read.table("./tokenPrices/omisego.txt",
                 col.names = c('Date',	'Open',	'High',	'Low',	'Close',	'Volume',	'MarketCap'),
                 skip = 1,
                 header = FALSE)

# Check for duplicated
cat("Number of duplicates: ", anyDuplicated(omg_price_df), "\n")

# Convert date to the correct format
omg_price_df$Date = as.Date(omg_price_df$Date,format='%m/%d/%Y')


# Plot Opening Price over Time
p = ggplot(aes(x=Date, y=Open), data = omg_price_df) + geom_point()
print(p)

omg_edge_df <- read_delim('./edgeFiles/omisego.txt', delim = " ", col_names = F)

# Extract the data from the omisego edge file. Example:
# 142341 75994 1524611536 5301102205520000000000
names(omg_edge_df) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')


decimals = 10^18 # correct for omisego
supply = 140245398 # correct for omisego

# Filter out rows where the token amount is greater than the total tokenAmount
omg_edge_df_filtered = omg_edge_df %>% filter(tokenAmount < decimals*supply)
cat("Num Rows before Filtering: ", nrow(omg_edge_df), "\n")
cat("Num Rows after Filtering: ", nrow(omg_edge_df_filtered), "\n")
cat("Num Rows cut: ", (nrow(omg_edge_df)-nrow(omg_edge_df_filtered)), "\n")

# Set omg_edge_df to the filtered dataframe
omg_edge_df = omg_edge_df %>% filter(tokenAmount <= decimals * supply)




####################### Question 1 ##############################
# Find the distribution of how many times a pair users 
# (i.e., address1 and address2) 1 - buys, 2 - sells a token with each other. 
# Which distribution type fits these distributions best? 
# Estimate population distribution parameters.
#################################################################


# number of buys and sells by user id
# Great description here: https://stackoverflow.com/questions/25869378/what-does-n-n-mean-in-r
buys.distribution <- omg_edge_df %>% group_by(toID) %>% summarise(n = n()) %>% ungroup
sells.distribution <- omg_edge_df %>% group_by(fromID) %>% summarise(n = n()) %>% ungroup

## show highest 20 buyers and their number of buys 
cat("Buys Top 20")
print(buys.distribution %>% arrange(-n) %>% head(20))


## show highest 20 buyers and their number of buys 
cat("Sells Top 20")
print(sells.distribution %>% arrange(-n) %>% head(20))

# Create a bar chart to plot the top 20 sellers by their total tokens sold.
selldf = sells.distribution %>% arrange(-n) %>% head(20)
selldf$row_id <- as.numeric(row.names(selldf))
sells_quant_bar = ggplot(data=selldf, aes(x=row_id, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=-0.3, size=3.5)+
  theme_minimal()
print(sells_quant_bar)

# Create a bar chart to plot the top 20 sellers by their total tokens sold.
buydf = buys.distribution %>% arrange(-n) %>% head(20)
buydf$row_id <- as.numeric(row.names(buydf))
buys_quant_bar = ggplot(data=buydf, aes(x=row_id, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=-0.3, size=3.5)+
  theme_minimal()
print(buys_quant_bar)


# Sort into sale partners per question 2
cat("\n\nQuestion 2\n\n")
transaction_pair_df <- omg_edge_df
transaction_pair_df$pair = paste(as.character(transaction_pair_df$fromID),"-",as.character(transaction_pair_df$toID))
transactions_by_pair_df = transaction_pair_df %>% group_by(pair) %>% summarise(n = n()) %>% arrange(-n) %>% ungroup
print(transactions_by_pair_df %>% head(20))

# Optionally Drop out the outlier pair(311608 - 311608), n(30024)
# Comment this line out if you want to leave it in
transactions_by_pair_df = transactions_by_pair_df %>% filter(n < 30000)

# Create a bar chart to plot the top 50 transaction address pairs
pairdf = transactions_by_pair_df %>% head(50)
pairdf$row_id <- as.numeric(row.names(pairdf))
pair_bar = ggplot(data=pairdf, aes(x=row_id, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  # geom_text(aes(label=n), vjust=-0.3, size=3.5)+
  theme_minimal()
print(pair_bar)

