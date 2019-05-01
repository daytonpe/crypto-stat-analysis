# CS6313: Statistial Methods for Data Science -S19
# Pat Dayton and AJ Rahendran

# This question is similar to the first question.
# You will find the most active buyers and sellers
# in each of your three token network, and track them
# in other tokens. Develop a regression model where “buys”
# of the top K buyers (by number of buys or amount of buys)
# are regressors, and token price is the outcome. Determine a
# K value to have the best regression results. This means that
# you will develop three regression models for three tokens,
# and K can be different for each model.

# TODO: switch to a list
# install.packages("plyr")
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("dply")
# install.packages("fitdistrplus")
# install.packages("anytime")
# install.packages("data.table")
library(plyr)
library(readr)
library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(anytime)
library(data.table)

setwd('/Users/daytonpe/Dropbox/utd/6316_stat_methods_for_ds_akcora/project/src')

# Read and write out the first lines of the edge file
# writeLines(readLines("./edgeFiles/omisego.txt", 2))

omg_edge_df <- read_delim('./edgeFiles/omisego.txt', delim = " ", col_names = F)

# Extract the data from the omisego edge file. Example:
# 142341 75994 1524611536 5301102205520000000000
names(omg_edge_df) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')

# Load in the Etherium Tokens Information
omg_price_df = read.table("./tokenPrices/omisego.txt",
                          col.names = c('Date',	'Open',	'High',	'Low',	'Close',	'Volume',	'MarketCap'),
                          skip = 1,
                          header = FALSE)

# Convert date to the correct format
omg_price_df$Date = as.Date(omg_price_df$Date,format='%m/%d/%Y')


# Check for duplicated
cat("Number of duplicates: ", anyDuplicated(omg_price_df), "\n")

decimals = 10^18 # correct for omisego
supply = 140245398 # correct for omisego
K = 150

# Filter out rows where the token amount is greater than the total tokenAmount
omg_edge_df_filtered = omg_edge_df %>% filter(tokenAmount < decimals*supply)
# cat("Num Rows before Filtering: ", nrow(omg_edge_df), "\n") # 1150737
# cat("Num Rows after Filtering: ", nrow(omg_edge_df_filtered), "\n") # 1150726
# cat("Num Rows cut: ", (nrow(omg_edge_df)-nrow(omg_edge_df_filtered)), "\n") # 11

# Set omg_edge_df to the filtered dataframe
omg_edge_df = omg_edge_df %>% filter(tokenAmount <= decimals * supply)

# Convert the timestamp to a date
omg_edge_df$Date = anydate(omg_edge_df$unixTime)

print(head(omg_edge_df, 3))

# number of buys and sells by user id
# Great description here: https://stackoverflow.com/questions/25869378/what-does-n-n-mean-in-r
buys.distribution <- omg_edge_df %>% group_by(toID) %>% summarise(n = n()) %>% ungroup
sells.distribution <- omg_edge_df %>% group_by(fromID) %>% summarise(n = n()) %>% ungroup







# FEATURE ENGINEERING!
# TODO: Group by the day so we only have ~2years of data DONE
# TODO: Add the volume on each day DONE
# TODO: Add the number of transactions each day DONE
# TODO: Add average token amount DONE
# TODO: Unique buyers each day (in top K) DONE


# Filter to only include top K buyers
top_k_buys = buys.distribution %>% arrange(-n) %>% head(K)
edge_df_top_k <- omg_edge_df %>% filter(omg_edge_df$toID %in% top_k_buys$toID)

# Create a dataframe with summarized data for fitting a regression model
fit_data <- edge_df_top_k %>% group_by(Date) %>%
  summarise(
    Avg_Tok_Amt = mean(tokenAmount),
    Tot_Tok_Amt = sum(tokenAmount),
    Transactions = n(),
    Distinct_Buyers = n_distinct(toID),
    Distinct_Sellers = n_distinct(fromID)
  ) %>%
  ungroup

# Join edge data to pricing data based on Date
# We lose a small percentage of the data here due to the fact that the timeframes for the two
# data files do not match perfectly
fit_data <- merge(fit_data, omg_price_df, by="Date")

# Calculate previous days closes (Close_m1 = close minus 1)
fit_data$Close_m1 <- shift(fit_data$Close, n=1)
fit_data$Close_m2 <- shift(fit_data$Close, n=2)
fit_data$Close_m3 <- shift(fit_data$Close, n=3)






# LOOK AT CORRELATIONS

cor(fit_data$Close, fit_data$Transactions)     #  0.3225312
cor(fit_data$Close, fit_data$Tot_Tok_Amt)      # -0.2362847
cor(fit_data$Close, fit_data$Avg_Tok_Amt)      # -0.4194703
cor(fit_data$Close, fit_data$Distinct_Buyers)  #  0.5984479
cor(fit_data$Close, fit_data$Distinct_Sellers) #  0.2065241




# CREATE THE MODEL
print(head(fit_data, 5))

#Fit the data to a multiple linear regression model and print the summary
fit <- lm(
  Close ~ Avg_Tok_Amt +
    Tot_Tok_Amt +
    Transactions +
    Distinct_Buyers +
    Distinct_Sellers ,
    # Close_m1 +
    # Close_m2 +
    # Close_m3,
  data=fit_data)

print(summary(fit))
print(coefficients(fit))

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit) # Prints 4 Plots. Click through them to the right

# # Plot Opening Price over Time
# p = ggplot(aes(x=Open, y=Close), data = fit_data) + geom_point()
# # + geom_smooth(method="lm") # draw linear fit line
# print(p)


cat("OMG\n")
cat("Transactions: ", cor(omg_fit_data$Close, omg_fit_data$Transactions), "\n")
cat("Total Token Amount: ", cor(omg_fit_data$Close, omg_fit_data$Tot_Tok_Amt), "\n")
cat("Average Token Amount: ", cor(omg_fit_data$Close, omg_fit_data$Avg_Tok_Amt), "\n")
cat("Distinct Buyers: ", cor(omg_fit_data$Close, omg_fit_data$Distinct_Buyers), "\n")
cat("Distinct Sellers: ", cor(omg_fit_data$Close, omg_fit_data$Distinct_Sellers), "\n")
cat("Close Minus 1: ", cor(omg_fit_data$Close, omg_fit_data$Close_m1), "\n")
cat("Close Minus 2: ", cor(omg_fit_data$Close, omg_fit_data$Close_m2), "\n")
cat("Close Minus 3: ", cor(omg_fit_data$Close, omg_fit_data$Close_m3), "\n")


cat("\n\nTRX\n")
cat("Transactions: ", cor(trn_fit_data$Close, trn_fit_data$Transactions), "\n")
cat("Total Token Amount: ", cor(trn_fit_data$Close, trn_fit_data$Tot_Tok_Amt), "\n")
cat("Average Token Amount: ", cor(trn_fit_data$Close, trn_fit_data$Avg_Tok_Amt), "\n")
cat("Distinct Buyers: ", cor(trn_fit_data$Close, trn_fit_data$Distinct_Buyers), "\n")
cat("Distinct Sellers: ", cor(trn_fit_data$Close, trn_fit_data$Distinct_Sellers), "\n")
cat("Close Minus 1: ", cor(trn_fit_data$Close, trn_fit_data$Close_m1), "\n")
cat("Close Minus 2: ", cor(trn_fit_data$Close, trn_fit_data$Close_m2), "\n")
cat("Close Minus 3: ", cor(trn_fit_data$Close, trn_fit_data$Close_m3), "\n")

cat("\n\nYOC\n")
cat("Transactions: ", cor(yoc_fit_data$Close, yoc_fit_data$Transactions), "\n")
cat("Total Token Amount: ", cor(yoc_fit_data$Close, yoc_fit_data$Tot_Tok_Amt), "\n")
cat("Average Token Amount: ", cor(yoc_fit_data$Close, yoc_fit_data$Avg_Tok_Amt), "\n")
cat("Distinct Buyers: ", cor(yoc_fit_data$Close, yoc_fit_data$Distinct_Buyers), "\n")
cat("Distinct Sellers: ", cor(yoc_fit_data$Close, yoc_fit_data$Distinct_Sellers), "\n")
cat("Close Minus 1: ", cor(yoc_fit_data$Close, yoc_fit_data$Close_m1), "\n")
cat("Close Minus 2: ", cor(yoc_fit_data$Close, yoc_fit_data$Close_m2), "\n")
cat("Close Minus 3: ", cor(yoc_fit_data$Close, yoc_fit_data$Close_m3), "\n")
