# CS6313: Statistial Methods for Data Science -S19
# Pat Dayton and AJ Rahendran

# install.packages("plyr")
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("dply")
# install.packages("fitdist")
library(plyr)
library(readr)
library(ggplot2)
library(dplyr)
library(fitdistrplus)

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

# Calculate Total Traded Valume (1120702)
total_trade_volume = sum(transactions_by_pair_df$n)

# Optionally Drop out the outlier pair(311608 - 311608), n(30024)
# Comment this line out if you want to leave it in
transactions_by_pair_df = transactions_by_pair_df %>% filter(n < 30000)
cat('FILTERING!\n')

pairdf = transactions_by_pair_df %>% head(100)
print(sum(pairdf$n))

pairdf$row_id <- as.numeric(row.names(pairdf))

pairdf$n_scaled <- (pairdf$n - min(pairdf$n) + 0.001) / (max(pairdf$n) - min(pairdf$n) + 0.002)

# Reverse it so it goes up and right
# pairdf$n_scaled <- rev(pairdf$n_scaled)

print(pairdf$n_scaled)
# normalize by total_trade_volume
pairdf$n_norm = pairdf$n / total_trade_volume


print(pairdf$n_scaled)
fit.lnorm.pairdf = fitdistr(pairdf$n_scaled, densfun='lognormal')
fit.exp.pairdf = fitdistr(pairdf$n_scaled, densfun='exponential')
print(fit.exp.pairdf)

# fit.beta.pairdf = fitdistr(pairdf$n_scaled, densfun='beta', start = list(shape1 = .5, shape2 = .5))
print(fit.lnorm.pairdf)
# print(fit.beta.pairdf)
# fit.pois.pairdf = fitdist(pairdf$n, 'pois')
# fit.weibull.pairdf <- fitdist(pairdf$n, 'weibull')

print(fit.exp.pairdf$estimate[1])

# gofstat(fit.lnorm.pairdf)

# gofstat(list(fit.log.pairdf, fit.lnorm.pairdf))

as.data.frame(pairdf)
keeps <- c("", "a")

clean_data <- as.data.frame(pairdf)[row_id]

# Create a bar chart to plot the top 50 transaction address pairs
pair_bar = ggplot(as.data.frame(pairdf)) +
  # scale_y_continuous(trans = 'log10') +
  # ylim(0, .0035) +
  
  # geom_histogram(mapping = aes(x=row_id, y=n_scaled), stat = "density", fill="steelblue") +
  
  geom_histogram(stat = "density", aes(x=row_id, y=n_scaled))  +
  stat_function( fun = "dlnorm",
                args = list(meanlog = -4, sdlog = 1),
                n = 501,
                color = "red") +
  # stat_function(fun = dlnorm,
  #               args = list(meanlog = fit.lnorm.pairdf$estimate[1],
  #                           sdlog = fit.lnorm.pairdf$estimate[2]),
  #               size=1,
  #               color = "red") +
  
  
  # stat_function(fun = dexp,
  #               aes(row_id),
  #               color = "green") +
  # stat_function(fun = dexp, list(rate = fit.exp.pairdf$estimate[1]), colour = "green") +
  # stat_function(fun = dlnorm, args = list(meanlog = -3.36, sdlog =10.94, log = FALSE),  size=1, color = "green") +
  # stat_function(fun = dexp, args = list(rate = 10),  size=1, color = "green")+
  theme_minimal()

# Can do this later...
# list(mean = fit$estimate[1], sd = fit$estimate[2])) 

print(pair_bar)


set.seed(42)

dat <- data.frame(x = rlnorm(1000, meanlog = -4, sdlog = 1))
print(class(dat))
ggplot(dat) +
  geom_histogram(mapping = aes(x = x), stat = "density")  +
  stat_function(fun = "dlnorm",
                args = list(meanlog = -4, sdlog = 1),
                n = 501,
                color = "red")


