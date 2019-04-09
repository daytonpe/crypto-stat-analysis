# CS6313: Statistial Methods for Data Science -S19
# Pat Dayton and AJ Rahendran

# install.packages("dplyr")
library(plyr)
library(readr)
library(ggplot2)
library(dplyr)

# The modulo of the sum of our UTD IDs was 2, so we will be using tronix, omisego, and yocoin

# Load in the Etherium Tokens Information
df <- read.table("./tokenPrices/OMISEGO",
                 col.names = c('Date',	'Open',	'High',	'Low',	'Close',	'Volume',	'MarketCap'),
                 skip = 1,
                 header = FALSE)

# Check for duplicated
cat("Number of duplicates: ", anyDuplicated(df), "\n")

# Convert date to the correct format
df$Date = as.Date(df$Date,format='%m/%d/%Y')


# Plot Opening Price over Time
p = ggplot(aes(x=Date, y=Open), data = df) + geom_point()
# print(p)

omisego <- read_delim('./edgeFiles/omisego.txt', delim = " ", col_names = F)

names(omisego) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')
# Example:
# 142341 75994 1524611536 5301102205520000000000

decimals <- 10^18 # correct for omisego
supply <- 140245398 # correct for omisego

omisegoFiltered <- omisego %>% filter(tokenAmount < decimals * supply)

## Find number of transactions with > 10^18 tokens
## This is negligable, so move on.
omisego %>% filter(tokenAmount >= decimals * supply) %>% nrow()

## number of buys by user id
buys.distribution <- omisegoFiltered %>% group_by(toID) %>% summarise(n = n()) %>% ungroup

## show highest 10 buyers and their number of buys 
print(buys.distribution %>% arrange(-n) %>% head(10))

## number of sells by user id
sells.distribution <- omisegoFiltered %>% group_by(fromID) %>% summarise(n = n()) %>% ungroup

## show highest 10 buyers and their number of buys 
selldf <- sells.distribution %>% arrange(-n) %>% head(10)

print(selldf)
p<-ggplot(data=selldf, aes(x=0, y=1)) +
  geom_bar(stat="identity")
print(p)

