# Decentraland pull data using The Graph
library(ghql)
library(jsonlite)
library(tidyverse)
library(lubridate)

# connect to the endpoint
con = GraphqlClient$new(
  url = "https://gateway.thegraph.com/api/df67e6ae9e9528722631488a6731198f/subgraphs/id/0x89fddab2f93417182cdcdb0b8b3322b93ab3a192-0"
)

temp = data.frame()
min_created_at = 0
round = 1

while(is.data.frame(temp) == TRUE){ # when out of results temp will be a list not data frame
  
  print(round)
  
  # initialize a new query
  graphql_request = Query$new()
  
  # Define query
  graphql_request$query('mydata', paste0('{
  orders(where:{status:sold, category: parcel, createdAt_gt: ',min_created_at,'}, orderBy: createdAt, orderDirection: asc, first:1000){
    id
    txHash
    price
    owner
    buyer
    status
    tokenId
    category
    createdAt
    nft{
      searchParcelX
      searchParcelY
    }
  }
}
'))
  
  if(round==1){ # on first round build data, subsequent rounds append temp to data
    # Run query (pull data)
    data = con$exec(graphql_request$queries$mydata)
    # convert results to JSON
    data = fromJSON(data)
    # extract result
    data = data$data$orders
    # extract x and y coordinates
    data$x = data$nft$searchParcelX
    data$y = data$nft$searchParcelY
    # remove nft column/table from dataset
    data = select(data, -nft)
  } else{
    # Run query (pull data)
    temp = con$exec(graphql_request$queries$mydata)
    # convert results to JSON
    temp = fromJSON(temp)
    # extract result
    temp = temp$data$orders
    if(is.data.frame(temp) == TRUE){
      # extract x and y coordinates
      temp$x = temp$nft$searchParcelX
      temp$y = temp$nft$searchParcelY
      # remove nft column/table from dataset
      temp = select(temp, -nft)
      # rbind temp to data
      data = rbind(data, temp)
    }    
  }
  # increment round
  round = round + 1
  # print number of rows in data
  print(nrow(data))
  # overwrite createdAt
  min_created_at = max(data$createdAt)
}
# Adjust price column
data = mutate(data, price_MANA = as.numeric(price)/10^18)
# convert to tibble
data = as_tibble(data)
# convert createdAt to datetime
data = mutate(data,
              createdAt = as.POSIXct(as.numeric(data$createdAt), origin="1970-01-01"))


# Convert MANA prices to USD ----------------------------------------------
# Source pricing data from Uniswap
# connect to the endpoint
con = GraphqlClient$new(
  url = "https://api.thegraph.com/subgraphs/name/uniswap/uniswap-v2"
)

# initialize a new query
graphql_request = Query$new()

# Define query
graphql_request$query('mydata', '{
  tokenDayDatas(where:{token:"0x0f5d2fb29fb7d3cfee444a200298f468908cc942"}, orderBy: date, orderDirection: desc, first: 1000){
    token{
      symbol
    }
    priceUSD
    date
  }
}
')
# Run query (pull data)
prices = con$exec(graphql_request$queries$mydata)
# convert results to JSON
prices = fromJSON(prices)
# extract result
prices = prices$data$tokenDayDatas
# Convert timestamps to dates
prices = mutate(prices, date = as.Date(as_datetime(date)))

# Extract date value out of MANA sales
data = mutate(data, date = as.Date(createdAt))

# Join prices to dataset
data = left_join(data, prices)
# calculate sale price in USD
data = mutate(data, price_USD = as.numeric(price_MANA)*as.numeric(priceUSD))

# arrange columns
# data = select(data, category, createdAt, x, y, price_MANA, status, owner, buyer, tokenId, txHash)
# arrange from newest to oldest
data = arrange(data, desc(createdAt))
# remove nested column
data = select(data, -token)

# Apply function to calculate average price of a given area
area_avg_price_usd_fun = function(x_min, x_max, y_min, y_max){
  mean(filter(data, x >= x_min, x <= x_max, y >= y_min, y <= y_max)$price_USD, na.rm=T)
}
# convert x and y to numeric
data = mutate(data, x=as.numeric(x), y=as.numeric(y))
# Define range
data = mutate(data,x_range_min = x-20, x_range_max = x+20,
              y_range_min = y-20, y_range_max = y+20)
# Now use function
data = mutate(data, area_avg_price_USD_20 = as.numeric(unlist(pmap(list(x_range_min, x_range_max,
                                                                        y_range_min, y_range_max), area_avg_price_usd_fun))))

# Define range
data = mutate(data,
              x_range_min = x-50, x_range_max = x+50,
              y_range_min = y-50, y_range_max = y+50)
# Now use function
data = mutate(data, area_avg_price_USD_50 = as.numeric(unlist(pmap(list(x_range_min, x_range_max,
                                                                        y_range_min, y_range_max), area_avg_price_usd_fun))))

# Define range
data = mutate(data,
              x_range_min = x-100, x_range_max = x+100,
              y_range_min = y-100, y_range_max = y+100)
# Now use function
data = mutate(data, area_avg_price_USD_100 = as.numeric(unlist(pmap(list(x_range_min, x_range_max,
                                                                         y_range_min, y_range_max), area_avg_price_usd_fun))))


# remove ranges
data = select(data, -x_range_min, -x_range_max, -y_range_min, -y_range_max)
# remove old price columns
data = select(data, -price, -priceUSD)
# rename createdAt
data = rename(data, purchasedAt = 'createdAt')


# write data 
readr::write_csv(data, 'decentraland/data/decentraland_thegraph.csv')

