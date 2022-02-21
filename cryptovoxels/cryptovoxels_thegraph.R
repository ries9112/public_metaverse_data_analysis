# Cryptovoxels pull data using The Graph
library(ghql)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(httr)

# Pull parcels info -------------------------------------------------------

# connect to the endpoint
con = GraphqlClient$new(
  url = "https://api.thegraph.com/subgraphs/name/protofire/cryptovoxels"
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
  parcels(where:{createdAt_gt: ',min_created_at,'}, orderBy: createdAt, orderDirection: asc, first:1000) {
    id
    createdAt
    tokenID
    location
    buildHeight
    area
    length
    width
    volumeInVoxels
    owner {
      id
    }
    tokenURI
  }
}
'))
  
  if(round==1){ # on first round build data, subsequent rounds append temp to data
    # Run query (pull data)
    data = con$exec(graphql_request$queries$mydata)
    # convert results to JSON
    data = fromJSON(data)
    # extract result
    data = data$data$parcels
    # extract x and y coordinates
    data$owner = data$owner$id
    # remove owner column/table from dataset
    data = select(data, -owner)
  } else{
    # Run query (pull data)
    temp = con$exec(graphql_request$queries$mydata)
    # convert results to JSON
    temp = fromJSON(temp)
    # extract result
    temp = temp$data$parcels
    if(is.data.frame(temp) == TRUE){
      # extract x and y coordinates
      temp$owner = temp$owner$id
      # remove owner column/table from dataset
      temp = select(temp, -owner)
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
# convert to tibble
data = as_tibble(data)
# convert createdAt to datetime
data = mutate(data,
              createdAt = as.POSIXct(as.numeric(data$createdAt), origin="1970-01-01"))
# parse location string into coordinates
data = data %>% mutate(x = gsub(",.*$", "", location),
                       y = gsub(".*\\,", "", location))
# Now check whether W,E,N,S and make numbers positive or negative based on the result
data = data %>% mutate(x = case_when(str_detect(x, 'W') ~ -as.numeric(gsub("([0-9]+).*$", "\\1", x)),
                                     str_detect(x, 'E') ~ as.numeric(gsub("([0-9]+).*$", "\\1", x))),
                       y = case_when(str_detect(y, 'S') ~ -as.numeric(gsub("([0-9]+).*$", "\\1", y)),
                                     str_detect(y, 'N') ~ as.numeric(gsub("([0-9]+).*$", "\\1", y))))
# calculate volume
data = data %>% mutate(volume = area*buildHeight)


# Pull orders info --------------------------------------------------------

# connect to the endpoint
con = GraphqlClient$new(
  url = "https://api.thegraph.com/subgraphs/name/benjythebee/cryptovoxels-parcels"
)

temp = data.frame()
min_created_at = 0
round = 1

while(is.data.frame(temp) == TRUE){ # when out of results temp will be a list not orders frame
  
  print(round)
  
  # initialize a new query
  graphql_request = Query$new()
  
  # Define query
  graphql_request$query('mydata', paste0('{
    saleEvents(first: 1000, orderBy: date, orderDirection: asc, where:{parcel_gte: "0", date_gt: ',min_created_at,'}){
      price
      date
      saleKind
      parcel{
        id
      }
      taker{
        id
      }
      maker{
        id
      }
      sellOrder{
        paymentToken{
          id
          symbol
        }
    }
    }
  }
'))
  
  if(round==1){ # on first round build orders, subsequent rounds append temp to orders
    # Run query (pull orders)
    orders = con$exec(graphql_request$queries$mydata)
    # convert results to JSON
    orders = fromJSON(orders)
    # extract result
    orders = orders$data$saleEvents
    # extract values
    orders$parcel_id = orders$parcel$id
    orders$purchaser = orders$taker$id
    orders$seller = orders$maker$id
    orders$payment_token = orders$sellOrder$paymentToken$symbol
    # remove nested columns
    orders = select(orders, -parcel, -taker, -maker, -sellOrder)
  } else{
    # Run query (pull orders)
    temp = con$exec(graphql_request$queries$mydata)
    # convert results to JSON
    temp = fromJSON(temp)
    # extract result
    temp = temp$data$saleEvents
    if(is.data.frame(temp) == TRUE){
      # extract values
      temp$parcel_id = temp$parcel$id
      temp$purchaser = temp$taker$id
      temp$seller = temp$maker$id
      temp$payment_token = temp$sellOrder$paymentToken$symbol
      # remove nested columns
      temp = select(temp, -parcel, -taker, -maker, -sellOrder)
      # rbind temp to orders
      orders = rbind(orders, temp)
    }    
  }
  # increment round
  round = round + 1
  # print number of rows in orders
  print(nrow(orders))
  # overwrite date
  min_created_at = max(orders$date, na.rm=T)
}
# Adjust price column
orders = mutate(orders, price_ETH = as.numeric(price)/10^18)
orders = select(orders, -price)
# convert to tibble
orders = as_tibble(orders)
# convert createdAt to datetime
orders = mutate(orders,
                date = as.POSIXct(as.numeric(orders$date), origin="1970-01-01"))
# exclude those with payment token in DAI
orders = filter(orders, payment_token != 'DAI')
# NOTE: saleKind of 1 is primary sale, 0 is secondary sale!


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
  tokenDayDatas(where:{token:"0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2"}, orderBy: date, orderDirection: desc, first: 1000){
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



# Join all datasets -------------------------------------------------------

# FIRST FULL PARCEL DATA (KEEPING THOSE THAT NEVER SOLD)

# first join sales to parcel info
full_parcel_data = left_join(data, orders, by=c('tokenID'='parcel_id'))
# modify date before next join
full_parcel_data = full_parcel_data %>% mutate(date = as.Date(date))
# Join prices to dataset
full_parcel_data = left_join(full_parcel_data, prices, by=c('date'='date'))
# calculate sale price in USD
full_parcel_data = mutate(full_parcel_data, price_USD = as.numeric(price_ETH)*as.numeric(priceUSD))
# remove old priceUSD column
full_parcel_data = select(full_parcel_data, -priceUSD)
# rename date to sale date for clarity
full_parcel_data = rename(full_parcel_data, saleDate = 'date')

# NOW FULL ORDERS DATA (KEEPING THOSE THAT SOLD MORE THAN ONCE)
full_orders_data = left_join(orders, data, by=c('parcel_id' = 'tokenID'))
# modify date before next join
full_orders_data = full_orders_data %>% mutate(date = as.Date(date))
# Join prices to dataset
full_orders_data = left_join(full_orders_data, prices, by=c('date'='date'))
# calculate sale price in USD
full_orders_data = mutate(full_orders_data, price_USD = as.numeric(price_ETH)*as.numeric(priceUSD))
# remove old priceUSD column
full_orders_data = select(full_orders_data, -priceUSD)
# rename date to sale date for clarity
full_orders_data = rename(full_orders_data, saleDate = 'date')
# Only keep those where we have a valid price_USD (uniswap data only goes back so far, and really old data not of interest anyway)
full_orders_data = filter(full_orders_data, is.na(price_USD)==F)
# remove nested column
full_orders_data = select(full_orders_data, -token)

# calculate price per volume
full_orders_data = full_orders_data %>% mutate(price_per_volume_USD = price_USD/volume)
full_orders_data = full_orders_data %>% mutate(price_per_volume_ETH = price_ETH/volume)

# show map
# full_orders_data %>% ggplot(aes(x, y, color=price_USD)) + geom_point() 
# example viz
# full_orders_data %>% group_by(saleDate) %>% summarize(avg_price_USD = mean(price_USD, na.rm=T)) %>% filter(is.na(avg_price_USD)==F, avg_price_USD != 0) %>% ggplot(aes(saleDate, avg_price_USD)) + geom_point() + scale_y_continuous(labels=scales::dollar_format())
# 
# # Example finding those that sold most underprice
# full_orders_data %>% arrange(price_per_volume)  %>% filter(price_USD > 300, saleDate > '2021-09-01')
# # visualize underpriced
# full_orders_data %>% arrange(price_per_volume)  %>% filter(price_USD > 300, saleDate > '2021-09-01') %>% ggplot(aes(saleDate, price_per_volume, color=as.character(saleKind))) + geom_point() + scale_y_continuous(labels=scales::dollar_format())
# # ggplotly version
# ggplotly(full_orders_data %>% arrange(price_per_volume)  %>% filter(price_USD > 300, saleDate > '2021-09-01') %>% ggplot(aes(saleDate, price_per_volume, color=as.character(saleKind))) + geom_point() + scale_y_continuous(labels=scales::dollar_format()) + geom_smooth())
# 
# # Show the ones I purchased
# full_orders_data %>% arrange(price_per_volume) %>% filter(purchaser=='0x8115afd8dffce5579381ad27524b6feeae917bef') %>% ggplot(aes(saleDate, price_per_volume, color=as.character(saleKind))) + geom_point() + scale_y_continuous(labels=scales::dollar_format())
# full_orders_data %>% arrange(price_per_volume) %>% filter(purchaser=='0x74dbb201ecc0b16934e68377bc13013883d9417b') %>% ggplot(aes(saleDate, price_per_volume, color=as.character(saleKind))) + geom_point() + scale_y_continuous(labels=scales::dollar_format())
# 


# pull parcels info (add missing info like island) ------------------------

url <- 'https://www.cryptovoxels.com/api/parcels.json'
# Get the data from the url
parcels_info <- content(GET(url))

# initialize data
parcels_info_data = data.frame(id = parcels_info$parcels[[1]]$id,
                               address = parcels_info$parcels[[1]]$address,
                               suburb = parcels_info$parcels[[1]]$suburb,
                               island = parcels_info$parcels[[1]]$island,
                               distance = parcels_info$parcels[[1]]$distance,
                               floor_height = parcels_info$parcels[[1]]$y1)
# iterate over options
for (i in 2:length(parcels_info$parcels)){
  temp = data.frame(id = parcels_info$parcels[[i]]$id,
                    address = parcels_info$parcels[[i]]$address,
                    suburb = parcels_info$parcels[[i]]$suburb,
                    island = parcels_info$parcels[[i]]$island,
                    distance = parcels_info$parcels[[i]]$distance,
                    floor_height = parcels_info$parcels[[i]]$y1)
  # union to main data
  parcels_info_data = rbind(parcels_info_data, temp)
}
# Arrange by id
parcels_info_data = arrange(parcels_info_data, id)
# make id into character for join to have same data type
parcels_info_data = mutate(parcels_info_data, id = as.character(id))

# Join to datasets
full_parcel_data = left_join(full_parcel_data, parcels_info_data, by=c('tokenID'='id'))
full_orders_data = left_join(full_orders_data, parcels_info_data, by=c('parcel_id'='id'))

# Apply function to calculate average price of a given area
area_avg_price_usd_fun = function(x_min, x_max, y_min, y_max){
  mean(filter(full_orders_data, x >= x_min, x <= x_max, y >= y_min, y <= y_max)$price_USD, na.rm=T)
}
# Define range
full_orders_data = mutate(full_orders_data,
                          x_range_min = x-100, x_range_max = x+100,
                          y_range_min = y-100, y_range_max = y+100)
# Now use function
full_orders_data = mutate(full_orders_data, area_avg_price_USD_100 = as.numeric(unlist(pmap(list(x_range_min, x_range_max,
                                                                                                 y_range_min, y_range_max), area_avg_price_usd_fun))))
# Define new range
full_orders_data = mutate(full_orders_data,
                          x_range_min = x-50, x_range_max = x+50,
                          y_range_min = y-50, y_range_max = y+50)
# Now use function
full_orders_data = mutate(full_orders_data, area_avg_price_USD_50 = as.numeric(unlist(pmap(list(x_range_min, x_range_max,
                                                                                                y_range_min, y_range_max), area_avg_price_usd_fun))))
# Do the same with ETH
area_avg_price_eth_fun = function(x_min, x_max, y_min, y_max){
  mean(filter(full_orders_data, x >= x_min, x <= x_max, y >= y_min, y <= y_max)$price_ETH, na.rm=T)
}
# Define range
full_orders_data = mutate(full_orders_data,
                          x_range_min = x-100, x_range_max = x+100,
                          y_range_min = y-100, y_range_max = y+100)
# Now use function
full_orders_data = mutate(full_orders_data, area_avg_price_ETH_100 = as.numeric(unlist(pmap(list(x_range_min, x_range_max,
                                                                                                 y_range_min, y_range_max), area_avg_price_eth_fun))))
# Define new range
full_orders_data = mutate(full_orders_data,
                          x_range_min = x-50, x_range_max = x+50,
                          y_range_min = y-50, y_range_max = y+50)
# Now use function
full_orders_data = mutate(full_orders_data, area_avg_price_ETH_50 = as.numeric(unlist(pmap(list(x_range_min, x_range_max,
                                                                                                y_range_min, y_range_max), area_avg_price_eth_fun))))

# remove ranges
full_orders_data = select(full_orders_data, -x_range_min, -x_range_max, -y_range_min, -y_range_max)

# Only keep relevant data for full_parcel_data (no sales info)
full_parcel_data = dplyr::select(full_parcel_data, tokenID, createdAt, address, suburb, island, location, x, y, tokenURI, length, width, area, buildHeight, volumeInVoxels, volume, floor_height)

# write data 
readr::write_csv(full_orders_data, 'cryptovoxels/data/cryptovoxels_orders.csv')
readr::write_csv(full_parcel_data, 'cryptovoxels/data/cryptovoxels_all_parcels.csv')
