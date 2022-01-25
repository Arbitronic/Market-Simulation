## Synchronized DOUBLE AUCTION
# ZI_Constrained
# Changing volume Orders 

library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# First Step, intitializing 

rm(list = ls())
initializiation <- function(){
  transaction <<- c()
  price_index <<- c()
  profit <<- c()
  profit_index <<- c()
  profit_margin <<- c()
  costs <<- c()
  volume_index <<- c()
  price <<- c()
  total_rounds <<- c()
  keep_money <<- c()
  money_saved_index <<- c()
  payoffs_buyer <<- c()
  redemption_value_index <<- c()
  cost_index <<- c()
  market_capitalization_index_index <<- c()
  value_per_unit_index <<- c()
  accumulated_profit <<- c()
  price <<- c() 
  transaction <<- c()
  asks <<- c()
  bids <<- c()
  q_bid <<- c()
  value_by_bot <<- c()
  bid_ask_spread <<- c()
  asks <<- c()
  bids <<- c()
  sorted_bid_ask_spread <<-c()
  q_bid_accepted <<- c()
  q_ask_accepted <<- c()
  market_capitalization <<- c()
  market_capitalization_index <<- c()
}
initializiation() 




#volume_bid <<- sample(1:4, size = 1, replace = TRUE)
#public_fundamental_value <- sample(0:1000, size = 1, replace = TRUE)

# The following is the "Zero-Intelligence Trader" who makes bids by drawing integers from a uniform distribution

ZI_C_Buyer <- function(){
  upper_limit <- 200
  lower_limit <- 1
  redemption_value_i <<- sample(lower_limit:upper_limit, size = 1, replace = TRUE)
  bid_i <<- sample(lower_limit:redemption_value_i, size = 1, replace = TRUE) # how much for one volume
  volume <<- sample(1:50, size = 1, replace = TRUE)
  if(bid_i*volume <= redemption_value_i){
    q_bid_i <<- volume
    bid_i <<- bid_i
    print(q_bid_i)
    print(bid_i)
    print(redemption_value_i)
  } else {
    ZI_C_Buyer()
}
}
ZI_C_Buyer() 

# In this Order Book bids get collected

ORDER_BOOK_BID <- function(){
  bid_by_bot <<- c()
  value_by_bot <<- c()
  q_bid_by_bot <<- c()
  for(i in 1:6){
    ZI_C_Buyer() 
    bid_by_bot <<- append(bid_by_bot, bid_i)
    value_by_bot <<- append(value_by_bot, redemption_value_i) 
    q_bid_by_bot <<- append(q_bid_by_bot, q_bid_i)
  }
  bidder <<- 1:6
  message_bids <<- c(bid_by_bot, value_by_bot, bidder, q_bid_by_bot)
  message_bid_matrix <<- matrix(message_bids, ncol = 4)
  sorted_m_b_m <<- message_bid_matrix[order(message_bid_matrix[ , 1],decreasing = TRUE),]
  
  bid <<- sorted_m_b_m[1,1]
  redemption_value <<- sorted_m_b_m[1,2]
  bidder <<- sorted_m_b_m[1,3]
  q_bid <<- sorted_m_b_m[1,4]
  
}
ORDER_BOOK_BID()

# The following is the "Zero-Intelligence Trader" who makes asks by drawing integers from a uniform distribution

ZI_C_Seller <- function(){
  upper_limit <- 200
  lower_limit <- 1
  cost_i <<- sample(lower_limit:upper_limit, size = 1, replace = TRUE)
  ask_i <<- sample(cost_i:upper_limit, size = 1, replace = TRUE)
  capacity <<- sample(1:50, size = 1, replace = TRUE)
  q_ask_i <<- sample(1:capacity, size = 1, replace = TRUE)
  print(cost_i)
  print(ask_i)
  print(q_ask_i)
}
ZI_C_Seller() 

# In this Order Book asks get collected

ORDER_BOOK_ASK <- function(){
  ask_by_bot <<- c()
  cost_by_bot <<- c()
  q_ask_by_bot <<- c()
  for(i in 1:6){
    ZI_C_Seller() 
    ask_by_bot <<- append(ask_by_bot, ask_i)
    cost_by_bot <<- append(cost_by_bot, cost_i) 
    q_ask_by_bot <<- append(q_ask_by_bot, q_ask_i)
  }
  seller <<- 1:6
  message_asks <<- c(ask_by_bot, cost_by_bot, seller, q_ask_by_bot)
  message_ask_matrix <<- matrix(message_asks, ncol = 4)
  sorted_m_a_m <<- message_ask_matrix[order(message_ask_matrix[ ,1]),]
  
  ask <<- sorted_m_a_m[1,1]
  cost <<- sorted_m_a_m[1,2]
  seller <<- sorted_m_a_m[1,3]
  q_ask <<- sorted_m_a_m[1,4]
  
  print(ask)
  print(cost)
  print(seller)
  print(q_ask)
}
ORDER_BOOK_ASK()

# The following is the actual market algorithm where bids and asks get matched

Continous_Double_Auction <- function(){
initializiation()
  
  k <- 0
  t <- 0
  
  
  while(t < 100){
    
    ORDER_BOOK_BID() 
    ORDER_BOOK_ASK()
    
    k <- k + 1
    match <- bid >= ask
    spread <- (bid+ask)/2
    q_match <- q_bid <= q_ask # this condition matches bids and asks
    
    if(match){
      asks <<- append(asks, ask)
      bids <<- append(bids, bid)
      price <<- spread
      price_index <<- append(price_index, price) # the prices of a trading session get appended
      t <- t + 1
      
      redemption_value_index <<- append(redemption_value_index, redemption_value)
      cost_index <<- append(cost_index, cost)
      
      realized_payoff_buyer <- (redemption_value - price)
      payoffs_buyer <<- append(payoffs_buyer, realized_payoff_buyer)
      
      profit <- (price - cost)
      profit_index <<- append(profit_index, profit)
      
      #volume_index <<- append(volume_index, q_ask)
      
      bid_ask_spread <<- append(bid_ask_spread, bid - ask)
      if(q_match) {
        volume_index <<- append(volume_index, q_bid)
        q_bid_accepted <<- append(q_bid_accepted, volume_index)
        
      } else {
        volume_index <<- append(volume_index, q_ask)
        q_ask_accepted <<- append(q_ask_accepted, q_ask) 
      }
      market_capitalization <<- tail(price_index, 1) * tail(volume_index, 1)
      market_capitalization_index <<- append(market_capitalization_index, market_capitalization) 
      
    } else {
      
    }
    # Protocol
    transaction <<- 1:t
    rounds <<- k

  }

}
Continous_Double_Auction()


# This function calculates allocative efficiency and other market indicators.
Calculations <- function(){
  sorted_profits <<- sort(profit_index)
  sorted_volume <<- sort(volume_index)
  sorted_costs <<- sort(cost_index)
  sorted_redemption_values <<- sort(redemption_value_index, decreasing = TRUE)
  accumulated_profit <<- cumsum(profit_index)
  
  sorted_bids <<- sort(bids, decreasing = TRUE)
  sorted_asks <<- sort(asks)
  sorted_bid_ask_spread <<- sort(bid_ask_spread)
  sorted_market_cap <<- sort(market_capitalization_index)
  exchange_index <<- cumsum(market_capitalization_index)
  
  y <- any(sorted_redemption_values <= sorted_costs)
  
  # Realized Profits:
  payoffs <<- (payoffs_buyer) + (profit_index)
  total_surplus <<- round(sum(payoffs)) 
  total_surplus
  
  if(y == TRUE){
    for(i in transaction){
      if(sorted_redemption_values[i] <= sorted_costs[i]){
        eq_c <<-  i 
        equilibrium_price <<- (sorted_redemption_values[i-1] + sorted_costs[i])/2
        break
      } else {
      }
    }
    # Theoretical Profits:
    theoretical_profits_buyers <<- (sorted_redemption_values[1:eq_c] - equilibrium_price)
    theoretical_profits_sellers <<- (equilibrium_price - sorted_costs[1:eq_c])
    
    theoretical_surplus <<- sum(theoretical_profits_buyers) + sum(theoretical_profits_sellers)
    theoretical_surplus # aka Marshallian Path
    
    # Realized Profits:
    payoffs <<- (payoffs_buyer) + (profit_index)
    total_surplus <<- round(sum(payoffs)) 
    total_surplus
    
    # Distribution of Profits
    share_of_seller <<- sum(profit_index)/total_surplus
    
    # Ex-Post Efficiency:
    allocation_efficiency <<- total_surplus / theoretical_surplus
    #  trade_ratio <<- max(transaction)/total_rounds   
    
    # Market Performance:
    print(paste("Price", price))
    print(paste("capacity", capacity))
    print(paste("profit margin", profit_margin))
    print(paste("equilibrium transaction:", eq_c))
    print(paste("equilibrium price:", equilibrium_price))
    print(paste("Theoretical Surplus:", theoretical_surplus))
    print(paste("Total Surplus", total_surplus))
    print(paste("Total Profit by Seller", sum(profit_index)))
    print(paste("Total Quantities Moved", sum(sorted_volume)))
    print(paste("Total market_capitalization", sum(market_capitalization_index)))
    print(paste("Allocation efficiency:", allocation_efficiency))
    print(paste("share of seller", share_of_seller))  
    print(paste("Total market_capitalization", sum(market_capitalization_index)))
    
    
  } else {
    print("no allocative efficiency can be calculated")
    print("there is no equilibrium price")
    print("reason: no crossing of theoretical profits")
    print(paste("Price", price))
    print(paste("capacity", capacity))
    print(paste("profit margin", profit_margin))
    print(paste("Total Surplus", total_surplus))
    print(paste("Total Profit by Seller", sum(profit_index)))
    print(paste("Total Quantities Moved", sum(sorted_volume)))
    print(paste("Total market_capitalization", sum(market_capitalization_index)))
    
  }
  Profit_index <<- data.frame(profit_index, price_index, volume_index,accumulated_profit, transaction)
  Market_Report <<- data.frame(price_index, transaction, sorted_bids, sorted_asks, sorted_redemption_values, sorted_costs, bids, asks, bid_ask_spread, sorted_bid_ask_spread)
  Exchange_Report <<- data.frame(transaction, sorted_market_cap, exchange_index, market_capitalization_index)
  
  Price_Plot <<-ggplot(data = Profit_index, aes(x=transaction)) +  geom_line(aes(y=price_index))
  Profit_Plot <<- ggplot(data = Profit_index, aes(x=transaction)) +  geom_line(aes(y=profit_index))
  Order_Plot <- ggplot(data=Market_Report, aes(x = transaction)) + geom_line(aes(y=sorted_bids)) + geom_line(aes(y=sorted_asks))
  Bid_Ask_Plot <- ggplot(data=Market_Report, aes(x = transaction)) + geom_line(aes(y= sorted_bid_ask_spread))+ geom_line(aes(y= bid_ask_spread))
  Exchange_Plot <<- ggplot(data=Exchange_Report, aes(x=transaction)) + geom_line(aes(y=sorted_market_cap))+ geom_line(aes(y=market_capitalization_index))
  
  Volume_Plot <<- ggplot(data = Profit_index, aes(x=transaction)) +  geom_line(aes(y=volume_index))
  Break_even <<-ggplot(data = Profit_index, aes(x=transaction)) +  geom_line(aes(y=accumulated_profit))
  grid.arrange(Price_Plot, Profit_Plot, Volume_Plot,  Break_even, nrow=4)
  
  Payoffs <<- data.frame(transaction, sorted_redemption_values, sorted_costs)
  Marshallian_Path_Plot <- ggplot(data=Payoffs, aes(x = transaction)) + geom_line(aes(y=sorted_redemption_values)) + geom_line(aes(y=sorted_costs)) 
  Marshallian_Path_Plot + labs(y="Value or Cost", x = "Transaction")
  
  grid.arrange(Marshallian_Path_Plot,Order_Plot, Price_Plot, Volume_Plot, Bid_Ask_Plot, Exchange_Plot, ncol = 2, nrow=3)
  
  
}
Calculations()






