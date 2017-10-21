install.packages("anytime")
require("XML")
require("lubridate")
require("anytime")

# Program 1

loadData <- function(){
  # Load the XML into data frame
  # Args:
  #  
  # Return: 
  #   returning the data frame containing ebay auction data
  if(!exists("ebayBid")){
    url <- "http://www.cs.washington.edu/research/xmldatasets/data/auctions/ebay.xml"
    download.file(url, destfile = "ebay.xml")
    ebayObj <- xmlTreeParse("ebay.xml")
    xmlRoot(ebayObj)
    
  }else{
    ebayBid
  }
  
}

ebayBid <- loadData()

moreFiveBids <- function(){
  # Compute the number of auctions having more than 5 bids
  # Args:
  #  
  # Return: 
  #   returning the count of auctions having bid >5
  count <- 0
  for(i in 1:xmlSize(ebayBid)){
    if(as.numeric(xmlValue(ebayBid[[i]][[5]][[5]])) > 5){
      count <- count + 1
    }
  }
  return(count)
}

moreFiveBids()


# Program 2 

loadFutureTradeData <- function(){
  # Load the XML into data frame
  # Args:
  #  
  # Return: 
  #   returning the data frame containing Future Trades data
  if(!exists("FututreTradeData")){
    url <- "http://www.barchartmarketdata.com/data-samples/getHistory15.xml"
    download.file(url, destfile = "futureTrades.xml")
    FutureTradeObj <<- xmlTreeParse("futureTrades.xml", useInternalNodes=TRUE)
    xmlRoot(FutureTradeObj)
    
  }else{
    FututreTradeData
  }
}

FututreTradeData <- loadFutureTradeData()
FutureTradeData

# Program 2.a
# Assigned a random number to max object
# Iterate through each security
# Compare each value of the closing price of the security with the max
# if the price is greater than max then assigning that value to the max

highestClosingPrice <- function(){
  # compute the highest closing price for the security
  # Args:
  #  
  # Return: 
  #   returning the highest closing price
  max <- -9999;
  for(i in 2:xmlSize(FututreTradeData)){
    list <- as.numeric(xmlValue(FututreTradeData[[i]][[7]]))
    if(list > max){
      max <- list
    }
  }
  return(max)
}

highestClosingPrice()

# Program 2.b



totalVolume <- function(){
  # compute the total volume traded for the security
  # Args:
  #  
  # Return: 
  #   returning the total volume
  
  # Using xpathApply to access the xml nodes values of Volume node in a vector
  # since returned xmlValue is character therefore converting it to numeric using 
  # as.numeric function to perform arithmetic operations on it.
  
  volumeList <- as.numeric(xpathApply(FutureTradeObj, "//volume", xmlValue))
  total <- sum(volumeList, is.na = FALSE)
  return(total)
  
}

totalVolume()

# Program 2.c



averageVolume <- function(){
  # compute the average volume during each hour of the trading day.
  # Args:
  #  
  # Return: 
  #   returning the data frame containing each hour and avg vol per each hour
  
  # extract the timestamp node from the xml
  timestampList <- as.character(xpathApply(FutureTradeObj, "//timestamp", xmlValue))
  timestampList
  # use anytime package to convert the the datetime to the standard format
  timeList <- ymd_hms(timestampList)
  #timeList <- format(anytime(timestampList), "%Y-%m-%d %H:%M:%S")
  
  #timeList <- as.POSIXct(timeList)
  
  # abstarcting hour from the timestamp.
  eachHourList <- hour(timeList)
  
  volumeList <- as.numeric(xpathApply(FutureTradeObj, "//volume", xmlValue))
  
  DF <- data.frame(hour = eachHourList, volume = volumeList)
  hourDataFrame <- aggregate(volume ~ hour,DF,mean)
 
  
  #hourDataFrame <- data.frame(hour= 0:23,AvgVolume =NA)
  # I took first i loop to iterate over time from 0:23
  # I took second loop j through each element of the vector containing volume
  # i compare the each extracted hour of the vector to the i 
  # and adding the volumne until the i == j
  # finding the average
  
  
  return(hourDataFrame)
  
}

averageVolume()
