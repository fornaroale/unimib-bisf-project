

dailyData <- function(stock, startDate, endDate) {
  data <- get.hist.quote(
    instrument = stock,
    start = startDate,
    end = endDate,
    quote = "AdjClose",
    provider = "yahoo",
    origin = "1970-01-01",
    retclass = "zoo"
  )
  return(data)
}

monthlyData <- function(stock, startDate, endDate) {
  data <- get.hist.quote(
    instrument = stock,
    start = startDate,
    end = endDate,
    quote = "AdjClose",
    provider = "yahoo",
    origin = "1970-01-01",
    compression = "monthly",
    retclass = "zoo"
  )
  return(data)
}

getCCReturnsXTS <- function(stock, startDate, endDate) {
  DATA.xts <- getSymbols(
    stock,
    from = startDate,
    to = endDate,
    src = 'yahoo',
    auto.assign = FALSE
  )
  DATA.xts.monthly <- to.monthly(DATA.xts)
  DATA.AdjClose <- DATA.xts.monthly$DATA.xts.Adjusted
  DATA.xts.CC <-
    na.omit(CalculateReturns(DATA.AdjClose, method = "compound"))
  
  return(DATA.xts.CC)
}

dailyToMonthly <- function(dailyData.z) {
  index(dailyData.z) <- as.yearmon(index(dailyData.z))
  dailyData.z <- aggregate(dailyData.z, index(dailyData.z), tail, 1)
  index(dailyData.z) <- as.yearmon(index(dailyData.z))
  return(dailyData.z)
}

calculateSimpleReturns <- function(stockData) {
  SimpleReturns <- na.omit((diff(stockData) / lag(stockData, k = -1)))
  return(SimpleReturns)
}

calculateCCReturns <- function(stockData) {
  CCReturns <- na.omit(diff(log(stockData)))
  return(CCReturns)
}

calculateRelativeSD <- function(data) {
  data.sd <- sd(data)
  data.sdrel <- data.sd / mean(data)
  return(data.sdrel)
}

univariateStats <- function(data) {
  message(sprintf("Media: %s\n", mean(data)))
  message(sprintf("Varianza: %s\n", var(data)))
  message(sprintf("Dev. Std.: %s\n", sd(data)))
  message(sprintf("Asimmetria: %s\n", skewness(data)))
  message(sprintf("Curtosi: %s\n", kurtosis(data)))
  message(sprintf("Sommario: %s\n", summary(data)))
  message(sprintf("Quantile: %s\n", quantile(data)))
}

computeThreeMonthsCor <- function(data) {
  rowNames <- c()
  corMatrix = matrix(NA, nrow = nrow(data) / 3, ncol = 6)
  
  corIterator <- 1
  rowIterator <- 1
  
  while (corIterator < nrow(data)) {
    corIteratorEnd <- corIterator + 2
    periodValues <- (data[corIterator:corIteratorEnd, ])
    periodCor <- cor(periodValues)
    
    corAAPLMSFT <- periodCor["Apple", "Microsoft"]
    corAAPLAMZN <- periodCor["Apple", "Amazon"]
    corAAPLGOOG <- periodCor["Apple", "Google"]
    corMSFTAMZN <- periodCor["Microsoft", "Amazon"]
    corMSFTGOOG <- periodCor["Microsoft", "Google"]
    corAMZNGOOG <- periodCor["Amazon", "Google"]
    
    yearMon <- format(index(data[corIterator, ]))
    rowNames <- cbind(rowNames, yearMon)
    
    corMatrix[rowIterator, ] <- c(corAAPLMSFT,
                                  corAAPLAMZN,
                                  corAAPLGOOG,
                                  corMSFTAMZN,
                                  corMSFTGOOG,
                                  corAMZNGOOG)
    
    corIterator <- corIterator + 3
    rowIterator <- rowIterator + 1
  }
  corMatrix.z = as.zoo(coredata(corMatrix))
  index(corMatrix.z) <- rowNames
  colnames(corMatrix.z) <-
    c("AAPL_MSFT",
      "AAPL_AMZN",
      "AAPL_GOOG",
      "MSFT_AMZN",
      "MSFT_GOOG",
      "AMZN_GOOG")
  
  return(corMatrix.z)
}
