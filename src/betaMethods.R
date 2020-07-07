
calcoloBeta <- function(stock, marketIndex) {
  covariance <- cov(stock, marketIndex)
  marketVariance <- var(marketIndex)
  beta <- covariance / marketVariance
  return(beta)
}

calcoloBetaTrimestrale <- function(data, index, nome) {
  betaValues = zoo(matrix(nrow = nrow(data) / 3, ncol = 1))
  
  colnames(betaValues) <- c(nome)
  
  rowIterator <- 1
  betaIterator <- 1
  datesToKeep <- c()
  
  while (rowIterator < nrow(data)) {
    rowIteratorEnd <- rowIterator + 2
    
    betaValues[betaIterator, 1] <-
      calcoloBeta(data[rowIterator:rowIteratorEnd],
                  index[rowIterator:rowIteratorEnd])
    datesToKeep <- cbind(datesToKeep, index(data[rowIterator]))
    
    rowIterator <- rowIterator + 3
    betaIterator <- betaIterator + 1
  }
  
  index(betaValues) <- as.Date(datesToKeep)
  
  return(betaValues)
}

calcoloExpReturnCAPM <-
  function(betaValues, marketReturn, riskFreeReturn) {
    return(riskFreeReturn + (betaValues * (marketReturn - riskFreeReturn)))
  }

calcoloBetaPortafogli <- function(data) {
  return(calcoloBeta(coredata(data)[, 1], coredata(data)[, 2]))
}
