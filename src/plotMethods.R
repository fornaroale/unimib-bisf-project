

plotDy <- function(data.z, mainText, xlabText, ylabText) {
  dygraph(data.z,
          main = mainText,
          xlab = xlabText,
          ylab = ylabText) %>%
    dyLegend(width = 600) %>%
    dySeries("Apple",
             color = "black") %>%
    dySeries("Microsoft",
             color = "red") %>%
    dySeries("Amazon",
             color = "orange") %>%
    dySeries("Google",
             color = "blue")
}

plotHist <- function(data, mainText, color, numBins) {
  hist(
    data,
    main = mainText,
    probability = TRUE,
    col = color,
    xlab = "CC Returns",
    breaks = numBins
  )
}

plotSmoothedDensity <- function(data, mainText, color, numBins) {
  plotHist(data, mainText, color, numBins)
  data_density <- density(data)
  points(data_density, type = "l", col = "black")
}

plotSummary <- function(data, mainText, numBins) {
  par(mfrow = c(2, 2))
  plotHist(data, mainText, "red", numBins)
  boxplot(data, ylab = "CC Return", col = "aliceblue")
  plot(
    density(data),
    type = "l",
    xlab = "CC Return",
    col = "red",
    lwd = 2,
    ylab = "Density Estimate",
    main = "Smoothed Density"
  )
  qqnorm(data, col = "blue")
  qqline(data)
}

plotCorAnalysis <- function(data, column) {
  xlabText <- "Date"
  ylabText <- "Correlation"
  dataColNames <- colnames(data)
  plot(
    as.yearmon(index(data)),
    coredata(data[, column]),
    type = "l",
    main = paste("Trimestral Correlation", dataColNames[[column]]),
    xlab = xlabText,
    ylab = ylabText
  )
}

plotBetaTrimestrali <- function(data, beta, stockName) {
  par(mfrow = c(2, 1))
  plot(
    data,
    type = "l",
    main = paste("Trimestral", stockName, "CC Return"),
    xlab = "Date",
    ylab = "CC Return"
  )
  plot(
    beta,
    type = "o",
    main = paste("Trimestral", stockName, "Beta"),
    col = "blue",
    xlab = "Date",
    ylab = "Beta"
  )
}

plotPortfolioValue <- function(data.z) {
  dygraph(data.z,
          main = "Portfolio Value over time (12/2018 - 10/2019)",
          xlab = "Date",
          ylab = "Value [Euro]")
}

plotPFSPCComparison <- function(data.z) {
  dygraph(data.z,
          main = "Portofolio and SPC CC returns comparison",
          xlab = "Date",
          ylab = "CC Return") %>%
    dyLegend(width = 600) %>%
    dySeries("Portfolio",
             color = "orange") %>%
    dySeries("SP500",
             color = "blue")
}
