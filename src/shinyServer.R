shinyServer <- function(input, output, session) {
  output$plotAdjClose <- renderDygraph({
    AdjCloseData <- zoo()
    
    if (input$checkboxAAPL)
      AdjCloseData <- merge(AdjCloseData, AAPL.z)
    if (input$checkboxMSFT)
      AdjCloseData <- merge(AdjCloseData, MSFT.z)
    if (input$checkboxAMZN)
      AdjCloseData <- merge(AdjCloseData, AMZN.z)
    if (input$checkboxGOOG)
      AdjCloseData <- merge(AdjCloseData, GOOG.z)
    
    if (length(AdjCloseData) > 0)
      dygraph(AdjCloseData,
              main = "Monthly Adjusted Close",
              xlab = "Date",
              ylab = "Adj. Close")
  })
  
  
  output$plotCCReturn <- renderDygraph({
    CCRetData <- zoo()
    
    if (input$checkboxAAPL_CC)
      CCRetData <- merge(CCRetData, AAPL.retcc)
    if (input$checkboxMSFT_CC)
      CCRetData <- merge(CCRetData, MSFT.retcc)
    if (input$checkboxAMZN_CC)
      CCRetData <- merge(CCRetData, AMZN.retcc)
    if (input$checkboxGOOG_CC)
      CCRetData <- merge(CCRetData, GOOG.retcc)
    
    if (length(CCRetData) > 0)
      dygraph(CCRetData,
              main = "Monthly CC Return",
              xlab = "Date",
              ylab = "CC Return")
  })
  
  output$plotSmoothedHistogram <- renderPlot({
    numBins <- as.numeric(input$numbins)
    
    if (input$menuSmtHst == "Apple")
      plotSmoothedDensity(AAPL.ret.mat,
                          "Smoothed density AAPL stock",
                          "white",
                          numBins)
    if (input$menuSmtHst == "Microsoft")
      plotSmoothedDensity(MSFT.ret.mat, "Smoothed density MSFT stock", "red", numBins)
    if (input$menuSmtHst == "Amazon")
      plotSmoothedDensity(AMZN.ret.mat,
                          "Smoothed density AMZN stock",
                          "orange",
                          numBins)
    if (input$menuSmtHst == "Google")
      plotSmoothedDensity(GOOG.ret.mat, "Smoothed density GOOG stock", "blue", numBins)
  })
  
  output$plotCor <- renderPlot({
    if (input$menuCor == "AAPL-MSFT")
      plotCorAnalysis(corAnalysis.z, 1)
    if (input$menuCor == "AAPL-AMZN")
      plotCorAnalysis(corAnalysis.z, 2)
    if (input$menuCor == "AAPL-GOOG")
      plotCorAnalysis(corAnalysis.z, 3)
    if (input$menuCor == "MSFT-AMZN")
      plotCorAnalysis(corAnalysis.z, 4)
    if (input$menuCor == "MSFT-GOOG")
      plotCorAnalysis(corAnalysis.z, 5)
    if (input$menuCor == "AMZN-GOOG")
      plotCorAnalysis(corAnalysis.z, 6)
  })
  
  output$plotBeta <- renderPlot({
    if (input$menuBeta == "Apple")
      plotBetaTrimestrali(AAPL.retcc, beta_AAPL_Trimestral.z, "Apple")
    if (input$menuBeta == "Microsoft")
      plotBetaTrimestrali(MSFT.retcc, beta_MSFT_Trimestral.z, "Microsoft")
    if (input$menuBeta == "Amazon")
      plotBetaTrimestrali(AMZN.retcc, beta_AMZN_Trimestral.z, "Amazon")
    if (input$menuBeta == "Google")
      plotBetaTrimestrali(GOOG.retcc, beta_GOOG_Trimestral.z, "Google")
  })
  
  output$plotPFValue <- renderDygraph({
    plotPortfolioValue(valorePortafogli)
  })
  
  output$plotPFComparison <- renderDygraph({
    plotPFSPCComparison(pfSPC.Merge.retcc)
  })
  
  output$plotSummary <- renderPlot({
    if (input$menuSummary == "Apple")
      plotSummary(AAPL.ret.mat, "Apple monthly CC Returns", numOfBins)
    if (input$menuSummary == "Microsoft")
      plotSummary(MSFT.ret.mat, "Microsft monthly CC Returns", numOfBins)
    if (input$menuSummary == "Amazon")
      plotSummary(AMZN.ret.mat, "Amazon monthly CC Returns", numOfBins)
    if (input$menuSummary == "Google")
      plotSummary(GOOG.ret.mat, "Google monthly CC Returns", numOfBins)
  })
  
}