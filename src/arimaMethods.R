

plotSTL <- function(data, stockName) {
  # La funzione STL restituisce un oggetto con il vettore seasonal, che
  # contiene la stagionalità, e il vettore trend, contenente il trend
  # della serie
  data <- ts(data[, 1], frequency = 12, start = c(2010, 1))
  fitRet <- stl(data, s.window = "period")
  plot(fitRet, main = paste("STL Decomposition - CC Returns -", stockName))
}

forecastArima <- function(returns, n, m, l, arimaParams, stockName) {
  # Converto ad oggetto zoo:
  returns <- as.zoo(returns)
  
  # Divido i dati in 3 parti:
  # - training set: su cui viene eseguito ARIMA
  # - validation set: su cui si è verificata la bontà dei parametri (p,d,q) ARIMA
  # - test set: su cui si verifica l'accuratezza delle previsioni
  trainingSet <- returns[1:n, drop = FALSE]
  validationSet <- returns[(n + 1):(n + m), drop = FALSE]
  testSet <-
    returns[(length(returns) - l + 1):length(returns), drop = FALSE]
  
  # Forecasting con ARIMA
  dataForArima <- rbind(trainingSet, validationSet)
  print(trainingSet)
  print(validationSet)
  arimaData <- arima(dataForArima, arimaParams)
  predictions <- predict(arimaData, l)
  dataWithPredictions <- rbind(dataForArima, predictions$pred)
  
  # Stampo accuratezza delle previsioni, verificata sul test set
  fcAccuracy <- accuracy(predictions$pred, testSet)[2]
  cat("\nAccuratezza previsioni su titolo", stockName, ":", fcAccuracy)
  
  # Plot del forecasting
  mainText <- paste("ARIMA Forecast -",
                    stockName,
                    "- Params:",
                    arimaParams[1],
                    arimaParams[2],
                    arimaParams[3],
                    "\nAccuracy: ",
                    fcAccuracy)
  plot(
    dataWithPredictions,
    main = mainText,
    xlab = "Date",
    ylab = "CC Return"
  )
  upper <- predictions$pred + predictions$se
  lower <- predictions$pred - predictions$se
  polygon(c(index(upper), rev(index(upper))),
          c(upper, rev(lower)),
          col = "lightblue",
          border = NA)
  lines(lower, col = 'green')
  lines(upper, col = 'green')
  lines(predictions$pred, col = 'red')
  
  # Ritorno i dati con le previsioni
  return(dataWithPredictions)
}

getRMSE <- function(trainingSet, validationSet, m, params) {
  # Eseguo ARIMA sui parametri passati per parametro e verifico
  # la bontà del modello sul set di validazione
  arimaData <- arima(trainingSet, order = params)
  predictions <- predict(arimaData, m)
  cat("\n --> RMSE:",
      accuracy(predictions$pred, validationSet)[2],
      "with params:",
      params)
  return(accuracy(predictions$pred, validationSet)[2])
}

computeArimaParameters <- function(trainingSet, validationSet, m) {
  minRMSE <- +Inf
  
  # Cerco la miglior terna (p,d,q) basandomi sull'RMSE (sqrt dell'errore quadratico
  # medio), ovvero la discrepanza quadratica media fra i valori dei dati osservati
  # (test set) ed i valori dei dati stimati (forecast set)
  for (p in 0:10) {
    for (q in 0:10) {
      # (max q = 5 è lo standard per auto.arima!)
      tryCatch(
        expr = {
          RMSE <- getRMSE(trainingSet, validationSet, m, c(p, 0, q))
        },
        error = function(e) {
          RMSE <- 1000
        }
      )
      
      if (RMSE < minRMSE) {
        minRMSE <- RMSE
        minP <- p
        minQ <- q
      }
    }
  }
  
  params <- c(minP, 0, minQ)
  return(rbind(minRMSE, params))
}
