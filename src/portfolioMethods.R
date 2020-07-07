

portafogliRibMensile <-
  function(RetCC.z,
           prezziStock,
           budget,
           pesiStock,
           costoTransazione,
           nomiStock) {
    # Preparo una lista per i valori da returnare
    returnList <- list()
    # 1a riga: capitale di ogni mese (budget + valore portafogli)
    # 2a riga: peso di ogni stock
    
    # Calcolo i pesi "reali" prendendo in considerazione i costi di transazione (mese 1)
    numMese <- 1
    pesiPortafogli <- calcolaPesiPortafogli(numMese,
                                            budget,
                                            pesiStock,
                                            costoTransazione,
                                            prezziStock)
    #cat("Pesi iniziali:", pesiPortafogli, "\n")
    
    # Inserisco i dati del 1o mese nella lista da returnare
    returnList[[1]] <- budget
    returnList[[2]] <- pesiPortafogli
    
    # Salvo il budget iniziale per il calcolo finale del rendimento portafogli
    budgetIniziale <- budget
    
    # Eseguo il primo acquisto di stock (mese 1)
    budget <-
      budget - (sum(
        pesiPortafogli * costoAcquisto(prezziStock, 1, costoTransazione)
      ))
    #cat("Budget dopo primo acquisto stock:", budget, "\n")
    
    ### INIZIO CICLO SCORRIMENTO MESI
    numMese <- numMese + 1
    while (numMese < nrow(prezziStock)) {
      pesiStockPrec <- pesiPortafogli
      budgetMesePrec <- budget
      
      # Quanto guadagno se vendo tutto?
      guadagnoTot <-
        guadagnoVendita(prezziStock, numMese, costoTransazione)
      guadagnoTot[, 1] <-
        coredata(pesiStockPrec[, 1]) * coredata(guadagnoTot[, 1])
      guadagnoTot[, 2] <-
        coredata(pesiStockPrec[, 2]) * coredata(guadagnoTot[, 2])
      guadagnoTot[, 3] <-
        coredata(pesiStockPrec[, 3]) * coredata(guadagnoTot[, 3])
      guadagnoTot[, 4] <-
        coredata(pesiStockPrec[, 4]) * coredata(guadagnoTot[, 4])
      portfolioValue <- budgetMesePrec + sum(guadagnoTot)
      
      # Inserisco pesi nella lista da returnare
      returnList[[1]] <- c(returnList[[1]], portfolioValue)
      
      # Ribilanciamento mensile dei pesi basandosi su tutti i mesi
      # precedenti a quello in analisi
      pfDataLimit <- nrow(RetCC.z) - 10 + numMese
      ##cat("\n","Ribilancio su adj close mese: ", as.yearmon(index(RetCCmerge.z[1:pfDataLimit,])),"\n")
      portfolioOpt <- portfolio.optim(RetCCmerge.z[1:pfDataLimit, ])
      pesiPortafogli <- portfolioOpt$pw
      
      # Ricalcolo il peso dei singoli titoli sapendo il mio nuovo budget (portofolioValue)!
      pesiPortafogli <-
        calcolaPesiPortafogli(numMese,
                              portfolioValue,
                              pesiPortafogli,
                              costoTransazione,
                              prezziStock)
      
      # Inserisco pesi nella lista da returnare
      returnList[[2]] <- c(returnList[[2]], pesiPortafogli)
      
      # Calcolo la differenza tra i pesi degli stock tra mese attuale e precedente
      WgtDifference <-
        coredata(pesiPortafogli) - coredata(pesiStockPrec)
      
      # Verifico se ci sono stati stock acquistati/venduti
      stockComprati <- c(0, 0, 0, 0)
      stockVenduti <- c(0, 0, 0, 0)
      for (i in 1:4) {
        if (coredata(WgtDifference[i]) > 0) {
          stockComprati[i] <- coredata(WgtDifference[i])
          stockVenduti[i] <- 0
        } else{
          stockComprati[i] <- 0
          stockVenduti[i] <- coredata(WgtDifference[i]) * (-1)
        }
      }
      
      # Calcolo nuovo budget (=budget precedente + titoli venduti - titoli comprati)
      # dopo l'effettiva vendita o acquisto di azioni
      guadagnoTot <-
        guadagnoVendita(prezziStock, numMese, costoTransazione)
      guadagnoTot[, 1] <-
        coredata(stockVenduti[1]) * coredata(guadagnoTot[, 1])
      guadagnoTot[, 2] <-
        coredata(stockVenduti[2]) * coredata(guadagnoTot[, 2])
      guadagnoTot[, 3] <-
        coredata(stockVenduti[3]) * coredata(guadagnoTot[, 3])
      guadagnoTot[, 4] <-
        coredata(stockVenduti[4]) * coredata(guadagnoTot[, 4])
      perditaTot <-
        costoAcquisto(prezziStock, numMese, costoTransazione)
      perditaTot[, 1] <-
        coredata(stockComprati[1]) * coredata(perditaTot[, 1])
      perditaTot[, 2] <-
        coredata(stockComprati[2]) * coredata(perditaTot[, 2])
      perditaTot[, 3] <-
        coredata(stockComprati[3]) * coredata(perditaTot[, 3])
      perditaTot[, 4] <-
        coredata(stockComprati[4]) * coredata(perditaTot[, 4])
      budget <- budgetMesePrec + sum(guadagnoTot) - sum(perditaTot)
      #cat("Budget dopo acquisto stock:", budget, "mese", numMese, "\n")
      
      numMese <- numMese + 1
    }
    
    # Quanto guadagno se vendo tutto ALLA FINE?
    guadagnoTot <-
      guadagnoVendita(prezziStock, numMese, costoTransazione)
    guadagnoTot[, 1] <-
      coredata(pesiStockPrec[, 1]) * coredata(guadagnoTot[, 1])
    guadagnoTot[, 2] <-
      coredata(pesiStockPrec[, 2]) * coredata(guadagnoTot[, 2])
    guadagnoTot[, 3] <-
      coredata(pesiStockPrec[, 3]) * coredata(guadagnoTot[, 3])
    guadagnoTot[, 4] <-
      coredata(pesiStockPrec[, 4]) * coredata(guadagnoTot[, 4])
    budget <- budget + sum(guadagnoTot)
    cat("Budget finale", budget)
    
    # Inserisco valore portafogli finale nella lista da returnare
    returnList[[1]] <- c(returnList[[1]], budget)
    
    # Ritorno la lista contenente i dati del portafogli
    return(returnList)
  }

calcolaPesiPortafogli <-
  function(numMese,
           budget,
           pesiStock,
           costoTransazione,
           prezziStock) {
    return(budget * pesiStock / costoAcquisto(prezziStock, numMese, costoTransazione))
  }

costoAcquisto <- function(prezziStock, numMese, costoTransazione) {
  costo <-
    prezziStock[numMese] + prezziStock[numMese] * costoTransazione
  return(costo)
}

guadagnoVendita <- function(prezziStock, numMese, costoTransazione) {
  guadagno <-
    prezziStock[numMese] - prezziStock[numMese] * costoTransazione
  return(guadagno)
}