# Nome: Alessandro Fornaro
# Matricola: 830065

rm(list = ls())
set.seed(1)

# Librerie necessarie
library(tseries)
library(zoo)
library(dygraphs)
library(quantmod)
library(PerformanceAnalytics)
library(Metrics)
library(forecast)
library(markdown)

# File metodi
source("src/descAnalyticsMethods.R")
source("src/plotMethods.R")
source("src/betaMethods.R")
source("src/arimaMethods.R")
source("src/portfolioMethods.R")

# Definizione periodo di interesse
startDate <- "2010-01-01"
endDate   <- "2019-10-31"

# Download dati
AAPL.z <- monthlyData("AAPL", startDate, endDate)
MSFT.z <- monthlyData("MSFT", startDate, endDate)
AMZN.z <- monthlyData("AMZN", startDate, endDate)
GOOG.z <- monthlyData("GOOG", startDate, endDate)

# Rinomino colonne
colnames(AAPL.z) <- "Apple"
colnames(MSFT.z) <- "Microsoft"
colnames(AMZN.z) <- "Amazon"
colnames(GOOG.z) <- "Google"

# Stampo in un plot adj. close
AdjCloseMerge.z <- merge(AAPL.z, MSFT.z, AMZN.z, GOOG.z)
plotDy(AdjCloseMerge.z,
       "Adj. Close mensile - AAPL, MSFT, AMZN, GOOG",
       "Data",
       "Adj. Close")

### ------------------------ Descriptive Analytics 1.1   ------------------------

# Calcolo Simple Returns mensili
AAPL.ret <- calculateSimpleReturns(AAPL.z)
MSFT.ret <- calculateSimpleReturns(MSFT.z)
AMZN.ret <- calculateSimpleReturns(AMZN.z)
GOOG.ret <- calculateSimpleReturns(GOOG.z)

# Stampo in un plot ritorni semplici
AAPLMSFTAMZNGOOG.ret.z <-
   merge(AAPL.ret, MSFT.ret, AMZN.ret, GOOG.ret)
plot(
   AAPLMSFTAMZNGOOG.ret.z,
   main = "Monthly Simple Return - AAPL, MSFT, AMZN, GOOG",
   col = c("black", "red", "orange", "blue")
)

# Calcolo Continuously Compounded Returns mensili
AAPL.retcc <- calculateCCReturns(AAPL.z)
MSFT.retcc <- calculateCCReturns(MSFT.z)
AMZN.retcc <- calculateCCReturns(AMZN.z)
GOOG.retcc <- calculateCCReturns(GOOG.z)

# Stampo in un plot a sezioni CC Returns mensili
RetCCmerge.z <-
   merge(AAPL.retcc, MSFT.retcc, AMZN.retcc, GOOG.retcc)
plot(RetCCmerge.z,
     main = "Monthly CC Return - AAPL, MSFT, AMZN, GOOG",
     col = c("black", "red", "orange", "blue"))

# Stampo in un plot unico CC Returns mensili
plotDy(RetCCmerge.z,
       "Monthly CC Return - AAPL, MSFT, AMZN, GOOG",
       "Date",
       "CC Return")

# Calcolo correlazione tra titoli (basandomi su valori CC Returns mensili)
cor(RetCCmerge.z)

# Calcolo beta dei titoli su SP500
SPC.z <- monthlyData("^gspc", startDate, endDate)
colnames(SPC.z) <- "SP500"
SPC.retcc <- calculateCCReturns(SPC.z)
calcoloBeta(AAPL.retcc, SPC.retcc)
calcoloBeta(MSFT.retcc, SPC.retcc)
calcoloBeta(AMZN.retcc, SPC.retcc)
calcoloBeta(GOOG.retcc, SPC.retcc)

### ------------------------ Descriptive Analytics 1.2   ------------------------

par(mfrow = c(2, 2))

# Come numero di intervalli prendo l'arrotondamento della radice quadrata
# della cardinalità del campione di dati
numOfBins <- round(sqrt(nrow(coredata(AAPL.retcc))), 0)

# Istogramma CC AAPL monthly returns (estrazione coredata = rimozione timeindex)
AAPL.ret.mat <- coredata(AAPL.retcc)
plotHist(AAPL.ret.mat,
         "Histogram of CC AAPL monthly returns",
         "white",
         numOfBins)
# Std. Dev. and Relative SD
AAPL.retcc.sd <- sd(AAPL.ret.mat)
cat("Apple SD: ", AAPL.retcc.sd)
AAPL.RelativeSD <- calculateRelativeSD(AAPL.ret.mat)
cat("Apple Relative SD %: ", AAPL.RelativeSD*100)

# Istogramma CC MSFT monthly returns
MSFT.ret.mat <- coredata(MSFT.retcc)
plotHist(MSFT.ret.mat,
         "Histogram of CC MSFT monthly returns",
         "red",
         numOfBins)
# Std. Dev. and Relative SD
MSFT.retcc.sd <- sd(MSFT.ret.mat)
cat("Microsoft SD: ", MSFT.retcc.sd)
MSFT.RelativeSD <- calculateRelativeSD(MSFT.ret.mat)
cat("Microsoft Relative SD %: ", MSFT.RelativeSD*100)

# Istogramma CC AMZN monthly returns
AMZN.ret.mat <- coredata(AMZN.retcc)
plotHist(AMZN.ret.mat,
         "Histogram of CC AMZN monthly returns",
         "orange",
         numOfBins)
# Std. Dev. and Relative SD
AMZN.retcc.sd <- sd(AMZN.ret.mat)
cat("Amazon SD: ", AMZN.retcc.sd)
AMZN.RelativeSD <- calculateRelativeSD(AMZN.ret.mat)
cat("Amazon Relative SD %: ", AMZN.RelativeSD*100)

# Istogramma CC GOOG monthly returns
GOOG.ret.mat <- coredata(GOOG.retcc)
plotHist(GOOG.ret.mat,
         "Histogram of CC GOOG monthly returns",
         "blue",
         numOfBins)
# Std. Dev. and Relative SD
GOOG.retcc.sd <- sd(GOOG.ret.mat)
cat("Google SD: ", GOOG.retcc.sd)
GOOG.RelativeSD <- calculateRelativeSD(GOOG.ret.mat)
cat("Google Relative SD %: ", GOOG.RelativeSD*100)


### ------------------------ Descriptive Analytics 2.1 ------------------------

par(mfrow = c(2, 2))

# Dispersion diagnostic plot di AAPL
plotSmoothedDensity(AAPL.ret.mat, "Smoothed density AAPL stock", "white", numOfBins)
# Shapiro-Wilk normality test
shapiro.test(AAPL.ret.mat)

# Dispersion diagnostic plot di MSFT
plotSmoothedDensity(MSFT.ret.mat, "Smoothed density MSFT stock", "red", numOfBins)
# Shapiro-Wilk normality test
shapiro.test(MSFT.ret.mat)

# Dispersion diagnostic plot di AMZN
plotSmoothedDensity(AMZN.ret.mat, "Smoothed density AMZN stock", "orange", numOfBins)
# Shapiro-Wilk normality test
shapiro.test(AMZN.ret.mat)

# Dispersion diagnostic plot di GOOG
plotSmoothedDensity(GOOG.ret.mat, "Smoothed density GOOG stock", "blue", numOfBins)
# Shapiro-Wilk normality test
shapiro.test(GOOG.ret.mat)


### ------------------------ Descriptive Analytics 2.2 ------------------------

# AAPL univariate statistics
cat("AAPL univariate statistics")
univariateStats(AAPL.ret.mat)

# MSFT univariate statistics
cat("MSFT univariate statistics")
univariateStats(MSFT.ret.mat)

# AMZN univariate statistics
cat("AMZN univariate statistics")
univariateStats(AMZN.ret.mat)

# GOOG univariate statistics
cat("GOOG univariate statistics")
univariateStats(GOOG.ret.mat)


### ------------------------ Descriptive Analytics 2.3   ------------------------

# Sommario AAPL
plotSummary(AAPL.ret.mat, "AAPL monthly CC Returns", numOfBins)

# Sommario MSFT
plotSummary(MSFT.ret.mat, "MSFT monthly CC Returns", numOfBins)
# Caso emblematico valori outliers: stampo boxplot
par(mfrow = c(1, 1))
boxplot(MSFT.ret.mat, main = "Microsoft Outliers Values")

# Sommario AMZN
plotSummary(AMZN.ret.mat, "AMZN monthly CC Returns", numOfBins)

# Sommario GOOG
plotSummary(GOOG.ret.mat, "GOOG monthly CC Returns", numOfBins)


### ------------------------ Descriptive Analytics 3   ------------------------

# Covarianza e Correlazione
covRetCC <- cov(RetCCmerge.z)
corRetCC <- cor(RetCCmerge.z)

# pairwise scatter plot
pairs(RetCCmerge.z,
      pch = 18,
      col = "blue",
      main = "Correlation Graphs")

# Correlazione massima e minima (escludendo i valori insignificanti)
max(corRetCC[corRetCC != max(corRetCC)])
min(corRetCC)

# Analizzo la correlazioni a periodi di 3 mesi
corAnalysis.z <- computeThreeMonthsCor(RetCCmerge.z)
par(mfrow = c(3, 2))

# Plot analisi correlazione AAPL_MSFT
plotCorAnalysis(corAnalysis.z, 1)
# Plot analisi correlazione AAPL_AMZN
plotCorAnalysis(corAnalysis.z, 2)
# Plot analisi correlazione AAPL_GOOG
plotCorAnalysis(corAnalysis.z, 3)
# Plot analisi correlazione MSFT_AMZN
plotCorAnalysis(corAnalysis.z, 4)
# Plot analisi correlazione MSFT_GOOG
plotCorAnalysis(corAnalysis.z, 5)
# Plot analisi correlazione AMZN_GOOG
plotCorAnalysis(corAnalysis.z, 6)


### ---------------------        Forecasting: ARIMA       ---------------------

# Valori per ARIMA
n <- 96
m <- 12
l <- 10

graphics.off()
# Verifico stazionarietà serie AAPL (per poter avere coefficiente ARIMA I = 0),
# verifico decomposizione (stag., cicli, trend, ecc.) e cerco la miglior terna
# di parametri ARIMA (p,d,q) -> infine, avvio ARIMA con i parametri scelti

# 0. ADF TEST (test di non stazionarietà)
adf.test(AAPL.retcc)
adf.test(MSFT.retcc)
adf.test(AMZN.retcc)
adf.test(GOOG.retcc)

# 1. APPLE
plotSTL(AAPL.retcc, "Apple")
# --- CON ZOO: (da problemi!)
# plotSTL(AAPL.retcc, "Apple")
# ParamsAAPL <- computeArimaParameters(AAPL.retcc[1:n, drop = FALSE],
#                                      AAPL.retcc[(n + 1):(n + m), drop = FALSE],
#                                      m)[2, ]
# forecastArima(AAPL.retcc, n, m, l, ParamsAAPL, "Apple")

# --- CON XTS (perchè con zoo da problemi!):
AAPL.CC.xts <- getCCReturnsXTS("AAPL", startDate, endDate)
ParamsAAPLxts <-
   computeArimaParameters(AAPL.CC.xts[1:n, drop = FALSE],
                          AAPL.CC.xts[(n + 1):(n + m), drop = FALSE],
                          m)[2, ]
forecastArima(AAPL.CC.xts, n, m, l, ParamsAAPLxts, "Apple")

# 2. MICROSOFT
plotSTL(MSFT.retcc, "Microsoft")
# --- CON ZOO: (da problemi!)
# plotSTL(MSFT.retcc, "Microsoft")
# ParamsMSFT <- computeArimaParameters(MSFT.retcc[1:n, drop = FALSE],
#                                      MSFT.retcc[(n + 1):(n + m), drop = FALSE],
#                                      m)[2, ]
# forecastArima(MSFT.retcc, n, m, l, ParamsMSFT, "Microsoft")

# --- CON XTS (perchè con zoo da problemi!):
MSFT.CC.xts <- getCCReturnsXTS("MSFT", startDate, endDate)
ParamsMSFTxts <-
   computeArimaParameters(MSFT.CC.xts[1:n, drop = FALSE],
                          MSFT.CC.xts[(n + 1):(n + m), drop = FALSE],
                          m)[2, ]
forecastArima(MSFT.CC.xts, n, m, l, ParamsMSFTxts, "Microsoft")

# 3. AMAZON
plotSTL(AMZN.retcc, "Amazon")
# --- CON ZOO: (da problemi!)
# plotSTL(AMZN.retcc, "Amazon")
# ParamsAMZN <- computeArimaParameters(AMZN.retcc[1:n, drop = FALSE],
#                                      AMZN.retcc[(n + 1):(n + m), drop = FALSE],
#                                      m)[2, ]
# forecastArima(AMZN.retcc, n, m, l, ParamsAMZN, "Amazon")

# --- CON XTS (perchè con zoo da problemi!):
AMZN.CC.xts <- getCCReturnsXTS("AMZN", startDate, endDate)
ParamsAMZNxts <-
   computeArimaParameters(AMZN.CC.xts[1:n, drop = FALSE],
                          AMZN.CC.xts[(n + 1):(n + m), drop = FALSE],
                          m)[2, ]
forecastArima(AMZN.CC.xts, n, m, l, ParamsAMZNxts, "Amazon")

# 4. GOOGLE
plotSTL(GOOG.retcc, "Google")
# --- CON ZOO: (da problemi!)
# plotSTL(GOOG.retcc, "Google")
# ParamsGOOG <- computeArimaParameters(GOOG.retcc[1:n, drop = FALSE],
#                                      GOOG.retcc[(n + 1):(n + m), drop = FALSE],
#                                      m)[2, ]
# forecastArima(GOOG.retcc, n, m, l, ParamsGOOG, "Google")

# --- CON XTS (perchè con zoo da problemi!):
GOOG.CC.xts <- getCCReturnsXTS("GOOG", startDate, endDate)
ParamsGOOGxts <-
   computeArimaParameters(GOOG.CC.xts[1:n, drop = FALSE],
                          GOOG.CC.xts[(n + 1):(n + m), drop = FALSE],
                          m)[2, ]
forecastArima(GOOG.CC.xts, n, m, l, ParamsGOOGxts, "Google")


### ------------------------           Beta            ------------------------

# Beta value sul periodo complessivo d'analisi (calcolata su CC Return dell'indice SP500)
SP500.z <- monthlyData("^gspc", startDate, endDate)
SP500.retcc <- calculateCCReturns(SP500.z)
colnames(SP500.retcc) <- c("SP500")
beta_AAPL <- calcoloBeta(AAPL.retcc, SP500.retcc)
cat("Beta AAPL on SP500: ", beta_AAPL)
beta_MSFT <- calcoloBeta(MSFT.retcc, SP500.retcc)
cat("Beta MSFT on SP500: ", beta_MSFT)
beta_AMZN <- calcoloBeta(AMZN.retcc, SP500.retcc)
cat("Beta AMZN on SP500: ", beta_AMZN)
beta_GOOG <- calcoloBeta(GOOG.retcc, SP500.retcc)
cat("Beta GOOG on SP500: ", beta_GOOG)

# Stampo plot con bande di Bollinger
chartSeries(AAPL.retcc, theme = "white", type = "candles")
addBBands()

# Beta value su finestra temporale trimestrale
beta_AAPL_Trimestral.z <-
   calcoloBetaTrimestrale(AAPL.retcc, SP500.retcc, "Apple")
beta_MSFT_Trimestral.z <-
   calcoloBetaTrimestrale(MSFT.retcc, SP500.retcc, "Microsoft")
beta_AMZN_Trimestral.z <-
   calcoloBetaTrimestrale(AMZN.retcc, SP500.retcc, "Amazon")
beta_GOOG_Trimestral.z <-
   calcoloBetaTrimestrale(GOOG.retcc, SP500.retcc, "Google")

# Stampo grafici Beta
plotBetaTrimestrali(AAPL.retcc, beta_AAPL_Trimestral.z, "Apple")
plotBetaTrimestrali(MSFT.retcc, beta_MSFT_Trimestral.z, "Microsoft")
plotBetaTrimestrali(AMZN.retcc, beta_AMZN_Trimestral.z, "Amazon")
plotBetaTrimestrali(GOOG.retcc, beta_GOOG_Trimestral.z, "Google")

# Calcolo l'expected return of market portfolio (CAPM)
riskFreeReturn <- 0.015 # USA bond risk-free return
expectedReturnAAPL <-
   calcoloExpReturnCAPM(AAPL.retcc, SP500.retcc, riskFreeReturn)
assetRiskPremiumAAPL <- expectedReturnAAPL - riskFreeReturn
expectedReturnMSFT <-
   calcoloExpReturnCAPM(MSFT.retcc, SP500.retcc, riskFreeReturn)
assetRiskPremiumMSFT <- expectedReturnMSFT - riskFreeReturn
expectedReturnAMZN <-
   calcoloExpReturnCAPM(AMZN.retcc, SP500.retcc, riskFreeReturn)
assetRiskPremiumAMZN <- expectedReturnAMZN - riskFreeReturn
expectedReturnGOOG <-
   calcoloExpReturnCAPM(GOOG.retcc, SP500.retcc, riskFreeReturn)
assetRiskPremiumGOOG <- expectedReturnGOOG - riskFreeReturn


### ------------------------        Portfolio         ------------------------

# Creo un portafogli ottimale basandomi sui ritorni CC
# degli stock, esclusi gli ultimi 10 dati
pfBudget <- 50000
l <- 10
dataToConsider <- nrow(RetCCmerge.z) - l
portfolioOpt <- portfolio.optim(RetCCmerge.z[1:dataToConsider])
# Estraggo i pesi degli stock
pf_weights <- portfolioOpt$pw
# Assegno nomi agli stock
names(pf_weights) <- colnames(RetCCmerge.z)
# Trasformo i pesi degli stock in percentuale e stampo
portfolio_weights <- pf_weights[pf_weights >= 0.01]
percPesi <- portfolio_weights * 100
# Stampo grafico a barre dei pesi
graphics.off()
barplot(percPesi, main = "Theoretical Optimized Portfolio - First month",
        ylab = "Weight %")
# Stampo ritorno atteso del portafogli e varianza
portfolioOpt$pm
portfolioOpt$ps

# Creo portafogli partendo dalla data base del test set,
# usando i pesi raccomandati dal portafogli ottimale,considerando
# un costo di transazione di 0.01 sul valore della singola azione
datiPortafogli <- portafogliRibMensile(
   RetCCmerge.z,
   AdjCloseMerge.z[(dataToConsider + 1):nrow(AdjCloseMerge.z)],
   pfBudget,
   portfolio_weights,
   0.01,
   c("AAPL", "MSFT", "AMZN", "GOOG")
)

# Estraggo dati dalla lista returnata
valorePortafogli <- zoo(datiPortafogli[[1]])
pesiStockPortafogli <- datiPortafogli[[2]]
index(valorePortafogli) <-
   c(index(pesiStockPortafogli), index(RetCCmerge.z[dataToConsider + l]))

# Stampo andamento capitale
plotPortfolioValue(valorePortafogli)

# Calcolo ritorno semplice portafogli
pfSimpleReturn <-
   as.numeric((last(valorePortafogli) - pfBudget) / pfBudget)

# Calcolo ritorno CC portafogli e confronto con indice SP500
pf.retCC <- calculateCCReturns(valorePortafogli)
primaDataRitorniPF <- first(index(pf.retCC))
ultimaDataRitorniPF <- last(index(pf.retCC))
pfSPC.Merge.retcc <- last(merge(pf.retCC, SPC.retcc) , 10)
colnames(pfSPC.Merge.retcc) <- c("Portfolio", "SP500")
plotPFSPCComparison(pfSPC.Merge.retcc)

# Calcolo beta per capire come va rispetto all'indice SP500
betaPF <- calcoloBetaPortafogli(pfSPC.Merge.retcc)

# Calcolo correlazione con SP500
cor(pfSPC.Merge.retcc)


### ------------------------     WEB APPLET     ------------------------

# Avvio web-applet Shiny (richiede dati forniti dal codice soprastante!)
library(shiny)
source("src/shinyServer.R")
source("src/shinyUI.R")
shinyApp(ui = shinyUI, server = shinyServer)
