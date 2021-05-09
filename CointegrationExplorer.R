options(java.parameters = "-Xmx2048m")
Sys.setlocale("LC_TIME","English_United States.1252")
options(scipen = 999)
rm(list=ls()) 
library(plyr) 
library(dplyr) 
library(tidyr)  
library(purrr) 
library(magrittr) 
library(lubridate)
library(httr)  
library(jsonlite)
library(xlsx)         
library(openxlsx)
library(quantmod)     
library(stringr)
library(timeDate)
library(PerformanceAnalytics)
library(fBasics)
library(urca)
library(vars)
options(stringsAsFactors = FALSE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("Prices.rda")
load("Cointegrated.rda")

# Set pair
a1name <- "CGRA3.SA.Close"
a2name <- "AGRO3.SA.Close"
#

toDelete <- base::apply(Prices,2,function(x) sum(is.na(x))/nrow(Prices) >= 0.05) %>% as.logical()
Prices <- Prices[,!toDelete]
Train <- Prices[1:which(index(Prices) == "2020-03-31"),]

a1 <- Train[,which(names(Prices) == a1name)]
# Treat NA
if(is.na(a1[1,])) a1[1,] <- na.omit(a1)[1]
for(i in 2:nrow(Train)) {
  if(is.na(a1[i,])) a1[i,] <- a1[i-1]
}

a2 <- Train[,which(names(Prices) == a2name)]
# Treat NA
if(is.na(a2[1,])) a2[1,] <- na.omit(a2)[1]
for(i in 2:nrow(Train)) {
  if(is.na(a2[i,])) a2[i,] <- a2[i-1]
}
  
# Select lag 
idx <- ts.intersect(diff(as.ts(a1)),diff(as.ts(a2)))
BestLag <- NULL
d_vec <- seq(nrow(idx) - 120)
for(a in 1:10) {
  idx_temp <- sample(d_vec,1)
  vModel <- VARselect(idx[idx_temp:(idx_temp+120)], lag.max=8, type="const")
  BestLag %<>% c(as.numeric(vModel$selection))
}
BestLag <- table(BestLag)
ifelse(as.numeric(names(BestLag[which(BestLag == max(BestLag))[1]])) == 1,
       BestLag <-as.numeric(names(BestLag[which(BestLag == max(BestLag))[2]])),
       BestLag <-as.numeric(names(BestLag[which(BestLag == max(BestLag))[1]])))

varX <- VAR(idx, p = BestLag, type = "const")
    
# Cointegration
xPrices <- ts.intersect(as.ts(a1),as.ts(a2))
    
# Johansen Test
# https://www.quantstart.com/articles/Johansen-Test-for-Cointegrating-Time-Series-Analysis-in-R/
    
JTest <- try(ca.jo(log(xPrices),type="trace", K=BestLag, ecdet="none", spec="longrun"),silent = TRUE)
sObject <- summary(JTest)
Cointg <- sObject@teststat[2] >= sObject@cval[2,2]

plot.ts(xPrices)
ccf(diff(as.numeric(a1)),diff(as.numeric(a2)))

jPrices <- a1/a2
plot.ts(jPrices)

## In order to simplify, we will use normal Z-score

hist(jPrices)
basicStats(jPrices)

# Distribution is not symmetric

# Param
Threshold <- 2.5 # Entry
SL <- 0.5 # Distance (in standard deviation) from entry to SL
size <- 100 # 1 lot = 100
statPeriod <- 250 # 256 is approximately 1 year. The higher, the more extreme in Z-values.

AllSeries <- Prices[,which(names(Prices) == a1name | names(Prices) == a2name)]
if(is.na(AllSeries[1,1])) AllSeries[1,1] <- na.omit(AllSeries[,1])[1]
if(is.na(AllSeries[1,2])) AllSeries[1,2] <- na.omit(AllSeries[,2])[1]
for(i in 2:nrow(AllSeries)) {
  if(is.na(AllSeries[i,1])) AllSeries[i,1] <- AllSeries[(i-1),1]
  if(is.na(AllSeries[i,2])) AllSeries[i,2] <- AllSeries[(i-1),2]
}
jSeries <- AllSeries[,2]/AllSeries[,1]

# Denominator is a1 (AGRO), thus long jSeries -> SHORT a1, and short jSeries -> LONG a1

###


CalcVolume <- function(type, size, p1, p2) {
  n <- which(c(p1,p2) == max(c(p1,p2)))
  if(n == 1) {
    lot1 <- ifelse(type == "BUY", -1, 1) * size
    totalvol1 <- p1*size*ifelse(type == "BUY", -1, 1)
    totvol2int <- p2*size*ifelse(type == "SELL", -1, 1)
    totvol2frac <- (abs(totalvol2)-abs(totvol2int))*ifelse(type == "SELL", -1, 1)
    totalvol2 <- totvol2int + (round(totvol2frac/p2,0)*p2)
    lot2 <- ifelse(type == "SELL", -1, 1) * (size+abs(round(totvol2frac/p2,0)))
    
  } else {
    lot2 <- ifelse(type == "SELL", -1, 1) * size
    totalvol2 <- p2*size*ifelse(type == "SELL", -1, 1)
    totvol1int <- p1*size*ifelse(type == "BUY", -1, 1)
    totvol1frac <- (abs(totalvol2)-abs(totvol1int))*ifelse(type == "BUY", -1, 1)
    totalvol1 <- totvol1int + (round(totvol1frac/p1,0)*p1)
    lot1 <- ifelse(type == "BUY", -1, 1) * (size+abs(round(totvol1frac/p1,0)))
  }
  return(list(Lots =c(as.numeric(lot1),as.numeric(lot2)), VolumeBRL = c(as.numeric(totalvol1),as.numeric(totalvol2))))
}

# Set starting point
now <- which(index(Prices) >= as.Date("2020-04-01"))[1]

Trading <- matrix(ncol=12) %>% as.data.frame()
colnames(Trading) <- c("DateEntry","Type","V1","V2","L1","L2","PriceEntry","Z","SL","DateExit","PriceExit","PL")
Trading$DateEntry %<>% as.Date()
Trading$DateExit %<>% as.Date()

TotPosition <- NULL
ClosedPL <- NULL
FloatingPL <- NULL

for(i in now:nrow(jSeries)) {
  # Get param
  Series.temp <- jSeries[(i-statPeriod):i,]
  mu <- mean(Series.temp[,1])
  UL <- mu + sd(Series.temp)*Threshold
  LL <- mu - sd(Series.temp)*Threshold
  
  Trading.temp <- Trading[which(is.na(Trading$DateExit) & !is.na(Trading$DateEntry)),]
  
  if(nrow(Trading.temp) > 0) {
     for(e in seq(nrow(Trading.temp))) {
       rowtoClose <- which(Trading$DateEntry == Trading.temp$DateEntry[e])
       if((Trading.temp$Type[e] == "BUY" & (jSeries[i,] >= mu | jSeries[i,] <= Trading.temp$SL[e])) |
          (Trading.temp$Type[e] == "SELL" & (jSeries[i,] <= mu | jSeries[i,] >= Trading.temp$SL[e]))) {
         
           PL1 <- ((AllSeries[i,1] * abs(Trading.temp$L1[e])) - abs(Trading.temp$V1[e])) * ifelse(Trading.temp$L1[e] < 0,-1,1)
           PL2 <- ((AllSeries[i,2] * abs(Trading.temp$L2[e])) - abs(Trading.temp$V2[e])) * ifelse(Trading.temp$L2[e] > 0,1,-1)
           Trading$PL[rowtoClose] <- PL1 + PL2
           Trading$DateExit[rowtoClose] <- index(jSeries[i,])
           Trading$PriceExit[rowtoClose] <- jSeries[i,] %>% as.numeric()
         
         ##### TO DO: ADD COST (2% over short volume)
       }
     } # close for each row in trading.temp
  }
  
  # Entry
  if(jSeries[i,] >= UL | jSeries[i,] <= LL) {
    Trading.add <- matrix(ncol=12) %>% as.data.frame()
    colnames(Trading.add) <- c("DateEntry","Type","V1","V2","L1","L2","PriceEntry","Z","SL","DateExit","PriceExit","PL")
    Trading.add$DateEntry %<>% as.Date()
    Trading.add$DateExit %<>% as.Date()
    
    Trading.add$DateEntry[1] <- index(jSeries[i,])
    Trading.add$Type[1] <- ifelse(jSeries[i,] >= UL, "SELL", "BUY")
    VolData <- CalcVolume(type = Trading.add$Type[1], size = size, p1 = AllSeries[i,1], p2 = AllSeries[i,2])
    Trading.add$V1[1] <- VolData$VolumeBRL[1]
    Trading.add$V2[1] <- VolData$VolumeBRL[2]
    Trading.add$L1[1] <- VolData$Lots[1]
    Trading.add$L2[1] <- VolData$Lots[2]
    Trading.add$PriceEntry[1] <- jSeries[i,] %>% as.numeric()
    Trading.add$Z[1] <- (jSeries[i,]-mu)/sd(Series.temp) %>% as.numeric()
    Trading.add$SL[1] <- ifelse(Trading.add$Type[1] == "SELL", Trading.add$PriceEntry[1] + sd(Series.temp)*SL,
                                Trading.add$PriceEntry[1] - sd(Series.temp)*SL)
    
    Trading %<>% rbind(Trading.add)
  }
  
  # EOD Total Position
  Trading.temp <- Trading[which(is.na(Trading$DateExit) & !is.na(Trading$DateEntry)),]
  ifelse(nrow(Trading.temp) > 0, TotPosition %<>% c(abs(sum(Trading.temp$V1)) + abs(sum(Trading.temp$V2))),
         TotPosition %<>% c(0))
  
  # ClosedPL
  Trading.closed <- Trading[which(Trading$DateExit == index(jSeries[i,])),]
  ifelse(nrow(Trading.closed) > 0, ClosedPL %<>% c(sum(Trading.closed$PL)),
         ClosedPL %<>% c(0))
  
  # FloatingPL
  if(nrow(Trading.temp) > 0) {
    Floating <- 0
    for(e in seq(nrow(Trading.temp))) {
      PL1 <- ((AllSeries[i,1] * abs(Trading.temp$L1[e])) - abs(Trading.temp$V1[e])) * ifelse(Trading.temp$L1[e] < 0,-1,1)
      PL2 <- ((AllSeries[i,2] * abs(Trading.temp$L2[e])) - abs(Trading.temp$V2[e])) * ifelse(Trading.temp$L2[e] > 0,1,-1)
      Floating <- Floating + (PL1 + PL2)
    } # close for each row in trading.temp
    FloatingPL %<>% c(Floating)
  } else {
    FloatingPL %<>% c(0)
  }
}

Trading <- Trading[complete.cases(Trading),]
Trading$PL %>% sum()

# Max EOD TotPosition
plot(TotPosition,t="l")
TotPosition %<>% as.xts(order.by = as.Date(index(jSeries[now:nrow(jSeries)])))
max(TotPosition)
index(TotPosition[which(TotPosition == max(TotPosition))])

# Check Closed + NetFloating PL
ClosedPL %<>% as.xts(order.by = as.Date(index(jSeries[now:nrow(jSeries)])))
FloatingPL %<>% as.xts(order.by = as.Date(index(jSeries[now:nrow(jSeries)])))
NetFloatingPL <- diff(FloatingPL)
NetFloatingPL[is.na(NetFloatingPL)] <- 0
TotPL <- ClosedPL + NetFloatingPL
sum(TotPL)

# Max drawdown
InitDep <- 50000
Equity <- cumsum(c(InitDep,TotPL[2:nrow(TotPL)]))
PL.xts <- Equity %>% as.xts(order.by = index(TotPL))
Returns <- CalculateReturns(PL.xts)
maxDrawdown(Returns)

# Sharpe
SharpeRatio(Returns, FUN = "StdDev")



# Validation file
wb<-xlsx::createWorkbook(type="xlsx")
sheet <- xlsx::createSheet(wb, sheetName = "Trading")
xlsx::addDataFrame(Trading, sheet, row.names=FALSE)
Dates <- index(AllSeries) %>% as.Date()
AllSeries.df <- cbind(as.data.frame(Dates),(AllSeries %>% as.data.frame()))
sheet <- xlsx::createSheet(wb, sheetName = "Prices")
xlsx::addDataFrame(AllSeries.df, sheet, row.names=FALSE)
xlsx::saveWorkbook(wb, "Trading L&S.xlsx")


