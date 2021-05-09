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
options(stringsAsFactors = FALSE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#
Data <- read.csv2("Instruments.csv")
Data$MktNm %>% unique()
Delete <- c("EQUITY-DERIVATE","FUTURE","OPTIONS ON FUTURE","OPTIONS ON SPOT","FORWARD")
Data <- Data[-which(Data$MktNm %in% Delete),]


Start <- "2019-04-01" %>% as.Date()
Finish <- "2021-03-31" %>% as.Date()

Prices <- NULL

for (i in seq(nrow(Data))){
  stock_index <- Data$TckrSymb[i] %>% paste0(".SA")
  cat(paste("Trying to get",stock_index,"-",i,"of",nrow(Data),"\n"))
  try(Df.temp <- getSymbols(stock_index, verbose = FALSE, src = "yahoo", from=Start, to=Finish, auto.assign = FALSE), silent = TRUE)
  
  if(exists("Df.temp")) {
    Close <- Df.temp[,which(names(Df.temp) == paste0(stock_index,".Close"))]
    Prices = cbind(Prices, Close)
    rm(Df.temp)
  }
  
  gc()
  Sys.sleep(1)
}

save(Prices, file = "Prices.rda")

##

# load("Prices.rda")

library(vars)
library(urca)
library(aTSA)

toDelete <- base::apply(Prices,2,function(x) sum(is.na(x))/nrow(Prices) >= 0.05) %>% as.logical()
Prices <- Prices[,!toDelete]

Train <- Prices[1:which(index(Prices) == "2020-03-31"),]

Cointegrated <- matrix(ncol = 2) %>% as.data.frame()
colnames(Cointegrated) <- c("Tckr1","Tckr2")

set.seed(123)
for(p1 in seq(ncol(Train)-1)) {
  cat(paste("Testing cointegration for",names(Train[,p1]),"-",p1,"of",(ncol(Train)-1),"\n"))
  a1 <- Train[,p1]
  # Treat NA
  if(is.na(a1[1,])) a1[1,] <- na.omit(a1)[1]
  for(i in 2:nrow(Train)) {
    if(is.na(a1[i,])) a1[i,] <- a1[i-1]
  }
  for(p2 in seq(ncol(Train)-p1)) {
    a2 <- Train[,(p1+p2)]
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
    if(!is.na(BestLag)) {
      varX <- VAR(idx, p = BestLag, type = "const")
    
      # Cointegration
      xPrices <- ts.intersect(as.ts(a1),as.ts(a2))
    
      # Johansen Test
      # https://www.quantstart.com/articles/Johansen-Test-for-Cointegrating-Time-Series-Analysis-in-R/
    
      JTest <- try(ca.jo(log(xPrices),type="trace", K=BestLag, ecdet="none", spec="longrun"),silent = TRUE)
      if(class(JTest) == "try-error") {
        Cointg <- FALSE
      } else {
        sObject <- summary(JTest)
        Cointg <- sObject@teststat[2] >= sObject@cval[2,2]
      }
        
       if(Cointg) {
        Df.temp <- matrix(ncol = 2,nrow = 1) %>% as.data.frame()
        colnames(Df.temp) <- c("Tckr1","Tckr2")
        Df.temp$Tckr1[1] <- names(a1)
        Df.temp$Tckr2[1] <- names(a2)
      
        Cointegrated %<>% rbind(Df.temp)
      }
    }
  }
}


save(Cointegrated, file = "Cointegrated.rda")







