## === technical indicators implement by R    KYGu ===

# Simple n(10 here)-day Moving Average (SMA)
# here is general case : interval is n 
SMA_model <- function(interval, now, data){
   targetData <- dta.filterByWindowFromNow( interval-1, now, data)
   targetClosePrice <- targetData$Close
   SMA_value <- sum(targetClosePrice) / interval
   
   return(SMA_value)
}

# Weighted n-day Moveing Average (WMA)

WMA_model <- function(interval, now, data){
   targetData <- dta.filterByWindowFromNow( interval-1, now, data)
   targetClosePrice <- targetData$Close
   temp <- 0
   temp2 <- 0
   total_temp <- 0
   for(i in 1:nrow(targetData)){
       temp <- temp + targetClosePrice[i] * i
       temp2 <- temp2 + 1
       total_temp <- total_temp + temp2
   }
   WMA_value <- temp / total_temp
   
   return(total_temp)
}



#
RSV_model <- function(interval, now, data){
  targetData <- dta.filterByWindowFromNow( interval-1, now, data)
  Ct <- targetData$Close
  Ct <- Ct[interval]
  Ht <- targetData$High
  HH <- max(Ht)
  Lt <- targetData$Low
  LL <- min(Lt)
  RSV_value <- (Ct - LL) / (HH - LL) * 100
  
  return(RSV_value)
}


KValue_model <- function(interval, now, data, lastKValue) {
  targetData <- dta.filterByWindowFromNow(interval-1, now, data)
 
  if (now == interval){
     KValue <- 50
     return(KValue)
  }
  else{
     RSV <- RSV_model(interval, now, data)
     KValue <- (lastKValue * 2/3) + (RSV * 1/3)
     
     return(KValue)
  }
}


DValue_model <- function(interval, now, data, lastDValue, nowKValue){
  targetData <- dta.filterByWindowFromNow( interval-1, now, data)
  
  if(now == interval){
    DValue <- 50
    return(DValue)
  }
  else{
    DValue <- (lastDValue * 2/3) + (nowKValue * 1/3)
    
    return(DValue)
  }
  
}



# Momentum (MTM) : Ct - Ct-(interval-1) , here interval is 10
MTM_model <- function(interval, now, data){
  targetData <- dta.filterByWindowFromNow( interval-1, now, data)
  targetClosePrice <- targetData$Close
  MOM_value <- targetClosePrice[length(targetClosePrice)] - targetClosePrice[1]
  
  return(MOM_value)
}



MTM5_model <- function(now, data){
  targetData <- dta.filterByWindowFromNow( 4, now, data)
  targetClosePrice <- targetData$Close
  MOM_value <- targetClosePrice[length(targetClosePrice)] - targetClosePrice[1]
  
  return(MOM_value)
}



MTM10_model <- function(now, data){
  targetData <- dta.filterByWindowFromNow( 9, now, data)
  targetClosePrice <- targetData$Close
  MOM_value <- targetClosePrice[length(targetClosePrice)] - targetClosePrice[1]
  
  return(MOM_value)
}



ROC_model <- function(interval, now, data){
  targetData <- dta.filterByWindowFromNow( interval-1, now, data)
  targetClosePrice <- targetData$Close
  ROC_value <- ((targetClosePrice[length(targetClosePrice)] - targetClosePrice[1]) / targetClosePrice[1]) * 100
  
  return(ROC_value)
}


# Accumulation/Distribution (A/D) Oscillator (ADO) : [(Ht - Ot) + (Ct - Lt)] / [2 * (Ht - Lt)] * 100
# where: Ct- close price , Ht: high price , Lt- low price , Ot- open price 
ADO_model.ori <- function( now, data){
  targetData <- dta.filterByWindowFromNow( 0, now, data)
  Ct <- targetData$Close
  Ht <- targetData$High
  Lt <- targetData$Low
  Ot <- targetData$Open
  ADO_value <- ((Ht - Ot) + (Ct - Lt)) /  (2 * (Ht - Lt)) * 100
  
  if(is.na(ADO_value)) # avoid NAN case, set NAN to be 0
    ADO_value <- 0
  
  return(ADO_value)
}


# Accumulation/Distribution (A/D) Oscillator (ADO) : [(Ht - Ot) + (Ct - Lt)] / [2 * (Ht - Lt)] * 100
# where: Ct- close price , Ht: high price , Lt- low price , Ot- open price 
ADO_model <- function(interval, now, data){
  targetData <- dta.filterByWindowFromNow( interval-1, now, data)
  Ct <- targetData$Close
  Ct <- Ct[interval]
  Ht <- targetData$High
  Ht <- max(Ht)
  Lt <- targetData$Low
  Lt <- min(Lt)
  Ot <- targetData$Open
  Ot <- Ot[1]
  ADO_value <- ((Ht - Ot) + (Ct - Lt)) /  (2 * (Ht - Lt)) * 100
  
  if(is.na(ADO_value)) # avoid NAN case, set NAN to be 0
    ADO_value <- 0
  
  return(ADO_value)
}


# Commodity Channel Index (CCI) : (TP - MA) / (MD * 0.015)
# where: TP: (Ht+Lt+Ct)/3 , MA: N interval close price sum / N interval , MD: N interval (MA - close price) sum / N interval
CCI_model <- function(interval, now, data){
  targetData <- dta.filterByWindowFromNow( interval-1, now, data)
  targetHighPrice <- targetData$High
  targetLowPrice <- targetData$Low
  targetClosePrice <- targetData$Close
  
  TP_interval <- (targetHighPrice + targetLowPrice + targetClosePrice) / 3
  TP <- TP_interval[length(TP_interval)]
  
  MA <- sum(targetClosePrice) / interval
  
  MD <- sum(abs(MA - targetClosePrice)) / interval
  
  CCI_value <- (TP - MA) / (MD * 0.015)
  
  return(CCI_value)
}


# Relative Strength Index (RSI) : 100 - (100 / 1 + (sum(UP)/n / sum(DW)/n))
# where: UP:upward price change , DW:downward price change during interval
# use close price
RSI_model <- function(interval, now, data){
  targetData <- dta.filterByWindowFromNow( interval-1, now, data)
  targetOpenPrice <- targetData$Open
  UP <- 0
  DW <- 0
  for(i in 2:length(targetOpenPrice)){
     if(targetOpenPrice[i] - targetOpenPrice[i-1] > 0)
        UP <- UP + (targetOpenPrice[i] - targetOpenPrice[i-1])
     if(targetOpenPrice[i] - targetOpenPrice[i-1] < 0)
        DW <- DW + (targetOpenPrice[i] - targetOpenPrice[i-1])
  }
  RS <- abs(UP / interval) / abs(DW / interval)
  RSI_value <- 100 - (100 / (1 + RS))
  
  return(RSI_value)
}


DI_model <- function( now, data){
  targetData <- dta.filterByWindowFromNow( 0, now, data)
  Ct <- targetData$Close
  Ht <- targetData$High
  Lt <- targetData$Low

  DI <- (Ht + Lt + 2 * Ct) / 4
  
  return(DI)
}


EMA12_model <- function(interval, now, data, lastEMA12){
  
  if (lastEMA12 == 0){
     targetData <- dta.filterByWindowFromNow( interval-1, now, data)
     targetHighPrice <- targetData$High
     targetLowPrice <- targetData$Low
     targetClosePrice <- targetData$Close
     
     DI <- (targetHighPrice + targetLowPrice + 2 * targetClosePrice) / 4
     EMA_value <- sum(DI) / interval
     
     return(EMA_value)
  }else{
     EMA_value <- lastEMA12 * (11 / 13) + DI_model(now, data) * (2 / 13)
     
     return(EMA_value)
  }
  
}


EMA26_model <- function(interval, now, data, lastEMA26){
  
  if (lastEMA26 == 0){
    targetData <- dta.filterByWindowFromNow( interval-1, now, data)
    targetHighPrice <- targetData$High
    targetLowPrice <- targetData$Low
    targetClosePrice <- targetData$Close
    
    DI <- (targetHighPrice + targetLowPrice + 2 * targetClosePrice) / 4
    EMA_value <- sum(DI) / interval
    
    return(EMA_value)
  }else{
    EMA_value <- lastEMA26 * (25 / 27) + DI_model(now, data) * (2 / 27)
    
    return(EMA_value)
  }
  
}


DIF_model <- function( EMA12_value, EMA26_value){

  DIF <- EMA12_value - EMA26_value
  
  return(DIF)
}



EMA12_model_v2 <- function(interval, now, data, lastEMA12){
  
  if (lastEMA12 == 0){
    targetData <- dta.filterByWindowFromNow( interval-1, now, data)
    targetClosePrice <- targetData$Close
    EMA_value <- sum(targetClosePrice) / interval
    
    return(EMA_value)
  }else{
    targetData <- dta.filterByWindowFromNow( 0, now, data)
    Ct <- targetData$Close
    EMA_value <- lastEMA12 * (11 / 13) + Ct * (2 / 13)
    
    return(EMA_value)
  }
  
}




EMA26_model_v2 <- function(interval, now, data, lastEMA26){
  
  if (lastEMA26 == 0){
    targetData <- dta.filterByWindowFromNow( interval-1, now, data)
    targetClosePrice <- targetData$Close
    EMA_value <- sum(targetClosePrice) / interval
    
    return(EMA_value)
  }else{
    targetData <- dta.filterByWindowFromNow( 0, now, data)
    Ct <- targetData$Close
    EMA_value <- lastEMA26 * (25 / 27) + Ct * (2 / 27)
    
    return(EMA_value)
  }
  
}


MV_model <- function(interval, now, data){
  targetData <- dta.filterByWindowFromNow( interval-1, now, data)
  targetTotalVolume <- targetData$TotalVolume
  MV_value <- sum(targetTotalVolume) / interval
  
  return(MV_value)
}


BIAS_model <- function(interval, now, data){
  targetData <- dta.filterByWindowFromNow( interval-1, now, data)
  targetClosePrice <- targetData$Close
  SMA_value <- sum(targetClosePrice) / interval
  
  currentClose <- dta.filterByWindowFromNow( 0, now, data)$Close
  
  BIAS <- (currentClose - SMA_value) / SMA_value * 100
  
  return(BIAS)
}



