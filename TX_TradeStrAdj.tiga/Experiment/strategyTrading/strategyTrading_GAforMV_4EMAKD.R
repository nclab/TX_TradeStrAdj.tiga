
setwd("C:/Users/cageb/Desktop/TX_TradeStrAdj.tiga/")
source("lib/include.R")
source("lib/eval.R")
source("lib/techIndicator_model.R")
#source("technicalIndicator/tradingSystem_lib/endDaySell_model.R")
#source("technicalIndicator/tradingSystem_lib/simulateTrading_model.R")
setup(res.path = "technicalIndicator/")

library("knitr")
library("GA")
library("genalg")


## load preprocessing workspace
load("Projfinal_preprocessing_workspace_final.Rdata")


## buy sell at the next_close version, use 4MA, 20MA, 4MV and KD (4MA begin up and 20MA begin up and 4MV begin up and K>D && 4MA begin down and 20MA begin down and 4MV begin up and K<D)
simulateTradingResult_4MA_4MV_KD <- function( beginDay_index, endDay_index, upLimit, downLimit){
  
  # == declare data frame to store information
  dayPredResultDetail <- data.frame(
    shiftT = as.numeric(0),
    buyDatetime = as.character.Date(0),
    sellDatetime = as.character.Date(0),
    predictUpDown = as.numeric(0),
    buyPrice = as.numeric(0),
    sellPrice = as.numeric(0),
    profit = as.numeric(0)
  )
  
  dayPredResultSummary <- data.frame(
    date = as.Date("1-1-1"),
    predictUpNum = as.numeric(0),
    predictUpCorrectNum = as.numeric(0),
    predictUpWrongNum = as.numeric(0),
    predictDownNum = as.numeric(0),
    predictDownCorrectNum = as.numeric(0),
    predictDownWrongNum = as.numeric(0),
    totalUpProfit = as.numeric(0),
    totalDownProfit = as.numeric(0),
    notGetBoundNum_Up = as.numeric(0),
    notGetBoundNum_Down = as.numeric(0)
  )
  
  #TFX[tradingSignal[1,]$Datetime == as.character.Date(TFX$Datetime),]
  
  for(mindex in beginDay_index:endDay_index){
    
    testMonth_pred <- tradingSignal[tradingSignal$testYMCount == mindex,]
    monthBC <- testMonth_pred[1,]$testDayCount
    monthEC <- testMonth_pred[nrow(testMonth_pred),]$testDayCount
    
    for(index in monthBC:monthEC){
      testDay_pred <- tradingSignal[tradingSignal$testDayCount == index,]
      actualBuySellData <- TFXdata[TFXdata$TFXdataCount == index,]  
      
      if (nrow(testDay_pred) != 0){  # this month doesn't have this day
        
        dayPredictUpNum <- 0
        dayPredictDownNum <- 0
        dayUpCorrectNum <- 0
        dayUpWrongNum <- 0
        dayDownCorrectNum <- 0
        dayDownWrongNum <- 0
        dayUpProfit <- 0
        dayDownProfit <- 0
        notGetBoundNum_Up <- 0
        notGetBoundNum_Down <- 0
        
        #### start test prediction  ####
        for (shiftT in 2:(nrow(testDay_pred)-1) ){
          # output shiftT
          updownFlag <- 0
          profit <- 0
          buyPrice <- 0
          sellPrice <- 0
          buyDatetime <- as.character.Date(0)
          sellDatetime <- as.character.Date(0)
          
          #cat("shiftT = ", shiftT, " ,SMA5 = ", testDay_pred[shiftT,]$SMA5, " ,SMA10 = ", testDay_pred[shiftT,]$SMA10, " ,KValue = ", testDay_pred[shiftT,]$KValue, " ,DValue = ", testDay_pred[shiftT,]$DValue, "\n")
          #
          if ((testDay_pred[shiftT,]$EMA4_flag == 1 && testDay_pred[shiftT-1,]$EMA4_flag == -1) && testDay_pred[shiftT,]$MV4_flag == 1 && (testDay_pred[shiftT,]$frontDay_dayKValue > testDay_pred[shiftT,]$frontDay_dayDValue) ){
            updownFlag <- 1
            dayPredictUpNum <- dayPredictUpNum + 1
            hold_flag_up <- 1
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= upLimit){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -downLimit){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              
            }
            
            # check has future or not 
            if(hold_flag_up == 1){
              #  cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              
              if (finalPrice - currentPrice > 0){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                #   cat("Predict Up, then the real is wrong!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Up <- notGetBoundNum_Up + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayUpProfit = ", dayUpProfit, "\n")
            dayUpProfit <- dayUpProfit + profit
          }
          
          if ((testDay_pred[shiftT,]$EMA4_flag == -1 && testDay_pred[shiftT-1,]$EMA4_flag == 1) && testDay_pred[shiftT,]$MV4_flag == 1  && (testDay_pred[shiftT,]$frontDay_dayKValue < testDay_pred[shiftT,]$frontDay_dayDValue) ){ 
            updownFlag <- -1
            dayPredictDownNum <- dayPredictDownNum + 1
            hold_flag_up <- 1
            
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= downLimit){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                hold_flag_up <- 0
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -upLimit){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is corrent!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              
            }
            # check has future or not 
            if(hold_flag_up == 1){
              #    cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              if (finalPrice - currentPrice > 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Down <- notGetBoundNum_Down + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayDownProfit = ", dayDownProfit, "\n")
            dayDownProfit <- dayDownProfit + profit
          }
          
          # store each day trading result          
          dayPredResultDetail <- rbind( dayPredResultDetail, data.frame(
            shiftT = shiftT,
            buyDatetime = buyDatetime,
            sellDatetime = sellDatetime,
            predictUpDown = updownFlag,
            buyPrice = buyPrice,
            sellPrice = sellPrice,
            profit = profit
          ))
          
          
        }
        
        # summary each day trading result
        dayPredResultSummary <- rbind( dayPredResultSummary, data.frame(
          date = testDay_pred[1,]$Dates,
          predictUpNum = dayPredictUpNum,
          predictUpCorrectNum = dayUpCorrectNum,
          predictUpWrongNum = dayUpWrongNum,
          predictDownNum = dayPredictDownNum,
          predictDownCorrectNum = dayDownCorrectNum,
          predictDownWrongNum = dayDownWrongNum,
          totalUpProfit = dayUpProfit,
          totalDownProfit = dayDownProfit,
          notGetBoundNum_Up = notGetBoundNum_Up,
          notGetBoundNum_Down = notGetBoundNum_Down
        ))
        
      }
      
    }
  }
  
  dayPredResultDetail <- dayPredResultDetail[-1,]
  dayPredResultSummary <- dayPredResultSummary[-1,]
  
  return_list <- list("dayPredResultDetail" = dayPredResultDetail, "dayPredResultSummary" = dayPredResultSummary)
  return(return_list)
  
}


simulateTradingResult_4MA_1MV_KD <- function( beginDay_index, endDay_index, upLimit, downLimit){
  
  # == declare data frame to store information
  dayPredResultDetail <- data.frame(
    shiftT = as.numeric(0),
    buyDatetime = as.character.Date(0),
    sellDatetime = as.character.Date(0),
    predictUpDown = as.numeric(0),
    buyPrice = as.numeric(0),
    sellPrice = as.numeric(0),
    profit = as.numeric(0)
  )
  
  dayPredResultSummary <- data.frame(
    date = as.Date("1-1-1"),
    predictUpNum = as.numeric(0),
    predictUpCorrectNum = as.numeric(0),
    predictUpWrongNum = as.numeric(0),
    predictDownNum = as.numeric(0),
    predictDownCorrectNum = as.numeric(0),
    predictDownWrongNum = as.numeric(0),
    totalUpProfit = as.numeric(0),
    totalDownProfit = as.numeric(0),
    notGetBoundNum_Up = as.numeric(0),
    notGetBoundNum_Down = as.numeric(0)
  )
  
  #TFX[tradingSignal[1,]$Datetime == as.character.Date(TFX$Datetime),]
  
  for(mindex in beginDay_index:endDay_index){
    
    testMonth_pred <- tradingSignal[tradingSignal$testYMCount == mindex,]
    monthBC <- testMonth_pred[1,]$testDayCount
    monthEC <- testMonth_pred[nrow(testMonth_pred),]$testDayCount
    
    for(index in monthBC:monthEC){
      testDay_pred <- tradingSignal[tradingSignal$testDayCount == index,]
      actualBuySellData <- TFXdata[TFXdata$TFXdataCount == index,]  
      
      if (nrow(testDay_pred) != 0){  # this month doesn't have this day
        
        dayPredictUpNum <- 0
        dayPredictDownNum <- 0
        dayUpCorrectNum <- 0
        dayUpWrongNum <- 0
        dayDownCorrectNum <- 0
        dayDownWrongNum <- 0
        dayUpProfit <- 0
        dayDownProfit <- 0
        notGetBoundNum_Up <- 0
        notGetBoundNum_Down <- 0
        
        #### start test prediction  ####
        for (shiftT in 2:(nrow(testDay_pred)-1) ){
          # output shiftT
          updownFlag <- 0
          profit <- 0
          buyPrice <- 0
          sellPrice <- 0
          buyDatetime <- as.character.Date(0)
          sellDatetime <- as.character.Date(0)
          
          #cat("shiftT = ", shiftT, " ,SMA5 = ", testDay_pred[shiftT,]$SMA5, " ,SMA10 = ", testDay_pred[shiftT,]$SMA10, " ,KValue = ", testDay_pred[shiftT,]$KValue, " ,DValue = ", testDay_pred[shiftT,]$DValue, "\n")
          #
          if ((testDay_pred[shiftT,]$EMA4_flag == 1 && testDay_pred[shiftT-1,]$EMA4_flag == -1) && testDay_pred[shiftT,]$MV1_flag == 1 && (testDay_pred[shiftT,]$frontDay_dayKValue > testDay_pred[shiftT,]$frontDay_dayDValue) ){
            updownFlag <- 1
            dayPredictUpNum <- dayPredictUpNum + 1
            hold_flag_up <- 1
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= upLimit){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -downLimit){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              
            }
            
            # check has future or not 
            if(hold_flag_up == 1){
              #  cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              
              if (finalPrice - currentPrice > 0){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                #   cat("Predict Up, then the real is wrong!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Up <- notGetBoundNum_Up + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayUpProfit = ", dayUpProfit, "\n")
            dayUpProfit <- dayUpProfit + profit
          }
          
          if ((testDay_pred[shiftT,]$EMA4_flag == -1 && testDay_pred[shiftT-1,]$EMA4_flag == 1) && testDay_pred[shiftT,]$MV1_flag == 1  && (testDay_pred[shiftT,]$frontDay_dayKValue < testDay_pred[shiftT,]$frontDay_dayDValue) ){ 
            updownFlag <- -1
            dayPredictDownNum <- dayPredictDownNum + 1
            hold_flag_up <- 1
            
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= downLimit){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                hold_flag_up <- 0
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -upLimit){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is corrent!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              
            }
            # check has future or not 
            if(hold_flag_up == 1){
              #    cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              if (finalPrice - currentPrice > 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Down <- notGetBoundNum_Down + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayDownProfit = ", dayDownProfit, "\n")
            dayDownProfit <- dayDownProfit + profit
          }
          
          # store each day trading result          
          dayPredResultDetail <- rbind( dayPredResultDetail, data.frame(
            shiftT = shiftT,
            buyDatetime = buyDatetime,
            sellDatetime = sellDatetime,
            predictUpDown = updownFlag,
            buyPrice = buyPrice,
            sellPrice = sellPrice,
            profit = profit
          ))
          
          
        }
        
        # summary each day trading result
        dayPredResultSummary <- rbind( dayPredResultSummary, data.frame(
          date = testDay_pred[1,]$Dates,
          predictUpNum = dayPredictUpNum,
          predictUpCorrectNum = dayUpCorrectNum,
          predictUpWrongNum = dayUpWrongNum,
          predictDownNum = dayPredictDownNum,
          predictDownCorrectNum = dayDownCorrectNum,
          predictDownWrongNum = dayDownWrongNum,
          totalUpProfit = dayUpProfit,
          totalDownProfit = dayDownProfit,
          notGetBoundNum_Up = notGetBoundNum_Up,
          notGetBoundNum_Down = notGetBoundNum_Down
        ))
        
      }
      
    }
  }
  
  dayPredResultDetail <- dayPredResultDetail[-1,]
  dayPredResultSummary <- dayPredResultSummary[-1,]
  
  return_list <- list("dayPredResultDetail" = dayPredResultDetail, "dayPredResultSummary" = dayPredResultSummary)
  return(return_list)
  
}


simulateTradingResult_4MA_2MV_KD <- function( beginDay_index, endDay_index, upLimit, downLimit){
  
  # == declare data frame to store information
  dayPredResultDetail <- data.frame(
    shiftT = as.numeric(0),
    buyDatetime = as.character.Date(0),
    sellDatetime = as.character.Date(0),
    predictUpDown = as.numeric(0),
    buyPrice = as.numeric(0),
    sellPrice = as.numeric(0),
    profit = as.numeric(0)
  )
  
  dayPredResultSummary <- data.frame(
    date = as.Date("1-1-1"),
    predictUpNum = as.numeric(0),
    predictUpCorrectNum = as.numeric(0),
    predictUpWrongNum = as.numeric(0),
    predictDownNum = as.numeric(0),
    predictDownCorrectNum = as.numeric(0),
    predictDownWrongNum = as.numeric(0),
    totalUpProfit = as.numeric(0),
    totalDownProfit = as.numeric(0),
    notGetBoundNum_Up = as.numeric(0),
    notGetBoundNum_Down = as.numeric(0)
  )
  
  #TFX[tradingSignal[1,]$Datetime == as.character.Date(TFX$Datetime),]
  
  for(mindex in beginDay_index:endDay_index){
    
    testMonth_pred <- tradingSignal[tradingSignal$testYMCount == mindex,]
    monthBC <- testMonth_pred[1,]$testDayCount
    monthEC <- testMonth_pred[nrow(testMonth_pred),]$testDayCount
    
    for(index in monthBC:monthEC){
      testDay_pred <- tradingSignal[tradingSignal$testDayCount == index,]
      actualBuySellData <- TFXdata[TFXdata$TFXdataCount == index,]  
      
      if (nrow(testDay_pred) != 0){  # this month doesn't have this day
        
        dayPredictUpNum <- 0
        dayPredictDownNum <- 0
        dayUpCorrectNum <- 0
        dayUpWrongNum <- 0
        dayDownCorrectNum <- 0
        dayDownWrongNum <- 0
        dayUpProfit <- 0
        dayDownProfit <- 0
        notGetBoundNum_Up <- 0
        notGetBoundNum_Down <- 0
        
        #### start test prediction  ####
        for (shiftT in 2:(nrow(testDay_pred)-1) ){
          # output shiftT
          updownFlag <- 0
          profit <- 0
          buyPrice <- 0
          sellPrice <- 0
          buyDatetime <- as.character.Date(0)
          sellDatetime <- as.character.Date(0)
          
          #cat("shiftT = ", shiftT, " ,SMA5 = ", testDay_pred[shiftT,]$SMA5, " ,SMA10 = ", testDay_pred[shiftT,]$SMA10, " ,KValue = ", testDay_pred[shiftT,]$KValue, " ,DValue = ", testDay_pred[shiftT,]$DValue, "\n")
          #
          if ((testDay_pred[shiftT,]$EMA4_flag == 1 && testDay_pred[shiftT-1,]$EMA4_flag == -1) && testDay_pred[shiftT,]$MV2_flag == 1 && (testDay_pred[shiftT,]$frontDay_dayKValue > testDay_pred[shiftT,]$frontDay_dayDValue) ){
            updownFlag <- 1
            dayPredictUpNum <- dayPredictUpNum + 1
            hold_flag_up <- 1
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= upLimit){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -downLimit){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              
            }
            
            # check has future or not 
            if(hold_flag_up == 1){
              #  cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              
              if (finalPrice - currentPrice > 0){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                #   cat("Predict Up, then the real is wrong!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Up <- notGetBoundNum_Up + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayUpProfit = ", dayUpProfit, "\n")
            dayUpProfit <- dayUpProfit + profit
          }
          
          if ((testDay_pred[shiftT,]$EMA4_flag == -1 && testDay_pred[shiftT-1,]$EMA4_flag == 1) && testDay_pred[shiftT,]$MV2_flag == 1  && (testDay_pred[shiftT,]$frontDay_dayKValue < testDay_pred[shiftT,]$frontDay_dayDValue) ){ 
            updownFlag <- -1
            dayPredictDownNum <- dayPredictDownNum + 1
            hold_flag_up <- 1
            
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= downLimit){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                hold_flag_up <- 0
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -upLimit){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is corrent!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              
            }
            # check has future or not 
            if(hold_flag_up == 1){
              #    cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              if (finalPrice - currentPrice > 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Down <- notGetBoundNum_Down + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayDownProfit = ", dayDownProfit, "\n")
            dayDownProfit <- dayDownProfit + profit
          }
          
          # store each day trading result          
          dayPredResultDetail <- rbind( dayPredResultDetail, data.frame(
            shiftT = shiftT,
            buyDatetime = buyDatetime,
            sellDatetime = sellDatetime,
            predictUpDown = updownFlag,
            buyPrice = buyPrice,
            sellPrice = sellPrice,
            profit = profit
          ))
          
          
        }
        
        # summary each day trading result
        dayPredResultSummary <- rbind( dayPredResultSummary, data.frame(
          date = testDay_pred[1,]$Dates,
          predictUpNum = dayPredictUpNum,
          predictUpCorrectNum = dayUpCorrectNum,
          predictUpWrongNum = dayUpWrongNum,
          predictDownNum = dayPredictDownNum,
          predictDownCorrectNum = dayDownCorrectNum,
          predictDownWrongNum = dayDownWrongNum,
          totalUpProfit = dayUpProfit,
          totalDownProfit = dayDownProfit,
          notGetBoundNum_Up = notGetBoundNum_Up,
          notGetBoundNum_Down = notGetBoundNum_Down
        ))
        
      }
      
    }
  }
  
  dayPredResultDetail <- dayPredResultDetail[-1,]
  dayPredResultSummary <- dayPredResultSummary[-1,]
  
  return_list <- list("dayPredResultDetail" = dayPredResultDetail, "dayPredResultSummary" = dayPredResultSummary)
  return(return_list)
  
}


simulateTradingResult_4MA_3MV_KD <- function( beginDay_index, endDay_index, upLimit, downLimit){
  
  # == declare data frame to store information
  dayPredResultDetail <- data.frame(
    shiftT = as.numeric(0),
    buyDatetime = as.character.Date(0),
    sellDatetime = as.character.Date(0),
    predictUpDown = as.numeric(0),
    buyPrice = as.numeric(0),
    sellPrice = as.numeric(0),
    profit = as.numeric(0)
  )
  
  dayPredResultSummary <- data.frame(
    date = as.Date("1-1-1"),
    predictUpNum = as.numeric(0),
    predictUpCorrectNum = as.numeric(0),
    predictUpWrongNum = as.numeric(0),
    predictDownNum = as.numeric(0),
    predictDownCorrectNum = as.numeric(0),
    predictDownWrongNum = as.numeric(0),
    totalUpProfit = as.numeric(0),
    totalDownProfit = as.numeric(0),
    notGetBoundNum_Up = as.numeric(0),
    notGetBoundNum_Down = as.numeric(0)
  )
  
  #TFX[tradingSignal[1,]$Datetime == as.character.Date(TFX$Datetime),]
  
  for(mindex in beginDay_index:endDay_index){
    
    testMonth_pred <- tradingSignal[tradingSignal$testYMCount == mindex,]
    monthBC <- testMonth_pred[1,]$testDayCount
    monthEC <- testMonth_pred[nrow(testMonth_pred),]$testDayCount
    
    for(index in monthBC:monthEC){
      testDay_pred <- tradingSignal[tradingSignal$testDayCount == index,]
      actualBuySellData <- TFXdata[TFXdata$TFXdataCount == index,]  
      
      if (nrow(testDay_pred) != 0){  # this month doesn't have this day
        
        dayPredictUpNum <- 0
        dayPredictDownNum <- 0
        dayUpCorrectNum <- 0
        dayUpWrongNum <- 0
        dayDownCorrectNum <- 0
        dayDownWrongNum <- 0
        dayUpProfit <- 0
        dayDownProfit <- 0
        notGetBoundNum_Up <- 0
        notGetBoundNum_Down <- 0
        
        #### start test prediction  ####
        for (shiftT in 2:(nrow(testDay_pred)-1) ){
          # output shiftT
          updownFlag <- 0
          profit <- 0
          buyPrice <- 0
          sellPrice <- 0
          buyDatetime <- as.character.Date(0)
          sellDatetime <- as.character.Date(0)
          
          #cat("shiftT = ", shiftT, " ,SMA5 = ", testDay_pred[shiftT,]$SMA5, " ,SMA10 = ", testDay_pred[shiftT,]$SMA10, " ,KValue = ", testDay_pred[shiftT,]$KValue, " ,DValue = ", testDay_pred[shiftT,]$DValue, "\n")
          #
          if ((testDay_pred[shiftT,]$EMA4_flag == 1 && testDay_pred[shiftT-1,]$EMA4_flag == -1) && testDay_pred[shiftT,]$MV3_flag == 1 && (testDay_pred[shiftT,]$frontDay_dayKValue > testDay_pred[shiftT,]$frontDay_dayDValue) ){
            updownFlag <- 1
            dayPredictUpNum <- dayPredictUpNum + 1
            hold_flag_up <- 1
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= upLimit){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -downLimit){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              
            }
            
            # check has future or not 
            if(hold_flag_up == 1){
              #  cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              
              if (finalPrice - currentPrice > 0){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                #   cat("Predict Up, then the real is wrong!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Up <- notGetBoundNum_Up + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayUpProfit = ", dayUpProfit, "\n")
            dayUpProfit <- dayUpProfit + profit
          }
          
          if ((testDay_pred[shiftT,]$EMA4_flag == -1 && testDay_pred[shiftT-1,]$EMA4_flag == 1) && testDay_pred[shiftT,]$MV3_flag == 1  && (testDay_pred[shiftT,]$frontDay_dayKValue < testDay_pred[shiftT,]$frontDay_dayDValue) ){ 
            updownFlag <- -1
            dayPredictDownNum <- dayPredictDownNum + 1
            hold_flag_up <- 1
            
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= downLimit){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                hold_flag_up <- 0
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -upLimit){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is corrent!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              
            }
            # check has future or not 
            if(hold_flag_up == 1){
              #    cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              if (finalPrice - currentPrice > 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Down <- notGetBoundNum_Down + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayDownProfit = ", dayDownProfit, "\n")
            dayDownProfit <- dayDownProfit + profit
          }
          
          # store each day trading result          
          dayPredResultDetail <- rbind( dayPredResultDetail, data.frame(
            shiftT = shiftT,
            buyDatetime = buyDatetime,
            sellDatetime = sellDatetime,
            predictUpDown = updownFlag,
            buyPrice = buyPrice,
            sellPrice = sellPrice,
            profit = profit
          ))
          
          
        }
        
        # summary each day trading result
        dayPredResultSummary <- rbind( dayPredResultSummary, data.frame(
          date = testDay_pred[1,]$Dates,
          predictUpNum = dayPredictUpNum,
          predictUpCorrectNum = dayUpCorrectNum,
          predictUpWrongNum = dayUpWrongNum,
          predictDownNum = dayPredictDownNum,
          predictDownCorrectNum = dayDownCorrectNum,
          predictDownWrongNum = dayDownWrongNum,
          totalUpProfit = dayUpProfit,
          totalDownProfit = dayDownProfit,
          notGetBoundNum_Up = notGetBoundNum_Up,
          notGetBoundNum_Down = notGetBoundNum_Down
        ))
        
      }
      
    }
  }
  
  dayPredResultDetail <- dayPredResultDetail[-1,]
  dayPredResultSummary <- dayPredResultSummary[-1,]
  
  return_list <- list("dayPredResultDetail" = dayPredResultDetail, "dayPredResultSummary" = dayPredResultSummary)
  return(return_list)
  
}


simulateTradingResult_4MA_5MV_KD <- function( beginDay_index, endDay_index, upLimit, downLimit){
  
  # == declare data frame to store information
  dayPredResultDetail <- data.frame(
    shiftT = as.numeric(0),
    buyDatetime = as.character.Date(0),
    sellDatetime = as.character.Date(0),
    predictUpDown = as.numeric(0),
    buyPrice = as.numeric(0),
    sellPrice = as.numeric(0),
    profit = as.numeric(0)
  )
  
  dayPredResultSummary <- data.frame(
    date = as.Date("1-1-1"),
    predictUpNum = as.numeric(0),
    predictUpCorrectNum = as.numeric(0),
    predictUpWrongNum = as.numeric(0),
    predictDownNum = as.numeric(0),
    predictDownCorrectNum = as.numeric(0),
    predictDownWrongNum = as.numeric(0),
    totalUpProfit = as.numeric(0),
    totalDownProfit = as.numeric(0),
    notGetBoundNum_Up = as.numeric(0),
    notGetBoundNum_Down = as.numeric(0)
  )
  
  #TFX[tradingSignal[1,]$Datetime == as.character.Date(TFX$Datetime),]
  
  for(mindex in beginDay_index:endDay_index){
    
    testMonth_pred <- tradingSignal[tradingSignal$testYMCount == mindex,]
    monthBC <- testMonth_pred[1,]$testDayCount
    monthEC <- testMonth_pred[nrow(testMonth_pred),]$testDayCount
    
    for(index in monthBC:monthEC){
      testDay_pred <- tradingSignal[tradingSignal$testDayCount == index,]
      actualBuySellData <- TFXdata[TFXdata$TFXdataCount == index,]  
      
      if (nrow(testDay_pred) != 0){  # this month doesn't have this day
        
        dayPredictUpNum <- 0
        dayPredictDownNum <- 0
        dayUpCorrectNum <- 0
        dayUpWrongNum <- 0
        dayDownCorrectNum <- 0
        dayDownWrongNum <- 0
        dayUpProfit <- 0
        dayDownProfit <- 0
        notGetBoundNum_Up <- 0
        notGetBoundNum_Down <- 0
        
        #### start test prediction  ####
        for (shiftT in 2:(nrow(testDay_pred)-1) ){
          # output shiftT
          updownFlag <- 0
          profit <- 0
          buyPrice <- 0
          sellPrice <- 0
          buyDatetime <- as.character.Date(0)
          sellDatetime <- as.character.Date(0)
          
          #cat("shiftT = ", shiftT, " ,SMA5 = ", testDay_pred[shiftT,]$SMA5, " ,SMA10 = ", testDay_pred[shiftT,]$SMA10, " ,KValue = ", testDay_pred[shiftT,]$KValue, " ,DValue = ", testDay_pred[shiftT,]$DValue, "\n")
          #
          if ((testDay_pred[shiftT,]$EMA4_flag == 1 && testDay_pred[shiftT-1,]$EMA4_flag == -1) && testDay_pred[shiftT,]$MV5_flag == 1 && (testDay_pred[shiftT,]$frontDay_dayKValue > testDay_pred[shiftT,]$frontDay_dayDValue) ){
            updownFlag <- 1
            dayPredictUpNum <- dayPredictUpNum + 1
            hold_flag_up <- 1
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= upLimit){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -downLimit){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              
            }
            
            # check has future or not 
            if(hold_flag_up == 1){
              #  cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              
              if (finalPrice - currentPrice > 0){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                #   cat("Predict Up, then the real is wrong!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Up <- notGetBoundNum_Up + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayUpProfit = ", dayUpProfit, "\n")
            dayUpProfit <- dayUpProfit + profit
          }
          
          if ((testDay_pred[shiftT,]$EMA4_flag == -1 && testDay_pred[shiftT-1,]$EMA4_flag == 1) && testDay_pred[shiftT,]$MV5_flag == 1  && (testDay_pred[shiftT,]$frontDay_dayKValue < testDay_pred[shiftT,]$frontDay_dayDValue) ){ 
            updownFlag <- -1
            dayPredictDownNum <- dayPredictDownNum + 1
            hold_flag_up <- 1
            
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= downLimit){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                hold_flag_up <- 0
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -upLimit){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is corrent!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              
            }
            # check has future or not 
            if(hold_flag_up == 1){
              #    cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              if (finalPrice - currentPrice > 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Down <- notGetBoundNum_Down + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayDownProfit = ", dayDownProfit, "\n")
            dayDownProfit <- dayDownProfit + profit
          }
          
          # store each day trading result          
          dayPredResultDetail <- rbind( dayPredResultDetail, data.frame(
            shiftT = shiftT,
            buyDatetime = buyDatetime,
            sellDatetime = sellDatetime,
            predictUpDown = updownFlag,
            buyPrice = buyPrice,
            sellPrice = sellPrice,
            profit = profit
          ))
          
          
        }
        
        # summary each day trading result
        dayPredResultSummary <- rbind( dayPredResultSummary, data.frame(
          date = testDay_pred[1,]$Dates,
          predictUpNum = dayPredictUpNum,
          predictUpCorrectNum = dayUpCorrectNum,
          predictUpWrongNum = dayUpWrongNum,
          predictDownNum = dayPredictDownNum,
          predictDownCorrectNum = dayDownCorrectNum,
          predictDownWrongNum = dayDownWrongNum,
          totalUpProfit = dayUpProfit,
          totalDownProfit = dayDownProfit,
          notGetBoundNum_Up = notGetBoundNum_Up,
          notGetBoundNum_Down = notGetBoundNum_Down
        ))
        
      }
      
    }
  }
  
  dayPredResultDetail <- dayPredResultDetail[-1,]
  dayPredResultSummary <- dayPredResultSummary[-1,]
  
  return_list <- list("dayPredResultDetail" = dayPredResultDetail, "dayPredResultSummary" = dayPredResultSummary)
  return(return_list)
  
}


simulateTradingResult_4MA_10MV_KD <- function( beginDay_index, endDay_index, upLimit, downLimit){
  
  # == declare data frame to store information
  dayPredResultDetail <- data.frame(
    shiftT = as.numeric(0),
    buyDatetime = as.character.Date(0),
    sellDatetime = as.character.Date(0),
    predictUpDown = as.numeric(0),
    buyPrice = as.numeric(0),
    sellPrice = as.numeric(0),
    profit = as.numeric(0)
  )
  
  dayPredResultSummary <- data.frame(
    date = as.Date("1-1-1"),
    predictUpNum = as.numeric(0),
    predictUpCorrectNum = as.numeric(0),
    predictUpWrongNum = as.numeric(0),
    predictDownNum = as.numeric(0),
    predictDownCorrectNum = as.numeric(0),
    predictDownWrongNum = as.numeric(0),
    totalUpProfit = as.numeric(0),
    totalDownProfit = as.numeric(0),
    notGetBoundNum_Up = as.numeric(0),
    notGetBoundNum_Down = as.numeric(0)
  )
  
  #TFX[tradingSignal[1,]$Datetime == as.character.Date(TFX$Datetime),]
  
  for(mindex in beginDay_index:endDay_index){
    
    testMonth_pred <- tradingSignal[tradingSignal$testYMCount == mindex,]
    monthBC <- testMonth_pred[1,]$testDayCount
    monthEC <- testMonth_pred[nrow(testMonth_pred),]$testDayCount
    
    for(index in monthBC:monthEC){
      testDay_pred <- tradingSignal[tradingSignal$testDayCount == index,]
      actualBuySellData <- TFXdata[TFXdata$TFXdataCount == index,]  
      
      if (nrow(testDay_pred) != 0){  # this month doesn't have this day
        
        dayPredictUpNum <- 0
        dayPredictDownNum <- 0
        dayUpCorrectNum <- 0
        dayUpWrongNum <- 0
        dayDownCorrectNum <- 0
        dayDownWrongNum <- 0
        dayUpProfit <- 0
        dayDownProfit <- 0
        notGetBoundNum_Up <- 0
        notGetBoundNum_Down <- 0
        
        #### start test prediction  ####
        for (shiftT in 2:(nrow(testDay_pred)-1) ){
          # output shiftT
          updownFlag <- 0
          profit <- 0
          buyPrice <- 0
          sellPrice <- 0
          buyDatetime <- as.character.Date(0)
          sellDatetime <- as.character.Date(0)
          
          #cat("shiftT = ", shiftT, " ,SMA5 = ", testDay_pred[shiftT,]$SMA5, " ,SMA10 = ", testDay_pred[shiftT,]$SMA10, " ,KValue = ", testDay_pred[shiftT,]$KValue, " ,DValue = ", testDay_pred[shiftT,]$DValue, "\n")
          #
          if ((testDay_pred[shiftT,]$EMA4_flag == 1 && testDay_pred[shiftT-1,]$EMA4_flag == -1) && testDay_pred[shiftT,]$MV10_flag == 1 && (testDay_pred[shiftT,]$frontDay_dayKValue > testDay_pred[shiftT,]$frontDay_dayDValue) ){
            updownFlag <- 1
            dayPredictUpNum <- dayPredictUpNum + 1
            hold_flag_up <- 1
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= upLimit){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -downLimit){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              
            }
            
            # check has future or not 
            if(hold_flag_up == 1){
              #  cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              
              if (finalPrice - currentPrice > 0){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                #   cat("Predict Up, then the real is wrong!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Up <- notGetBoundNum_Up + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayUpProfit = ", dayUpProfit, "\n")
            dayUpProfit <- dayUpProfit + profit
          }
          
          if ((testDay_pred[shiftT,]$EMA4_flag == -1 && testDay_pred[shiftT-1,]$EMA4_flag == 1) && testDay_pred[shiftT,]$MV10_flag == 1  && (testDay_pred[shiftT,]$frontDay_dayKValue < testDay_pred[shiftT,]$frontDay_dayDValue) ){ 
            updownFlag <- -1
            dayPredictDownNum <- dayPredictDownNum + 1
            hold_flag_up <- 1
            
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= downLimit){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                hold_flag_up <- 0
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -upLimit){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is corrent!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              
            }
            # check has future or not 
            if(hold_flag_up == 1){
              #    cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              if (finalPrice - currentPrice > 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Down <- notGetBoundNum_Down + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayDownProfit = ", dayDownProfit, "\n")
            dayDownProfit <- dayDownProfit + profit
          }
          
          # store each day trading result          
          dayPredResultDetail <- rbind( dayPredResultDetail, data.frame(
            shiftT = shiftT,
            buyDatetime = buyDatetime,
            sellDatetime = sellDatetime,
            predictUpDown = updownFlag,
            buyPrice = buyPrice,
            sellPrice = sellPrice,
            profit = profit
          ))
          
          
        }
        
        # summary each day trading result
        dayPredResultSummary <- rbind( dayPredResultSummary, data.frame(
          date = testDay_pred[1,]$Dates,
          predictUpNum = dayPredictUpNum,
          predictUpCorrectNum = dayUpCorrectNum,
          predictUpWrongNum = dayUpWrongNum,
          predictDownNum = dayPredictDownNum,
          predictDownCorrectNum = dayDownCorrectNum,
          predictDownWrongNum = dayDownWrongNum,
          totalUpProfit = dayUpProfit,
          totalDownProfit = dayDownProfit,
          notGetBoundNum_Up = notGetBoundNum_Up,
          notGetBoundNum_Down = notGetBoundNum_Down
        ))
        
      }
      
    }
  }
  
  dayPredResultDetail <- dayPredResultDetail[-1,]
  dayPredResultSummary <- dayPredResultSummary[-1,]
  
  return_list <- list("dayPredResultDetail" = dayPredResultDetail, "dayPredResultSummary" = dayPredResultSummary)
  return(return_list)
  
}


simulateTradingResult_4MA_15MV_KD <- function( beginDay_index, endDay_index, upLimit, downLimit){
  
  # == declare data frame to store information
  dayPredResultDetail <- data.frame(
    shiftT = as.numeric(0),
    buyDatetime = as.character.Date(0),
    sellDatetime = as.character.Date(0),
    predictUpDown = as.numeric(0),
    buyPrice = as.numeric(0),
    sellPrice = as.numeric(0),
    profit = as.numeric(0)
  )
  
  dayPredResultSummary <- data.frame(
    date = as.Date("1-1-1"),
    predictUpNum = as.numeric(0),
    predictUpCorrectNum = as.numeric(0),
    predictUpWrongNum = as.numeric(0),
    predictDownNum = as.numeric(0),
    predictDownCorrectNum = as.numeric(0),
    predictDownWrongNum = as.numeric(0),
    totalUpProfit = as.numeric(0),
    totalDownProfit = as.numeric(0),
    notGetBoundNum_Up = as.numeric(0),
    notGetBoundNum_Down = as.numeric(0)
  )
  
  #TFX[tradingSignal[1,]$Datetime == as.character.Date(TFX$Datetime),]
  
  for(mindex in beginDay_index:endDay_index){
    
    testMonth_pred <- tradingSignal[tradingSignal$testYMCount == mindex,]
    monthBC <- testMonth_pred[1,]$testDayCount
    monthEC <- testMonth_pred[nrow(testMonth_pred),]$testDayCount
    
    for(index in monthBC:monthEC){
      testDay_pred <- tradingSignal[tradingSignal$testDayCount == index,]
      actualBuySellData <- TFXdata[TFXdata$TFXdataCount == index,]  
      
      if (nrow(testDay_pred) != 0){  # this month doesn't have this day
        
        dayPredictUpNum <- 0
        dayPredictDownNum <- 0
        dayUpCorrectNum <- 0
        dayUpWrongNum <- 0
        dayDownCorrectNum <- 0
        dayDownWrongNum <- 0
        dayUpProfit <- 0
        dayDownProfit <- 0
        notGetBoundNum_Up <- 0
        notGetBoundNum_Down <- 0
        
        #### start test prediction  ####
        for (shiftT in 2:(nrow(testDay_pred)-1) ){
          # output shiftT
          updownFlag <- 0
          profit <- 0
          buyPrice <- 0
          sellPrice <- 0
          buyDatetime <- as.character.Date(0)
          sellDatetime <- as.character.Date(0)
          
          #cat("shiftT = ", shiftT, " ,SMA5 = ", testDay_pred[shiftT,]$SMA5, " ,SMA10 = ", testDay_pred[shiftT,]$SMA10, " ,KValue = ", testDay_pred[shiftT,]$KValue, " ,DValue = ", testDay_pred[shiftT,]$DValue, "\n")
          #
          if ((testDay_pred[shiftT,]$EMA4_flag == 1 && testDay_pred[shiftT-1,]$EMA4_flag == -1) && testDay_pred[shiftT,]$MV15_flag == 1 && (testDay_pred[shiftT,]$frontDay_dayKValue > testDay_pred[shiftT,]$frontDay_dayDValue) ){
            updownFlag <- 1
            dayPredictUpNum <- dayPredictUpNum + 1
            hold_flag_up <- 1
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= upLimit){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -downLimit){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              
            }
            
            # check has future or not 
            if(hold_flag_up == 1){
              #  cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              
              if (finalPrice - currentPrice > 0){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                #   cat("Predict Up, then the real is wrong!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Up <- notGetBoundNum_Up + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayUpProfit = ", dayUpProfit, "\n")
            dayUpProfit <- dayUpProfit + profit
          }
          
          if ((testDay_pred[shiftT,]$EMA4_flag == -1 && testDay_pred[shiftT-1,]$EMA4_flag == 1) && testDay_pred[shiftT,]$MV15_flag == 1  && (testDay_pred[shiftT,]$frontDay_dayKValue < testDay_pred[shiftT,]$frontDay_dayDValue) ){ 
            updownFlag <- -1
            dayPredictDownNum <- dayPredictDownNum + 1
            hold_flag_up <- 1
            
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= downLimit){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                hold_flag_up <- 0
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -upLimit){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is corrent!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              
            }
            # check has future or not 
            if(hold_flag_up == 1){
              #    cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              if (finalPrice - currentPrice > 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Down <- notGetBoundNum_Down + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayDownProfit = ", dayDownProfit, "\n")
            dayDownProfit <- dayDownProfit + profit
          }
          
          # store each day trading result          
          dayPredResultDetail <- rbind( dayPredResultDetail, data.frame(
            shiftT = shiftT,
            buyDatetime = buyDatetime,
            sellDatetime = sellDatetime,
            predictUpDown = updownFlag,
            buyPrice = buyPrice,
            sellPrice = sellPrice,
            profit = profit
          ))
          
          
        }
        
        # summary each day trading result
        dayPredResultSummary <- rbind( dayPredResultSummary, data.frame(
          date = testDay_pred[1,]$Dates,
          predictUpNum = dayPredictUpNum,
          predictUpCorrectNum = dayUpCorrectNum,
          predictUpWrongNum = dayUpWrongNum,
          predictDownNum = dayPredictDownNum,
          predictDownCorrectNum = dayDownCorrectNum,
          predictDownWrongNum = dayDownWrongNum,
          totalUpProfit = dayUpProfit,
          totalDownProfit = dayDownProfit,
          notGetBoundNum_Up = notGetBoundNum_Up,
          notGetBoundNum_Down = notGetBoundNum_Down
        ))
        
      }
      
    }
  }
  
  dayPredResultDetail <- dayPredResultDetail[-1,]
  dayPredResultSummary <- dayPredResultSummary[-1,]
  
  return_list <- list("dayPredResultDetail" = dayPredResultDetail, "dayPredResultSummary" = dayPredResultSummary)
  return(return_list)
  
}


simulateTradingResult_4MA_20MV_KD <- function( beginDay_index, endDay_index, upLimit, downLimit){
  
  # == declare data frame to store information
  dayPredResultDetail <- data.frame(
    shiftT = as.numeric(0),
    buyDatetime = as.character.Date(0),
    sellDatetime = as.character.Date(0),
    predictUpDown = as.numeric(0),
    buyPrice = as.numeric(0),
    sellPrice = as.numeric(0),
    profit = as.numeric(0)
  )
  
  dayPredResultSummary <- data.frame(
    date = as.Date("1-1-1"),
    predictUpNum = as.numeric(0),
    predictUpCorrectNum = as.numeric(0),
    predictUpWrongNum = as.numeric(0),
    predictDownNum = as.numeric(0),
    predictDownCorrectNum = as.numeric(0),
    predictDownWrongNum = as.numeric(0),
    totalUpProfit = as.numeric(0),
    totalDownProfit = as.numeric(0),
    notGetBoundNum_Up = as.numeric(0),
    notGetBoundNum_Down = as.numeric(0)
  )
  
  #TFX[tradingSignal[1,]$Datetime == as.character.Date(TFX$Datetime),]
  
  for(mindex in beginDay_index:endDay_index){
    
    testMonth_pred <- tradingSignal[tradingSignal$testYMCount == mindex,]
    monthBC <- testMonth_pred[1,]$testDayCount
    monthEC <- testMonth_pred[nrow(testMonth_pred),]$testDayCount
    
    for(index in monthBC:monthEC){
      testDay_pred <- tradingSignal[tradingSignal$testDayCount == index,]
      actualBuySellData <- TFXdata[TFXdata$TFXdataCount == index,]  
      
      if (nrow(testDay_pred) != 0){  # this month doesn't have this day
        
        dayPredictUpNum <- 0
        dayPredictDownNum <- 0
        dayUpCorrectNum <- 0
        dayUpWrongNum <- 0
        dayDownCorrectNum <- 0
        dayDownWrongNum <- 0
        dayUpProfit <- 0
        dayDownProfit <- 0
        notGetBoundNum_Up <- 0
        notGetBoundNum_Down <- 0
        
        #### start test prediction  ####
        for (shiftT in 2:(nrow(testDay_pred)-1) ){
          # output shiftT
          updownFlag <- 0
          profit <- 0
          buyPrice <- 0
          sellPrice <- 0
          buyDatetime <- as.character.Date(0)
          sellDatetime <- as.character.Date(0)
          
          #cat("shiftT = ", shiftT, " ,SMA5 = ", testDay_pred[shiftT,]$SMA5, " ,SMA10 = ", testDay_pred[shiftT,]$SMA10, " ,KValue = ", testDay_pred[shiftT,]$KValue, " ,DValue = ", testDay_pred[shiftT,]$DValue, "\n")
          #
          if ((testDay_pred[shiftT,]$EMA4_flag == 1 && testDay_pred[shiftT-1,]$EMA4_flag == -1) && testDay_pred[shiftT,]$MV20_flag == 1 && (testDay_pred[shiftT,]$frontDay_dayKValue > testDay_pred[shiftT,]$frontDay_dayDValue) ){
            updownFlag <- 1
            dayPredictUpNum <- dayPredictUpNum + 1
            hold_flag_up <- 1
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= upLimit){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -downLimit){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
                
              }
              
            }
            
            # check has future or not 
            if(hold_flag_up == 1){
              #  cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              
              if (finalPrice - currentPrice > 0){
                dayUpCorrectNum <- dayUpCorrectNum + 1
                # cat("Predict Up, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                #   cat("Predict Up, then the real is wrong!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayUpWrongNum <- dayUpWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Up <- notGetBoundNum_Up + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayUpProfit = ", dayUpProfit, "\n")
            dayUpProfit <- dayUpProfit + profit
          }
          
          if ((testDay_pred[shiftT,]$EMA4_flag == -1 && testDay_pred[shiftT-1,]$EMA4_flag == 1) && testDay_pred[shiftT,]$MV20_flag == 1  && (testDay_pred[shiftT,]$frontDay_dayKValue < testDay_pred[shiftT,]$frontDay_dayDValue) ){ 
            updownFlag <- -1
            dayPredictDownNum <- dayPredictDownNum + 1
            hold_flag_up <- 1
            
            
            currentPrice <-  actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$next_close
            buyPrice <- currentPrice
            buyDatetime <- as.character.Date(actualBuySellData[testDay_pred[shiftT,]$Datetime == as.character.Date( actualBuySellData$Datetime),]$nextDatetime)
            
            findBuyIndex <- as.matrix(testDay_pred[shiftT,]$Datetime == as.character.Date(actualBuySellData$Datetime))
            
            buyIndex <- 0
            for(by in 1:nrow(findBuyIndex)){
              if(findBuyIndex[by] == TRUE) buyIndex <- by
            }
            
            for (testT in (buyIndex+1):(nrow(actualBuySellData)-1)){
              if (hold_flag_up == 0)
                break;
              
              finalPrice <- actualBuySellData[testT,]$Open
              
              if (hold_flag_up == 1 && (finalPrice - currentPrice) >= downLimit){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                hold_flag_up <- 0
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              if (hold_flag_up == 1 && (finalPrice - currentPrice) <= -upLimit){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is corrent!!\n")
                hold_flag_up <- 0
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- actualBuySellData[testT+1,]$next_close - currentPrice
                sellPrice <- actualBuySellData[testT+1,]$next_close
                
                sellDatetime <- as.character.Date(actualBuySellData[testT+1,]$nextDatetime)
              }
              
            }
            # check has future or not 
            if(hold_flag_up == 1){
              #    cat("First, still has future !!!", "\n")
              finalPrice <- actualBuySellData[nrow(actualBuySellData),]$next_close
              sellPrice <- finalPrice
              
              sellDatetime <- as.character.Date(actualBuySellData[nrow(actualBuySellData),]$nextDatetime)
              
              if (finalPrice - currentPrice > 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                #     cat("Predict Down, then the real is wrong!!\n")
                #    cat("finalPrice - currentPrice = ", finalPrice-currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice < 0){
                dayDownCorrectNum <- dayDownCorrectNum + 1
                #   cat("Predict Down, then the real is correct!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              if (finalPrice - currentPrice == 0){
                dayDownWrongNum <- dayDownWrongNum + 1
                # cat("Predict Up, then the real is wrong (same value)!!\n")
                #  cat("finalPrice - currentPrice = ", finalPrice- currentPrice, " , hold_flag_up = ", hold_flag_up, "\n")
                profit <- finalPrice-currentPrice
              }
              
              notGetBoundNum_Down <- notGetBoundNum_Down + 1
              hold_flag_up <- 0
            }
            if(hold_flag_up == 1){
              cat("## Still has future !!! not sell at the end of the day ##", "\n")
            }
            
            #  cat("Profit = ", profit, " , dayDownProfit = ", dayDownProfit, "\n")
            dayDownProfit <- dayDownProfit + profit
          }
          
          # store each day trading result          
          dayPredResultDetail <- rbind( dayPredResultDetail, data.frame(
            shiftT = shiftT,
            buyDatetime = buyDatetime,
            sellDatetime = sellDatetime,
            predictUpDown = updownFlag,
            buyPrice = buyPrice,
            sellPrice = sellPrice,
            profit = profit
          ))
          
          
        }
        
        # summary each day trading result
        dayPredResultSummary <- rbind( dayPredResultSummary, data.frame(
          date = testDay_pred[1,]$Dates,
          predictUpNum = dayPredictUpNum,
          predictUpCorrectNum = dayUpCorrectNum,
          predictUpWrongNum = dayUpWrongNum,
          predictDownNum = dayPredictDownNum,
          predictDownCorrectNum = dayDownCorrectNum,
          predictDownWrongNum = dayDownWrongNum,
          totalUpProfit = dayUpProfit,
          totalDownProfit = dayDownProfit,
          notGetBoundNum_Up = notGetBoundNum_Up,
          notGetBoundNum_Down = notGetBoundNum_Down
        ))
        
      }
      
    }
  }
  
  dayPredResultDetail <- dayPredResultDetail[-1,]
  dayPredResultSummary <- dayPredResultSummary[-1,]
  
  return_list <- list("dayPredResultDetail" = dayPredResultDetail, "dayPredResultSummary" = dayPredResultSummary)
  return(return_list)
  
}
# ====


## == fitness function

fitnessFunction_GAStr <- function(x){
  
  cat("test_index: ", test_index, ", x: ",x,"\n")
  upLimit <- 0
  downLimit <- 0
  strNum <- 0
  
  for(i in 1:upSize){
    upLimit <- upLimit + x[i] * 2^(i - 1)
  }
  
  for(j in (upSize+1):(downSize+upSize)){
    downLimit <- downLimit + x[j] * 2^(j - (upSize+1))
  }
  
  
  for(k in (downSize+upSize+1):(downSize+upSize+strSize)){
    strNum <- strNum + x[k] * 2^(k - (downSize+upSize+1))
  }
  
  
  
  if(strNum == 0)
    returnList <- simulateTradingResult_4MA_1MV_KD( beginMIndex, endMIndex, upLimit, downLimit)
  else if(strNum == 1)
    returnList <- simulateTradingResult_4MA_2MV_KD( beginMIndex, endMIndex, upLimit, downLimit)
  else if(strNum == 2)
    returnList <- simulateTradingResult_4MA_3MV_KD( beginMIndex, endMIndex, upLimit, downLimit)
  else if(strNum == 3)
    returnList <- simulateTradingResult_4MA_4MV_KD( beginMIndex, endMIndex, upLimit, downLimit)
  else if(strNum == 4)
    returnList <- simulateTradingResult_4MA_5MV_KD( beginMIndex, endMIndex, upLimit, downLimit)
  else if(strNum == 5)
    returnList <- simulateTradingResult_4MA_10MV_KD( beginMIndex, endMIndex, upLimit, downLimit)
  else if(strNum == 6)
    returnList <- simulateTradingResult_4MA_15MV_KD( beginMIndex, endMIndex, upLimit, downLimit)
  else if(strNum == 7)
    returnList <- simulateTradingResult_4MA_20MV_KD( beginMIndex, endMIndex, upLimit, downLimit)

  
  
  dayPredResultDetail <- returnList$dayPredResultDetail
  dayPredResultSummary <- returnList$dayPredResultSummary
  
  totalUpProfit <- 0
  totalDownProfit <- 0
  totalUpNum <- 0
  totalDownNum <- 0
  
  totalUpProfit <- sum(dayPredResultSummary$totalUpProfit)
  totalDownProfit <- sum(dayPredResultSummary$totalDownProfit)
  
  totalUpNum <- sum(dayPredResultSummary$predictUpNum)
  totalDownNum <- sum(dayPredResultSummary$predictDownNum)
  
  cat("StrNum = ", strNum, (totalUpProfit - totalDownProfit) / (totalUpNum + totalDownNum), "totalUpProfit = ", totalUpProfit, ", totalDownProfit = ", totalDownProfit, ", totalUpNum = ", totalUpNum, ", totalDownNum", totalDownNum, "\n")
  
  
  return ( -( (totalUpProfit - totalDownProfit) / (totalUpNum + totalDownNum) ) )
}


# ================

## ====== GA to get parameter setting  =================

upSize <- 5
downSize <- 5

strSize <- 3

iter = 15

GA_information <- data.frame(
  population = toString("0"),
  evaluations = as.numeric(0),
  iter = as.numeric(0),
  size = as.numeric(0),
  popSize = as.numeric(0),
  mutation = as.numeric(0)
)

# == declare data frame to store information
testing_resultDetail <- data.frame(
  shiftT = as.numeric(0),
  buyDatetime = as.numeric(0),
  sellDatetime = as.numeric(0),
  predictUpDown = as.numeric(0),
  buyPrice = as.numeric(0),
  sellPrice = as.numeric(0),
  profit = as.numeric(0)
)

testing_resultSummary <- data.frame(
  date = as.Date("1-1-1"),
  predictUpNum = as.numeric(0),
  predictUpCorrectNum = as.numeric(0),
  predictUpWrongNum = as.numeric(0),
  predictDownNum = as.numeric(0),
  predictDownCorrectNum = as.numeric(0),
  predictDownWrongNum = as.numeric(0),
  totalUpProfit = as.numeric(0),
  totalDownProfit = as.numeric(0),
  notGetBoundNum_Up = as.numeric(0),
  notGetBoundNum_Down = as.numeric(0)
)

testing_monthUpDownBound <- data.frame(
  monthIndex = as.numeric(0),
  upBound = as.numeric(0),
  downBound = as.numeric(0),
  strNum_test = as.numeric(0)
)


for (a in 11:(tradingYMCount-2)){  # (tradingDayCount-31)
  beginMIndex <- a
  endMIndex <- a + 1
  test_index <- a + 2 
  
  GAmodel <- rbga.bin(size = upSize + downSize + strSize, popSize = 40, iters = iter, mutationChance = 0.01, 
                      elitism = T, evalFunc = fitnessFunction_GAStr)
  
  # cat(summary.rbga(GAmodel))
  
  # because of function design, profit value give negative, so max profit means min value of evaluations
  maxProfit_minIndex <- which.min(GAmodel$evaluations)
  
  getMaxProPopu <- GAmodel$population[maxProfit_minIndex,]
  
  upLimit_test <- 0
  downLimit_test <- 0
  strNum_test <- 0
  
  for(i in 1:upSize){
    upLimit_test <- upLimit_test + getMaxProPopu[i] * 2^(i - 1)
  }
  
  for(j in (upSize+1):(downSize+upSize)){
    downLimit_test <- downLimit_test + getMaxProPopu[j] * 2^(j - (upSize+1))
  }
  
  
  for(k in (downSize+upSize+1):(downSize+upSize+strSize)){
    strNum_test <- strNum_test + getMaxProPopu[k] * 2^(k - (downSize+upSize+1))
  }
  
  if(strNum_test == 0)
    returnList_test <- simulateTradingResult_4MA_1MV_KD( test_index, test_index, upLimit_test, downLimit_test)
  else if(strNum_test == 1)
    returnList_test <- simulateTradingResult_4MA_2MV_KD( test_index, test_index, upLimit_test, downLimit_test)
  else if(strNum_test == 2)
    returnList_test <- simulateTradingResult_4MA_3MV_KD( test_index, test_index, upLimit_test, downLimit_test)
  else if(strNum_test == 3)
    returnList_test <- simulateTradingResult_4MA_4MV_KD( test_index, test_index, upLimit_test, downLimit_test)
  else if(strNum_test == 4)
    returnList_test <- simulateTradingResult_4MA_5MV_KD( test_index, test_index, upLimit_test, downLimit_test)
  else if(strNum_test == 5)
    returnList_test <- simulateTradingResult_4MA_10MV_KD( test_index, test_index, upLimit_test, downLimit_test)
  else if(strNum_test == 6)
    returnList_test <- simulateTradingResult_4MA_15MV_KD( test_index, test_index, upLimit_test, downLimit_test)
  else if(strNum_test == 7)
    returnList_test <- simulateTradingResult_4MA_20MV_KD( test_index, test_index, upLimit_test, downLimit_test)
  
  
  testing_resultDetail <- rbind( testing_resultDetail, returnList_test$dayPredResultDetail)
  testing_resultSummary <- rbind( testing_resultSummary, returnList_test$dayPredResultSummary)
  
  
  testing_monthUpDownBound <- rbind( testing_monthUpDownBound, data.frame(
    monthIndex = test_index,
    upBound = upLimit_test,
    downBound = downLimit_test,
    strNum_test = strNum_test
  ))
  
  cat("Index: ", test_index)
  cat("- predictUpNum: ", sum(testing_resultSummary$predictUpNum))
  cat(" predictUpCorrectNum: ", sum(testing_resultSummary$predictUpCorrectNum))
  cat(" predictUpWrongNum: ", sum(testing_resultSummary$predictUpWrongNum))
  cat(" predictDownNum: ", sum(testing_resultSummary$predictDownNum))
  cat(" predictDownCorrectNum: ", sum(testing_resultSummary$predictDownCorrectNum))
  cat(" predictDownWrongNum: ", sum(testing_resultSummary$predictDownWrongNum))
  cat(" totalUpProfit: ", sum(testing_resultSummary$totalUpProfit))
  cat(" totalDownProfit: ", sum(testing_resultSummary$totalDownProfit), "\n")
  
  
  cat(sum(testing_resultSummary$totalUpProfit) - sum(testing_resultSummary$totalDownProfit), " , " , sum(testing_resultSummary$predictUpNum)+sum(testing_resultSummary$predictDownNum), "\n")
  
}

testing_resultDetail <- testing_resultDetail[-1,]
testing_resultSummary <- testing_resultSummary[-1,]


save.image("strategyTrading_GAstr_MAMVKD_result.Rdata")