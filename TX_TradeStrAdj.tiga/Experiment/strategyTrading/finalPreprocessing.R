
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


## === get the relation between K value and D value, store this information into tradingSignal 
# read training dataset from training_dataInfo.rds
tradingSignal <- readRDS( "C:/Users/cageb/Desktop/Experiment_finalVer_end2/prep_data_15Min/dataInfoAfterPrep_2010_2016_techIndicatorFinal_final.rds" )
TFXdata_2012 <- readRDS("C:/Users/cageb/Desktop/Experiment_finalVer_end2/prep_data_1Min/whole_getDayInfo_2012_ver.rds")
TFXdata_2013 <- readRDS("C:/Users/cageb/Desktop/Experiment_finalVer_end2/prep_data_1Min/whole_getDayInfo_2013_ver.rds")
TFXdata_2014 <- readRDS("C:/Users/cageb/Desktop/Experiment_finalVer_end2/prep_data_1Min/whole_getDayInfo_2014_ver.rds")
TFXdata_2015 <- readRDS("C:/Users/cageb/Desktop/Experiment_finalVer_end2/prep_data_1Min/whole_getDayInfo_2015_ver.rds")
TFXdata_2016 <- readRDS("C:/Users/cageb/Desktop/Experiment_finalVer_end2/prep_data_1Min/whole_getDayInfo_2016_ver.rds")

tradingSignal <- tradingSignal[as.character(tradingSignal$Dates) >= "2012-01-01",]
TFXdata <- rbind(TFXdata_2012, TFXdata_2013)
TFXdata <- rbind(TFXdata, TFXdata_2014)
TFXdata <- rbind(TFXdata, TFXdata_2015)
TFXdata <- rbind(TFXdata, TFXdata_2016)


#-------------------------------------------

tradingDayCount <- 0
testDayCount <- as.matrix(0)
TFXdataCount <- as.matrix(0)

for(year in 2012:2016){
  
  for(month in 1:12){
    # according to different month set different day
    test_dates <- ""
    
    switch(month,
           { lastDay <- 31 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 28
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 31 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 30 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 31 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 30 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 31 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 31 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 30 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 31 
           test_dates <- paste0( paste0(year, "-"), month)
           },
           { lastDay <- 30 
           test_dates <- paste0( paste0(year, "-"), month)
           },
           { lastDay <- 31
           test_dates <- paste0( paste0(year, "-"), month)
           }
    )
    
    for (day in 1:lastDay) {
      # get current test dates
      test_dates_f <- ""
      if (day < 10){  test_dates_f <- paste0( paste0( paste0(test_dates, "-"), "0"), day) }
      else{ test_dates_f <- paste0( paste0(test_dates, "-"), day) }
      
      testDay_pred <- tradingSignal[as.character(tradingSignal$Dates) == test_dates_f,]
      actualBuySellData <- TFXdata[as.character(TFXdata$Dates) == test_dates_f,]
      
      if (nrow(testDay_pred) != 0){  # this month doesn't have this day
        tradingDayCount <- tradingDayCount + 1
        
        for(testDaySize in 1:nrow(testDay_pred)){
          testDayCount <- rbind(testDayCount, tradingDayCount)
        }
        
        for(TFXdataSize in 1:nrow(actualBuySellData)){
          TFXdataCount <- rbind(TFXdataCount, tradingDayCount)
        }
        
        cat(day ,"\n")
      }
    }
  }
}
testDayCount <- as.matrix(testDayCount[-1,])
TFXdataCount <- as.matrix(TFXdataCount[-1,])

tradingSignal <- cbind(tradingSignal, testDayCount)
TFXdata <- cbind(TFXdata, TFXdataCount)


tradingYMCount <- 0
testYMCount <- as.matrix(0)
TFXdataYMCount <- as.matrix(0)

for(year in 2012:2016){
  
  for(month in 1:12){
    # according to different month set different day
    test_dates <- ""
    
    switch(month,
           { lastDay <- 31 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 28
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 31 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 30 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 31 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 30 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 31 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 31 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 30 
           test_dates <- paste0( paste0( paste0(year, "-"), "0"), month)
           },
           { lastDay <- 31 
           test_dates <- paste0( paste0(year, "-"), month)
           },
           { lastDay <- 30 
           test_dates <- paste0( paste0(year, "-"), month)
           },
           { lastDay <- 31
           test_dates <- paste0( paste0(year, "-"), month)
           }
    )
    
    
    testYM_pred <- tradingSignal[as.character(tradingSignal$Dates) >=  paste0(test_dates, "-01"),]
    testYM_pred <- testYM_pred[as.character(testYM_pred$Dates) <=  paste0(test_dates, "-31"),]
    
    actualBuySellData <- TFXdata[as.character(TFXdata$Dates) >=  paste0(test_dates, "-01"),] 
    actualBuySellData <- actualBuySellData[as.character(actualBuySellData$Dates) <=  paste0(test_dates, "-31"),] 
    
    
    if (nrow(testYM_pred) != 0){  # this month doesn't have this day
      tradingYMCount <- tradingYMCount + 1
      
      for(testYMSize in 1:nrow(testYM_pred)){
        testYMCount <- rbind(testYMCount, tradingYMCount)
      }
      
      for(TFXdataSize in 1:nrow(actualBuySellData)){
        TFXdataYMCount <- rbind(TFXdataYMCount, tradingYMCount)
      }
      
      cat(test_dates ,"\n")
    }
    
  }
}
testYMCount <- as.matrix(testYMCount[-1,])
TFXdataYMCount <- as.matrix(TFXdataYMCount[-1,])

tradingSignal <- cbind(tradingSignal, testYMCount)
TFXdata <- cbind(TFXdata, TFXdataYMCount)


#---------------------------------------------

frontDay_dayKValue <- as.matrix(tradingSignal[tradingSignal$testDayCount == 1,]$day_KValue)

for(dc in 2:tradingDayCount){
  frontDay_dayKValue <- rbind(frontDay_dayKValue, as.matrix(tradingSignal[tradingSignal$testDayCount == (dc-1),]$day_KValue))
}

frontDay_dayDValue <- as.matrix(tradingSignal[tradingSignal$testDayCount == 1,]$day_DValue)

for(dc in 2:tradingDayCount){
  frontDay_dayDValue <- rbind(frontDay_dayDValue, as.matrix(tradingSignal[tradingSignal$testDayCount == (dc-1),]$day_DValue))
}

tradingSignal <- cbind(tradingSignal, frontDay_dayKValue)
tradingSignal <- cbind(tradingSignal, frontDay_dayDValue)

#---------------------------------------------


EMA4_flag <- as.matrix(0)

for(b in 2:nrow(tradingSignal)){
  if(tradingSignal[b,]$EMA4_value - tradingSignal[b-1,]$EMA4_value > 0)
    EMA4_flag <- rbind(EMA4_flag, 1)
  else if(tradingSignal[b,]$EMA4_value - tradingSignal[b-1,]$EMA4_value < 0)
    EMA4_flag <- rbind(EMA4_flag, -1)
  else
    EMA4_flag <- rbind(EMA4_flag, 0)
}

tradingSignal <- cbind(tradingSignal, EMA4_flag)


EMA5_flag <- as.matrix(0)

for(b in 2:nrow(tradingSignal)){
  if(tradingSignal[b,]$EMA5_value - tradingSignal[b-1,]$EMA5_value > 0)
    EMA5_flag <- rbind(EMA5_flag, 1)
  else if(tradingSignal[b,]$EMA5_value - tradingSignal[b-1,]$EMA5_value < 0)
    EMA5_flag <- rbind(EMA5_flag, -1)
  else
    EMA5_flag <- rbind(EMA5_flag, 0)
}

tradingSignal <- cbind(tradingSignal, EMA5_flag)


EMA10_flag <- as.matrix(0)

for(b in 2:nrow(tradingSignal)){
  if(tradingSignal[b,]$EMA10_value - tradingSignal[b-1,]$EMA10_value > 0)
    EMA10_flag <- rbind(EMA10_flag, 1)
  else if(tradingSignal[b,]$EMA10_value - tradingSignal[b-1,]$EMA10_value < 0)
    EMA10_flag <- rbind(EMA10_flag, -1)
  else
    EMA10_flag <- rbind(EMA10_flag, 0)
}

tradingSignal <- cbind(tradingSignal, EMA10_flag)


EMA15_flag <- as.matrix(0)

for(b in 2:nrow(tradingSignal)){
  if(tradingSignal[b,]$EMA15_value - tradingSignal[b-1,]$EMA15_value > 0)
    EMA15_flag <- rbind(EMA15_flag, 1)
  else if(tradingSignal[b,]$EMA15_value - tradingSignal[b-1,]$EMA15_value < 0)
    EMA15_flag <- rbind(EMA15_flag, -1)
  else
    EMA15_flag <- rbind(EMA15_flag, 0)
}

tradingSignal <- cbind(tradingSignal, EMA15_flag)


EMA20_flag <- as.matrix(0)

for(b in 2:nrow(tradingSignal)){
  if(tradingSignal[b,]$EMA20_value - tradingSignal[b-1,]$EMA20_value > 0)
    EMA20_flag <- rbind(EMA20_flag, 1)
  else if(tradingSignal[b,]$EMA20_value - tradingSignal[b-1,]$EMA20_value < 0)
    EMA20_flag <- rbind(EMA20_flag, -1)
  else
    EMA20_flag <- rbind(EMA20_flag, 0)
}

tradingSignal <- cbind(tradingSignal, EMA20_flag)


MV1_flag <- as.matrix(0)

for(b in 2:nrow(tradingSignal)){
  if(tradingSignal[b,]$MV1_value - tradingSignal[b-1,]$MV1_value > 0)
    MV1_flag <- rbind(MV1_flag, 1)
  else if(tradingSignal[b,]$MV1_value - tradingSignal[b-1,]$MV1_value < 0)
    MV1_flag <- rbind(MV1_flag, -1)
  else
    MV1_flag <- rbind(MV1_flag, 0)
}

tradingSignal <- cbind(tradingSignal, MV1_flag)


MV2_flag <- as.matrix(0)

for(b in 2:nrow(tradingSignal)){
  if(tradingSignal[b,]$MV2_value - tradingSignal[b-1,]$MV2_value > 0)
    MV2_flag <- rbind(MV2_flag, 1)
  else if(tradingSignal[b,]$MV2_value - tradingSignal[b-1,]$MV2_value < 0)
    MV2_flag <- rbind(MV2_flag, -1)
  else
    MV2_flag <- rbind(MV2_flag, 0)
}

tradingSignal <- cbind(tradingSignal, MV2_flag)


MV3_flag <- as.matrix(0)

for(b in 2:nrow(tradingSignal)){
  if(tradingSignal[b,]$MV3_value - tradingSignal[b-1,]$MV3_value > 0)
    MV3_flag <- rbind(MV3_flag, 1)
  else if(tradingSignal[b,]$MV3_value - tradingSignal[b-1,]$MV3_value < 0)
    MV3_flag <- rbind(MV3_flag, -1)
  else
    MV3_flag <- rbind(MV3_flag, 0)
}

tradingSignal <- cbind(tradingSignal, MV3_flag)


MV4_flag <- as.matrix(0)

for(b in 2:nrow(tradingSignal)){
  if(tradingSignal[b,]$MV4_value - tradingSignal[b-1,]$MV4_value > 0)
    MV4_flag <- rbind(MV4_flag, 1)
  else if(tradingSignal[b,]$MV4_value - tradingSignal[b-1,]$MV4_value < 0)
    MV4_flag <- rbind(MV4_flag, -1)
  else
    MV4_flag <- rbind(MV4_flag, 0)
}

tradingSignal <- cbind(tradingSignal, MV4_flag)


MV5_flag <- as.matrix(0)

for(b in 2:nrow(tradingSignal)){
  if(tradingSignal[b,]$MV5_value - tradingSignal[b-1,]$MV5_value > 0)
    MV5_flag <- rbind(MV5_flag, 1)
  else if(tradingSignal[b,]$MV5_value - tradingSignal[b-1,]$MV5_value < 0)
    MV5_flag <- rbind(MV5_flag, -1)
  else
    MV5_flag <- rbind(MV5_flag, 0)
}

tradingSignal <- cbind(tradingSignal, MV5_flag)


MV10_flag <- as.matrix(0)

for(b in 2:nrow(tradingSignal)){
  if(tradingSignal[b,]$MV10_value - tradingSignal[b-1,]$MV10_value > 0)
    MV10_flag <- rbind(MV10_flag, 1)
  else if(tradingSignal[b,]$MV10_value - tradingSignal[b-1,]$MV10_value < 0)
    MV10_flag <- rbind(MV10_flag, -1)
  else
    MV10_flag <- rbind(MV10_flag, 0)
}

tradingSignal <- cbind(tradingSignal, MV10_flag)


MV15_flag <- as.matrix(0)

for(b in 2:nrow(tradingSignal)){
  if(tradingSignal[b,]$MV15_value - tradingSignal[b-1,]$MV15_value > 0)
    MV15_flag <- rbind(MV15_flag, 1)
  else if(tradingSignal[b,]$MV15_value - tradingSignal[b-1,]$MV15_value < 0)
    MV15_flag <- rbind(MV15_flag, -1)
  else
    MV15_flag <- rbind(MV15_flag, 0)
}

tradingSignal <- cbind(tradingSignal, MV15_flag)


MV20_flag <- as.matrix(0)

for(b in 2:nrow(tradingSignal)){
  if(tradingSignal[b,]$MV20_value - tradingSignal[b-1,]$MV20_value > 0)
    MV20_flag <- rbind(MV20_flag, 1)
  else if(tradingSignal[b,]$MV20_value - tradingSignal[b-1,]$MV20_value < 0)
    MV20_flag <- rbind(MV20_flag, -1)
  else
    MV20_flag <- rbind(MV20_flag, 0)
}

tradingSignal <- cbind(tradingSignal, MV20_flag)



save.image("Projfinal_preprocessing_workspace_final.Rdata")