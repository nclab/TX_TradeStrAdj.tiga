# ==== 15min ver ====

setwd("C:/Users/cageb/Desktop/TX_TradeStrAdj.tiga/")
source("lib/include.R")
source("lib/eval.R")
source("lib/techIndicator_model.R")
setup(res.path = "Experiment/")

todayData_Y <- readRDS("prep_data_15Min/whole_getDayInfo_2010_2016_ver.rds")
dataOneDay <- readRDS("prep_data_day/whole_getDayInfo_2010_2016_dayVer.rds")



# use a dataframe to store every shift time calculate technical indicator value 
techIndicator <- data.frame(
  Dates = as.Date("1-1-1"),
  Datetime = as.character.Date(0),
  High = as.numeric(0),
  Low = as.numeric(0),
  TotalVolume = as.numeric(0),
  Open = as.numeric(0),
  Close = as.numeric(0),
  nextDates = as.Date("1-1-1"),
  nextDatetime = as.character.Date(0),
  next_open = as.numeric(0),
  next_close = as.numeric(0),
  RSV_value = as.numeric(0),
  KValue = as.numeric(0),
  DValue = as.numeric(0),
  MV1_value = as.numeric(0),
  MV2_value = as.numeric(0),
  MV3_value = as.numeric(0),
  MV4_value = as.numeric(0),
  MV5_value = as.numeric(0),
  MV10_value = as.numeric(0),
  MV15_value = as.numeric(0),
  MV20_value = as.numeric(0),
  EMA4_value = as.numeric(0),
  EMA5_value = as.numeric(0),
  EMA10_value = as.numeric(0),
  EMA15_value = as.numeric(0),
  EMA20_value = as.numeric(0)
)



KValueM <- as.matrix(50.0)
KV_count <- 1
DValueM <- as.matrix(50.0)
DV_count <- 1


EMA4_value <- as.matrix(SMA_model( 4, 71, todayData_Y))
EMA4index <- 1

EMA5_value <- as.matrix(SMA_model( 5, 71, todayData_Y))
EMA5index <- 1

EMA10_value <- as.matrix(SMA_model( 10, 71, todayData_Y))
EMA10index <- 1

EMA15_value <- as.matrix(SMA_model( 15, 71, todayData_Y))
EMA15index <- 1

EMA20_value <- as.matrix(SMA_model( 20, 71, todayData_Y))
EMA20index <- 1


for (shiftT in 21 : nrow(todayData_Y)){
  
  RSV_value = RSV_model( 9, shiftT, todayData_Y)
  KValue = KValue_model( 9, shiftT, todayData_Y, KValueM[KV_count])
  KValueM <- rbind(KValueM, KValue)
  KV_count <- KV_count + 1
  DValue = DValue_model( 9, shiftT, todayData_Y, DValueM[DV_count], KValueM[KV_count])
  DValueM <- rbind(DValueM, DValue)
  DV_count <- DV_count + 1
  MV1_value <- MV_model(1, shiftT, todayData_Y)
  MV2_value <- MV_model(2, shiftT, todayData_Y)
  MV3_value <- MV_model(3, shiftT, todayData_Y)
  MV4_value <- MV_model(4, shiftT, todayData_Y)
  MV5_value <- MV_model(5, shiftT, todayData_Y)
  MV10_value <- MV_model(10, shiftT, todayData_Y)
  MV15_value <- MV_model(15, shiftT, todayData_Y)
  MV20_value <- MV_model(20, shiftT, todayData_Y)
  EMA4_value <- rbind(EMA4_value, EMA4_value[EMA4index] * 3/4 + todayData_Y[shiftT,]$Close * 1/4)
  EMA4index <- EMA4index + 1
  EMA5_value <- rbind(EMA5_value, EMA5_value[EMA5index] * 4/5 + todayData_Y[shiftT,]$Close * 1/5)
  EMA5index <- EMA5index + 1
  EMA10_value <- rbind(EMA10_value, EMA10_value[EMA10index] * 9/10 + todayData_Y[shiftT,]$Close * 1/10)
  EMA10index <- EMA10index + 1
  EMA15_value <- rbind(EMA15_value, EMA15_value[EMA15index] * 14/15 + todayData_Y[shiftT,]$Close * 1/15)
  EMA15index <- EMA15index + 1
  EMA20_value <- rbind(EMA20_value, EMA20_value[EMA20index] * 19/20 + todayData_Y[shiftT,]$Close * 1/20)
  EMA20index <- EMA20index + 1
  
  # store information to centerInfo
  techIndicator <- rbind( techIndicator, data.frame(
    Dates = todayData_Y[shiftT,]$Dates,
    Datetime = as.character.Date(todayData_Y[shiftT,]$Datetime),
    High = todayData_Y[shiftT,]$High,
    Low = todayData_Y[shiftT,]$Low,
    TotalVolume = todayData_Y[shiftT,]$TotalVolume,
    Open = todayData_Y[shiftT,]$Open,
    Close = todayData_Y[shiftT,]$Close,
    nextDates = todayData_Y[shiftT,]$nextDates,
    nextDatetime = as.character.Date(todayData_Y[shiftT,]$nextDatetime),
    next_open = todayData_Y[shiftT,]$next_open,
    next_close = todayData_Y[shiftT,]$next_close,
    RSV_value = RSV_value,
    KValue = KValue,
    DValue = DValue,
    MV1_value = MV1_value,
    MV2_value = MV2_value,
    MV3_value = MV3_value,
    MV4_value = MV4_value,
    MV5_value = MV5_value,
    MV10_value = MV10_value,
    MV15_value = MV15_value,
    MV20_value = MV20_value,
    EMA4_value = EMA4_value[EMA4index],
    EMA5_value = EMA5_value[EMA5index],
    EMA10_value = EMA10_value[EMA10index],
    EMA15_value = EMA15_value[EMA15index],
    EMA20_value = EMA20_value[EMA20index]
  ))
  
  cat( as.character.Date(todayData_Y[shiftT,]$Datetime), "\n")
}

# remove the first unuse row
techIndicator <- techIndicator[-1,]
#techIndicator <- techIndicator[rowSums(is.na(techIndicator))==0,]




KD_flag <- as.matrix(0)

for (i in 1:nrow(techIndicator)){
  if (techIndicator[i,]$KValue > techIndicator[i,]$DValue)
    KD_flag <- rbind(KD_flag, 1)
  else if(techIndicator[i,]$KValue < techIndicator[i,]$DValue)
    KD_flag <- rbind(KD_flag, -1)
  else
    KD_flag <- rbind(KD_flag, 0)
}

KD_flag <- as.matrix(KD_flag[-1,])

techIndicator <- cbind(techIndicator, KD_flag)


EMA4_flag <- as.matrix(0)

for (i in 2:nrow(techIndicator)){
  if (techIndicator[i,]$EMA4_value > techIndicator[i-1,]$EMA4_value)
    EMA4_flag <- rbind(EMA4_flag, 1)
  else if (techIndicator[i,]$EMA4_value < techIndicator[i-1,]$EMA4_value)
    EMA4_flag <- rbind(EMA4_flag, -1)
  else
    EMA4_flag <- rbind(EMA4_flag, 0)
}

techIndicator <- cbind(techIndicator, EMA4_flag)


EMA5_flag <- as.matrix(0)

for (i in 2:nrow(techIndicator)){
  if (techIndicator[i,]$EMA5_value > techIndicator[i-1,]$EMA5_value)
    EMA5_flag <- rbind(EMA5_flag, 1)
  else if (techIndicator[i,]$EMA5_value < techIndicator[i-1,]$EMA5_value)
    EMA5_flag <- rbind(EMA5_flag, -1)
  else
    EMA5_flag <- rbind(EMA5_flag, 0)
}

techIndicator <- cbind(techIndicator, EMA5_flag)


EMA10_flag <- as.matrix(0)

for (i in 2:nrow(techIndicator)){
  if (techIndicator[i,]$EMA10_value > techIndicator[i-1,]$EMA10_value)
    EMA10_flag <- rbind(EMA10_flag, 1)
  else if (techIndicator[i,]$EMA10_value < techIndicator[i-1,]$EMA10_value)
    EMA10_flag <- rbind(EMA10_flag, -1)
  else
    EMA10_flag <- rbind(EMA10_flag, 0)
}

techIndicator <- cbind(techIndicator, EMA10_flag)


EMA15_flag <- as.matrix(0)

for (i in 2:nrow(techIndicator)){
  if (techIndicator[i,]$EMA15_value > techIndicator[i-1,]$EMA15_value)
    EMA15_flag <- rbind(EMA15_flag, 1)
  else if (techIndicator[i,]$EMA15_value < techIndicator[i-1,]$EMA15_value)
    EMA15_flag <- rbind(EMA15_flag, -1)
  else
    EMA15_flag <- rbind(EMA15_flag, 0)
}

techIndicator <- cbind(techIndicator, EMA15_flag)


EMA20_flag <- as.matrix(0)

for (i in 2:nrow(techIndicator)){
  if (techIndicator[i,]$EMA20_value > techIndicator[i-1,]$EMA20_value)
    EMA20_flag <- rbind(EMA20_flag, 1)
  else if (techIndicator[i,]$EMA20_value < techIndicator[i-1,]$EMA20_value)
    EMA20_flag <- rbind(EMA20_flag, -1)
  else
    EMA20_flag <- rbind(EMA20_flag, 0)
}

techIndicator <- cbind(techIndicator, EMA20_flag)


MV1_flag <- as.matrix(0)

for (i in 2:nrow(techIndicator)){
  if (techIndicator[i,]$MV1_value > techIndicator[i-1,]$MV1_value)
    MV1_flag<- rbind(MV1_flag, 1)
  else if (techIndicator[i,]$MV1_value < techIndicator[i-1,]$MV1_value)
    MV1_flag<- rbind(MV1_flag, -1)
  else
    MV1_flag<- rbind(MV1_flag, 0)
}

techIndicator <- cbind(techIndicator, MV1_flag)


MV2_flag <- as.matrix(0)

for (i in 2:nrow(techIndicator)){
  if (techIndicator[i,]$MV2_value > techIndicator[i-1,]$MV2_value)
    MV2_flag<- rbind(MV2_flag, 1)
  else if (techIndicator[i,]$MV2_value < techIndicator[i-1,]$MV2_value)
    MV2_flag<- rbind(MV2_flag, -1)
  else
    MV2_flag<- rbind(MV2_flag, 0)
}

techIndicator <- cbind(techIndicator, MV2_flag)


MV3_flag <- as.matrix(0)

for (i in 2:nrow(techIndicator)){
  if (techIndicator[i,]$MV3_value > techIndicator[i-1,]$MV3_value)
    MV3_flag<- rbind(MV3_flag, 1)
  else if (techIndicator[i,]$MV3_value < techIndicator[i-1,]$MV3_value)
    MV3_flag<- rbind(MV3_flag, -1)
  else
    MV3_flag<- rbind(MV3_flag, 0)
}

techIndicator <- cbind(techIndicator, MV3_flag)


MV4_flag <- as.matrix(0)

for (i in 2:nrow(techIndicator)){
  if (techIndicator[i,]$MV4_value > techIndicator[i-1,]$MV4_value)
    MV4_flag<- rbind(MV4_flag, 1)
  else if (techIndicator[i,]$MV4_value < techIndicator[i-1,]$MV4_value)
    MV4_flag<- rbind(MV4_flag, -1)
  else
    MV4_flag<- rbind(MV4_flag, 0)
}

techIndicator <- cbind(techIndicator, MV4_flag)


MV5_flag <- as.matrix(0)

for (i in 2:nrow(techIndicator)){
  if (techIndicator[i,]$MV5_value > techIndicator[i-1,]$MV5_value)
    MV5_flag<- rbind(MV5_flag, 1)
  else if (techIndicator[i,]$MV5_value < techIndicator[i-1,]$MV5_value)
    MV5_flag<- rbind(MV5_flag, -1)
  else
    MV5_flag<- rbind(MV5_flag, 0)
}

techIndicator <- cbind(techIndicator, MV5_flag)


MV10_flag <- as.matrix(0)

for (i in 2:nrow(techIndicator)){
  if (techIndicator[i,]$MV10_value > techIndicator[i-1,]$MV10_value)
    MV10_flag<- rbind(MV10_flag, 1)
  else if (techIndicator[i,]$MV10_value < techIndicator[i-1,]$MV10_value)
    MV10_flag<- rbind(MV10_flag, -1)
  else
    MV10_flag<- rbind(MV10_flag, 0)
}

techIndicator <- cbind(techIndicator, MV10_flag)


MV15_flag <- as.matrix(0)

for (i in 2:nrow(techIndicator)){
  if (techIndicator[i,]$MV15_value > techIndicator[i-1,]$MV15_value)
    MV15_flag<- rbind(MV15_flag, 1)
  else if (techIndicator[i,]$MV15_value < techIndicator[i-1,]$MV15_value)
    MV15_flag<- rbind(MV15_flag, -1)
  else
    MV15_flag<- rbind(MV15_flag, 0)
}

techIndicator <- cbind(techIndicator, MV15_flag)


MV20_flag <- as.matrix(0)

for (i in 2:nrow(techIndicator)){
  if (techIndicator[i,]$MV20_value > techIndicator[i-1,]$MV20_value)
    MV20_flag<- rbind(MV20_flag, 1)
  else if (techIndicator[i,]$MV20_value < techIndicator[i-1,]$MV20_value)
    MV20_flag<- rbind(MV20_flag, -1)
  else
    MV20_flag<- rbind(MV20_flag, 0)
}

techIndicator <- cbind(techIndicator, MV20_flag)



# use a dataframe to store every shift time calculate technical indicator value 
KD_forDay <- data.frame(
  Dates = as.Date("1-1-1"),
  RSV_value = as.numeric(0),
  KValue = as.numeric(0),
  DValue = as.numeric(0)
)


KValueMD <- as.matrix(50.0)
KV_countD <- 1
DValueMD <- as.matrix(50.0)
DV_countD <- 1


for (shiftT in 9 : nrow(dataOneDay)){
  
  RSV_value = RSV_model( 9, shiftT, dataOneDay)
  KValue = KValue_model( 9, shiftT, dataOneDay, KValueMD[KV_countD])
  KValueMD <- rbind(KValueMD, KValue)
  KV_countD <- KV_countD + 1
  DValue = DValue_model( 9, shiftT, todayData_Y, DValueMD[DV_countD], KValueMD[KV_countD])
  DValueMD <- rbind(DValueMD, DValue)
  DV_countD <- DV_countD + 1

  # store information to centerInfo
  KD_forDay <- rbind( KD_forDay, data.frame(
    Dates = dataOneDay[shiftT,]$Date,
    RSV_value = RSV_value,
    KValue = KValue,
    DValue = DValue
  ))
  
}

KD_forDay <- KD_forDay[-1,]

techIndicator_final <- data.frame(
  Dates = as.Date("1-1-1"),
  Datetime = as.character.Date(0),
  High = as.numeric(0),
  Low = as.numeric(0),
  TotalVolume = as.numeric(0),
  Open = as.numeric(0),
  Close = as.numeric(0),
  nextDates = as.Date("1-1-1"),
  nextDatetime = as.character.Date(0),
  next_open = as.numeric(0),
  next_close = as.numeric(0),
  RSV_value = as.numeric(0),
  KValue = as.numeric(0),
  DValue = as.numeric(0),
  MV1_value = as.numeric(0),
  MV2_value = as.numeric(0),
  MV3_value = as.numeric(0),
  MV4_value = as.numeric(0),
  MV5_value = as.numeric(0),
  MV10_value = as.numeric(0),
  MV15_value = as.numeric(0),
  MV20_value = as.numeric(0),
  EMA4_value = as.numeric(0),
  EMA5_value = as.numeric(0),
  EMA10_value = as.numeric(0),
  EMA15_value = as.numeric(0),
  EMA20_value = as.numeric(0),
  dayDates = as.Date("1-1-1"),
  day_RSV = as.numeric(0),
  day_KValue = as.numeric(0),
  day_DValue = as.numeric(0)
)


for (a in 1:nrow(techIndicator)){
   cat("a :", a, "\n")
   for (b in 1:nrow(KD_forDay)){
     if (techIndicator[a,]$Dates == KD_forDay[b,]$Dates){
       techIndicator_final <- rbind(techIndicator_final, data.frame(
         Dates = techIndicator[a,]$Dates,
         Datetime = as.character.Date(techIndicator[a,]$Datetime),
         High = techIndicator[a,]$High,
         Low = techIndicator[a,]$Low,
         TotalVolume = techIndicator[a,]$TotalVolume,
         Open = techIndicator[a,]$Open,
         Close = techIndicator[a,]$Close,
         nextDates = techIndicator[a,]$nextDates,
         nextDatetime = as.character.Date(techIndicator[a,]$nextDatetime),
         next_open = techIndicator[a,]$next_open,
         next_close = techIndicator[a,]$next_close,
         RSV_value = techIndicator[a,]$RSV_value,
         KValue = techIndicator[a,]$KValue,
         DValue = techIndicator[a,]$DValue,
         MV1_value = techIndicator[a,]$MV1_value,
         MV2_value = techIndicator[a,]$MV2_value,
         MV3_value = techIndicator[a,]$MV3_value,
         MV4_value = techIndicator[a,]$MV4_value,
         MV5_value = techIndicator[a,]$MV5_value,
         MV10_value = techIndicator[a,]$MV10_value,
         MV15_value = techIndicator[a,]$MV15_value,
         MV20_value = techIndicator[a,]$MV20_value,
         EMA4_value = techIndicator[a,]$EMA4_value,
         EMA5_value = techIndicator[a,]$EMA5_value,
         EMA10_value = techIndicator[a,]$EMA10_value,
         EMA15_value = techIndicator[a,]$EMA15_value,
         EMA20_value = techIndicator[a,]$EMA20_value,
         dayDates = KD_forDay[b,]$Dates,
         day_RSV = KD_forDay[b,]$RSV_value,
         day_KValue = KD_forDay[b,]$KValue,
         day_DValue = KD_forDay[b,]$DValue
       ))
     }
     
   }
}

techIndicator_final <- techIndicator_final[-1,]

saveRDS(techIndicator, "C:/Users/cageb/Desktop/TXtradingsystem.asga/prep_data_15Min/dataInfoAfterPrep_2010_2016_techIndicator_final.rds" )
saveRDS(KD_forDay, "C:/Users/cageb/Desktop/TXtradingsystem.asga/prep_data_15Min/dataInfoAfterPrep_2010_2016_KDforDay_final.rds" )
saveRDS(techIndicator_final, "C:/Users/cageb/Desktop/TXtradingsystem.asga/prep_data_15Min/dataInfoAfterPrep_2010_2016_techIndicatorFinal_final.rds" )
