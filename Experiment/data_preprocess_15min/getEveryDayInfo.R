# === 15 min versions

setwd("C:/Users/cageb/Desktop/TX_TradeStrAdj.tiga/")
source("lib/include.R")
source("lib/eval.R")
source("lib/techIndicator_model.R")
setup(res.path = "Experiment/")


getEveryDayInfo <- data.frame(
  Dates = as.Date("1-1-1"),
  Datetime = as.character.Date(0),
  Open = as.numeric(0),
  Close = as.numeric(0),
  nextDates = as.Date("1-1-1"),
  nextDatetime = as.character.Date(0),
  next_open = as.numeric(0),
  next_close = as.numeric(0),
  High = as.numeric(0),
  Low = as.numeric(0),
  TotalVolume = as.numeric(0)
)


for (year in 2010:2016){
  
  for (month in 1:12){
    # according to different month set different day
    switch(month,
           { lastDay <- 31 },
           { lastDay <- 28 },
           { lastDay <- 31 },
           { lastDay <- 30 },
           { lastDay <- 31 },
           { lastDay <- 30 },
           { lastDay <- 31 },
           { lastDay <- 31 },
           { lastDay <- 30 },
           { lastDay <- 31 },
           { lastDay <- 30 },
           { lastDay <- 31 }
    )
    
    for (day in 1:lastDay) {
      # get data, here is TFX) information
      todayData_Y <- time.series.filter.specificYMD(data = TFX, year = year, month = month, day = day)
      
      if (nrow(todayData_Y) != 0){  # this month doesn't have this day
        num <- nrow(todayData_Y)
        for (i in 1:(floor(num/15)-1)){
          targetData <- dta.filterByWindowFromNow( 14, i*15, todayData_Y)
          targetHigh <- max(targetData$High)
          targetLow <- min(targetData$Low)
          targetOpen <- targetData[1,]$Open
          targetClose <- targetData[nrow(targetData),]$Close
          targetVolume <- sum(targetData$TotalVolume)
          
          target_nextData <- dta.filterByWindowFromNow( 14, ((i*15)+15), todayData_Y)
          target_nextOpen <- target_nextData[1,]$Open
          target_nextClose <- target_nextData[nrow(target_nextData),]$Close
          
          # store information to centerInfo
          getEveryDayInfo <- rbind( getEveryDayInfo, data.frame(
            Dates = as.Date(dates(targetData[1,]$Datetime)),
            Datetime = as.character.Date(targetData[1,]$Datetime),
            Open = targetOpen,
            Close = targetClose,
            nextDates = as.Date(dates(target_nextData[1,]$Datetime)),
            nextDatetime = as.character.Date(target_nextData[1,]$Datetime),
            next_open = target_nextOpen,
            next_close = target_nextClose,
            High = targetHigh,
            Low = targetLow,
            TotalVolume = targetVolume
          ))
          
        }           
        cat(year, "-", month, "-", day, "\n")
      } 
    }
  }
}

getEveryDayInfo <- getEveryDayInfo[-1,]


# if it doesn't have file, create it
if ( !file.exists( "C:/Users/cageb/Desktop/TXtradingsystem.asga/prep_data_15Min") )
  dir.create( "C:/Users/cageb/Desktop/TXtradingsystem.asga/prep_data_15Min")

saveRDS( getEveryDayInfo, "C:/Users/cageb/Desktop/TXtradingsystem.asga/prep_data_15Min/whole_getDayInfo_2010_2016_ver.rds" )
