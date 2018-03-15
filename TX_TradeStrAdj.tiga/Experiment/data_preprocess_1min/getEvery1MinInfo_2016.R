# ==== 1min ver ====

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


for (year in 2016:2016){
  
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
      # get data, here is TFX(大台) information
      todayData_Y <- time.series.filter.specificYMD(data = TFX, year = year, month = month, day = day)
      
      if (nrow(todayData_Y) != 0){  # this month doesn't have this day
        num <- nrow(todayData_Y)
        for (i in 1:(num-1)){
          
          # store information to centerInfo
          getEveryDayInfo <- rbind( getEveryDayInfo, data.frame(
            Dates = as.Date(dates(todayData_Y[i,]$Datetime)),
            Datetime = as.character.Date(todayData_Y[i,]$Datetime),
            Open = todayData_Y[i,]$Open,
            Close = todayData_Y[i,]$Close,
            nextDates = as.Date(dates(todayData_Y[i+1,]$Datetime)),
            nextDatetime = as.character.Date(todayData_Y[i+1,]$Datetime),
            next_open = todayData_Y[i+1,]$Open,
            next_close = todayData_Y[i+1,]$Close,
            High = todayData_Y[i,]$High,
            Low = todayData_Y[i,]$Low,
            TotalVolume = todayData_Y[i,]$TotalVolume
          ))
          
        }           
        cat(year, "-", month, "-", day, "\n")
      } 
    }
  }
}

getEveryDayInfo <- getEveryDayInfo[-1,]


# if it doesn't have file, create it
if ( !file.exists( "C:/Users/cageb/Desktop/TXtradingsystem.asga/prep_data_1Min") )
  dir.create( "C:/Users/cageb/Desktop/TXtradingsystem.asga/prep_data_1Min")

saveRDS( getEveryDayInfo, "C:/Users/cageb/Desktop/TXtradingsystem.asga/prep_data_1Min/whole_getDayInfo_2016_ver.rds" )
