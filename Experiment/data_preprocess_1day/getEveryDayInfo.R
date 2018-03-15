
# set work path
setwd("C:/Users/cageb/Desktop/TX_TradeStrAdj.tiga/")
source("lib/include.R")
source("lib/eval.R")
source("lib/techIndicator_model.R")
setup(res.path = "Experiment/")


getEveryDayInfo <- data.frame(
  Date = as.Date("1-1-1"),
  High = as.numeric(0),
  Low = as.numeric(0),
  Open = as.numeric(0),
  Close = as.numeric(0),
  TotalVolume = as.numeric(0)
)


for (year in 2012:2016){
  
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
      # get data, here is TFX(TX°) information
      todayData_Y <- time.series.filter.specificYMD(data = TFX, year = year, month = month, day = day)
      
      if (nrow(todayData_Y) != 0){  # this month doesn't have this day
        todayHigh <- max(todayData_Y$High)
        todayLow <- min(todayData_Y$Low)
        todayOpen <- todayData_Y[1,]$Open
        todayClose <- todayData_Y[nrow(todayData_Y),]$Close
        todayVolume <- sum(todayData_Y$TotalVolume)
        
        # store information to centerInfo
        getEveryDayInfo <- rbind( getEveryDayInfo, data.frame(
          Date = as.Date(dates(todayData_Y[1,]$Datetime)),
          High = todayHigh,
          Low = todayLow,
          Open = todayOpen,
          Close = todayClose,
          TotalVolume = todayVolume
        ))
        
        cat(year, "-", month, "-", day, "\n")
      } 
      
    }
    
  }
}

getEveryDayInfo <- getEveryDayInfo[-1,]


# if it doesn't have file, create it
if ( !file.exists( "C:/Users/cageb/Desktop/TXtradingsystem.asga/prep_data_day") )
  dir.create( "C:/Users/cageb/Desktop/TXtradingsystem.asga/prep_data_day")

saveRDS( getEveryDayInfo, "C:/Users/cageb/Desktop/TXtradingsystem.asga/prep_data_day/whole_getDayInfo_2010_2016_dayVer.rds" )
