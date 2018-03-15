finance.read <- function(file, var.names, type = "time", time.name = 'Datetime') {
  require(chron)
  if (type == "time") {
    # read file
    data <- read.csv(file, sep=",", stringsAsFactors = F)
    # transform dates
    Datetime <- chron(dates=data$Date, times=data$Time, format=c('y/m/d','h:m:s'))
    res <- cbind(Datetime, data[var.names])
    colnames(res) <- c(time.name, var.names)
    res
  }
}

time.series.transform <- function(model, data, diffM = F, 
                                  selector = list(begin = -Inf, 
                                                  end = Inf, 
                                                  lag = 1, 
                                                  order = 1)) {
  res.name <- as.character(model)[2]
  var.names <- strsplit(as.character(model)[3], split = " + ", fixed = T)[[1]]
  order <- selector$order
  lag <- selector$lag
  if (is.null(selector$begin)) selector$begin <- -Inf
  if (is.null(selector$end)) selector$end <- Inf
  data <- data[data$Datetime >= selector$begin & data$Datetime < selector$end,]
  data.var <- data[, var.names]
  data.res <- data[, c("Datetime", var.names)]
  if (diffM) {
    n <- dim(data)[1]
    data.var <- data.var[2:n, ] - data.var[1:(n-1), ]
    data.res[var.names][2:n,] <- data.res[var.names][2:n,] - data.res[var.names][1:(n-1),]
    data.res <- data.res[2:n,]
  }
  n <- dim(data.res)[1]
  stopifnot(order >= 1 && lag >= 1 && order + lag < n)
  # handling lagged response
  data.res <- data.res[(order+lag):n,]
  data.var <- do.call(cbind, lapply(order:1, function(order.i) {
    data.var[order.i:(n-lag-(order - order.i)),]
  }))
  var.names <- paste0(
    rep(var.names, order),
    unlist(lapply(1:order, function(x) rep(x, length(var.names))))
  )
  colnames(data.var) <- var.names
  data <- cbind(data.res, data.var)
  list(
    data = data,
    model = formula(paste(res.name, "~", paste(var.names, collapse = " + ")))
  )
}

time.series.filter <- function(data, begin = -Inf, end = Inf, density = 1) {
  data <- data[data$Datetime >= begin & data$Datetime < end,]
  n <- dim(data)[1]
  selector <- rep(c(T, rep(F, density - 1)), length.out = n)
  data <- data[selector,]
  data
}

# KYGu function: get specific year every day last time
time.series.filter.lastTimeEverydayY <- function(data, year = Inf, density = 1) {
  
  begin <- chron(dates = "2014/01/01", format=c(dates = "y/m/d"))
  end <- chron(dates = "2014/12/31", format=c(dates = "y/m/d"))
  
  data <- data[data$Datetime >= begin & data$Datetime < end,]
  
  temp <- data.frame(
    Datetime = chron(dates = "1001/01/01", times = " 01:01:01", format=c(dates = "y/m/d", times = "h:m:s") ),
    High = as.numeric(0),
    Low = as.numeric(0),
    Open = as.numeric(0),
    Close = as.numeric(0),
    TotalVolume = as.integer(0)
  )
  
  for(i in 1:12){
    for(j in 1:31){
      if(i < 10){
        if(j < 10)
          temp <- rbind(temp, data[data$Datetime == chron(dates = paste0(paste0(paste0(paste0(year, "/0"), i), "/0"), j), times = "13:45:00", format=c(dates = "y/m/d", times = "h:m:s") ),])
        else
          temp <- rbind(temp, data[data$Datetime == chron(dates = paste0(paste0(paste0(paste0(year, "/0"), i), "/"), j), times = "13:45:00", format=c(dates = "y/m/d", times = "h:m:s") ),])
      }
      else{
        if(j < 10)
          temp <- rbind(temp, data[data$Datetime == chron(dates = paste0(paste0(paste0(paste0(year, "/"), i), "/0"), j), times = "13:45:00", format=c(dates = "y/m/d", times = "h:m:s") ),])
        else
          temp <- rbind(temp, data[data$Datetime == chron(dates = paste0(paste0(paste0(paste0(year, "/"), i), "/"), j), times = "13:45:00", format=c(dates = "y/m/d", times = "h:m:s") ),])
      }
    }
  }
  # remove NA 
  temp <- temp[!is.na(temp[2]),]
  n <- dim(temp)[1]
  selector <- rep(c(T, rep(F, density - 1)), length.out = n)
  temp <- temp[selector,]
  temp   
}

# KYGu function: get data with specific year and month
# 當2月有29天時 有bug
time.series.filter.yearM <- function(data, year = Inf, month = Inf , density = 1) {
  switch(month,
         { month <- paste0("0", month)
           date1 <- paste0( paste0( paste0( year, "/"), month), "/01")
           date2 <- paste0( paste0( paste0( year, "/"), month), "/31")
           begin <- chron(dates = date1, times = "08:50:00", format = c(dates = "y/m/d", times = "h:m:s")) 
           end <- chron(dates = date2 , times = "13:40:00", format=c(dates = "y/m/d", times = "h:m:s"))
         },
         { month <- paste0("0", month)
           date1 <- paste0( paste0( paste0( year, "/"), month), "/01")
           date2 <- paste0( paste0( paste0( year, "/"), month), "/28")
           begin <- chron(dates = date1, times = "08:50:00", format = c(dates = "y/m/d", times = "h:m:s")) 
           end <- chron(dates = date2 , times = "13:40:00", format=c(dates = "y/m/d", times = "h:m:s"))
         },
         { month <- paste0("0", month)
           date1 <- paste0( paste0( paste0( year, "/"), month), "/01")
           date2 <- paste0( paste0( paste0( year, "/"), month), "/31")
           begin <- chron(dates = date1, times = "08:50:00", format = c(dates = "y/m/d", times = "h:m:s")) 
           end <- chron(dates = date2 , times = "13:40:00", format=c(dates = "y/m/d", times = "h:m:s"))
         },
         { month <- paste0("0", month)
           date1 <- paste0( paste0( paste0( year, "/"), month), "/01")
           date2 <- paste0( paste0( paste0( year, "/"), month), "/30")
           begin <- chron(dates = date1, times = "08:50:00", format = c(dates = "y/m/d", times = "h:m:s")) 
           end <- chron(dates = date2 , times = "13:40:00", format=c(dates = "y/m/d", times = "h:m:s"))
         },
         { month <- paste0("0", month)
           date1 <- paste0( paste0( paste0( year, "/"), month), "/01")
           date2 <- paste0( paste0( paste0( year, "/"), month), "/31")
           begin <- chron(dates = date1, times = "08:50:00", format = c(dates = "y/m/d", times = "h:m:s")) 
           end <- chron(dates = date2 , times = "13:40:00", format=c(dates = "y/m/d", times = "h:m:s"))
         },
         { month <- paste0("0", month)
           date1 <- paste0( paste0( paste0( year, "/"), month), "/01")
           date2 <- paste0( paste0( paste0( year, "/"), month), "/30")
           begin <- chron(dates = date1, times = "08:50:00", format = c(dates = "y/m/d", times = "h:m:s")) 
           end <- chron(dates = date2 , times = "13:40:00", format=c(dates = "y/m/d", times = "h:m:s"))
         },
         { month <- paste0("0", month)
           date1 <- paste0( paste0( paste0( year, "/"), month), "/01")
           date2 <- paste0( paste0( paste0( year, "/"), month), "/31")
           begin <- chron(dates = date1, times = "08:50:00", format = c(dates = "y/m/d", times = "h:m:s")) 
           end <- chron(dates = date2 , times = "13:40:00", format=c(dates = "y/m/d", times = "h:m:s"))
         },
         { month <- paste0("0", month)
           date1 <- paste0( paste0( paste0( year, "/"), month), "/01")
           date2 <- paste0( paste0( paste0( year, "/"), month), "/31")
           begin <- chron(dates = date1, times = "08:50:00", format = c(dates = "y/m/d", times = "h:m:s")) 
           end <- chron(dates = date2 , times = "13:40:00", format=c(dates = "y/m/d", times = "h:m:s"))
         },
         { month <- paste0("0", month)
           date1 <- paste0( paste0( paste0( year, "/"), month), "/01")
           date2 <- paste0( paste0( paste0( year, "/"), month), "/30")
           begin <- chron(dates = date1, times = "08:50:00", format = c(dates = "y/m/d", times = "h:m:s")) 
           end <- chron(dates = date2 , times = "13:40:00", format=c(dates = "y/m/d", times = "h:m:s"))
         },
         { date1 <- paste0( paste0( paste0( year, "/"), month), "/01")
           date2 <- paste0( paste0( paste0( year, "/"), month), "/31")
           begin <- chron(dates = date1, times = "08:50:00", format = c(dates = "y/m/d", times = "h:m:s")) 
           end <- chron(dates = date2 , times = "13:40:00", format=c(dates = "y/m/d", times = "h:m:s"))
         },
         { date1 <- paste0( paste0( paste0( year, "/"), month), "/01")
           date2 <- paste0( paste0( paste0( year, "/"), month), "/30")
           begin <- chron(dates = date1, times = "08:50:00", format = c(dates = "y/m/d", times = "h:m:s")) 
           end <- chron(dates = date2 , times = "13:40:00", format=c(dates = "y/m/d", times = "h:m:s"))
         },
         { date1 <- paste0( paste0( paste0( year, "/"), month), "/01")
           date2 <- paste0( paste0( paste0( year, "/"), month), "/31")
           begin <- chron(dates = date1, times = "08:50:00", format = c(dates = "y/m/d", times = "h:m:s")) 
           end <- chron(dates = date2 , times = "13:40:00", format=c(dates = "y/m/d", times = "h:m:s"))
         }
  ) 
  
  data <- data[data$Datetime >= begin & data$Datetime < end,]
  
  n <- dim(data)[1]
  selector <- rep(c(T, rep(F, density - 1)), length.out = n)
  data <- data[selector,]
  data
}

# KYGu function: get data with specific year and month and day
time.series.filter.specificYMD <- function(data, year = Inf, month = Inf, day = Inf , density = 1) {
  if(month < 10)
    month <- paste0("0", month)
  if(day < 10)
    day <- paste0("0", day)
  
  date1 <- paste0( paste0( paste0( paste0( year, "/"), month), "/"), day)
  date2 <- paste0( paste0( paste0( paste0( year, "/"), month), "/"), day)
  begin <- chron(dates = date1, times = "08:50:00", format = c(dates = "y/m/d", times = "h:m:s")) 
  end <- chron(dates = date2 , times = "13:40:00", format=c(dates = "y/m/d", times = "h:m:s"))
  data <- data[data$Datetime >= begin & data$Datetime < end,]
  
  n <- dim(data)[1]
  selector <- rep(c(T, rep(F, density - 1)), length.out = n)
  data <- data[selector,]
  data
}




