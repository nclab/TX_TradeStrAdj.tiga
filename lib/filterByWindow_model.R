
dta.filterByWindow <- function(begin, end, y) {
  y <- y[begin:end]
  data.frame(Y = y, X = 1:length(y))
}


#dta.filterByWindowFromNow <- function(interval, now, y) {
#  begin <- now - interval
#  end <- now -1
#  y <- y [begin:end]
#  data.frame(Y = y, X = 1:length(y))
#}


dta.filterByWindowFromNow <- function(interval, now, y) {
  begin <- now - interval
  end <- now 
  y <- y [begin:end,]
  # data.frame(Y = y, X = 1:nrow(y))
  
  return(y)
}


totalDta.filterByWindow <- function(begin, end, y) {
  y <- y[begin:end,]
  
  return(y)
}