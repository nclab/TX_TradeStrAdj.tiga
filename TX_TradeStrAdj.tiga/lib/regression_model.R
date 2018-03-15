####   regression_model.R : there are several different functions about regression here    ####

# == regression model without draw function =====
# Input  : trainingSize , shiftT, todayData_Y
# Output : coefficients of regression
# ==================================
regression_model.withoutDraw <- function( trainingSize, window.size, shiftT, todayData_Y){
  res <- sapply( (1 + shiftT):(trainingSize + shiftT) ,  function(index) {
    dta <- dta.filterByWindow(index, index + window.size - 1, todayData_Y)
    # normalize data, redo linear regression again
    dta$Y <- dta$Y - mean(dta$Y)
    data.lm <- lm(dta$Y ~ dta$X + I(dta$X^2))
    # get the regression coefficients and get samples, use data frame
    coef <- data.frame(
      Intercept = data.lm$coefficients[1],
      Poly_X1 = data.lm$coefficients[2],
      Poly_X2 = data.lm$coefficients[3]
    )
    
    data.lm$coefficients}
  )
  
  return(res)
}


# == regression model function =====
# Input  : trainingSize, window.size , shiftT, todayData_Y, regPath
# Output : coefficients of regression
# draw the regression picture 
# ==================================
regression_model <- function( trainingSize, window.size, shiftT, todayData_Y, RegPath){
  path.setup(RegPath)
  res <- sapply( (1 + shiftT):(trainingSize + shiftT) , function(index) {
    dta <- dta.filterByWindow(index, index + window.size - 1, todayData_Y)
    # normalize data, redo linear regression again
    dta$Y <- dta$Y - mean(dta$Y)
    data.lm <- lm(dta$Y ~ dta$X + I(dta$X^2))
    # get the regression coefficients and get samples, use data frame
    coef <- data.frame(
      Intercept = data.lm$coefficients[1],
      Poly_X1 = data.lm$coefficients[2],
      Poly_X2 = data.lm$coefficients[3]
    )
    
    # get 100 sample points from curve line 
    sampleX <- seq(1, window.size , length = 100)
    sampleFitY <- coef$Intercept + coef$Poly_X1 * sampleX + coef$Poly_X2 * sampleX^2 
    # draw the picture of regression
    draw(make.path(RegPath, paste0(index, '.png')), {
      plot(dta$Y, cex = 1)
      points(data.lm$fitted.values, cex = 1, col = 'red')
      lines(sampleX, sampleFitY, col = 'red')
    }, width = 800, height = 600)
    
    data.lm$coefficients }
  )
  
  return(res)
}


regression_model.noShift <- function( trainingSize, window.size, shiftT, todayData_Y){
  res <- sapply( (1 + shiftT):(trainingSize + shiftT) ,  function(index) {
    dta <- dta.filterByWindow(index, index + window.size - 1, todayData_Y)
    # normalize data, redo linear regression again
    data.lm <- lm(dta$Y ~ dta$X + I(dta$X^2))
    # get the regression coefficients and get samples, use data frame
    coef <- data.frame(
      Intercept = data.lm$coefficients[1],
      Poly_X1 = data.lm$coefficients[2],
      Poly_X2 = data.lm$coefficients[3]
    )
    
    data.lm$coefficients}
  )
  
  return(res)
}


regression_model.noShiftDraw <- function( trainingSize, window.size, shiftT, todayData_Y, RegPath){
  path.setup(RegPath)
  res <- sapply( (1 + shiftT):(trainingSize + shiftT) , function(index) {
    dta <- dta.filterByWindow(index, index + window.size - 1, todayData_Y)
    # normalize data, redo linear regression again
    data.lm <- lm(dta$Y ~ dta$X + I(dta$X^2))
    # get the regression coefficients and get samples, use data frame
    coef <- data.frame(
      Intercept = data.lm$coefficients[1],
      Poly_X1 = data.lm$coefficients[2],
      Poly_X2 = data.lm$coefficients[3]
    )
    
    # get 100 sample points from curve line 
    sampleX <- seq(1, window.size , length = 100)
    sampleFitY <- coef$Intercept + coef$Poly_X1 * sampleX + coef$Poly_X2 * sampleX^2 
    # draw the picture of regression
    draw(make.path(RegPath, paste0( paste0( past0('shift_', shiftT), index), '.png')), {
      plot(dta$Y, cex = 1)
      points(data.lm$fitted.values, cex = 1, col = 'red')
      lines(sampleX, sampleFitY, col = 'red')
    }, width = 800, height = 600)
    
    data.lm$coefficients }
  )
  
  return(res)
}


linearRegression_model.withoutDraw <- function( trainingSize, window.size, shiftT, todayData_Y){
  res <- sapply( (1 + shiftT):(trainingSize + shiftT) ,  function(index) {
    dta <- dta.filterByWindow(index, index + window.size - 1, todayData_Y)
    # normalize data, redo linear regression again
    dta$Y <- dta$Y - mean(dta$Y)
    data.lm <- lm(dta$Y ~ dta$X)
    # get the regression coefficients and get samples, use data frame
    coef <- data.frame(
      Intercept = data.lm$coefficients[1],
      Poly_X1 = data.lm$coefficients[2]
    )
    
    data.lm$coefficients}
  )
  
  return(res)
}


linearRegression_model.noShiftDraw <- function( trainingSize, window.size, shiftT, todayData_Y, resPath){
  path.setup(resPath)
  
  dtaTotal <- dta.filterByWindow(1+shiftT, trainingSize+shiftT+window.size-1, todayData_Y)
  dtaTotal_y <- dtaTotal$Y
  yMin <- min(dtaTotal_y)
  yMax <- max(dtaTotal_y)
  
  res <- sapply( (1 + shiftT):(trainingSize + shiftT) , function(index) {
    dta <- dta.filterByWindow(index, index + window.size - 1, todayData_Y)
    # normalize data, redo linear regression again
    data.lm <- lm(dta$Y ~ dta$X)
    # get the regression coefficients and get samples, use data frame
    coef <- data.frame(
      Intercept = data.lm$coefficients[1],
      Poly_X1 = data.lm$coefficients[2]
    )
    
    # get 100 sample points from curve line 
    sampleX <- seq(1, window.size , length = 100)
    sampleFitY <- coef$Intercept + coef$Poly_X1 * sampleX  
    # draw the picture of regression
    draw(make.path(resPath, paste0(index, '.png')), {
      plot(dta$Y, cex = 1, ylim = c(yMin,yMax))
      points(data.lm$fitted.values, cex = 1, col = 'red', ylim = c(yMin,yMax))
      lines(sampleX, sampleFitY, col = 'red', ylim = c(yMin,yMax))
    }, width = 800, height = 600)
    
    data.lm$coefficients }
  )
  
  return(res)
}