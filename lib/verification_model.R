
## ======= verification model ==================
# get all window.size data from future to check it trend and class type 

verification_model.withoutDraw <- function( trainingSize, window.size, shiftT, todayData_Y){
  
  startIndex <- trainingSize + window.size + shiftT - classify_comErrInterval + 1
  verifyData <- dta.filterByWindow( startIndex, startIndex+window.size-1, todayData_Y)
  #normalize data, redo linear regression again
  verifyData$Y <- verifyData$Y - mean(verifyData$Y)
  verify_Data.lm <- lm(verifyData$Y ~ verifyData$X + I(verifyData$X^2))
  
  currentData_classify <- dta.filterByWindow(startIndex, startIndex+(classify_comErrInterval-1), todayData_Y)
  cdClassify_y <- currentData_classify$Y
  cdClassify_y <- cdClassify_y - mean(cdClassify_y)
  cdClassify_x <- 1:length(cdClassify_y)
  cdClassify.lm <- lm(cdClassify_y ~ cdClassify_x + I(cdClassify_x^2))
  
  currentData_future <- dta.filterByWindow(startIndex+classify_comErrInterval, startIndex+(window.size-1), todayData_Y)
  cdFuture_y <- currentData_future$Y
  cdFuture_y <- cdFuture_y - mean(cdFuture_y)
  cdFuture_x <- 1:length(cdFuture_y)
  cdFuture_x <- classify_comErrInterval + cdFuture_x
  cdFuture.lm <- lm(cdFuture_y ~ cdFuture_x + I(cdFuture_x^2))

  verif_grad <- matrix(0, nrow = 1, ncol = 1)
  
  for(ver_x in 1:(length(verify_Data.lm$fitted.values) - 1)){
    verif_grad <- cbind(verif_grad, verify_Data.lm$fitted.values[ver_x + 1] - verify_Data.lm$fitted.values[ver_x])
  }
  # remove useless column
  verif_grad <- verif_grad[,-1]
  
  # if verif_grad 越來越大(- to +, + to +) => upward , else downward
  decreaseNum <- 0
  increaseNum <- 0
  
  for(udcheck in 2:length(verif_grad)){
    if(verif_grad[udcheck] > verif_grad[udcheck-1])
      increaseNum <- increaseNum + 1
    if(verif_grad[udcheck] < verif_grad[udcheck-1])
      decreaseNum <- decreaseNum + 1
  }
  
  verif_trend <- -1
  # get trend 
  if(increaseNum > 0 && decreaseNum == 0)
    verif_trend <- 1
  if(decreaseNum > 0 && increaseNum == 0)
    verif_trend <- 0
  
  # get the min max point of the cluster regression
  verif_minIndex <- as.numeric(which.min(verify_Data.lm$fitted.values))
  verif_maxIndex <- as.numeric(which.max(verify_Data.lm$fitted.values))
  verif_regType <- trendType_model.6type(verif_trend, verif_maxIndex, verif_minIndex, window.size)
  
  return_list <- list("verif_grad" = verif_grad, "verif_trend" = verif_trend, "verif_regType" = verif_regType, "verif_minIndex" = verif_minIndex, "verif_maxIndex" = verif_maxIndex, "regFittedValue" = verify_Data.lm$fitted.values, "cdClassify_fitValue" = cdClassify.lm$fitted.values, "cdFuture_fitValue" = cdFuture.lm$fitted.values)
  return(return_list)
  
}



classifyToCluster_draw <- function(temp, trainingSize, window.size, shiftT, todayData_Y, draw.path){
  center1 <- temp$cenPar_intecept
  center2 <- temp$cenPar_firstOrder
  center3 <- temp$cenPar_secondOrder
  
  startIndex <- trainingSize + window.size + shiftT - classify_comErrInterval + 1
  dta <- dta.filterByWindow( startIndex, startIndex + window.size - 1, todayData_Y)
  # normalize data, redo linear regression again
  dta$Y <- dta$Y - mean(dta$Y)
  
  # this function : regresssion function  y ~ intercept + center*x + center*x^2
  f <- function(x) {
    center1 + center2 * x + center3 * x * x
  }
  f <- Vectorize(f)
  
  path.setup(draw.path)
  draw(make.path(draw.path, paste0(shiftT, '.jpg')), {
    plot(f(1:window.size))
    points(dta$Y, cex = 1, col = 'blue')
    lines(f(1:100), col = 'red') # 100 samples => perform a line
  }, width = 800, height = 600)
}


verificationCluster_draw <- function(cluster_clusterInfo, targetIndex, trainingSize, window.size, shiftT, draw.path){
   intercept <- cluster_clusterInfo[targetIndex,]$cenPar_intecept
   firstOrder <- cluster_clusterInfo[targetIndex,]$cenPar_firstOrder
   secondOrder <- cluster_clusterInfo[targetIndex,]$cenPar_secondOrder
   
   # this function : regresssion function  y ~ intercept + center*x + center*x^2
   f <- function(x) {
      intercept + firstOrder * x + secondOrder * x * x
   }
   f <- Vectorize(f)

   draw(make.path(draw.path, paste0( paste0( paste0( paste0("ShiftT_", shiftT), "_belongto_"), targetIndex), ".png"))  , {
      plot(f(1:window.size))
      lines(f(1:100), col = 'red') # 100 samples => perform a line
   }, width = 800, height = 600)
}

