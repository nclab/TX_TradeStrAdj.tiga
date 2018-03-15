

classify_model_reg6Type.correlation <- function( trainingSize, window.size, shiftT, todayData_Y, classify_comErrInterval, cluster_cluster1RegP, cluster_cluster2RegP, cluster_cluster3RegP, cluster_cluster4RegP, cluster_cluster5RegP, cluster_cluster6RegP, cluster_clusterInfo, classify.path){
  
  startIndex <- trainingSize + window.size + shiftT - classify_comErrInterval + 1
  classifyData <- dta.filterByWindow( startIndex, startIndex+window.size-1, todayData_Y)
  classify_seqX <- classifyData[1:classify_comErrInterval,]
  # shift data by minus mean 
  classify_seqX$Y <- classify_seqX$Y - mean(classify_seqX$Y)
  data.lm <- lm(classify_seqX$Y ~ classify_seqX$X + I(classify_seqX$X^2))
  # let value range to be between 0 and 1
  classify_seqX$Y <- classify_seqX$Y - min(classify_seqX$Y)
  classify_seqX$Y <- classify_seqX$Y / max(classify_seqX$Y)
  lmFittedValue <- data.lm$fitted.values
  lmFittedValue <- lmFittedValue - min(lmFittedValue)
  lmFittedValue <- lmFittedValue / max(lmFittedValue)
  
  lmFitV.grad <- matrix(0, nrow = 1, ncol = 1)
  for(i in 1:length(lmFittedValue)-1)
     lmFitV.grad <- cbind( lmFitV.grad, lmFittedValue[i+1] - lmFittedValue[i])
  lmFitV.grad <- lmFitV.grad[,-1]  

#  
  classify_corM <- c(0)
  classify_corMax <- -1000
  classify_maxIndex <- 0
  
  clusterRegPointM <- matrix(1:window.size, ncol = 1, nrow = window.size)
  if( !is.na(cluster_cluster1RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster1RegP)
  if( !is.na(cluster_cluster2RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster2RegP)
  if( !is.na(cluster_cluster3RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster3RegP)
  if( !is.na(cluster_cluster4RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster4RegP)
  if( !is.na(cluster_cluster5RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster5RegP)
  if( !is.na(cluster_cluster6RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster6RegP)
  clusterRegPointM <- clusterRegPointM[,-1]
  
  for(a in 1:ncol(clusterRegPointM)){ # each cluster result
    clusterReg_seqY <- dta.filterByWindow(1, classify_comErrInterval, clusterRegPointM[,a])
    clusterReg_seqY$Y <- clusterReg_seqY$Y - mean(clusterReg_seqY$Y)
    # let value range to be between 0 and 1
    clusterReg_seqY$Y <- clusterReg_seqY$Y - min(clusterReg_seqY$Y) 
    clusterReg_seqY$Y <- clusterReg_seqY$Y / max(clusterReg_seqY$Y)
    
    seqY.grad <- matrix(0, nrow = 1, ncol = 1)
    for(i in 1:length(clusterReg_seqY$Y)-1)
       seqY.grad <- cbind( seqY.grad, clusterReg_seqY$Y[i+1] - clusterReg_seqY$Y[i])
    seqY.grad <- seqY.grad[,-1]
   
    # compute correlation(x,y)
    # compute numerator
    cor_numerator <- sum( lmFitV.grad * seqY.grad )
    
    # compute denominator
    temp1 <- sum( lmFitV.grad * lmFitV.grad )
    temp2 <- sum( seqY.grad * seqY.grad )
    cor_denominator <- sqrt(temp1 * temp2)
    
    correlation <- cor_numerator / cor_denominator
    classify_corM <- rbind(classify_corM, correlation)
    if(correlation > classify_corMax){
      classify_corMax <- correlation
      classify_maxIndex <- a
    }
  }
  
  # Delete row 1 (which is useless)
  classify_corM <- classify_corM[-1,]
  return_list <- list("classify_corM" = classify_corM, "classify_corMax" = classify_corMax, "classify_maxIndex" = classify_maxIndex)
  
  return(return_list)
}



classify_model_reg6Type.correlation.Draw <- function( trainingSize, window.size, shiftT, todayData_Y, classify_comErrInterval, cluster_cluster1RegP, cluster_cluster2RegP, cluster_cluster3RegP, cluster_cluster4RegP, cluster_cluster5RegP, cluster_cluster6RegP, cluster_clusterInfo, classify.path){
  
  
  startIndex <- trainingSize + window.size + shiftT - classify_comErrInterval + 1
  classifyData <- dta.filterByWindow( startIndex, startIndex+window.size-1, todayData_Y)
  classify_seqX <- classifyData[1:classify_comErrInterval,]
  # shift data by minus mean 
  classify_seqX$Y <- classify_seqX$Y - mean(classify_seqX$Y)
  data.lm <- lm(classify_seqX$Y ~ classify_seqX$X + I(classify_seqX$X^2))
  #  
  classify_seqX$Y <- classify_seqX$Y - min(classify_seqX$Y)
  classify_seqX$Y <- classify_seqX$Y / max(classify_seqX$Y)
  lmFittedValue <- data.lm$fitted.values
  lmFittedValue <- lmFittedValue - min(lmFittedValue)
  lmFittedValue <- lmFittedValue / max(lmFittedValue)
  
  lmFitV.grad <- matrix(0, nrow = 1, ncol = 1)
  for(i in 1:length(lmFittedValue)-1)
    lmFitV.grad <- cbind( lmFitV.grad, lmFittedValue[i+1] - lmFittedValue[i])
  lmFitV.grad <- lmFitV.grad[,-1]  
  
  #  
  classify_corM <- c(0)
  classify_corMax <- -1000
  classify_maxIndex <- 0
  
  clusterRegPointM <- matrix(1:window.size, ncol = 1, nrow = window.size)
  if( !is.na(cluster_cluster1RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster1RegP)
  if( !is.na(cluster_cluster2RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster2RegP)
  if( !is.na(cluster_cluster3RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster3RegP)
  if( !is.na(cluster_cluster4RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster4RegP)
  if( !is.na(cluster_cluster5RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster5RegP)
  if( !is.na(cluster_cluster6RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster6RegP)
  clusterRegPointM <- clusterRegPointM[,-1]
  
  for(a in 1:ncol(clusterRegPointM)){ # each cluster result
    clusterReg_seqY <- dta.filterByWindow(1, classify_comErrInterval, clusterRegPointM[,a])
    clusterReg_seqY$Y <- clusterReg_seqY$Y - mean(clusterReg_seqY$Y)
    #    
    clusterReg_seqY$Y <- clusterReg_seqY$Y - min(clusterReg_seqY$Y) 
    clusterReg_seqY$Y <- clusterReg_seqY$Y / max(clusterReg_seqY$Y)
    
    seqY.grad <- matrix(0, nrow = 1, ncol = 1)
    for(i in 1:length(clusterReg_seqY$Y)-1)
      seqY.grad <- cbind( seqY.grad, clusterReg_seqY$Y[i+1] - clusterReg_seqY$Y[i])
    seqY.grad <- seqY.grad[,-1]
    #    
    # compute correlation(x,y)
    # compute numerator
    cor_numerator <- sum( lmFitV.grad * seqY.grad )
    
    # compute denominator
    temp1 <- sum( lmFitV.grad * lmFitV.grad )
    temp2 <- sum( seqY.grad * seqY.grad )
    cor_denominator <- sqrt(temp1 * temp2)
    
    correlation <- cor_numerator / cor_denominator
    classify_corM <- rbind(classify_corM, correlation)
    if(correlation > classify_corMax){
      classify_corMax <- correlation
      classify_maxIndex <- a
    }
  }
  
  color <- c('blue','green','deeppink','orange','peachpuff4','cyan')
  path.setup(classify.path)
  draw(make.path(classify.path, paste0( paste0( paste0( paste0('result_', classify_maxIndex),'_trend6:'), cluster_clusterInfo[classify_maxIndex,]$type2), '.png')), {
    plot(classify_seqX$Y, cex = 1, col = 'red')
    lines(lmFittedValue, cex = 1, col = 'red')
    for(a in 1:ncol(clusterRegPointM)){ # each cluster result
      clusterReg_seqY <- dta.filterByWindow(1, classify_comErrInterval, clusterRegPointM[,a])
      clusterReg_seqY$Y <- clusterReg_seqY$Y - mean(clusterReg_seqY$Y)
      #    
      clusterReg_seqY$Y <- clusterReg_seqY$Y - min(clusterReg_seqY$Y) 
      clusterReg_seqY$Y <- clusterReg_seqY$Y / max(clusterReg_seqY$Y)
      lines(clusterReg_seqY$X, clusterReg_seqY$Y, cex = 1, col = color[a])
    }
  }, width = 800, height = 600)
  
  
  # Delete row 1 (which is useless)
  classify_corM <- classify_corM[-1,]
  return_list <- list("classify_corM" = classify_corM, "classify_corMax" = classify_corMax, "classify_maxIndex" = classify_maxIndex)
  
  return(return_list)
}



classify_model_reg6Type.v2 <- function( trainingSize, window.size, shiftT, todayData_Y, classify_comErrInterval, cluster_cluster1RegP, cluster_cluster2RegP, cluster_cluster3RegP, cluster_cluster4RegP, cluster_cluster5RegP, cluster_cluster6RegP){
  
  startIndex <- trainingSize + window.size + shiftT - classify_comErrInterval + 1
  classifyData <- dta.filterByWindow( startIndex, startIndex+window.size-1, todayData_Y)
  #normalize data
  classifyData$Y <- classifyData$Y - mean(classifyData$Y)
  classify_computeErr <- classifyData[1:classify_comErrInterval,]
  
  classify_tempErrM <- c(0)
  classify_errMin <- 1000
  classify_minIndex <- 0
  
  clusterRegPointM <- matrix(1:30, ncol = 1, nrow = 30)
  if( !is.na(cluster_cluster1RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster1RegP)
  if( !is.na(cluster_cluster2RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster2RegP)
  if( !is.na(cluster_cluster3RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster3RegP)
  if( !is.na(cluster_cluster4RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster4RegP)
  if( !is.na(cluster_cluster5RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster5RegP)
  if( !is.na(cluster_cluster6RegP)[1] )
    clusterRegPointM <- cbind(clusterRegPointM, cluster_cluster6RegP)
  clusterRegPointM <- clusterRegPointM[,-1]
  
  for(a in 1:ncol(clusterRegPointM)){ # each cluster result
    # normalize data
    tempClusterReg <- dta.filterByWindow(1, classify_comErrInterval, clusterRegPointM[,a])
    tempClusterReg <- tempClusterReg$Y - mean(tempClusterReg$Y)
    # compute min square error
    sError <- 0
    for(b in 1:classify_comErrInterval){
      # cat(test$Y[b], clusterRegArr[,a][b], sError, "\n", sep=",")
      sError <- sError + abs(classify_computeErr$Y[b] - tempClusterReg[b])
      # cat(sError,"\n")
    }
    classify_tempErrM <- rbind(classify_tempErrM, sError)
    if(sError < classify_errMin){
      classify_errMin <- sError
      classify_minIndex <- a
    }
  }
  # Delete row 1 (which is useless)
  classify_tempErrM <- classify_tempErrM[-1,]
  return_list <- list("classify_ErrM" = classify_tempErrM, "classify_errMin" = classify_errMin, "classify_minIndex" = classify_minIndex)
  
  return(return_list)
}


classify_model_reg6Type.gradient <- function( trainingSize, window.size, shiftT, todayData_Y, classify_comErrInterval, cluster_clusterInfo){
  
  startIndex <- trainingSize + window.size + shiftT - classify_comErrInterval + 1
  classifyData <- dta.filterByWindow( startIndex, startIndex+window.size-1, todayData_Y)
  classify_seqX <- classifyData[1:classify_comErrInterval,]
  # shift data by minus mean 
  classify_seqX$Y <- classify_seqX$Y - mean(classify_seqX$Y)
  
  classify.lm <- lm(classify_seqX$Y ~ classify_seqX$X + I(classify_seqX$X^2))
  
  classify_lmGrad <- matrix(0, nrow = 1, ncol = 1)
  # classify each regression to upward or downward by gradient
  for(i in 1:length(classify.lm$fitted.values) - 1)
    classify_lmGrad <- cbind( classify_lmGrad, classify.lm$fitted.values[i+1] - classify.lm$fitted.values[i])
  # remove useless column
  classify_lmGrad <- classify_lmGrad[,-1]
  
  # if clusterRegGradient 越來越大(- to +, + to +) => upward , else downward
  decreaseNum <- 0
  increaseNum <- 0
  
  for(udcheck in 2:length(classify_lmGrad)){
    if(classify_lmGrad[udcheck] > classify_lmGrad[udcheck - 1])
      increaseNum <- increaseNum + 1
    if(classify_lmGrad[udcheck] < classify_lmGrad[udcheck - 1])
      decreaseNum <- decreaseNum + 1
  }
  
  classify_trend <- -1
  # get trend 
  if(increaseNum > 0 && decreaseNum == 0)
    classify_trend <- 1
  if(decreaseNum > 0 && increaseNum == 0)
    classify_trend <- 0
  
  # get the min max point of the cluster regression
  classifyMinIndex <- which.min(classify.lm$fitted.values)
  classifyMaxIndex <- which.max(classify.lm$fitted.values)
  classify_diffMaxMin <- classifyMaxIndex - classifyMinIndex
  classify_trend4 <- trendType_model.4type( classify_trend, classifyMaxIndex, classifyMinIndex, window.size)
  classify_trend6 <- trendType_model.6type( classify_trend, classifyMaxIndex, classifyMinIndex, window.size)
  
  return_list <- list("classify_trend" = classify_trend, "classify_trend4" = classify_trend4, "classify_trend6" = classify_trend6)
  return(return_list)
  
}

