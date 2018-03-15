
clustering_model.reg_6_type <- function(res, window.size, trainingSize){
  # data frame to store information
  regInfo <- data.frame(
    trend = as.numeric(0),
    type = as.numeric(0),
    minIndex = as.integer(0),
    maxIndex = as.integer(0),
    cenPar_intecept = as.numeric(0),
    cenPar_firstOrder = as.numeric(0),
    cenPar_secondOrder = as.numeric(0)
  )
  
  regs <- as.matrix(t(res))
  regGradientM <- matrix(1:(window.size-1), nrow = window.size - 1, ncol = 1)
  
  for(i in 1:trainingSize){
    reg <- regs[i,]
    # this function : regresssion function  y ~ intercept + center*x + center*x^2
    f <- function(x) {
      reg[1] + reg[2]*x + reg[3] * x * x
    }
    f <- Vectorize(f)
    
    regPoint <- f(1:window.size)
    # call gradientCompute_model
    regGradient <- gradientCompute_model( f, window.size)
    regGradientM <- cbind(regGradientM, regGradient)
    
    # if clusterRegGradient 越來越大(- to +, + to +) => upward , else downward
    decreaseNum <- 0
    increaseNum <- 0
    
    for(udcheck in 2:length(regGradient)){
      if(regGradient[udcheck] > regGradient[udcheck - 1])
        increaseNum <- increaseNum + 1
      if(regGradient[udcheck] < regGradient[udcheck - 1])
        decreaseNum <- decreaseNum + 1
    }
    
    regTrend <- -1
    # get trend 
    if(increaseNum > 0 && decreaseNum == 0)
      regTrend <- 1
    if(decreaseNum > 0 && increaseNum == 0)
      regTrend <- 0
    
    # get the min max point of the cluster regression
    regMinIndex <- which.min(regPoint)
    regMaxIndex <- which.max(regPoint)
    regType <- trendType_model.6type( regTrend, regMaxIndex, regMinIndex, window.size)
    
    # store information to centerInfo   
    regInfo <- rbind(regInfo, data.frame(
      trend = regTrend,
      type = regType,
      minIndex = regMinIndex,
      maxIndex = regMaxIndex,
      cenPar_intecept = reg[1],
      cenPar_firstOrder = reg[2],
      cenPar_secondOrder = reg[3]
    )) 
    
  }
  regInfo <- regInfo[-1,]
  regGradientM <- regGradientM[,-1]
  
  ## compute 6 class regressions cluster center
  regType1M <- matrix(c(0,0,0), nrow = 1, ncol = 3)
  regType2M <- matrix(c(0,0,0), nrow = 1, ncol = 3)
  regType3M <- matrix(c(0,0,0), nrow = 1, ncol = 3)
  regType4M <- matrix(c(0,0,0), nrow = 1, ncol = 3)
  regType5M <- matrix(c(0,0,0), nrow = 1, ncol = 3)
  regType6M <- matrix(c(0,0,0), nrow = 1, ncol = 3)
  
  for(j in 1:trainingSize){
     if(regInfo$type[j] == 1){
        regType1M <- rbind(regType1M, matrix(c(regInfo$cenPar_intecept[j], regInfo$cenPar_firstOrder[j], regInfo$cenPar_secondOrder[j]), ncol = 3, nrow = 1))
     }
     if(regInfo$type[j] == 2){
        regType2M <- rbind(regType2M, matrix(c(regInfo$cenPar_intecept[j], regInfo$cenPar_firstOrder[j], regInfo$cenPar_secondOrder[j]), ncol = 3, nrow = 1))
     }
     if(regInfo$type[j] == 3){
        regType3M <- rbind(regType3M, matrix(c(regInfo$cenPar_intecept[j], regInfo$cenPar_firstOrder[j], regInfo$cenPar_secondOrder[j]), ncol = 3, nrow = 1))
     } 
     if(regInfo$type[j] == 4){
        regType4M <- rbind(regType4M, matrix(c(regInfo$cenPar_intecept[j], regInfo$cenPar_firstOrder[j], regInfo$cenPar_secondOrder[j]), ncol = 3, nrow = 1))
     }
     if(regInfo$type[j] == 5){
        regType5M <- rbind(regType5M, matrix(c(regInfo$cenPar_intecept[j], regInfo$cenPar_firstOrder[j], regInfo$cenPar_secondOrder[j]), ncol = 3, nrow = 1))
     } 
     if(regInfo$type[j] == 6){
        regType6M <- rbind(regType6M, matrix(c(regInfo$cenPar_intecept[j], regInfo$cenPar_firstOrder[j], regInfo$cenPar_secondOrder[j]), ncol = 3, nrow = 1))
     }
  }
  
  regType1M <- regType1M[-1,]
  regType1M <- matrix(regType1M, ncol = 3)
  regType2M <- regType2M[-1,]
  regType2M <- matrix(regType2M, ncol = 3)
  regType3M <- regType3M[-1,]
  regType3M <- matrix(regType3M, ncol = 3)
  regType4M <- regType4M[-1,]
  regType4M <- matrix(regType4M, ncol = 3)
  regType5M <- regType5M[-1,]
  regType5M <- matrix(regType5M, ncol = 3)
  regType6M <- regType6M[-1,]
  regType6M <- matrix(regType6M, ncol = 3)
  
  ## store cluster center information   
  clusterInfo <- data.frame(
    trend = as.numeric(0),
    type = as.numeric(0),
    minIndex = as.integer(0),
    maxIndex = as.integer(0),
    cenPar_intecept = as.numeric(0),
    cenPar_firstOrder = as.numeric(0),
    cenPar_secondOrder = as.numeric(0)
  )
  
  # use kmeans, k = 1
  if(nrow(regType1M) != 0){  
     cluster1 <- as.data.frame(regType1M)
     cluster1_center <- kmeans(cluster1, 1)
     
     clusterRegPointM1 <- matrix(1:window.size, nrow = window.size, ncol = 1)
     clusterRegGradientM1 <- matrix(1:(window.size-1), nrow = window.size - 1, ncol = 1)
     # cluster1 information
     f1 <- function(x) {
       cluster1_center$centers[1] + cluster1_center$centers[2] * x + cluster1_center$centers[3] * x * x
     }
     f1 <- Vectorize(f1)
     cluster1RegPoint <- f1(1:window.size)
     # call gradientCompute_model
     cluster1RegGradient <- gradientCompute_model( f1, window.size)
     
     # get the min max point of the cluster regression
     cluster1RegMinIndex <- which.min(cluster1RegPoint)
     cluster1RegMaxIndex <- which.max(cluster1RegPoint)

     
     # store information to centerInfo
     clusterInfo <- rbind(clusterInfo, data.frame(
       trend = 1,
       type = 1,
       minIndex = cluster1RegMinIndex,
       maxIndex = cluster1RegMaxIndex,
       cenPar_intecept = cluster1_center$centers[1],
       cenPar_firstOrder = cluster1_center$centers[2],
       cenPar_secondOrder = cluster1_center$centers[3]
     )) 
  }else{  # empty cluster 
     cluster1RegPoint <- NA
     cluster1_center <- NA
     # store information to centerInfo
     clusterInfo <- rbind(clusterInfo, data.frame(
        trend = NA,
        type = NA,
        minIndex = NA,
        maxIndex = NA,
        cenPar_intecept = NA,
        cenPar_firstOrder = NA,
        cenPar_secondOrder = NA
    )) 
  }
  
  if(nrow(regType2M) != 0){
     cluster2 <- as.data.frame(regType2M)
     cluster2_center <- kmeans(cluster2, 1)
     
     clusterRegPointM2 <- matrix(1:window.size, nrow = window.size, ncol = 1)
     clusterRegGradientM2 <- matrix(1:(window.size-1), nrow = window.size - 1, ncol = 1)
     # cluster2 information
     f2 <- function(x) {
       cluster2_center$centers[1] + cluster2_center$centers[2] * x + cluster2_center$centers[3] * x * x
     }
     f2 <- Vectorize(f2)
     cluster2RegPoint <- f2(1:window.size)
     # call gradientCompute_model
     cluster2RegGradient <- gradientCompute_model( f2, window.size)
     
     # get the min max point of the cluster regression
     cluster2RegMinIndex <- which.min(cluster2RegPoint)
     cluster2RegMaxIndex <- which.max(cluster2RegPoint)
     
     # store information to centerInfo
     clusterInfo <- rbind(clusterInfo, data.frame(
       trend = 1,
       type = 2,
       minIndex = cluster2RegMinIndex,
       maxIndex = cluster2RegMaxIndex,
       cenPar_intecept = cluster2_center$centers[1],
       cenPar_firstOrder = cluster2_center$centers[2],
       cenPar_secondOrder = cluster2_center$centers[3]
     )) 
  }else{  # empty cluster
     cluster2RegPoint <- NA
     cluster2_center <- NA
     # store information to centerInfo
     clusterInfo <- rbind(clusterInfo, data.frame(
       trend = NA,
       type = NA,
       minIndex = NA,
       maxIndex = NA,
       cenPar_intecept = NA,
       cenPar_firstOrder = NA,
       cenPar_secondOrder = NA
     )) 
  }
  
  if(nrow(regType3M) != 0){
     cluster3 <- as.data.frame(regType3M)
     cluster3_center <- kmeans(cluster3, 1)
     
     clusterRegPointM3 <- matrix(1:window.size, nrow = window.size, ncol = 1)
     clusterRegGradientM3 <- matrix(1:(window.size-1), nrow = window.size - 1, ncol = 1)
     # cluster3 information
     f3 <- function(x) {
       cluster3_center$centers[1] + cluster3_center$centers[2] * x + cluster3_center$centers[3] * x * x
     }
     f3 <- Vectorize(f3)
     cluster3RegPoint <- f3(1:window.size)
     # call gradientCompute_model
     cluster3RegGradient <- gradientCompute_model( f3, window.size)
     
     # get the min max point of the cluster regression
     cluster3RegMinIndex <- which.min(cluster3RegPoint)
     cluster3RegMaxIndex <- which.max(cluster3RegPoint)
     
     # store information to centerInfo
     clusterInfo <- rbind(clusterInfo, data.frame(
       trend = 1,
       type = 3,
       minIndex = cluster3RegMinIndex,
       maxIndex = cluster3RegMaxIndex,
       cenPar_intecept = cluster3_center$centers[1],
       cenPar_firstOrder = cluster3_center$centers[2],
       cenPar_secondOrder = cluster3_center$centers[3]
     )) 
  }else{
     cluster3RegPoint <- NA
     cluster3_center <- NA
     # store information to centerInfo
     clusterInfo <- rbind(clusterInfo, data.frame(
       trend = NA,
       type = NA,
       minIndex = NA,
       maxIndex = NA,
       cenPar_intecept = NA,
       cenPar_firstOrder = NA,
       cenPar_secondOrder = NA
     )) 
  }
  
  if(nrow(regType4M) != 0){
     cluster4 <- as.data.frame(regType4M)
     cluster4_center <- kmeans(cluster4, 1)
     
     clusterRegPointM4 <- matrix(1:window.size, nrow = window.size, ncol = 1)
     clusterRegGradientM4 <- matrix(1:(window.size-1), nrow = window.size - 1, ncol = 1)
     f4 <- function(x) {
       cluster4_center$centers[1] + cluster4_center$centers[2] * x + cluster4_center$centers[3] * x * x
     }
     f4 <- Vectorize(f4)
     cluster4RegPoint <- f4(1:window.size)
     # call gradientCompute_model
     cluster4RegGradient <- gradientCompute_model( f4, window.size)
     
     # get the min max point of the cluster regression
     cluster4RegMinIndex <- which.min(cluster4RegPoint)
     cluster4RegMaxIndex <- which.max(cluster4RegPoint)
     
     # store information to centerInfo
     clusterInfo <- rbind(clusterInfo, data.frame(
       trend = 0,
       type = 4,
       minIndex = cluster4RegMinIndex,
       maxIndex = cluster4RegMaxIndex,
       cenPar_intecept = cluster4_center$centers[1],
       cenPar_firstOrder = cluster4_center$centers[2],
       cenPar_secondOrder = cluster4_center$centers[3]
     )) 
  }else{  # cluster4 information
     cluster4RegPoint <- NA
     cluster4_center <- NA
     # store information to centerInfo
     clusterInfo <- rbind(clusterInfo, data.frame(
       trend = NA,
       type = NA,
       minIndex = NA,
       maxIndex = NA,
       cenPar_intecept = NA,
       cenPar_firstOrder = NA,
       cenPar_secondOrder = NA
     )) 
  }

  if(nrow(regType5M) != 0){
    cluster5 <- as.data.frame(regType5M)
    cluster5_center <- kmeans(cluster5, 1)
    
    clusterRegPointM5 <- matrix(1:window.size, nrow = window.size, ncol = 1)
    clusterRegGradientM5 <- matrix(1:(window.size-1), nrow = window.size - 1, ncol = 1)
    f5 <- function(x) {
      cluster5_center$centers[1] + cluster5_center$centers[2] * x + cluster5_center$centers[3] * x * x
    }
    f5 <- Vectorize(f5)
    cluster5RegPoint <- f5(1:window.size)
    # call gradientCompute_model
    cluster5RegGradient <- gradientCompute_model( f5, window.size)
    
    # get the min max point of the cluster regression
    cluster5RegMinIndex <- which.min(cluster5RegPoint)
    cluster5RegMaxIndex <- which.max(cluster5RegPoint)
    
    # store information to centerInfo
    clusterInfo <- rbind(clusterInfo, data.frame(
      trend = 0,
      type = 5,
      minIndex = cluster5RegMinIndex,
      maxIndex = cluster5RegMaxIndex,
      cenPar_intecept = cluster5_center$centers[1],
      cenPar_firstOrder = cluster5_center$centers[2],
      cenPar_secondOrder = cluster5_center$centers[3]
    )) 
  }else{  # cluster5 information
    cluster5RegPoint <- NA
    cluster5_center <- NA
    # store information to centerInfo
    clusterInfo <- rbind(clusterInfo, data.frame(
      trend = NA,
      type = NA,
      minIndex = NA,
      maxIndex = NA,
      cenPar_intecept = NA,
      cenPar_firstOrder = NA,
      cenPar_secondOrder = NA
    )) 
  }
  
  if(nrow(regType6M) != 0){
    cluster6 <- as.data.frame(regType6M)
    cluster6_center <- kmeans(cluster6, 1)
    
    clusterRegPointM6 <- matrix(1:window.size, nrow = window.size, ncol = 1)
    clusterRegGradientM6 <- matrix(1:(window.size-1), nrow = window.size - 1, ncol = 1)
    f6 <- function(x) {
      cluster6_center$centers[1] + cluster6_center$centers[2] * x + cluster6_center$centers[3] * x * x
    }
    f6 <- Vectorize(f6)
    cluster6RegPoint <- f6(1:window.size)
    # call gradientCompute_model
    cluster6RegGradient <- gradientCompute_model( f6, window.size)
    
    # get the min max point of the cluster regression
    cluster6RegMinIndex <- which.min(cluster6RegPoint)
    cluster6RegMaxIndex <- which.max(cluster6RegPoint)
    
    # store information to centerInfo
    clusterInfo <- rbind(clusterInfo, data.frame(
      trend = 0,
      type = 6,
      minIndex = cluster6RegMinIndex,
      maxIndex = cluster6RegMaxIndex,
      cenPar_intecept = cluster6_center$centers[1],
      cenPar_firstOrder = cluster6_center$centers[2],
      cenPar_secondOrder = cluster6_center$centers[3]
    )) 
  }else{  # cluster4 information
    cluster6RegPoint <- NA
    cluster6_center <- NA
    # store information to centerInfo
    clusterInfo <- rbind(clusterInfo, data.frame(
      trend = NA,
      type = NA,
      minIndex = NA,
      maxIndex = NA,
      cenPar_intecept = NA,
      cenPar_firstOrder = NA,
      cenPar_secondOrder = NA
    )) 
  }
  
  clusterInfo <- clusterInfo[-1,]
  
  return_list <- list("regInfo" = regInfo, "clusterInfo" = clusterInfo, "cluster1RegPoint" = cluster1RegPoint, "cluster2RegPoint" = cluster2RegPoint,  "cluster3RegPoint" = cluster3RegPoint, "cluster4RegPoint" = cluster4RegPoint,  "cluster5RegPoint" = cluster5RegPoint,
                      "cluster6RegPoint" = cluster6RegPoint, "cluster1_center" = cluster1_center, "cluster2_center" = cluster2_center, "cluster3_center" = cluster3_center, "cluster4_center" = cluster4_center,
                      "cluster5_center" = cluster5_center, "cluster6_center" = cluster6_center)
  return(return_list)
  
}


clustering_model.reg6type_draw <- function(cluster_clusterInfo, cluster.path){
  
  path.setup(cluster.path)
  color <- c('blue','green','deeppink','orange','peachpuff4','cyan')
  for (i in 1:nrow(cluster_clusterInfo)) {
    
    # this function : regresssion function  y ~ intercept + center*x + center*x^2
    f <- function(x) {
      cluster_clusterInfo$cenPar_intecept[i] + cluster_clusterInfo$cenPar_firstOrder[i] * x + cluster_clusterInfo$cenPar_secondOrder[i] * x * x
    }
    f <- Vectorize(f)
    
    draw(make.path(cluster.path, paste0( paste0( paste0( paste0('cluster_', i), '_trend6:'), cluster_clusterInfo[i,]$type), '.png')), {
      plot(f(1:window.size))
      lines(f(1:100), col = color[i]) # 100 samples => perform a line
    }, width = 800, height = 600)
  }
  
}
