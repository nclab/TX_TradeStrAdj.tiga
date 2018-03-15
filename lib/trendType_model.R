## =====  trendType_model     KYGu  =====
# distinguish different class of regression type
# ========================================

trendType_model.6type <- function( clusterRegTrend, clusterRegMaxIndex, clusterRegMinIndex, window.size){
  
  type <- 0
  if( clusterRegTrend == 1){  # upward 
    if(clusterRegMinIndex == 1 && clusterRegMaxIndex == window.size)  # line
      type <- 1   
    else{
       if(clusterRegMaxIndex == 1 && clusterRegMinIndex == window.size)
          type <- 2
       else
          type <- 3
    }
  }
  if( clusterRegTrend == 0){  # downward
    if(clusterRegMaxIndex == 1 && clusterRegMinIndex == window.size)  # line
      type <- 4
    else{
      if(clusterRegMinIndex == 1 && clusterRegMaxIndex == window.size)
        type <- 5
      else
        type <- 6
    }
  }
  
  return(type)
}


