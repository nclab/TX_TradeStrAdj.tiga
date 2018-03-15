

currentData_Info <- function(trainingSize, window.size, shiftT, classify_comErrInterval, todayData_Y, todayData_total, classifyRegPoint, classify_UpDown, classify_maxIndex, cluster_clusterInfo, draw.path){
   startIndex <- trainingSize + window.size + shiftT - classify_comErrInterval + 1
   
   currentData_total <- dta.filterByWindow(startIndex, startIndex+(window.size-1), todayData_Y)
   cdTotal_y <- currentData_total$Y
   #cdTotal_y <- cdTotal_y - mean(cdTotal_y)
   cdTotal_x <- 1:length(cdTotal_y)
   cdTotal.lm <- lm(cdTotal_y ~ cdTotal_x + I(cdTotal_x^2))
   
   currentData_classify <- dta.filterByWindow(startIndex, startIndex+(classify_comErrInterval-1), todayData_Y)
   cdClassify_y <- currentData_classify$Y
   #cdClassify_y <- cdClassify_y - mean(cdClassify_y)
   cdClassify_x <- 1:length(cdClassify_y)
   cdClassify.lm <- lm(cdClassify_y ~ cdClassify_x + I(cdClassify_x^2))
   data_volumn <- totalDta.filterByWindow(startIndex, startIndex+(classify_comErrInterval-1), todayData_total)$TotalVolume
   data_high <- totalDta.filterByWindow(startIndex, startIndex+(classify_comErrInterval-1), todayData_total)$High
   data_low <- totalDta.filterByWindow(startIndex, startIndex+(classify_comErrInterval-1), todayData_total)$Low
   data_close <- totalDta.filterByWindow(startIndex, startIndex+(classify_comErrInterval-1), todayData_total)$Close
   volumn.lm <- lm(data_volumn ~ cdClassify_x + I(cdClassify_x^2))
   
   currentData_future <- dta.filterByWindow(startIndex+classify_comErrInterval, startIndex+(window.size-1), todayData_Y)
   cdFuture_y <- currentData_future$Y
   #cdFuture_y <- cdFuture_y - mean(cdFuture_y)
   cdFuture_x <- 1:length(cdFuture_y)
   cdFuture_x <- classify_comErrInterval + cdFuture_x
   cdFuture.lm <- lm(cdFuture_y ~ cdFuture_x + I(cdFuture_x^2))
   
   #yMin <- min(cdTotal_y, classifyRegPoint)
   #yMax <- max(cdTotal_y, classifyRegPoint)
   yMin <- min(cdTotal_y)
   yMax <- max(cdTotal_y)
   
   vMin <- min(todayData_total$TotalVolume)
   vMax <- max(todayData_total$TotalVolume)
   
   path.setup(draw.path)
   draw(make.path(draw.path, paste0( paste0( paste0( paste0( paste0( paste0('Time_', shiftT), '_result'), classify_maxIndex), '_trend6:'), cluster_clusterInfo[classify_maxIndex,]$type2), '.png')), {
     par(mfrow = c(1,3)) 
     # first picture
     plot(1:length(classifyRegPoint),classifyRegPoint, cex = 1, xlim = c(1,window.size), xlab = 'window.size', ylab = 'shiftedPrice', main = 'classifyToRegressionType')
     par(new=TRUE)
     lines(1:length(classifyRegPoint),classifyRegPoint, col = 'red', cex = 1, xlim = c(1,window.size))
     
     # second picture
     plot(cdClassify_x, cdClassify_y, cex = 1, col = 'blue', xlim = c(1,window.size), ylim = c(yMin,yMax), xlab = 'window.size', ylab = 'Minute open price', main = paste0('shiftT_',shiftT))
     par(new=TRUE)
     plot(cdFuture_x, cdFuture_y, cex = 1, col = 'red', xlim = c(1,window.size), ylim = c(yMin,yMax))
     par(new=TRUE)
     lines(cdClassify_x, cdClassify.lm$fitted.values, cex = 1, col = 'blue', xlim = c(1,window.size), ylim = c(yMin,yMax))
     par(new=TRUE)
     lines(cdFuture_x, cdFuture.lm$fitted.values, cex = 1, col = 'red', xlim = c(1,window.size), ylim = c(yMin,yMax))
     par(new=TRUE)
     lines(cdClassify_x, data_high, cex = 1, col = 'green', xlim = c(1,window.size), ylim = c(yMin,yMax))
     par(new=TRUE)
     lines(cdClassify_x, data_low, cex = 1, col = 'orange', xlim = c(1,window.size), ylim = c(yMin,yMax))
     par(new=TRUE)
     lines(cdClassify_x, data_close, cex = 1, col = 'black', xlim = c(1,window.size), ylim = c(yMin,yMax))
     par(new=TRUE)
     lines(cdTotal_x, cdTotal.lm$fitted.values, cex = 1, col = 'brown', xlim = c(1,window.size), ylim = c(yMin,yMax))
         
     # third picture
     plot(cdClassify_x, data_volumn, cex = 1, col = 'red', xlim = c(1,window.size), ylim = c(vMin,vMax), main = 'TotalVolumn_everymin', xlab = 'min', ylab = 'total_volumn')
     lines(cdClassify_x, volumn.lm$fitted.values, cex = 1, col = 'red', xlim = c(1,window.size), ylim= c(vMin,vMax))
   }, width = 1200, height = 600)
}


