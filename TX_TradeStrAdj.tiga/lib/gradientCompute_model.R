# === gradient computing model function ===
# Input  : vector f, window.size
# Output : regGradient (matrix)
# =========================================
gradientCompute_model <- function( f, window.size){
  # get regression curve window.size points
  regPoint <- f(1:window.size)
  regGradient <- matrix(0, nrow = 1, ncol = 1)
  # classify each regression to upward or downward by gradient
  for(i in 1:length(regPoint) - 1)
    regGradient <- cbind( regGradient, regPoint[i+1] - regPoint[i])
  # remove useless column
  regGradient <- regGradient[,-1]
  
  return(regGradient)
}