cv_bws_npreg <- function(x, y, bandwidth=1:50, nfolds=10, cv.method = "uniform", visualize.path = NULL) {
  require(np)
  n <- length(x)
  stopifnot(n > 1, length(y) == n)
  stopifnot(length(bandwidth) > 1)
  stopifnot(nfolds > 0, nfolds == trunc(nfolds))
  if(!is.null(visualize.path)) {
    cat("\nVisualizing bandwidth selection... \n")
    bws_visualize(x, y, bandwidth, visualize.path)
  }
  cv.mse <- cv_uniform_npreg(x, y, bandwidth, nfolds)
  mean.mse <- rowMeans(cv.mse)
  best.bw <- bandwidth[which.min(mean.mse)]
  draw(make.path(visualize.path, 'mse.png'), {
    plot(bandwidth, mean.mse, xlab = 'bandwidth', ylab = 'mse')
  })
  list(
    best.bw = best.bw,
    mean.mse = mean.mse,
    cv.mse = cv.mse
  )
}

cv_uniform_npreg <- function(x, y, bandwidth, nfolds) {
  case.folds <- sample(rep(1:nfolds, length.out = length(x)))
  res <- sapply(1:nfolds, function(fold) {
    cat(paste("\n run", fold, "... "))
    train.row <- which(case.folds != fold)
    x.train <- data.frame(time = x[train.row])
    y.train <- y[train.row]
    x.test <- data.frame(time = x[-train.row])
    y.test <- y[-train.row]
    sapply(bandwidth, function(bw) {
      cat(paste0(bw, ", "))
      fit <- npreg(txdat = x.train, tydat = y.train, exdat = x.test, eydat = y.test, bws = bw)
      fit$MSE
    })
  })
  cat('\n')
  res
}

bws_visualize <- function(x, y, bandwidth=1:50, path) {
  source("lib/graphical.R")
  sapply(bandwidth, function(bw) {
    fit <- npreg(txdat = x, tydat = y, bws = bw)
    pic.name <- paste0(bw, ".png")
    draw(make.path(path, pic.name), {
      plot(x, y, cex = 0.01, col = 'red')
      par(new=T)
      plot(x, predict(fit), type = 'l')
      par(new=F)
    })
  })
}

filter_by_diff <- function(x, diff = NULL, probs = c(0.05, 0.95), inv = F) {
  n <- length(x)
  stopifnot(is.null(diff) || ((n - 1) == length(diff)))
  stopifnot(0 <= probs[1] && probs[2] <= 1)
  stopifnot((inv && probs[1] <= probs[2] && (probs[1] != 0 || probs[2] != 1)) ||
            (!inv && probs[1] < probs[2]))
  
  if (is.null(diff))
    diff <- x[2:n] - x[1:(n-1)]
  bounds <- quantile(diff, probs = probs)
  diff.index <- c()
  if (inv)
    diff.index <- which(diff <= bounds[1] | diff >= bounds[2])
  else
    diff.index <- which(diff >= bounds[1] & diff <= bounds[2])
  index <- union(diff.index, diff.index+1)
  index <- index[order(index)]
  x <- x[index]
  names(x) <- index
  x
}

smoothing.profile <- function(path, data,
                              bandwidth = c(1:9, (1:6)*10),
                              filter.steps = 0.5,
                              inv = T,
                              nfolds = 5) {
  result.path <- path.setup(path)
  n <- length(data)[1]
  # get first order difference
  data.diff <- data[2:n] - data[1:(n-1)]
  draw(make.path(result.path, '1stOrderDiff.png'), {
    plot(1:(n-1), data.diff, type = 'l')
  })
  # filtered data by filters specified in filter.step
  visualize.path.filtered <- path.setup(make.path(result.path, "diff_filtered"))
  bws.result <- sapply(filter.steps, function(step) {
    cat("\nbandwidth selection with filter:", c(step, 1-step), ", inv = ", inv, "\n")
    i <- which(step == filter.steps)
    filtered <- filter_by_diff(data, data.diff, c(step, 1-step), inv)
    pic.name <- paste0(i, "-", step, ".png")
    draw(make.path(visualize.path.filtered, pic.name), {
      plot(names(filtered), filtered, type = 'l')
    })
    # run bandwidth selection on filtered data
    visualize.path.smoothing <- path.setup(make.path(result.path, paste0(i, "-filtered_smoothing-", step)))
    cv_bws_npreg(x = as.integer(names(filtered)),
                 y = filtered,
                 bandwidth = bandwidth,
                 nfolds = 5,
                 visualize.path = visualize.path.smoothing)
  })
  draw(make.path(result.path, 'filterVsBestbw.png'), {
    plot(filter.steps, bws.result[1,], xlab = 'filter', ylab = 'best bw')
  })
  save(bws.result, file = make.path(result.path, "bws.result.rda"))
  res <- sapply(bws.result[1,], function(x) x)
  names(res) <- filter.steps
  res
}
