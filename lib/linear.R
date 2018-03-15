linear.profile <- function(model, data, type = 'ts', selector = list(begin = 0, end = Inf, lags = 1, orders = 1:10), visualize.path = NULL) {
  order.selection.res <- sapply(selector$orders, function(order) {
    sapply(selector$lags, function(lag) {
      cat("lag:", lag, ", order:", order, "\n")
      selector$lag <- lag
      selector$order <- order
      data.transformed <- time.series.transform(model = model, data = data, selector = selector)
      if (type == 'direct') ordinary_mse(data.transformed$model, data.transformed$data)
      else if (type == 'cv') cv_mse(data.transformed$model, data.transformed$data)
      else if (type == 'ts_cv') ts_cv_mse(data.transformed$model, data.transformed$data)
      else stop('type must be: cv, direct, or ts, or ts_cv')
    })
  })
  rownames(order.selection.res) <- paste0('lag-', selector$lags)
  colnames(order.selection.res) <- paste0('order-', selector$orders)
  if (!is.null(visualize.path)) {
    visualize.path <- path.setup(visualize.path)
    save(order.selection.res, file = make.path(visualize.path, 'order.selection.res.rda'))
    lapply(1:length(selector$lags), function(i) {
      pic.name <- paste0('lag-', selector$lags[i], '-orderSelection.png')
      draw(make.path(visualize.path, pic.name),
        plot(selector$orders, order.selection.res[i,], main = pic.name, xlab = 'order', ylab = 'mse')
      )
    })
    lapply(1:length(selector$orders), function(i) {
      pic.name <- paste0('order-', selector$orders[i], '-lagSelection.png')
      draw(make.path(visualize.path, pic.name),
           plot(selector$lags, order.selection.res[,i], main = pic.name, xlab = 'lag', ylab = 'mse')
      )
    })
  }
  order.selection.res
}

ordinary_mse <- function(model, data) {
  mean(residuals(lm(model, data))^2)
}

cv_mse <- function(model, data, nfolds = 10) {
  n <- dim(data)[1]
  case.folds <- sample(rep(1:nfolds, length.out = n))
  mses <- sapply(1:nfolds, function(fold) {
    train.row <- which(case.folds != fold)
    data.train <- data[train.row,]
    data.test <- data[-train.row,]
    data.lm <- lm(model, data.train)
    mean((predict(data.lm, data.test) - data.test$High)^2)
  })
  mean(mses)
}

ts_cv_mse <- function(model, data, nfolds = 10, maxfold = 300) {
  n <- dim(data)[1]
  k <- dim(data)[2]
  breaks <- which(sample(rep(1:nfolds, length.out = n)) == 1)
  breaks <- breaks[breaks >= 2 * k] # avoid rank deficiency
  breaks <- sample(breaks)
  breaks <- breaks[1:min(maxfold, length(breaks))] # limit computation resource
  mses <- sapply(breaks, function(T) {
    data.train <- data[1:T,]
    data.test <- data[(T+1):(T+5),]
    data.lm <- lm(model, data.train)
    mean((predict(data.lm, data.test) - data.test$High)^2)
  })
  mean(mses)
}
