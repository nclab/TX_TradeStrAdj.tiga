diagnostics <- function(data, model, res.path, data.diff = NULL) {
  res.name <- as.character(model)[2]
  # lineaar fit
  data.lm <- lm(model,  data = data)
  draw(paste0(res.path, "lm.fitted.png"), {
    plot(data[,res.name], type="p", cex=0.1, col="red")
    points(data.lm$fitted.values, type="p", cex=0.1)  
  })
  # graphical check for heteroskedasticity
  draw(paste0(res.path, "lm.diagnostics.png"), {
    par(mfrow=c(2,2))
    plot(data.lm, cex = 0.3)
  })

  # Summary 
  data.summary <- summary(data.lm)
  data.summary <- list(
    coef = data.summary$coefficients,
    r.squared = data.summary$r.squared, 
    adj.r.squared = data.summary$adj.r.squared,
    bptest = lmtest::bptest(data.lm),
    ncvTest = car::ncvTest(data.lm)
  )
  summarize(data.summary, file = paste0(res.path, "lm.summary.txt"))

  # elementary evaluation
  benchmark <- data[,paste0(res.name, 1)]
  pos.hit <- sum(data.lm$fitted.values > benchmark & data[,res.name] > benchmark)
  neg.hit <- sum(data.lm$fitted.values < benchmark & data[,res.name] < benchmark)
  correct.rate <- (pos.hit + neg.hit)/dim(data)[1]
  eval <- c(pos.hit = pos.hit, neg.hit = neg.hit, correct.rate = correct.rate)
  res <- list(summary = data.summary, eval = eval)
  if(is.null(data.diff)) {
    res
  } else {
    data.lm.diff <- lm(model,  data = data.diff)
    draw(paste0(res.path, "lm.fitted.1stdiff.png"), {
      plot(data.diff[,res.name], type="p", cex=0.1, col="red")
      points(data.lm.diff$fitted.values, type="p", cex=0.1)  
    })
    # graphical check for heteroskedasticity
    draw(paste0(res.path, "lm.diagnostics.1stdiff.png"), {
      par(mfrow=c(2,2))
      plot(data.lm.diff, cex = 0.3)
    })
    # Summary 
    data.summary <- summary(data.lm.diff)
    data.summary <- list(
      coef = data.summary$coefficients,
      r.squared = data.summary$r.squared, 
      adj.r.squared = data.summary$adj.r.squared,
      bptest = lmtest::bptest(data.lm),
      ncvTest = car::ncvTest(data.lm)
    )
    summarize(data.summary, file = paste0(res.path, "lm.summary.1stdiff.txt"))
    benchmark <- data.diff[,res.name]
    pos.hit <- sum(data.lm.diff$fitted.values > 0 & benchmark > 0)
    neg.hit <- sum(data.lm.diff$fitted.values < 0 & benchmark < 0)
    correct.rate <- (pos.hit + neg.hit)/dim(data.diff)[1]
    eval <- c(pos.hit = pos.hit, neg.hit = neg.hit, correct.rate = correct.rate)
    list(
      ori = res,
      diff = list(summary = data.summary, eval = eval)
    )
  }
}

ts_order_selection <- function(data, densities, effective.thres = 0.05, maxp = 20, FDR = T) {
  require(MTS)
  lapply(densities, function(density) {
    data <- time.series.filter(data = data,
                               density = density)[,2:6]
    n <- dim(data)[1]
    thres <- effective.thres/maxp
    mq.statistics <- VARorder(data, maxp = maxp, output = F)
    pvals <- mq.statistics$Mpv
    ordered.pvals <- pvals[order(pvals)]
    ordered.pvals <- ordered.pvals[sapply(1:maxp, function(k) {
      ordered.pvals[k] <= k/maxp * effective.thres
    })]
    thres <- effective.thres
    if (FDR && length(ordered.pvals) > 0) {
      thres <- max(ordered.pvals)
    }
    cat('maxp:', maxp, ', density:', density, 'threshold:', thres, '\n')
    likelihoodor <- which(mq.statistics$Mpv <= thres)
    if (length(likelihoodor) > 0)
      likelihoodor <- likelihoodor[length(likelihoodor)]
    else
      likelihoodor <- 0
    list(or = c(aic = mq.statistics$aicor, 
                bic = mq.statistics$bicor, 
                hq = mq.statistics$hqor, 
                likelihood = likelihoodor), 
         thres = thres, 
         pvals = pvals)
  })
}
