draw <- function(file, code, width = 1920, height = 1200) {
  png(file, width = width, height = height)
  code
  dev.off()
}

summarize <- function(file, results) {
  cat("", file = file)
  Map(function(title, result) {
    cat(title, sep = "\n", file = file, append=TRUE)
    cat(capture.output(result), sep = "\n", file = file, append=TRUE)
    cat("\n", file = file, append=TRUE)
  }, names(results), results)
  results
}

