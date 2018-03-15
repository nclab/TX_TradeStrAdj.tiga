path.setup <- function(path) {
  if (!file.exists(path)) dir.create(path, recursive=T)
  path
}

make.path <- function(...) {
  args <- list(...)
  n <- length(args)
  if (n == 1) {
    args[[1]]
  } else {
    path <- gsub("/$", "", args[[1]])
    n <- length(args)
    if (n > 2)
      for (i in  2:(n - 1)) {
        path <- paste(path, gsub("^/|/$", "", args[[i]]), sep ='/')
      }
    paste(path, gsub("^/", "", args[[n]]), sep ='/') 
  }
}

