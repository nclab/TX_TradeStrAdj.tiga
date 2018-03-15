evaluate <- function(data.ent, data.ext, entrance, exit) {
  n <- dim(data.ent)[1]
  actions <- lapply(1:n, function(i) {
    entrance(data.ent[1:i,])
  })
  strategy.return <- sum(sapply(actions, function(a) {exit(a, data.ext)}))
  strategy.return
}
