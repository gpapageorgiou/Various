random_seed <- function() {
  start_seed <- Sys.time()
  start_seed <- as.integer(start_seed)
  start_seed <<- round(start_seed / 10, digits = 0)
  start_seed
}


