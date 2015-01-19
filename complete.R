complete <- function(directory, id = 1:332) {

  
  dir.home <- getwd()
  setwd (directory)
  
  nobs <- rep (NA, length (id))
  nobs <- as.numeric (nobs)
  
  
  id.csv <- as.character (id)
  for (i in seq_along(id)) {
    if (id[i]<10) {
      id.csv[i] <- paste ("00",id.csv[i],".csv", sep="")
    } else if (id[i]<100) {
      id.csv[i] <- paste ("0",id.csv[i],".csv", sep="")
    } else {
      id.csv[i] <- paste(id.csv[i],".csv", sep="")
    }
    Aux <- read.csv (id.csv[i])
    obs <- rep (0, nrow(Aux))
    for (j in seq_along(obs)) {
      if (is.na(Aux$sulfate[j])==FALSE & is.na(Aux$nitrate[j])==FALSE) {
        obs[j] <- 1
      } else {
        obs[j] <- 0
      }
    }
    nobs [i] <- sum (obs)
  }
  result <- data.frame (id, nobs)
  print (result)
  setwd(dir.home)
}
