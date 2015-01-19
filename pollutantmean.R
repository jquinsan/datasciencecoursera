pollutantmean <- function(directory, pollutant, id = 1:332) {
    
  dir.home <- getwd()
  setwd (directory)
  result <- rep (NA, length (id))
  result <- as.numeric (result)
  obs <- rep (NA, length (id))
  obs <- as.numeric (result)
  
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
    if (pollutant=="sulfate") {
      Aux <- Aux[2]
      Aux <- as.vector(t(Aux))
      result[i] <- sum (Aux,na.rm=TRUE)
      obs [i] <- sum (!is.na(Aux))
    } else if (pollutant=="nitrate") {
      Aux <- Aux[3]
      Aux <- as.vector(t(Aux))
      result[i] <- sum (Aux,na.rm=TRUE)
      obs [i] <- sum (!is.na(Aux))
    }
  }
  mean <- round(sum (result)/sum (obs), digits=3)
  print (mean)
  setwd(dir.home)
}
