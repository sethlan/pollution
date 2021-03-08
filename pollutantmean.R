pollutantmean <- function(directory, pollutant, id = 1:332) {
  observationswithoutna <- c()
  for (i in id) {
    beginnameoffile <- if (i < 10) {
      "00"
    }
    else if (i < 100) {
      "0"
    } else {
      ""
    }
    nameoffile <- paste(beginnameoffile, i, ".csv", sep="")
    path <- file.path(getwd(), directory, nameoffile)
    data <- read.csv(path)
    observations <- data[pollutant]
    na <- is.na(observations)
    observationswithoutna <- c(observationswithoutna, observations[!na])
  }
  mean(observationswithoutna)
}