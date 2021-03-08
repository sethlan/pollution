complete <- function(directory, id=1:332) {
  result <- data.frame(id = numeric(), nobs = numeric())
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
    na <- is.na(data["sulfate"]) | is.na(data["nitrate"])
    newrow <- c(i, length(result[!na]))
    result[nrow(result) + 1,] <- newrow
  }
  result
}