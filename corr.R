corr <- function(directory, threshold = 0) {
  id <- 1:332
  result <- c()
  completecases <- complete(directory, id)
  for (i in id) {
    if (completecases$nobs[i] >= threshold) {
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
      obssulfate <- data["sulfate"][!na]
      obsnitrate <- data["nitrate"][!na]
      result <- if ((length(obsnitrate) != 0) & (length(obssulfate)) != 0){
        c(result, cor(obssulfate, obsnitrate))
      } else {c(result, c(cor(matrix())))}
    }
  }
  result
}