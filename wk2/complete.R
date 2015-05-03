complete <- function(directory, id = 1:332) {
  nobsDF <- data.frame(id, nobs = 0)
  count <- 1
  for (i in id) {
    fileString <- sprintf("./%s/%03d.%s", directory, i, "csv")
    tempDataset <- read.csv(fileString)
    numClean <- nrow(tempDataset[complete.cases(tempDataset), ])
    nobsDF[count, 2] <- numClean
    rm(tempDataset)
    count <- count + 1
  }
  nobsDF
}