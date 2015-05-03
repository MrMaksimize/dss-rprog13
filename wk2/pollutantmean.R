pollutantmean <- function(directory, pollutant, id = 1:332) {
  combinedDataset <- data.frame()
  for (i in id) {
    fileString <- sprintf("./%s/%03d.%s", directory, i, "csv")
    # If the merged dataset does exist, append to it.
    tempDataset <-read.csv(fileString)
    combinedDataset<-rbind(combinedDataset, tempDataset)
    rm(tempDataset) 
  }
  ## Extract from DF pollutant column w/o NA values.
  round(mean(combinedDataset[[pollutant]], na.rm = TRUE), 3)
}