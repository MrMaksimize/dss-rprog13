corr <- function(directory, threshold = 0) {
  allFiles <- list.files(directory, full.names = TRUE)
  cors = vector("numeric")
  
  for (file in allFiles) {
    numClean <- 0
    tempDataset <- read.csv(file)
    tempDataset <- tempDataset[complete.cases(tempDataset), ]
    
    numClean <- nrow(tempDataset)
    if (!numClean > threshold) 
      next
    
    cor <- cor(tempDataset[[2]], tempDataset[[3]])
    #cors <- append(cors, round(cor, 5))
    cors <- append(cors, cor)
  }
  cors
  
}