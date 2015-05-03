rankall <- function(outcomeArg, rankArg) {
  outcomes <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available");
  ## Subset, because it's really hard to work with so many columns.
  outcomes <- outcomes[, c("Hospital.Name", 
                           "State", 
                           "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                           "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                           "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  colnames(outcomes) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Get targetCol based on outcomeArg
  targetCol <- getOutcomeCol(outcomeArg)
  
  if (!is.numeric(rankArg)) 
    rankArg <- tolower(rankArg)
  
  ## Subset outcomes list only to what's relevant.
  ## Cut out nonrelevant columns, with only the outcome.
  outcomes <- outcomes[, c("hospital", "state", targetCol)]
  ## Cut out all NAs for that outcome.
  outcomes <- outcomes[complete.cases(outcomes), ]
  
  ## Split the hospital list by state.
  osplit = split(outcomes, outcomes$state)
  
  os <- sapply(osplit, rankHospitalsInState, targetCol, rankArg)
  os <- data.frame(state = names(os), hospital = os)
  os
  
}


rankHospitalsInState <- function(stateHospitals, targetCol, rankArg){
  ## Order Alphabetically and By Rank
  sh <- stateHospitals[order(stateHospitals[targetCol], stateHospitals$hospital), ]
  ## Set Rank
  sh$rank = 1:nrow(sh)
  hospitalRow <- getHospitalAtRank(sh, targetCol, rankArg)
  hname <- as.character(hospitalRow$hospital)
  if (length(hname) == 0)
    NA
  else 
    hname
}


getHospitalAtRank <- function(sh, targetCol, rankArg) {
  if (is.numeric(rankArg)) 
    best_hospital <- sh[sh$rank == rankArg, ]
  else if (rankArg == "worst")
    best_hospital <- sh[which.max(sh[[targetCol]]), ]
  else if (rankArg == "best")
    best_hospital <- sh[which.min(sh[[targetCol]]), ]
}

getOutcomeCol <- function(outcomeArg) {
  outcomeArg <- tolower(outcomeArg)
  
  ## Validate Outcome.
  possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (length(which(possible_outcomes == outcomeArg)) == 0)  
    stop("invalid outcome")
  
  outcomeArg
}