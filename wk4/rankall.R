rankall <- function(outcomeArg, rankArg) {
  outcomes <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available");
  ## Subset, because it's really hard to work with so many columns.
  outcomes <- outcomes[, c("Hospital.Name", 
                           "State", 
                           "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                           "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                           "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  
  osplit = split(outcomes, outcomes$State)
  
  ## Get targetCol based on outcomeArg
  targetCol <- getOutcomeCol(outcomeArg)
  
  ## Filter our the NAs from the column we're looking at.
  sh_clean <- states_hospitals[!is.na(states_hospitals[targetCol]), ]
  
  
  ## Set Ranks
  sh_clean$rank = 1:nrow(sh_clean)
  
  ## Order Alphabetically and By Rank
  sh_clean <- sh_clean[order(sh_clean[targetCol], sh_clean$Hospital.Name),]
  
  ## Set Ranks
  sh_clean$rank = 1:nrow(sh_clean)
  

  
  best_hospital_name <- as.character(best_hospital$Hospital.Name)
  if (length(best_hospital_name) == 0)
    NA
  else 
    best_hospital_name
  
}

getRankArg <- function(rankArg) {
  #todo -lowercase
  if (is.numeric(rankArg)) 
    best_hospital <- sh_clean[sh_clean$rank == rankArg, ]
  else if (rankArg == "worst")
    best_hospital <- sh_clean[which.max(sh_clean[[targetCol]]), ]
  else if (rankArg == "best")
    best_hospital <- sh_clean[which.min(sh_clean[[targetCol]]), ]
}

getOutcomeCol <- function(outcomeArg) {
  outcomeArg <- tolower(outcomeArg)
  ## Validate Outcome.
  possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (length(which(possible_outcomes == outcomeArg)) == 0)  
    stop("invalid outcome")
  
  ## Get Outcome ColName
  targetCol = "Hospital.30.Day.Death..Mortality..Rates.from."
  if (outcomeArg == "heart attack")
    targetCol = paste(targetCol, "Heart.Attack", sep = "")
  if (outcomeArg == "heart failure")
    targetCol = paste(targetCol, "Heart.Failure", sep = "")
  if (outcomeArg == "pneumonia")
    targetCol = paste(targetCol, "Pneumonia", sep = "")
  targetCol
}

## Subset by state
#states_hospitals = osplit[[state]]

## Validate state.
#if (is.null(nrow(states_hospitals)))  
#  stop("invalid state")

