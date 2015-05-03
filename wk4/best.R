best <- function(state, outcomeArg) {
  outcomes <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available");
  ## Subset, because it's really hard to work with so many columns.
  outcomes <- outcomes[, c("Hospital.Name", 
                           "State", 
                           "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                           "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                           "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  osplit = split(outcomes, outcomes$State)
  
  ## Normalize the args.
  state <- toupper(state)
  outcomeArg <- tolower(outcomeArg)
  
  ## Subset by state
  states_hospitals = osplit[[state]]
  
  ## Validate state.
  if (is.null(nrow(states_hospitals)))  
    stop("invalid state")
  
  ## Validate Outcome.
  possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (length(which(possible_outcomes == outcomeArg)) == 0)  
    stop("invalid outcome")
  
  
  ## Subset to Outcome.
  targetCol = "Hospital.30.Day.Death..Mortality..Rates.from."
  if (outcomeArg == "heart attack")
    targetCol = paste(targetCol, "Heart.Attack", sep = "")
  if (outcomeArg == "heart failure")
    targetCol = paste(targetCol, "Heart.Failure", sep = "")
  if (outcomeArg == "pneumonia")
    targetCol = paste(targetCol, "Pneumonia", sep = "")
  
  ## Filter our the NAs from the column we're looking at.
  sh_clean <- states_hospitals[!is.na(states_hospitals[targetCol]), ]
  
  ## Order Alphabetically.
  sh_clean <- sh_clean[order(sh_clean$Hospital.Name),]
  
  sh_clean
  
  best_hospital <- sh_clean[which.min(sh_clean[[targetCol]]), ]
  
  as.character(best_hospital$Hospital.Name)
  
}