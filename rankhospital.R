library(stringr)
library(dplyr)

rankhospital <- function(state, outcome, num = "best"){
  
  ## Reading outcome data
  outcome_data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Checking if state and outcome are valid using stop()
  valid_state <- unique(outcome_data$State)
  if(!(state %in% valid_state)){
    stop("invalid state")
  }
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% valid_outcome)){
    stop("invalid outcome")
  }
  
  ## Selecting the state-wise subset of the data and removing rows with NA values
  main_string <- gsub("\\s|\\(|\\-|\\)", ".", "Hospital 30-Day Death (Mortality) Rates from")
  
  outcome_formatted <- str_to_title(outcome) # Convert first letters of each word to uppercase in outcome
  outcome_formatted <- gsub("\\s", ".",outcome_formatted)
  
  outcome_column <- paste(main_string, outcome_formatted, sep = ".")
  
  state_select <- subset(outcome_data, outcome_data$State == state)
  state_select <- state_select[!is.na(state_select[, outcome_column]), ]
  state_select[, outcome_column] <- trimws(state_select[, outcome_column])
  state_select <- state_select[state_select[, outcome_column] != "Not Available", ]
  
  rank_count <- nrow(state_select)
  if (is.numeric(num)) {
    if (num > rank_count){
      return(NA)
    }
    else{
      #state_select <- state_select[order(as.numeric(state_select[,outcome_column]), state_select$Hospital.Name)]
      ordered_state <- state_select %>% arrange(as.numeric(!!sym(outcome_column)), Hospital.Name)
      ranked_hospital <- state_select$Hospital.Name[num]
      return(ranked_hospital)
    }
  }
  else if (num == "best"){
    best_hospital <- state_select[which.min(state_select[,outcome_column]), "Hospital.Name"]
    return(best_hospital)
  }
  else if (num == "worst"){
    worst_hospital <- state_select[which.max(state_select[,outcome_column]), "Hospital.Name"]
    return(worst_hospital)
  }
  else {
    return("Please Enter a Valid Ranking")
  }
  
  
  
  
  
  
}