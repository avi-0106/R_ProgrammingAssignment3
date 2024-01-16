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
  
  
  
  
  
  
}