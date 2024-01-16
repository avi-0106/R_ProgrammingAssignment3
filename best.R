library(stringr)

best <- function(state, outcome){
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
  
  best_hospital <- state_select[which.min(state_select[,outcome_column]), "Hospital.Name"]
  return(best_hospital)  
}

### Note: What I learned while solving this
# Print out the column names and see if you are getting or creating the correct column which is being used later on!
# Use print statements in order to find out!
# Use debugger and breakpoints just like we used to do in VS studio!
# Copare strings if you have to! 

#Print statements used here:

#print(outcome)
#print(state)
#print(outcome_column)
#print(colnames(state_select))
#print(outcome_column == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
#print(dim(state_select))
#print(dim(state_select_filtered))
#print(colnames(state_select_filtered))