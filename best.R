best <- function(state, outcome){
  ## Read outcome data
  care_data <- read.csv("~/Documents/Hopkins Data Science Course Assignments/r programming class/assignment 3/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  ## state column is number 7
  
  ## getting a list of states
  state_column <- care_data[,7]
  states <- unique(state_column)
  
  ## checking that the outcome is acceptable
  if( outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") {
    stop("invalid outcome")
  }
  
  #  checking to see if the state is valid
  if (!(state %in% states)){
    stop("invalid state")
  }
  
  ## get only information from the state
  state_data <- care_data[care_data[,7] == state,]
  
  
  ## Return hospital name in that state with lowers 30-day death rate
  
  ## for heart attacks
  if(outcome == "heart attack"){
    state_data[,11] <- as.numeric(state_data[,11])
    
    
    clean_data <- state_data[!is.na(state_data[,11]),]
    
    
    min_heart_attacks <- which.min(clean_data[,11])
    hosp_name <- clean_data[min_heart_attacks, 2]
    return(hosp_name)
  }
  
  ## for heart failure
  if(outcome == "heart failure"){
    state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    
    
    clean_data <- state_data[!is.na(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    
    
    min_heart_failures <- which.min(clean_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    hosp_name <- clean_data[min_heart_failures, 2]
    return(hosp_name)
  }
  
  ## for pneumonia
  if(outcome == "pneumonia"){
    state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    
    
    clean_data <- state_data[!is.na(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    
    
    min_heart_attacks <- which.min(clean_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    hosp_name <- clean_data[min_heart_attacks, 2]
    return(hosp_name)
  }
}
