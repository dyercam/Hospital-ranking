rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  care_data <- read.csv("~/Documents/Hopkins Data Science Course Assignments/r programming class/assignment 3/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  #get list of all states
  state_column <- care_data[,7]
  states <- unique(state_column)
  
  #Checking state is valid
  if (!(state %in% states)){
    stop("invalid state")
  }
  
  #checking outcome is valid
  if( outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") {
    stop("invalid outcome")
  }
  
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  ##getting state data only
  state_data <- care_data[care_data[,7] == state,]
  
  
  #heart attack rating
  if(outcome == 'heart attack'){
    ##prepping the data
    HA_data <- state_data[, c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    HA_data[,2] <- as.numeric(HA_data[,2])
    clean_data <- na.omit(HA_data)
    clean_data <- clean_data[order(clean_data[,2]), ]
    clean_data <- clean_data[order(clean_data[, 2], clean_data[, 1]), ]
  
    
    ## printing the proper output
    if(num == "best"){
      print(clean_data[1,1])
    } 
    else if(num == "worst"){
      
      print(clean_data[nrow(clean_data),1])
    }
    else{
      
      num <- as.numeric(num)
      print(clean_data[num,1])
    }
  }
  
  #heart failure
  if(outcome == 'heart failure'){
    ##prepping the data
    HF_data <- state_data[, c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    HF_data[,2] <- as.numeric(HF_data[,2])
    clean_data <- na.omit(HF_data)
    clean_data <- clean_data[order(clean_data[,2]), ]
    clean_data <- clean_data[order(clean_data[, 2], clean_data[, 1]), ]

    
    ## printing the proper output
    if(num == "best"){
      print(clean_data[1,1])
    } 
    else if(num == "worst"){
      
      print(clean_data[nrow(clean_data),1])
    }
    else{
      
      num <- as.numeric(num)
      print(clean_data[num,1])
    }
  }
  
  ## pneumonia
  if(outcome == 'pneumonia'){
    ##prepping the data
    HP_data <- state_data[, c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )]
    HP_data[,2] <- as.numeric(HP_data[,2])
    clean_data <- na.omit(HP_data)
    clean_data <- clean_data[order(clean_data[,2]), ]
    clean_data <- clean_data[order(clean_data[, 2], clean_data[, 1]), ]

    
    ## printing the proper output
    if(num == "best"){
      print(clean_data[1,1])
    } 
    else if(num == "worst"){
      
      print(clean_data[nrow(clean_data),1])
    }
    else{
      
      num <- as.numeric(num)
      print(clean_data[num,1])
    }
  }
  
}
