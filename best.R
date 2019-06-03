best <- function(state_abbr, outcome){
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    ## Check that state and outcome are valid - throw stop error with message "invalid state"
    if(!state_abbr %in% state.abb){
       stop("invalid state")
    }
        
    ## The outcomes can be one of "heart attack", "heart failure", or "pneumonia". 
    ## Hospitals that do not have data on a particular outcome should be excluded 
    ## from the set of hospitals when deciding the rankings.
    ## throw stop error with message of "invalid outcome"
    
    if(outcome=="heart attack"){
        #print ("heart attack")
        outcome_col_name <-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        
    }
    else if(outcome=="heart failure"){
        outcome_col_name <-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }
    else if(outcome=="pneumonia"){
        outcome_col_name <-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    else{
        stop("invalid outcome")        
    }
        
    ## Return hospital name in that state with lowest 30-day death rate
   
    outcome_data_reduced <- outcome_data[,c("Hospital.Name", "State", outcome_col_name)]
    
    split_outcome_data <- split(outcome_data_reduced, outcome_data_reduced$State)
    state_data <- as.data.frame(split_outcome_data[state_abbr])
    cc_state_data <- state_data[complete.cases(state_data),]
    colnames(cc_state_data) <- c("Hospital", "State", "Outcome")
    ranked_data <- arrange(cc_state_data, cc_state_data$Outcome)
    ##print(ranked_data)
   ranked_data[1, 1]
}


#print(best("TX", "heart attack"))
#print( best("TX", "heart failure"))
#print( best("MD", "heart attack"))
#print( best("MD", "pneumonia"))
#best("BB", "heart attack")
#best("NY", "hert attack")
