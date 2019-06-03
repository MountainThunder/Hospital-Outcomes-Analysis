rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
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
    
    outcome_data_reduced <- outcome_data[,c("Hospital.Name", "State", outcome_col_name)]
    cc_data <- outcome_data_reduced[complete.cases(outcome_data_reduced),]
    colnames(cc_data) <- c("Hospital", "State", "Outcome")
    ranked_data <- arrange(cc_data, cc_data$State, cc_data$Outcome, cc_data$Hospital)
    split_outcome_data <- split(ranked_data, ranked_data$State)
    
    
    if(num=="best"){
        
        num<-1
        final <- lapply(split_outcome_data, function(x) x[num, 1])
    }
    else if(num=="worst"){
       
        final <- lapply(split_outcome_data, function(x) x[nrow(x), 1])
    } 
    else{
        final <- lapply(split_outcome_data, function(x) x[num, 1]) 
    }
    
    hospital_list <- unlist(final)
    state_list <- names(final)
    data.frame(hospital=hospital_list, state=state_list, row.names=state_list)
    #print(final)
}

