
best <- function(state, outcome){
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ##rowCount <- ncol(charResult)
    ##names <- names(charResult)
    
    ## Check that state and outcome are valid
    
    if( "heart attack" != outcome &&  "heart failure" != outcome &&  "pneumonia" != outcome){
        print("outcome")
        print(outcome)
        stop(" invalid outcome")
    }
    
    
    
    stateList <- outcomeData[outcomeData$State == state,]
    colCount <- ncol(stateList)
    
    if(colCount <= 0){
        stop(" invalid state")
    }
    
    
    ## Return hospital name in that state with lowest 30-day death
    
    if(outcome == "heart attack") {
        state_subset <- outcomeData[outcomeData$State == state,]
        
        best_value <- min(state_subset[,11],na.rm = TRUE)
        hospital_names <- subset(state_subset[,2],state_subset[,11]==best_value)
        hospitals_sorted <- sort(hospital_names) #only want one hospital, take first alphabetically
        best_hospital <- hospitals_sorted[1]
        best_hospital
    } else if(outcome == "heart failure") {
        
        state_subset <- outcomeData[outcomeData[,7] == state,]
        
        best_value <- min(state_subset[,17],na.rm = TRUE)
        hospital_names <- subset(state_subset[,2],state_subset[,17]==best_value)
        hospitals_sorted <- sort(hospital_names) #only want one hospital, take first alphabetically
        best_hospital <- hospitals_sorted[1]
        best_hospital
        
        
    } else if(outcome == "pneumonia") {
        
        state_subset <- outcomeData[outcomeData[,7] == state,]
        
        best_value <- min(state_subset[,23],na.rm = T)
        hospital_names <- subset(state_subset[,2],state_subset[,23]==best_value)
        hospitals_sorted <- sort(hospital_names) #only want one hospital, take first alphabetically
        best_hospital <- hospitals_sorted[1]
        best_hospital
        
    }
    
    ## rate
    
    
}


## source("best.R")
## best("TX", "heart attack")


##best("TX", "heart failure")

##best("MD", "heart attack")

##best("MD", "pneumonia")

##best("MD", "pneumonia")




##best("BB", "heart attack")  -- error message

##best("NY", "hert attack") -- error message