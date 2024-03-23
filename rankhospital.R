
## example call: rankhospital("MD", "heart failure", 5)

rankhospital <- function(state, outcome, num = "best") { 
    ## Read outcome data 
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ##hospital 
    hospitalNameList <- outcomeData[,2]
    ##state  
    stateList <- outcomeData[,7]
    ##heart attack 
    attackList <- outcomeData[,11]
    ##heart failure 
    failureList <- outcomeData[,17]
    ##pneumonia 
    pneumoniaList <- outcomeData[,23]
    
    toCalData <- cbind( hospitalNameList, stateList, attackList, failureList, pneumoniaList)
    colnames(toCalData) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    
    ## Check that state and outcome are valid 
    ## -- validity of state and outcome
    
    if( "heart attack" != outcome &&  "heart failure" != outcome &&  "pneumonia" != outcome){
        print("outcome")
        print(outcome)
        stop(" invalid outcome")
    }
    
    valStateList <- outcomeData[outcomeData$State == state,]
    colCount <- ncol(valStateList)
    
    if(colCount <= 0){
        stop(" invalid state")
    }    
 
    
    toStateCalData <- toCalData[(toCalData[, "state"] == state), ]
    ##toStateCalData <- toCalData[(toCalData[, "state"] == "AL"), ]
    
    toStateCalData[, outcome] <- as.numeric(toStateCalData[, outcome])
    ##toStateCalData[, "heart failure"] <- as.numeric(toStateCalData[, "heart failure"])
    
    toStateCalData <- toStateCalData[!is.na(toStateCalData[, outcome]), ]
    ##toStateCalData <- toStateCalData[!is.na(toStateCalData[, "heart failure"]), ]
    
    ## sort
    ## 
    ##toStateCalData <- toStateCalData[order(toStateCalData[, outcome], toStateCalData[, outcome]), ]
    ##toStateCalData <- toStateCalData[order(toStateCalData[, "heart failure"], toStateCalData[, "hospital"]), ]

    toStateCalData <- toStateCalData[order(toStateCalData[, outcome], toStateCalData[, "hospital"]), ]
    
    ## -- num으로 주어진 숫자가 해당 주의 병원 수보다 크면 함수는 NA를 반환해야 합니다.
    ## -- 특정 결과에 대한 데이터가 없는 병원은 순위를 결정할 때 병원 집합에서 제외되어야 합니다.
    
    ## Return hospital name in that state with the given rank 

    if( num == "best"){
        num <- 1
        
    }else if(num == "worst"){
        num <- nrow(toStateCalData) 
        
    }else{

    }
    
    
    if(num > nrow(toStateCalData)){
        stop("NA")
    }  
    
    toStateCalData[num,1]

    
    ## 30-day death rate 
    
}

## source("rankhospital.R")
## rankhospital("TX", "heart failure", 4)
## rankhospital("MD", "heart attack", "worst")
## rankhospital("MN", "heart attack", 5000)  => print NA
