best <- function(state, outcome) {
  ## Read outcome data
  dataFrame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  dummyMatrix <- as.matrix(dataFrame)
  dummyMatrix[dummyMatrix=="Not Available"] <- NA
  dataFrame <- as.data.frame(dummyMatrix)
  names(dataFrame) <- tolower(gsub("[.]", "", names(dataFrame)))  
  outcome <- tolower(paste("Hospital30DayDeathMortalityRatesfrom", gsub(" ", "", outcome), sep=""))
  
  ## Check that state and outcome are valid
  if(!state %in% dataFrame$state) {
    stop("invalid state")
  }
  if(!outcome %in% names(dataFrame)) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  dataFrame <- dataFrame[dataFrame$state == state, ]
  dataFrame <- na.omit(dataFrame[c("hospitalname", outcome)])                
  dataFrame[,2] <- as.numeric(as.character(dataFrame[,2]))                 
  dataFrame <- dataFrame[order(dataFrame[outcome], dataFrame["hospitalname"]), ] 
  
  ## rate
  as.character(dataFrame$hospitalname[1])
}