rankall <- function(outcome, num = 'best'){
  dfCareMeasure <- read.csv('data/outcome-of-care-measures.csv', 
                            colClasses = 'character',
                            na.strings='Not Available')
  # validate state
  stateList <- unique(dfCareMeasure[,7])
  if (!state %in% stateList) {
    stop('Invalid state code')
  }
  
  # store the results and ignore the states that donot have a hospital
  #  for the outcome
  result <- data.frame()
  for (state in stateList){
    hospital <- rankhospital(state, outcome, num)
    if(!is.na(hospital)){
      result <- rbind(result, data.frame(hospital, state))
    }
      
  }
  result
}