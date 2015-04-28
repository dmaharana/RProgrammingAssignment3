rankhospital <- function(state, outcome, num = 'best'){
  dfCareMeasure <- read.csv('data/outcome-of-care-measures.csv', 
                            colClasses = 'character',
                            na.strings='NA')
  # validate state
  stateList <- unique(dfCareMeasure[,7])
  if (!state %in% stateList) {
    stop('Invalid state code')
  }
  # validate outcome
  outComeList <- c('heart attack', 'heart failure', 'pneumonia')
  if (!outcome %in% outComeList){
    stop('Invalid Outcome')
  }
  
  hospitalCount <- nrow(unique(dfCareMeasure[,1]))
  if (num == 'best'){
    choiceRank = 1
  }else if (num == 'worst'){
    choiceRank = -1
  }else if (num > hospitalCount){
    return (NA)
  }else {
    choiceRank = num
  }
  
  print(hospitalCount)
  
}