best <- function(state, outcome) {
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
 
  
  # strings to be matched
  myoutc <- c('mortality', unlist(strsplit(outcome, ' ')))
    
  OutcomeColName <- names(dfCareMeasure)[grepl('^hospital', names(dfCareMeasure), ignore.case = TRUE)]
  for (regPat in myoutc){
    outcomeMatchList <- grepl(regPat, OutcomeColName, ignore.case = TRUE)
    OutcomeColName <- OutcomeColName[outcomeMatchList]
  }
    
  ## Return hospital name in that state with lowest 30-day death rate
  dfCareMeasure.state <- dfCareMeasure[dfCareMeasure$State==state,]
  idx <- which.min(as.double(dfCareMeasure.state[,OutcomeColName]))
  dfCareMeasure.state[idx,"Hospital.Name"]
  
}