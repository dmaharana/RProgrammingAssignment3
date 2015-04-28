rankhospital <- function(state, outcome, num = 'best'){
  dfCareMeasure <- read.csv('data/outcome-of-care-measures.csv', 
                            colClasses = 'character',
                            na.strings='Not Available')
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
  
  hospitalCount <- nrow(data.frame(unique(dfCareMeasure[,1])))
  if (num == 'best'){
    choiceRank = 1
  }else if (num == 'worst'){
    choiceRank = -1
  }else if (is.numeric(num) == TRUE){
    if (num > hospitalCount){
      return (NA)
    }else {
      choiceRank = num
    }
  }else {
    stop('Invalid num')
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
  
  #get rid of the NA's in the desired outcome column
  dfCareMeasure.state[, OutcomeColName] <- as.numeric(dfCareMeasure.state[,OutcomeColName])
  bad <- is.na(dfCareMeasure.state[, OutcomeColName])
  desired_data <- dfCareMeasure.state[!bad, ]
  
  index <- with(desired_data, order(desired_data[OutcomeColName], desired_data['Hospital.Name']))
  ordered_desired_data <- desired_data[index, ]
  
  if (choiceRank == -1){
    choiceRank = length(ordered_desired_data[, OutcomeColName])
  }
  
  if(choiceRank <= nrow(ordered_desired_data)){
    ordered_desired_data[choiceRank, 'Hospital.Name']  
  }else{
    return (NA)
  }

}