data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)

best <- function(state, outcome){
   sortedData <- sorteddata(state, outcome)
   sortedData[1,2]
}

rankhospital <- function(state, outcome, num = "best"){
  sortedhospital <- sorteddata(state, outcome)
  if (num == "best"){
    sortedhospital[1,2]
  }
  else if (num == "worst"){
    sortedhospital[nrow(sortedhospital),2]
  }
  else {
    sortedhospital[num,2]
  }
}

rankall <- function(outcome, num = "best"){
  states <- uniquevectors(data,7)
  hospitals <- data.frame(hospital=character(), state=character(), stringsAsFactors = FALSE)
  for (i in 1:length(states)){
    state <- states[i]
    hospitals[i,] <- list(rankhospital(state, outcome, num),state)
  }
  hospitals[order(hospitals[,2]),]
}

sorteddata <- function(state, outcome){
  if (nrow(data[data$State == state,]) == 0){
    stop("invalid state")
  }
  if (outcome == "heart attack"){
    deathData <- data[data$State == state, c(1,2,7,11)]
  }
  else if (outcome == "heart failure"){
    deathData <- data[data$State == state, c(1,2,7,17)]
  }
  else if (outcome == "pneumonia"){
    deathData <- data[data$State == state, c(1,2,7,23)]
  }
  else {
    stop("invalid outcome")
  }
  deathData <- deathData[deathData[,4] != "Not Available",]
  deathData[,4] <- as.numeric(deathData[,4])
  deathData[order(as.numeric(deathData[,4]), deathData[,2]),]
}

uniquevectors <- function(data, colnum){
  unique(data[,colnum], incomparables = FALSE)
}
