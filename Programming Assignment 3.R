##################################################################################################

#Finding the best hospital in a state

#Input data
state_ ='TX'
outcome_ = 'heart failure'

#Function best

best <- function(state_, outcome_){
  ## Read outcome data
  outcome <- read.csv('C:/Users/edgar/Desktop/Cursos/R Programming/outcome-of-care-measures.csv')
  # Unique values for state
  list_states <- unique(outcome[['State']])
  # Unique values for outcome_
  list_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  '%notin%' <- Negate('%in%')
  
  # Lower  Mortality  Estimate  -  Hospital  30-Day  Death  (Mortality)  Rates 
  # for heart attack, heart failure, or pneumonia are in columns 13, 19, 25, respectively.
  if(outcome_ == 'heart attack'){
    Rate = c(13)
  }else if(outcome_ == 'heart failure'){
    Rate = c(19)
  }else if(outcome_ == 'pneumonia'){
    Rate = c(25)
  }
  # Check that state and outcome are valid
  if(state_ %notin% list_states){
    stop('invalid state')
  }else if(outcome_ %notin% list_outcomes){
    stop('invalid outcome')
  }else if(state_ %in% list_states & outcome_ %in% list_outcomes){
    ## Return hospital name in that state with lowest 30-day death rate
    aux <- outcome[,c(2,7,Rate)]
    colnames(aux)[3] <- 'Rate'
    aux <- subset(aux, State == state_ & Rate != 'Not Available')
    ranking <- aux[which.min(aux$Rate),]
    if(nrow(ranking) > 1){
      print('Tie')
      best <- ranking[order(ranking$Hospital.Name),]
      result <- best[1]
    }else{
      result <- ranking
    }
  }
  return(result$Hospital.Name)
}
best(state_, outcome_)

##################################################################################################

# Ranking hospitals by outcome in a state

#Input data
state_ = 'TX'
outcome_ = 'heart failure'
num = 4

# Fucntion rankhospital

rankhospital <- function(state_, outcome_, num = "best"){
  ## Read outcome data
  outcome <- read.csv('C:/Users/edgar/Desktop/Cursos/R Programming/outcome-of-care-measures.csv')
  # Unique values for state
  list_states <- unique(outcome[['State']])
  # Unique values for outcome_
  list_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  '%notin%' <- Negate('%in%')
  
  ## Check that state and outcome are valid
  if(state_ %notin% list_states){
    stop('invalid state')
  }else if(outcome_ %notin% list_outcomes){
    stop('invalid outcome')
  }else{
    if(outcome_ == 'heart attack'){
      Rate = c(11)
    }else if(outcome_ == 'heart failure'){
      Rate = c(17)
    }else if(outcome_ == 'pneumonia'){
      Rate = c(23)
    }
  }
  aux <- outcome[,c(2,7,Rate)]
  colnames(aux)[3] <- 'Rate'
  aux <- subset(aux, State == state_ & Rate != 'Not Available')
  aux[, 3] <- as.numeric(aux[, 3])
  aux <- aux[with(aux, order(aux$Rate, aux$Hospital.Name)),] #The aux$Hospital.Name breaks the tie, in certain cases.
  aux$Rank <- 1:nrow(aux)
  
  if(num == 'best'){
    result <- aux$Hospital.Name[1]
  }else if(num == 'worst'){
    result <- aux$Hospital.Name[nrow(aux)]
  }else if(num > nrow(aux)){
    result <- NA  
  }else{
    result <- aux$Hospital.Name[num]
  }
  ## Return hospital name in that state with the given rank## 30-day death rate
  return(result)
}
rankhospital(state_, outcome_, num)

##################################################################################################
# Function Rankall

# Input data
outcome_ <- 'heart attack'
num = 20

rankall <- function(outcome_, num = "best"){
  ## Read outcome data
  outcome <- read.csv('C:/Users/edgar/Desktop/Cursos/R Programming/outcome-of-care-measures.csv')
  list_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  list_states <- sort(unique(outcome[['State']])) #It's important to sort them alphabetically.
  '%notin%' <- Negate('%in%')
  
  ## Check that state and outcome are valid
  if(outcome_ %notin% list_outcomes){
    stop('invalid outcome')
  }else{
    if(outcome_ == 'heart attack'){
      Rate = c(11)
    }else if(outcome_ == 'heart failure'){
      Rate = c(17)
    }else if(outcome_ == 'pneumonia'){
      Rate = c(23)
    }
  }
  
  aux <- outcome[,c(2,7,Rate)]
  colnames(aux)[3] <- 'Rate'
  
  ## For each state, find the hospital of the given rank
  
  # Create an empty dataframe to append each observation 
  df <- data.frame(hospital = character(0), state = numeric(0))
  
  for(state_ in list_states){
    aux1 <- subset(aux, State == state_ & Rate != 'Not Available')
    aux1[, 3] <- as.numeric(aux1[, 3])
    aux1 <- aux1[with(aux1, order(aux1$Rate, aux1$Hospital.Name)),] #The aux$Hospital.Name breaks the tie, in certain cases.
    aux1$Rank <- 1:nrow(aux1)
    if(num == 'best'){
      result <- aux1$Hospital.Name[1]
    }else if(num == 'worst'){
      result <- aux1$Hospital.Name[nrow(aux1)]
    }else if(num > nrow(aux1)){
      result <- NA  
    }else{
      result <- aux1$Hospital.Name[num]
    }
    best_in_state <- c(result, state_)
    df[nrow(df)+1,] <- best_in_state
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  return(df)
}

head(rankall(outcome_ = 'heart attack', num = 20), 10)

##################################################################################################
#Programming Assignment 3: Quiz

#1
best("SC", "heart attack")
#2
best("NY", "pneumonia")
#3
best("AK", "pneumonia")
#4
rankhospital("NC", "heart attack", "worst")
#5
rankhospital("WA", "heart attack", 7)
#6
rankhospital("TX", "pneumonia", 10)
#7
rankhospital("NY", "heart attack", 7)
#8
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
#9
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
#10
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
