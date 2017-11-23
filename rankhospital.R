rankhospital <- function(state, outcome, num="best") {
  
  debug<-FALSE
  
  # Read the outcome file. Treat "Not Available" as NA
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
  
  # Initialize the list mapping column numbers for conditions
  outcomes<-c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  # Split the data state wise
  outcome_state_wise<-split(data, data$State)
  
  if(debug) {
    print(names(outcome_state_wise))
  }
  
  # Check if 'state' argument is valid
  if(! state %in% unique(data$State)){
    stop("invalid state")
  }
  
  # Check if 'outcome' argument is valid
  if(! outcome %in% names(outcomes)) {
    stop("invalid outcome")
  }
  
  # Get the dataframe corresponding to the required state
  df<-outcome_state_wise[[state]]
  
  # Collect only those columns that are necessary i.e. Name, State, condition.
  df<-df[,c(2, 7, outcomes[outcome])]
  
  # Rename the column names to facilitate ordering.
  names(df)<-c("Name", "State", "Outcome")
  
  print(names(df))
  print(c("Num rows:", nrow(df)))
  
  # Sort on outcome and then hospital name
  df<-df[order(df$Outcome, df$Name),]
  
  # Remove NAs
  df<-df[complete.cases(df[,"Outcome"]),]
  
  print(c("Num rows:", nrow(df)))
  
  if (identical(num, "best")) {
    
    # Print the hospital name on top as that has the least mortality rate.
    df[1, "Name"]
  } else if (identical(num, "worst")) {
    
    # Print the hospital name in the last row as that has the worst mortality rate.
    df[nrow(df), "Name"]
  } else {
    
    # Choose the hostpital name in the row indicated by the rank 'num'
    df[num, "Name"]
  }
}

