rankall <- function(outcome, num="best") {
  
  debug<-FALSE
  
  # Read the outcome file. Treat "Not Available" as NA
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available",
                   stringsAsFactors = FALSE)
  
  # Initialize the list mapping column numbers for conditions
  outcomes<-c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  # Check if 'outcome' argument is valid
  if(! outcome %in% names(outcomes)) {
    stop("invalid outcome")
  }
  
  # Extract only the necessart columns (Name, State, relevant outcome)
  data<-data[,c(2,7, outcomes[outcome])]
  
  # Rename the columns for easier manipulation
  names(data)<-c("Name", "State", "Outcome")
  
  # Remove NAs
  data<-data[complete.cases(data$Outcome),]
  
  # Order based on the outcome and then Name
  data<-data[order(data$Outcome, data$Name),]
  
  # Split state wise
  data_state_wise<-split(data, data$State)
  
  print(names(data_state_wise))
  
  # Create the result data frame
  df_result<-data.frame(matrix(nrow = 0, ncol=2))
  
  # Assign column names to the data frame
  names(df_result)<-c("hospital", "state")
  
  lapply(data_state_wise, extract <- function(x) { 
    if (identical(num, "best")) {
      
      # Index is the top row as that has the least mortality rate.
      r_idx = 1
    } else if (identical(num, "worst")) {
      
      # Index is the last row as that has the worst mortality rate.
      r_idx = nrow(x)
    } else {
      
      # Choose the hostpital name in the row indicated by the rank 'num'
      r_idx = num
    }
    df_result<-rbind(df_result, data.frame(hospital = x[r_idx, 1],
                                           state = x[r_idx, 2]))
    print(df_result)
    })
}

