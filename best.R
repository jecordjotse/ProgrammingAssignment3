best <- function(state, outcome){
  ## Read outcome data
  
  # reading outcomes into outcome variable
  if(file.exists('data')){                                    # check if directory
    if(file.exists('data/outcome-of-care-measures.csv')){     # and file exists
      data <- read.csv('data/outcome-of-care-measures.csv' , colClasses = "character")
      # coercing too numeric on Heart Attact (col 11), Heart failure (col 17), pneumonia (col 23)
      data[,11] <- suppressWarnings(as.numeric(data[, 11]))
      data[,17] <- suppressWarnings(as.numeric(data[, 17]))
      #data[,17] <- suppressWarnings(as.numeric(3))
      data[,23] <- suppressWarnings(as.numeric(data[, 23]))
      # extract columns 11, 17 and 23 together with Hospital name columns (col 2) and group by State (col 7)
      #tempData <- na.omit(data[c(2,7,11,17,23)])
      #outcomeByState <- split(tempData,tempData$State)
      f <- factor(data$State)
      states <- names(table(f))


  ## Check that state and outcome are valid
      
      st <- state %in% states  # check if state value is valid (exist if list of present states)
      switch ( outcome,                       # Used Switch to detemine col from outcome
                "heart attack" = col <- 11,
                "heart failure" = col <-17,
                "pneumonia" = col <- 23,
                "default" = col <- 0
               )
      col <- if(col==0) FALSE else col        # check if outcome is either "heart attack",  "heart failure", or "pneumonia"
      
      if(!st) stop("invalid state",call. = !st)
      if(!col) stop("invalid outcome",call. = !col)
      
      data <- data[c(2,7,col)]
      data <- na.omit(data[order(data[,3]),])
      colnames(data) <- c("hospital", "state", "outcome")
      outcomeByState <- split(data,data$state)
      
      splitData <<- outcomeByState
  ## Return hospital name in that state with lowest 30-day death rate
      #stop("invalid state",call. = !st)
      #stop("invalid outcome",call. = !col)
      outcomeByState[[state]][outcomeByState[[state]][3] == min(outcomeByState[[state]][3]),1]
###   if(st){
  #     if(col){
  #       outcomeByState[[state]][outcomeByState[[state]][col] == min(outcomeByState[[state]][col])][1]
  #     }else{
  #       cat("Error:",outcome,"is not a valid outcome in the dataset. Use \"heart attack\",  \"heart failure\", or \"pneumonia\" ", sep = " ")
  #     }
  #   }else{
  #     cat("Error:",state,"is not a valid dual-character state name. Use eg. \"CA\", \"AL\" ", sep = " ")
  ### }
    }else{
      print("Error: datafile \"outcome-of-care-measures.csv\" does not exist")
    }
  }else{
    print("Error: Not in ProgrammingAssignment3 dir or data/ dir missing from ProgrammingAssignment3/")
  }
}