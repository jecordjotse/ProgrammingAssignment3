rankall <- function(outcome, num = "best") {
  ## Read outcome data
  
  # reading outcomes into outcome variable
  if(file.exists('data')){                                    # check if directory
    if(file.exists('data/outcome-of-care-measures.csv')){     # and file exists
      data <- read.csv('data/outcome-of-care-measures.csv' , colClasses = "character" , na.strings="Not Available")
      # coercing too numeric on Heart Attact (col 11), Heart failure (col 17), pneumonia (col 23)
      # extract columns 11, 17 and 23 together with Hospital name columns (col 2) and group by State (col 7)
      #tempData <- na.omit(data[c(2,7,11,17,23)])
      #outcomeByState <- split(tempData,tempData$State)
      f <- factor(data$State)
      states <- names(table(f))
  ## Check that state and outcome are valid
      
      #st <- state %in% names(outcomeByState)  # check if state value is valid (exist if list of present states)
      switch ( outcome,                       # Used Switch to detemine col from outcome
               "heart attack" = col <- 11,
               "heart failure" = col <- 17,
               "pneumonia" = col <- 23,
               "default" = col <- 0
      )
      
      col <- if(col==0) FALSE else col        # check if outcome is either "heart attack",  "heart failure", or "pneumonia"
      
      
      
      
      if(!col) stop("invalid outcome",call. = !col)
      data <- data[c(2,7,col)]
      data[,3] <- suppressWarnings(as.numeric(data[, 3]))
      data <- na.omit(data[order(data[,3],data[,1]),])
      colnames(data) <- c("hospital", "state", "outcome")
      outcomeByState <- split(data,data$state)
      
      if(num!='best'&num!='worst'&!(is.numeric(num)|is.integer(num))) {print("Error");stop("invalid rank",call. = TRUE)}
      
      ## For each state, find the hospital of the gtempTry.tempTry.tempTry.tempTry.tempTry.tempTry.tempTry.tempTry.tempTry.tempTry.tempTry.iven rank
      
      #len <- nrow(stateData)
      rank <- if(is.numeric(num)|is.integer(num)) num else switch(num, "best" = 1,"worst" = -1)
      outcomeByState <- lapply(outcomeByState, function(x){len <- nrow(x);x$rank <- c(1:len);x})
      
      splitData <<- outcomeByState
      
      ##splitData <<- outcomeByState
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
      #outcomeByState
      hospital = character(0)
      for(i in states){
        if(rank == -1){
          temp <- outcomeByState[[i]][outcomeByState[[i]]['rank'] == nrow(outcomeByState[[i]]),1]
        }else if(!is.null(rank)){
          temp <- outcomeByState[[i]][outcomeByState[[i]]['rank'] == rank,1]
        }else{
          break
        }
        if(length(temp)==0) temp <- NA
        hospital <- c(hospital,temp)
      }
      state <- states
      data.frame(hospital,state)
    }
  }
}

