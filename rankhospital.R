rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  
  # reading outcomes into outcome variable
  if(file.exists('data')){                                    # check if directory
    if(file.exists('data/outcome-of-care-measures.csv')){     # and file exists
      data <- read.csv('data/outcome-of-care-measures.csv' , colClasses = "character", stringsAsFactors=FALSE)
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
               "heart failure" = col <- 17,
               "pneumonia" = col <- 23,
               "default" = col <- 0
      )
      col <- if(col==0) FALSE else col        # check if outcome is either "heart attack",  "heart failure", or "pneumonia"

      
      if(!st) stop("invalid state",call. = !st)
      if(!col) stop("invalid outcome",call. = !col)
      
      data <- data[c(2,7,col)]
      data <- na.omit(data[order(data[,3],data[,1]),])
      colnames(data) <- c("hospital", "state", "outcome")
      outcomeByState <- split(data,data$state)
      
      rank <- if(is.numeric(num)|is.integer(num)) num else switch(num, "best" = 1,"worst" = -1)
      outcomeByState <- lapply(outcomeByState, function(x){len <- nrow(x);x$rank <- c(1:len);x})
      
      splitData <<- outcomeByState
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
      return(outcomeByState[[state]][outcomeByState[[state]]['rank'] == rank,1])
      #if(!st) stop("invalid state",call. = !st)
      ##if(!col) stop("invalid outcome",call. = !col)
      #stateData <- outcomeByState[[state]]
      #stateData <- stateData[order(stateData[,col]),]
      #len <- nrow(stateData)
      ##print(num)
      #print(len)
      #rank <- if(num %in% c(1:len)) num else switch(num, "best" = 1,"worst" = len, 0)
      #row.names(stateData) <- 1:len
      #print(rank)
      #print(stateData)
      #return(if(!is.null(rank)) stateData[rank,][,1] else NA)
      
    }else{
      print("Error: datafile \"outcome-of-care-measures.csv\" does not exist")
    }
  }else{
    print("Error: Not in ProgrammingAssignment3 dir or data/ dir missing from ProgrammingAssignment3/")
  }
}