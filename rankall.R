rankall <- function(outcome, num = "best") {
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
      tempData <- na.omit(data[c(2,7,11,17,23)])
      #outcomeByState <- split(tempData,tempData$State)
      f <- factor(data$State)
      fLevels <- names(table(f))
  ## Check that state and outcome are valid
      
      #st <- state %in% names(outcomeByState)  # check if state value is valid (exist if list of present states)
      switch ( outcome,                       # Used Switch to detemine col from outcome
               "heart attack" = col <- 3,
               "heart failure" = col <- 4,
               "pneumonia" = col <- 5,
               "default" = col <- 0
      )
      col <- if(col==0) FALSE else col        # check if outcome is either "heart attack",  "heart failure", or "pneumonia"
      
      
      
      if(!col) stop("invalid outcome",call. = !col)
      tempData <- tempData[order(tempData[,col]),]
      outcomeByState <- split(tempData,tempData$State)
      
      
      ## For each state, find the hospital of the gtempTry.tempTry.tempTry.tempTry.tempTry.tempTry.tempTry.tempTry.tempTry.tempTry.tempTry.iven rank
      
      
      rank <- if(num %in% c(1:len)) num else switch(num, "best" = 1,"worst" = len, 0)
      outcomeByState <- lapply(outcomeByState, function(x){len <- nrow(x);write.csv(x,'data/b4Order.csv');x$rank <- c(1:len);write.csv(x,'data/aftOrder.csv');x})

      
      
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
      #outcomeByState
      output = character(0)
      for(i in fLevels){
        temp <- outcomeByState[[i]][outcomeByState[[i]]['rank'] == rank,1]
        if(length(temp)==0) temp <- NA
        output <- c(output,temp)
      }
      data.frame(output,fLevels)
    }
  }
}

