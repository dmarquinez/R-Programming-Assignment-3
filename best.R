best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if(!any(c("heart attack","heart failure","pneumonia")==outcome))
    stop("invalid outcome")
  if(!any(unique(data$State==state)))
     stop("invalid state")
  col <- if(outcome=="heart attack")
    11
  else if(outcome=="heart failure")
    17
  else if(outcome=="pneumonia")
    23
  oldw <- getOption("warn")
  options(warn = -1)
  filt <- filter(data,State==state) #filter by state
  filt <- arrange(filt,as.numeric(filt[,col]),filt[,2])#order by death and name of the hospital
  filt <- select(filt,2,col)
  options(warn = oldw)
  filt[1,1]
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
#11. Hospital 30-Day Death (Mortality) Rates from Heart Attack
#17. Hospital 30-Day Death (Mortality) Rates from Heart Failure
#23. Hospital 30-Day Death (Mortality) Rates from Pneumonia
