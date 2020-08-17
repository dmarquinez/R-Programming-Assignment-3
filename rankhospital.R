rankhospital <- function(state, outcome, num = "best") {
  oldw <- getOption("warn")
  options(warn = -1)
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
  
  filt <- filter(data,State==state) #filter by state
  filt <- arrange(filt,as.numeric(filt[,col]),filt[,2])#order by death and name of the hospital
  filt <- select(filt,2,col)
  filt <- filt[!is.na(as.numeric(as.character(filt[,2]))),]
  options(warn = oldw)
  if(num=="best")
    num <- 1
  else if(num=="worst")
    num <- dim(filt)[1]
  else if(as.numeric(num)>dim(filt)[1])
    return(NA)
  filt[num,1]
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}
##FALTA LIMPIAR DE NA