rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  states <- str_sort(unique(data$State))
  if(!any(c("heart attack","heart failure","pneumonia")==outcome))
    stop("invalid outcome")
  col <- if(outcome=="heart attack")
    11
  else if(outcome=="heart failure")
    17
  else if(outcome=="pneumonia")
    23
  oldw <- getOption("warn")
  options(warn = -1)
  filt <- select(data,2,7,col)
  result <- data.frame(hospital=character(),state=character(),stringsAsFactors = FALSE)
  filt <- split(filt,filt$State)#split by states
  for(i in states){
    sp <- filt[[i]] 
    sp <- sp[complete.cases(sp),] #clean na
    sp <- sp[!is.na(as.numeric(as.character(sp[,3]))),]
    sp <- arrange(sp,as.numeric(sp[,3]),sp[,1])  #order
    rank <- num
    if(num=="best")
      rank <- 1
    else if(num=="worst")
      rank <- dim(sp)[1]
    if(rank > as.numeric(dim(sp)[1]))
      name <- "<NA>"
    else
      name <- sp[rank,1]
    result[nrow(result)+1,] <- list(name,i)
      }
  result
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}