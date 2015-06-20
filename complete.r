complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  cid <- NULL
  nobs <- NULL
  count <- 0
  
  for (i in id) {
    if (i < 10) fileID = paste0(directory,"/00", i, ".csv") else
      if (i < 100) fileID = paste0(directory,"/0", i, ".csv") else
        fileID = paste0(directory,"/", i, ".csv")      
    temp <- read.csv(fileID, header=TRUE)
    count <- count+1
    nobs[count] = sum(!is.na(temp[,2]) & !is.na(temp[,3]))
    cid[count] = temp[1,4]
  }
  return(data.frame(id=cid,nobs=nobs))
}