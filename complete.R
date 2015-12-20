complete <- function(directory, id = 1:332) {
  ## create the data frame
  df <- data.frame(id=character(length(id)),nobs=numeric(length(id)),stringsAsFactors=F)
  
  ## Loop through the id
  for (i in seq_along(id)) {
    pos <- id[i]
    ## define the filename
    if (pos < 10) {
      filename = paste0("00",pos,".csv")
    } else if (pos < 100) {
      filename = paste0("0",pos,".csv")
    } else {
      filename = paste0(pos,".csv")
    }
    # print(filename)
    ## read the csv file
    f <- read.csv(paste0(directory,"/",filename))
    ## count complete cases
    count <- sum(complete.cases(f))
    ## write to output dataframe
    df$id[i] <- filename
    df$nobs[i] <-count
  }
  
  ## Return a data frame
  df
}