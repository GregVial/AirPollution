corr <- function(directory, threshold = 0) {
  ## read file list in directory provided as input
  fileList <- dir(directory)
  ## Initiate ouput vector and vector length counter
  output <- vector(mode="numeric")
  i<-0
  ## check the complete cases and store in cc
  for (fn in fileList) {
    file <- read.csv(paste0(directory,"/",fn))
    cc <- complete.cases(file)
    # print(paste(fn,sum(ccFile)))
    ## if the number of complete cases is higher than the thresholdn the add correlation to output
    if (sum(cc) > threshold) {
      # print(paste(fn,"OK"))
      i<-i+1
      length(output) <- i
      output[i] <- cor(file[cc,2],file[cc,3])
    }
  }
  output
}