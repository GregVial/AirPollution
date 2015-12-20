pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## intialize the variables
  res<-0
  totlenpoll<-0
  ## loop through the id
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
    # print(paste0("Loading ",filename))
    ## Read the csv vile
    poll <- read.csv(paste0(directory,"/",filename))
    lenpoll<-nrow(poll[!is.na(poll[,pollutant]),])
    totlenpoll<-totlenpoll+lenpoll
    res <- res+mean(poll[,pollutant],na.rm=T)*lenpoll
    ##print(res)
  }
  print(res/totlenpoll)
}