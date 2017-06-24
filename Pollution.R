pollutantmean <- function(directory, pollutant, id = 1:332){
   isFirst <- TRUE
   my_data_no_NA <- c()
   for (i in id){
     x <- csvName(i)
     my_data <- read.csv(paste(directory, "/", x, ".csv", sep=""))
     if (isFirst){
       isFirst <- FALSE
       my_data_no_NA <- na.omit(my_data[[pollutant]])
     }
     else { 
       my_data_no_NA <- c(my_data_no_NA, na.omit(my_data[[pollutant]]))
     }
   }
   mean(my_data_no_NA)
}

complete <- function(directory, id = 1:332){
  df <- data.frame(id = numeric(), nobs = numeric())
  for (i in id){
    x <- csvName(i)
    my_data <- read.csv(paste(directory, "/", x, ".csv", sep=""))
    n <- sum(!(is.na(my_data[["sulfate"]]) | is.na(my_data[["nitrate"]])))
    df[nrow(df)+1,] <- c(i, n)
  }
  df
}

csvName <- function(i){
  if (i >= 100){
    paste("", i, sep="")
  }
  else if (i >= 10){
    paste("0", i, sep="")
  }
  else {
    paste("00", i, sep="")
  }
}

corr <- function(directory, threshold = 0){
  completeLoc <- complete(directory)
  corVector <- c(numeric())
  ids <- completeLoc$id[completeLoc$nobs > threshold]
  if (length(ids) > 0){
    for (i in 1:length(ids)){
      x <- csvName(ids[i])
      my_data <- read.csv(paste(directory, "/", x, ".csv", sep=""))
      corVector <- c(corVector, cor(my_data[["sulfate"]], my_data[["nitrate"]], use = "complete.obs"))
    }
  }
  corVector
}