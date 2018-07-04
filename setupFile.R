# Function to load the libraries
loadPackages <- function(){
  library("readr", lib.loc="~/anaconda3/lib/R/library")
  library("dplyr", lib.loc="~/anaconda3/lib/R/library")
  library("ggplot2", lib.loc="~/anaconda3/lib/R/library")
  library("tidyr", lib.loc="~/anaconda3/lib/R/library")
  library("parallel", lib.loc="~/anaconda3/lib/R/library")
  library("doMC", lib.loc="~/anaconda3/lib/R/library")
  library("caret", lib.loc="~/anaconda3/lib/R/library")
  library("mgcv", lib.loc="~/anaconda3/lib/R/library")
}

### Load the data ###
loadDataTrain <- function(fraction = 0.001, seed){
  
  # Column names
  data <- read_csv("df_combined_40to49_dropped_y")
  
  # Set the seed
  set.seed(seed)
  
  # Shuffle row indicies: rows
  rows <- sample(nrow(data))
  
  # Randomly order the data frame
  data <- data[rows,]
  
  # Determine the row to split on
  split <- round(nrow(data) * fraction)
  
  # Create train
  train <- data[1:split,]
}

### === Clean the training data === ###

cleanTrainingFunction <- function(training, removeColumns){
  return(select(training, removeColumns))
}

### === Clean the training data === ###

### === PLOT THE RESULTS === ###

plotAgainstY <- function(train){
    
    # Plot against the y position
    train %>%
      select(c("a_yz", "predictions", "y")) %>%
      gather(key = "stress", value = "value", -y) %>%
      ggplot(aes(x = value, y = y, colour = stress)) + geom_point(shape = 1) 
    
}

plotAgainstYAverage <- function(train){
  train %>%
    select(c("a_yz", "predictions", "y")) %>%
    group_by(y) %>%
    summarise(a_yz_average = mean(a_yz), predictions_average = mean(predictions)) %>%
    gather(key = "stress", value = "value", -y) %>%
    ggplot(aes(x = value, y = y, colour = stress)) + geom_point(shape = 1) 
}

plotFit <- function(train){
  ggplot(train, aes(x = predictions, y = a_yz)) + geom_point() + geom_abline()
}

plotReynoldsStress <- function(train){
  train %>%
    mutate(Re_yz_prediction = predictions * (2*tke)) %>%
    select(c("Re_yz", "Re_yz_prediction", "y")) %>%
    group_by(y) %>%
    summarise(Re_yz_average = mean(Re_yz), Re_yz_prediction_average = mean(Re_yz_prediction)) %>%
    gather(key = "stress", value = "value", -y) %>%
    ggplot(aes(x = value, y = y, colour = stress)) + geom_point(shape = 1)
}

### === PLOT THE RESULTS === ###