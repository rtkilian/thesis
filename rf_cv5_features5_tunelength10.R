# Choose the working directory
setwd("/media/seagatedrive/rhys/Run/R/Thesis/Re225-les-3d-30to50tu_isothermal")

# Name of the file
filename <- "rf_cv5_features5_tunelength10"

# Source the setup file
source("setupFile.R")

# Load the packages
loadPackages()

# Load the training data
seed <- 42
fraction <- 0.0001 # Fraction of total data to sample from entire dataset - THIS NEEDS WORK
training <- loadDataTrain(fraction, seed)

# Load the training data removing the following columns - NOT GREAT AS I GET TWO BIG DATAFRAMES
removeColumns <- c(-1,-2,-3,-4,-5,-6,-36,-37, -39)
trainingClean <- cleanTrainingFunction(training, removeColumns)

### === MODEL BUILDING === ###

# Find and register the number of cores
numCores <- detectCores()
registerDoMC(cores = numCores)

# Fit random forest
model <- train(a_yz ~ .,
               tuneLength = 20,
               data = trainingClean,
               method = "ranger",
               trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

model

min(model[["results"]][["RMSE"]])

plot(model)

# Get the predictions
training$predictions <- predict(model, training)


### === MODEL === ###

### === PLOTS === ###

plotAgainstY(training)

plotAgainstYAverage(training)

plotReynoldsStress(training)

plotFit(training)

### === PLOTS === ###