# Change directory and import packages
setwd("/media/seagatedrive/rhys/Run/puffin/turb_channel/Re225-les-3d-30to50tu_isothermal/fields")
library("readr", lib.loc="~/anaconda3/lib/R/library")
#library("data.table", lib.loc="~/anaconda3/lib/R/library")
library("tibble", lib.loc="~/anaconda3/lib/R/library")
library("dplyr", lib.loc="~/anaconda3/lib/R/library")
#library("purrr", lib.loc="~/anaconda3/lib/R/library")
#library("devtools", lib.loc="~/anaconda3/lib/R/library")
library("ggplot2", lib.loc="~/anaconda3/lib/R/library")
library("tidyr", lib.loc="~/anaconda3/lib/R/library")
library("parallel", lib.loc="~/anaconda3/lib/R/library")
library("doMC", lib.loc="~/anaconda3/lib/R/library")
library("caret", lib.loc="~/anaconda3/lib/R/library")
library("mgcv", lib.loc="~/anaconda3/lib/R/library")

# Column names
data <- read_csv("df_combined_40to49_dropped_y")

# Set the seed
set.seed(42)

# Shuffle row indicies: rows
rows <- sample(nrow(data))

# Randomly order the data frame
data <- data[rows,]

# Determine the row to split on
split <- round(nrow(data) * 0.001)

# Create train
train <- data[1:split,]

# Get the y values
y_values <- train$y

# Filter out the columns I don't need
train <- train %>%
  select(c(-1,-2,-3,-4,-5,-6,-36,-37, -39))

# Find and register the number of cores
numCores <- detectCores()
registerDoMC(cores = numCores)

# Fit random forest
model <- train(a_yz ~ .,
               tuneLength = 10,
               data = train,
               method = "ranger",
               trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

model

plot(model)

# Get the predictions
train$predictions <- predict(model, train)

# Add the y values back in
train$y <- y_values

# Plot the results using ggplot
ggplot(train, aes(x = predictions, y = a_yz)) + geom_point() + geom_abline()

# Plot against the y position
train %>%
  select(c("a_yz", "predictions", "y")) %>%
  gather(key = "stress", value = "value", -y) %>%
  ggplot(aes(x = value, y = y, colour = stress)) + geom_point(shape = 1) 

# Plot again using smoothed geometry
ggplot(train, aes(x = a_yz, y = y)) + geom_point(shape = 1) + 
  geom_smooth(data = train, aes(x = predictions, y = y), method = "gam", 
              formula = y ~ s(x, k = 1000))

# Plot a new curve
train %>%
  select(c("a_yz", "predictions", "y")) %>%
  group_by(y) %>%
  summarise(a_yz_average = mean(a_yz), predictions_average = mean(predictions)) %>%
  gather(key = "stress", value = "value", -y) %>%
  ggplot(aes(x = value, y = y, colour = stress)) + geom_point(shape = 1) 