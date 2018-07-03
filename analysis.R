# Change the working directory
setwd("/media/seagatedrive/rhys/Run/puffin/turb_channel/Re225-les-3d-30to50tu_isothermal/postproc")

# Load the packages
library("ggplot2", lib.loc="~/anaconda3/lib/R/library")
library("purrr", lib.loc="~/anaconda3/lib/R/library")

#=== STRESS FILE ===#

# Import the data
stress_field <-read.table("profiles.strss.fields_001-040.stepsize_001.dat", header = FALSE)

# Rename the columns
names(stress_field) <- c("ys", "yswall", "wv", "kmdwdy", "wflux_turb", "nudwdy", "wflux_tot", "hv", "khdhdy", "hflux_turb", "kappadhdy", "hflux_tot")


# Plot the stress against the vertical position
# ggplot(stress_field, aes(x = wv, y = ys)) + geom_point()


# Columns to plot
colsToPlot <- c("wv", "kmdwdy", "wflux_turb", "nudwdy", "wflux_tot", "hv", "khdhdy", "hflux_turb", "kappadhdy", "hflux_tot")

# Function to call
plotAgainstY <- function(columnName){
  p <- ggplot(stress_field, aes(x = stress_field[columnName], y = ys)) + geom_point() + labs(x = columnName)
  print(p)
}

# Plot them all
walk(.x = colsToPlot, .f = plotAgainstY)


#=== FLUCTUATIONS FILE ===#

# Import the new data
flucts <- read.table("profiles.flucs.fields_001-040.stepsize_001.dat", header = FALSE)
names(flucts) <- c("y", "ywall", "urms", "vrms", "wrms", "hrms", "prms", "tke")

# Columns to plot
colsToPlot <- c("urms", "vrms", "wrms", "hrms", "prms", "tke")

# Function to call
plotAgainstY <- function(columnName){
  p <- ggplot(flucts, aes(x = flucts[columnName], y = y)) + geom_point() + labs(x = columnName)
  print(p)
}

# Plot them all
walk(.x = colsToPlot, .f = plotAgainstY)

