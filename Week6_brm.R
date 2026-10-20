#### Analysis stub: LING609, October 2026
#### This script accompanies VAS data collected 

#### If you haven't installed the required packages, run these:
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('rsample')
#install.packages('purrr')
#install.packages('brms')

library(dplyr)
library(ggplot2)
library(rsample)
library(purrr)
library(brms)

# Load in PCIbex data
column.labels <- c('Time','ID','Controller',"Item.Order",
                   'Sequence.Order','Trial.Type','Group',
                   'PennElementType','PennElementName',
                   'Parameter','Value','EventTime','VOT','RT','Comments')

# To use this, you must edit the file path to point to the file on your local computer
my.data <- read.csv('~/Dropbox/Courses/LING611-Spring2024/Code/Lab1/resultsBWD.csv',     # Edit the path between the quotes
                    header=F,comment.char="#",
                    col.names=column.labels)

# Inspect the data visually in Rstudio
View(my.data)

# EXERCISE 1: Determine your own personal boundary between 'd' and 't'. 
# The boundary is whichever value of VOT has approximately 50% 'd' and 't' responses. 
# For me, this was ~30ms, but yours may be different. You may analyze the data any way you like to determine this, 
# but put the code you use here.




# Save your result as this variable
my.boundary <-  ... 

# Create a new predictor that is distance from boundary point
response.data <- response.data %>%
                  mutate(boundary_distance = abs(as.numeric(VOT)-my.boundary))


# EXERCISE 2: Using brms, fit a linear regression that predicts RT as as a function of 'boundary_distance'. In a brief set of 
# comments, please 1) determine whether the model is well fit, and 2) evaluate the evidence in favor of the scientific hypothesis that "Decision RT increases near the VOT boundary"



# EXERCISE 3: Using ppcheck with 'dens overlay,' visually evaluate the posterior predictive distribution. Does your model 
# fit the data well? Say why, or why not.



