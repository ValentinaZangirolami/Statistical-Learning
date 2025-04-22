################################################################################
#                                                                              #
#                               GAM and MARS                                   #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################

# you can find the notes of this lecture in my github: https://github.com/ValentinaZangirolami/Statistical-Learning

#load libraries
library(ISLR2) 
library(car)
library(ggplot2)
library(tidymodels)
library(mgcv)  
library(tidyverse)
library(splines)

#data
data_college = College
head(data_college)
summary(data_college)

#remove outlier in Apps

data_college <- data_college |>
  slice(-which.max(Apps))  

summary(data_college)

#marginal interpretation

(Scatter_Matrix <- GGally::ggpairs(data_college[,c(2:10)], 
                                   title = "Scatter Plot Matrix for College (first 9 variables)", 
                                   axisLabels = "show"))
(Scatter_Matrix_2 <- GGally::ggpairs(data_college[,c(4, 11:18)], 
                                     title = "Scatter Plot Matrix for College (remaining variables)", 
                                     axisLabels = "show"))


#train-validation sets

set.seed(123)  # For reproducibility
split <- initial_split(data_college, prop = 0.8)

train_data <- training(split)
val_data <- testing(split)
