################################################################################
#                                                                              #
#                              Decision Trees                                  #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################

# you can find the notes of this lecture in my github: https://github.com/ValentinaZangirolami/Statistical-Learning

#load libraries
library(car)
library(ggplot2)
library(tidymodels)
library(tidyverse)


esg_data <- read.csv("esg_financial_dataset.csv")
head(esg_data)