################################################################################
#                                                                              #
#                     Polynomial regression and Splines                        #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################

#load libraries
library(car)
library(ggplot2)
library(tidyverse)

#load dataset
esg_data <- read.csv("esg_financial_dataset.csv")

#poly regression
poly_esg <- lm(MarketCap~Revenue + EnergyConsumption + Industry + Region + poly(GrowthRate, 4) + poly(ESG_Overall, 4), data=esg_data)

summary(poly_esg)