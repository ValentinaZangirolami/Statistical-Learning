################################################################################
#                                                                              #
#                     Multiple linear regression models                        #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################

#load libraries
library(car)
library(ggplot2)
library(tidyverse)

#load data
esg_data <- read.csv("esg_financial_dataset.csv")

#show data
head(esg_data)

# we're going to esclude some variables to: (i) alleviate correlation among independent variables, (ii) working just quantitative variables
cat_variables <- esg_data |> select(c(Industry, Region))
esg_data <- esg_data |> select(-c(Industry, Region, ProfitMargin, ESG_Environmental, ESG_Social, ESG_Governance, CarbonEmissions, WaterUsage))

#marginal interpretation

(Scatter_Matrix <- GGally::ggpairs(esg_data, 
                                  title = "Scatter Plot Matrix for ESG data", 
                                  axisLabels = "show"))
#linear regression

mod <- lm(MarketCap~., data=esg_data)
summary(mod)

round(mod$coefficients, 5)

#diagnostic plot
par(mfrow=c(2,2))
plot(mod) # some issues

# adding categorical variable

esg_data <- cbind(esg_data, cat_variables)

mod_2 =lm(MarketCap~., data=esg_data)
summary(mod_2)

unique(esg_data$Industry) # 8 dummies out of 9 categories

unique(esg_data$Region) # 6 dummies out of 7 categories

# we can fit separeted regression model for each category.

# toy example

ggplot(esg_data, aes(x=Revenue, y=MarketCap, color=Region))+ geom_point() + geom_smooth(method = "lm", aes(fill=Region))

#nested models

anova(mod, mod_2)

#diagnostic plot

par(mfrow=c(2,2))
plot(mod_2)

#interaction
mod_int <- lm(MarketCap ~ . + ESG_Overall:Industry, data = esg_data)
summary(mod_int)

#---------------------
#MODEL SELECTION
#---------------------

#CRITERION-BASED
#---------------

#CRITERION-BASED: considering the criterion AIC=-2sup loglik+2p 
# where p is the number of parameter
#(# explanatory variables + 1, if we estimate also sigma^2 
#the # of parameters increase of 1)
#AIC should be minimized. 
#For linear model is an increasing function of DR and p


#step function
#-------------
#An automatic selection can be done with 
#step(modello, formula, direction, trace, keep)

#direction can be "both", "backward", "forward" 
#(default "backward")
#trace can be 0 o 1 
#(default trace=1, shows all the selection process)
#keep alloos to keep track of the non significative variables

step(mod_int,trace=1,direction="backward")    #default: backward, trace=1
#we start from the full model:
#if we consider all the explanatory variables (none), l'AIC is 81729.24
#if we do not consider GrowthRate, AIC is 81727.24

#the best AIC is obtained with the model without GrowthRate

mod_nullo <- lm(MarketCap ~ 1, data = esg_data)
step(mod_nullo,trace=1,direction="forward",scope = formula(lm(MarketCap ~ . + EnergyConsumption:Industry, data = esg_data)))