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

#Gam with natural splines

gam_1 <- glm(Enroll ~ Apps + Accept + Private + F.Undergrad + ns(Top10perc, 3) + ns(Top25perc,3) + P.Undergrad + ns(Outstate,3) + ns(Room.Board, 4) + ns(Books) + ns(Personal,3) + ns(PhD,3) + ns(Terminal,3) + ns(S.F.Ratio,3) + ns(perc.alumni,3) + ns(Expend,3) + ns(Grad.Rate,3),family=poisson, data=train_data)

summary(gam_1)

#test about overall significance

mod_0 <- glm(Enroll ~ 1, family=poisson, data=train_data)
anova(mod_0,gam_1,test="Chisq")

#GAM with smoothing splines

gam_2 <- gam(Enroll ~ Apps + Accept + Private + F.Undergrad + 
               s(Top10perc, k=3) + s(Top25perc, k=3) + P.Undergrad + 
               s(Outstate, k=3) + s(Room.Board, k=4) + s(Books, k=3) + 
               s(Personal, k=3) + s(PhD, k=3) + s(Terminal, k=3) + 
               s(S.F.Ratio, k=3) + s(perc.alumni, k=3) + s(Expend, k=3) + 
               s(Grad.Rate, k=3), 
             family = poisson(), 
             data = train_data,
             method = "REML")

summary(gam_2)
plot(gam_2)

#prediction

pred_gam_1 <- predict(gam_1, val_data[,-4])
pred_gam_2 <- predict(gam_2, val_data[,-4])

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

(rmse_gam_1 <- rmse(val_data$Enroll, pred_gam_1))
(rmse_gam_2 <- rmse(val_data$Enroll, pred_gam_2))
