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
library(earth)
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
#train
pred_gam_1_train <- predict(gam_1, train_data[,-4], type = "response")
pred_gam_2_train <- predict(gam_2, train_data[,-4], type = "response")
#val
pred_gam_1 <- predict(gam_1, val_data[,-4], type = "response")
pred_gam_2 <- predict(gam_2, val_data[,-4], type = "response")

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

cat("RMSE on training data \n")
(rmse_gam_1_train <- rmse(train_data$Enroll, pred_gam_1_train)) #308.79
(rmse_gam_2_train <- rmse(train_data$Enroll, pred_gam_2_train)) #332.18

cat("RMSE on validation data \n")
(rmse_gam_1 <- rmse(val_data$Enroll, pred_gam_1)) #1076.63
(rmse_gam_2 <- rmse(val_data$Enroll, pred_gam_2)) #932.613

#mars

mars1 <- earth(Enroll ~ .,  data = train_data, glm=list(family=poisson))
mars1$cuts
mars1$coefficients

plot(mars1)
plot(evimp(mars1))

#train
pred_mars_1_train <- predict(mars1, train_data[,-4], type = "response")
#val
pred_mars_1 <- predict(mars1, val_data[,-4], type = "response")

cat("RMSE on training data \n")
(rmse_mars_1_train <- rmse(train_data$Enroll, pred_mars_1_train)) #1129.28

cat("RMSE on validation data \n")
(rmse_mars_1<- rmse(val_data$Enroll, pred_mars_1)) #1439.031

#classification task

data_college <- data_college |>
  mutate(Y = if_else(Enroll > 430, 1, 0))

data_college <- data_college |> select(-Enroll)

ggplot(data_college, aes(x = Y)) +
  geom_bar(aes(y = ..count..), fill = "pink") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1),
                y = ..count..), 
            stat = "count",
            vjust = -0.5) +
  labs(x = "Y",
       y = "Frequency") +
  theme_minimal() #balanced

#split train and test

# Set seed for reproducibility
set.seed(123)  

# Create stratified split
split_obj <- initial_split(data_college, prop = 0.8, strata = Y)  # Stratify by the binary variable

# Extract the splits
train_data <- training(split_obj)
val_data <- testing(split_obj)

#Gam with natural splines

gam_1 <- glm(Y ~ Apps + Accept + Private + F.Undergrad + ns(Top10perc, 3) + ns(Top25perc,3) + P.Undergrad + ns(Outstate,3) + ns(Room.Board, 4) + ns(Books) + ns(Personal,3) + ns(PhD,3) + ns(Terminal,3) + ns(S.F.Ratio,3) + ns(perc.alumni,3) + ns(Expend,3) + ns(Grad.Rate,3),family=binomial, data=train_data)

summary(gam_1)

#test about overall significance

mod_0 <- glm(Y ~ 1, family=binomial, data=train_data)
anova(mod_0,gam_1,test="Chisq")

#GAM with smoothing splines

gam_2 <- gam(Y ~ Apps + Accept + Private + F.Undergrad + 
               s(Top10perc, k=3) + s(Top25perc, k=3) + P.Undergrad + 
               s(Outstate, k=3) + s(Room.Board, k=4) + s(Books, k=3) + 
               s(Personal, k=3) + s(PhD, k=3) + s(Terminal, k=3) + 
               s(S.F.Ratio, k=3) + s(perc.alumni, k=3) + s(Expend, k=3) + 
               s(Grad.Rate, k=3), 
             family = binomial(), 
             data = train_data,
             method = "REML")

summary(gam_2)
plot(gam_2)

#prediction
#train
pred_gam_1_train <- predict(gam_1, train_data[,-18], type = "response")
pred_gam_2_train <- predict(gam_2, train_data[,-18], type = "response")

#val
pred_gam_1 <- predict(gam_1, val_data[,-18], type = "response")
pred_gam_2 <- predict(gam_2, val_data[,-18], type = "response")


accuracy <- function(truth, predicted) {
  mean(truth == predicted)
}

pred_gam_1_train <- ifelse(pred_gam_1_train >0.5, 1, 0 )
pred_gam_2_train <- ifelse(pred_gam_2_train >0.5, 1, 0 )

cat("Accuracy on training data \n")
(acc_gam_1_train <- accuracy(train_data$Y, pred_gam_1_train)) #0.973
(acc_gam_2_train <- accuracy(train_data$Y, pred_gam_2_train)) #0.971

pred_gam_1 <- ifelse(pred_gam_1 >0.5, 1, 0 )
pred_gam_2 <- ifelse(pred_gam_2 >0.5, 1, 0 )

cat("Accuracy on validation data \n")
(acc_gam_1 <- accuracy(val_data$Y, pred_gam_1)) #0.942
(acc_gam_2 <- accuracy(val_data$Y, pred_gam_2)) #0.949

#mars 

mars1 <- earth(Y ~ .,  data = train_data, glm=list(family=binomial))
mars1$cuts
mars1$coefficients

plot(mars1)
plot(evimp(mars1))

#train
pred_mars_1_train <- predict(mars1, train_data[,-18], type = "response")
#val
pred_mars_1 <- predict(mars1, val_data[,-18], type = "response")

pred_mars_1_train <- ifelse(pred_mars_1_train >0.5, 1, 0 )
pred_mars_1 <- ifelse(pred_mars_1 >0.5, 1, 0 )

cat("Accuracy on training data \n")
(acc_mars_1_train <- accuracy(train_data$Y, pred_mars_1_train)) #0.969

cat("Accuracy on validation data \n")
(acc_mars_1<- accuracy(val_data$Y, pred_mars_1)) #0.942

# Your turn! Propose new equation for gam: you can try to change the smooth terms inside the equation. 
# Eventually, have a look of the help page. And compute the final rmse.