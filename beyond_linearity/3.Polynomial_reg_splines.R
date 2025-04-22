################################################################################
#                                                                              #
#                     Polynomial regression and Splines                        #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################

# you can find the notes of this lecture in my github: https://github.com/ValentinaZangirolami/Statistical-Learning

#load libraries
library(car)
library(ggplot2)
library(ISLR2)
library(splines)
library(tidyverse)

#load dataset
data_house <- Boston
data_house <- data_house |> select(c(nox, dis))

head(data_house)

# plot

ggplot(data_house, aes(x = dis, y = nox)) +
  geom_point(size = 2)
labs(
  x = "dis",
  y = "nox"
) + 
  theme_minimal()


#poly regression

#2degree

poly_lm <- lm(nox~poly(dis, 2), data=data_house)

summary(poly_lm)

#lm plot

par(mfrow=c(2,2))
plot(poly_lm)

#3degree

poly_lm_2 <- lm(nox~poly(dis, 3), data=data_house)

summary(poly_lm_2)

#lm plot

par(mfrow=c(2,2))
plot(poly_lm_2)

#comparison estimated poly regression lines

ggplot(data_house, aes(x = dis, y = nox)) +
  geom_point(size = 1, shape=3) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +  # Adds 1st regression line
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), color = "blue") + # Adds 2nd regression line
  labs(
    x = "Distance Boston Centers",
    y = "NO concentration"
  ) + 
  theme_minimal()

#regression spline

#linear splines

#2 knots

mod_spline <- lm(nox ~ bs(dis , knots = c(3.75, 6.25), degree = 1), data = data_house) #linear spline
summary(mod_spline)

#3 knots

mod_spline_2 <- lm(nox ~ bs(dis , knots = c(2.5, 5, 7.5), degree = 1), data = data_house)
summary(mod_spline_2)


ggplot(data_house, aes(x = dis, y = nox)) +
  geom_point(size = 1, shape=3) +
  geom_smooth(method = "lm", formula = y ~ bs(x , knots = c(3.75, 6.25), degree = 1), color = "red", se = FALSE) +  # Adds 1st regression line
  geom_smooth(method = "lm", formula = y ~ bs(x , knots = c(2.5, 5, 7.5), degree = 1), color = "pink", se = FALSE) + # Adds 2nd regression line
  labs(
    x = "Distance Boston Centers",
    y = "NO concentration"
  ) + 
  theme_minimal()


#cubic splines

#1 knot

mod_spline_3 <- lm(nox ~ bs(dis , knots = c(5), degree = 3), data = data_house)
summary(mod_spline_3)

#2 knots

mod_spline_4 <- lm(nox ~ bs(dis , knots = c(3.75, 6.25), degree = 3), data = data_house)
summary(mod_spline_4)

#plot cubic splines

ggplot(data_house, aes(x = dis, y = nox)) +
  geom_point(size = 1, shape=3) +
  geom_smooth(method = "lm", formula = y ~ bs(x , knots = c(5), degree = 3), color = "blue", se = FALSE) +  # Adds 1st regression line
  geom_smooth(method = "lm", formula = y ~ bs(x , , knots = c(3.75, 6.25), degree = 3), color = "green", se = FALSE) + # Adds 2nd regression line
  labs(
    x = "Distance Boston Centers",
    y = "NO concentration"
  ) + 
  theme_minimal()

#natural spline

mod_nat_spline <- lm(nox ~ ns(dis , knots = c(3.75, 6.25)), data = data_house)
summary(mod_nat_spline)


ggplot(data_house, aes(x = dis, y = nox)) +
  geom_point(size = 1, shape=3) +
  geom_smooth(method = "lm", formula = y ~ ns(x, knots = c(3.75, 6.25)), color = "violet") +  # Adds regression line
  labs(
    x = "Distance Boston Centers",
    y = "NO concentration"
  ) + 
  theme_minimal()

#smoothing splines

#df=4
mod_smooth_spline <- smooth.spline(data_house$dis, data_house$nox, df = 4)
data_house$pred <- predict(mod_smooth_spline, data_house$dis)$y

#df=10
mod_smooth_spline_2 <- smooth.spline(data_house$dis, data_house$nox, df = 10)
data_house$pred_2 <- predict(mod_smooth_spline_2, data_house$dis)$y

#df=200
mod_smooth_spline_3 <- smooth.spline(data_house$dis, data_house$nox, df = 200)
data_house$pred_3 <- predict(mod_smooth_spline_3, data_house$dis)$y

#plot
ggplot(data_house, aes(y=nox, x=dis)) + geom_point(size = 1, shape=3) + geom_line(aes(y = pred), color = "red", linewidth = 1) + geom_line(aes(y = pred_2), color = "orange", linewidth = 1) + geom_line(aes(y = pred_3), color = "violet", linewidth = 1) +
  labs(title = "Smoothing Splines", x = "Distance Boston Centers", y = "NO concentrations")+ 
  theme_minimal()

#cv
mod_smooth_spline_cv <- smooth.spline(data_house$dis, data_house$nox, cv=TRUE)
data_house$pred_cv <- predict(mod_smooth_spline_cv, data_house$dis)$y

#plot
ggplot(data_house, aes(y=nox, x=dis)) + geom_point(size = 1, shape=3) + geom_line(aes(y = pred_cv), color = "blue", linewidth = 1) +
  labs(title = "Smoothing Splines", x = "Distance Boston Centers", y = "NO concentrations")+ 
  theme_minimal()

mod_smooth_spline_cv$lambda
