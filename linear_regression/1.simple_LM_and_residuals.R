################################################################################
#                      Simple linear regression models                         #
#                          & Analysis of residuals                             #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################

# you can find the notes of this lecture in my github: https://github.com/ValentinaZangirolami/Statistical-Learning

#libraries

library(tidyverse)
library(glmtoolbox)
library(moderndive)
library(ggplot2)

## load dataset

data(advertising)

summary(advertising)
head(advertising)

# We are interested in using the explanatory variable TV for explaining sales

##### Plot

theme_set(theme_bw())  ## sets default white background theme for ggplot
ggplot(data = advertising, aes(x=TV, y=sales)) + geom_point(col="red", alpha = 0.5) + labs(x="TV advertising", y="Sales", title = "Plot Sales and TV advertising")

ggplot(data = advertising, aes(x=TV, y=sales)) + geom_point(col="red", alpha = 0.5) + geom_smooth(method = "lm") + labs(x="TV advertising", y="Sales", title = "Plot Sales and TV advertising")

advertising %>% 
  get_correlation(formula = sales ~ TV)

#### simple lm regression

lm_sales <- lm(sales ~ TV, data = advertising)

summary(lm_sales)

#Confident Intervals for each regression coefficient with 95% confidence level
confint(lm_sales)
#you can verify the confidence level by looking the first line.

#Analysis of residuals
par(mfrow=c(2,2))
plot(lm_sales)

#In the first plot (Residual vs Fitted), we can observe the relationship between residuals and
#fitted values.
#Q-Q Residuals allows us to check the gaussian assumption of errors
#Scale-Location is used to verify if the assumption of homoschedasticity holds within data
#Residuals vs Leverage can highlights potential issues as outliers.

## Prediction

# new data for mheight
new_data <- data.frame(TV=c(350, 254, 168, 450))

# check range of mheight values
cat('Min:', min(advertising$TV), 'Max:', max(advertising$TV))

# confidence interval
(prev_ic <- predict(lm_sales, new_data, interval = "confidence"))

# Plot

plot(advertising$TV, advertising$sales, xlim=range(new_data$TV), ylab = "Sales", xlab = "TV adv")
matlines(new_data$TV, prev_ic, lty=c(1,2,2), col="violet")

rm(list=ls())

#### Simulating Data - Analysis of residuals
set.seed(123)
x <- sort(runif(50))

# no issues

y1 <- 0.5 + 0.7*x+rnorm(200,sd=.05)
par(mfrow=c(2, 2))
plot(lm(y1~x))

# Issues with Linearity assumption

y1 <- x^2+rnorm(200,sd=.05)
y2 <- -x^2+rnorm(200,sd=.05)
y3 <- (x-.5)^3+rnorm(200,sd=.01)

plot(x,residuals(lm(y1~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y2~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y3~x)),xlab="X",ylab="residuals")

# Diagnostic plot
par(mfrow=c(2, 2))
plot(lm(y1~x))

# Transformation of the independent variable
par(mfrow=c(2, 2))
plot(lm(y1~poly(x, 2)))

# Issues with Homoscedasticity assumption
y1 <- x+x*rnorm(200,sd=.03)
y2 <- x+(1-x)*rnorm(200,sd=.03)
y3 <- x+c(rnorm(50,sd=.1),rnorm(100,sd=.4),rnorm(50,sd=.02))
y4 <- x+c(rnorm(100,sd=.5),rnorm(50,sd=.1),rnorm(50,sd=.4))

plot(x,residuals(lm(y1~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y2~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y3~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y4~x)),xlab="X",ylab="residuals")

# Issues with Gaussian assumption

y1 <- x+.05*rt(200,df=2)
y2 <- x+.03*(rchisq(200,1)-.5)
y3 <- x-.03*(rchisq(200,1)-.5)
y4 <- x+.07*rt(200,df=2)

plot(x,residuals(lm(y1~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y2~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y3~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y4~x)),xlab="X",ylab="residuals")

# qqplot

qqnorm(residuals(lm(y1~x)), pch = 1, frame = FALSE)
qqline(residuals(lm(y1~x)), col = "steelblue", lwd = 2)

qqnorm(residuals(lm(y2~x)), pch = 1, frame = FALSE)
qqline(residuals(lm(y2~x)), col = "steelblue", lwd = 2)

# transformations of Y

qqnorm(residuals(lm(log(y2)~x)), pch = 1, frame = FALSE)
qqline(residuals(lm(log(y2)~x)), col = "steelblue", lwd = 2)

qqnorm(residuals(lm(sqrt(y2)~x)), pch = 1, frame = FALSE)
qqline(residuals(lm(sqrt(y2)~x)), col = "steelblue", lwd = 2)
