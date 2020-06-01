library(leaps)
library(ggplot2)
library(dplyr)
library(broom)
library(scatterplot3d)

#Read in the dataset and remove any observations (countries and territories) that
#are missing data with na.omit
covid <- read.csv(file.choose(), na.strings = c("", "NA"))
covid <- na.omit(covid)
attach(covid)

#Removing the two outliers of US and China to observe if the best model changes
covid2 <- covid%>%
  filter(GDP < 10^13)
attach(covid2)

sd(covCases)
sd(covDeaths)

#Creates a list of factors that could possibly contribute to predicting the
#number of COVID-19 cases in a given place
covidpredictors <- cbind(avgInc, percentEducation, GDP, airQual, landlocked,
                         waterlocked, popDens)

#Compares all possible combinations of given regressors to each other by
#assigning a score to every possible regression line called Mallows' Cp. The
#smaller the Cp value is, the better the model fits the data. This outputs all
#the possible best fit lines with their corresponding Cp values for the user to
#peruse. The line with the lowest Cp value that still has all significant
#coefficients of factors (P-value < 0.05 for all factors in the model) is the
#best fit line for the data gathered to predict # of cases of COVID-19
bestCplm <- leaps(covidpredictors, covCases, method = "Cp")
cbind(bestCplm$Cp, bestCplm$which)

#Same as above but now the aim is to find the line that best predicts the number
#of deaths due to COVID-19
bestCplm2 <- leaps(covidpredictors, covDeaths, method = "Cp")
cbind(bestCplm2$Cp, bestCplm2$which)

bestAdjR2 <- leaps(covidpredictors, covCases, method = "adjr2")
cbind(bestAdjR2$adjr2, bestAdjR2$which)

bestAdjR22 <- leaps(covidpredictors, covDeaths, method = "adjr2")
cbind(bestAdjR22$adjr2, bestAdjR22$which)

#Best model found for COVID-19 cases with Mallows' Cp
tidy(lm(covCases ~ GDP))
glance(lm(covCases ~ GDP))

#Best model found for COVID-19 deaths with Mallows' Cp
tidy(lm(covDeaths ~ GDP))
glance(lm(covDeaths ~ GDP))

#Best model found for COVID-19 cases with adjusted r-squared
tidy(lm(covCases ~ GDP))
glance(lm(covCases ~ GDP))

#Best model found for COVID-19 deaths with adjusted r-squared
tidy(lm(covDeaths ~ GDP))
glance(lm(covDeaths ~ GDP))

#Best model found for COVID-19 cases with forward selection
tidy(lm(covCases ~ GDP))
glance(lm(covCases ~ GDP))

#Best model found for COVID-19 deaths with forward selection
tidy(lm(covDeaths ~ GDP))
glance(lm(covDeaths ~ GDP))

#Best model found for COVID-19 cases with backwards elimination
tidy(lm(covCases ~ GDP))
glance(lm(covCases ~ GDP))

#Best model found for COVID-19 deaths with backwards elimination
tidy(lm(covDeaths ~ GDP))
glance(lm(covDeaths ~ GDP))

#Notice a pattern? All 4 methods provided the same answer!
#(predicted # of COVID-19 cases) = 2700 + (6.64 x 10^-8)(GDP)
#(predicted # of COVID-19 deaths) = 226 + (4.09 x 10^-9)(GDP)

#Best model found for COVID-19 cases with Mallows' Cp when US and China
#are removed, run with covid2 attached
tidy(lm(covCases ~ GDP + waterlocked))
glance(lm(covCases ~ GDP + waterlocked))

#Best model found for COVID-19 deaths with Mallows' Cp when US and China
#are removed, run with covid2 attached
tidy(lm(covDeaths ~ GDP))
glance(lm(covDeaths ~ GDP))

#Best model found for COVID-19 cases with backwards elimination when US and
#China are removed, run with covid2 attached
tidy(lm(covCases ~ GDP + waterlocked))
glance(lm(covCases ~ GDP + waterlocked))

#Best model found for COVID-19 deaths with backwards elimination when US and
#China are removed, run with covid2 attached
tidy(lm(covDeaths ~ GDP))
glance(lm(covDeaths ~ GDP))

#Both of these methods agree for both models:
#(predicted # of COVID-19 cases) = 11600 + (5.54 x 10^-8)(GDP)
# - 21800(waterlocked)
#(predicted # of COVID-19 deaths) = 78.2 + (5.08 x 10^-9)(GDP)

#Two scatterplots with their best fit lines found by the four different methods
#for visualization purposes
covid%>%
  ggplot(aes(x = GDP, y = covCases)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Best Fit Line with Mallows' Cp for Predicting COVID-19 Cases",
       x = "Country/Territory GDP", y = "Number of COVID-19 Cases")

covid%>%
  ggplot(aes(x = GDP, y = covDeaths)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Best Fit Line with Mallows' Cp for Predicting COVID-19 Deaths",
       x = "Country/Territory GDP", y = "Number of COVID-19 Deaths")

#Creating a 3D scatterplot to visualize the regression plane for the best model
#for predicting COVID-19 cases when the US and China are removed, run with 
#covid2 attached
s3d <- scatterplot3d(x = GDP, y = waterlocked, z = covCases, type = "h",
                     color = "blue", angle = 55, pch = 16)
s3d$plane3d(lm(covCases ~ GDP + waterlocked))

#Scatterplot to visualize the best model for predicting COVID-19 deaths when
#the US and China are removed, run with covid2 attached
covid2%>%
  ggplot(aes(x = GDP, y = covDeaths)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Best Fit Line with Mallows' Cp for Predicting COVID-19 Deaths",
       x = "Country/Territory GDP", y = "Number of COVID-19 Deaths")
