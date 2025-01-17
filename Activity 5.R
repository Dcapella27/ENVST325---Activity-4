# Load in libraries
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)

# Read in data
ghg <- read.csv("/cloud/project/activity05/Deemer_GHG_Data.csv")

# Restructure data
ghg$log.ch4 <- log(ghg$ch4 + 1)
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP + 1)
ghg$log.precip <- log(ghg$precipitation)
ghg$log.SA <- log(ghg$surface.area)
ghg$BorealV <- ifelse(ghg$Region == "Boreal", 1, 0)
ghg$TropicalV <- ifelse(ghg$Region == "Tropical", 1, 0)

# Fit full linear regression model 
mod.full <- lm(log.ch4 ~ airTemp +
                 log.age + mean.depth +
                 log.DIP +
                 log.precip + 
                 BorealV, 
                 data=ghg)
summary(mod.full)

# Fit forward stepwise regression model with AIC
full.step <- ols_step_forward_aic(mod.full)
full.step
