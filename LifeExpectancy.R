library(ggplot2)
library(moderndive)
library(dplyr)
library(readr)
library(faraway)
library(tidyverse)
library(olsrr)
library(glmnet)

#Cleaning Data set
lifeExpectancyData1 <- read_csv("Life Expectancy Data.csv")

lifeExpectancyData <- lifeExpectancyData1 %>%
  rename(Life.expectancy = "Life expectancy") %>%
  rename(Adult.Mortality = "Adult Mortality") %>%
  rename(infant.deaths = "infant deaths") %>%
  rename(percentage.expenditure = "percentage expenditure") %>%
  rename(Measles = "Measles") %>%
  rename(under.five.deaths = "under-five deaths" ) %>%
  rename(Polio = "Polio")%>%
  rename(Diphtheria = "Diphtheria" )%>%
  rename(HIV.AIDS = "HIV/AIDS")

lifeExpectancyDataUpdated <- lifeExpectancyData[which(grepl(pattern="2015", lifeExpectancyData$Year)),]

#After looking at the dataset, only 8 of the variables had values for all the countries (some of the columns had NA entries)
lm(formula = Life.expectancy ~ Adult.Mortality + infant.deaths + 
     percentage.expenditure + Measles + under.five.deaths + Polio + 
     Diphtheria + HIV.AIDS, data = lifeExpectancyDataUpdated)

summary(lmod)
anova(lmod)
#Based on the ANOVA table, we see that Adult Mortality, infant deaths, under-five deaths, polio, diphtheria have significant p values

pairs(~ Adult.Mortality + infant.deaths + under.five.deaths + Polio + Diphtheria, data=lifeExpectancyDataUpdated)

lifeExpectancyDataUpdated2 <- lifeExpectancyDataUpdated[c(-1,-2,-3,-7,-8,-9, -10, -11, -14, -16, -17, -18)]
print(cor(lifeExpectancyDataUpdated2[,2:5]),digits=4)

#Performing all possible, forward, and backward regressions
ourmodel <- lm(Life.expectancy~Adult.Mortality+infant.deaths+under.five.deaths+Polio+Diphtheria, data = lifeExpectancyDataUpdated2)
anova(ourmodel)
summary(ourmodel)

k <- ols_step_all_possible(ourmodel)
k

#forward
f <- ols_step_forward_p(ourmodel, penter= 0.3, details = TRUE)

#backward
b <- ols_step_backward_p(ourmodel, penter = 0.3, details = TRUE)

#check model assumptions
qqnorm(residuals(ourmodel),ylab="Residuals",main="Q-Q plot")
qqline(residuals(ourmodel))
plot(ourmodel)

#Check for multicollinearity
vif(ourmodel)

#Removed infant.deaths to remove multicollinearity? And to obtain final model, also this had higher r squared adj value and minimized Cp (error)
finalmod <- lm(Life.expectancy~Adult.Mortality+under.five.deaths+Polio+Diphtheria, data = lifeExpectancyDataUpdated)
summary(finalmod)
anova(finalmod)

#LASSO method
x<-(apply(lifeExpectancyDataUpdated2[,-1],2,as.numeric))
y<-(apply(lifeExpectancyDataUpdated2[,1],2,as.numeric))
set.seed(43215)
#x prob
lasso_cv = cv.glmnet(x, y,family=c("gaussian"),standardize=TRUE,nfolds=10,lambda=seq(0,1,0.001,alpha = 1))

plot(lasso_cv)

(best_lambda = lasso_cv$lambda.min)
best_lasso = glmnet(x,y, standardize=TRUE,alpha = 1, lambda = best_lambda)
coef(best_lasso)
best_lasso$dev.ratio
