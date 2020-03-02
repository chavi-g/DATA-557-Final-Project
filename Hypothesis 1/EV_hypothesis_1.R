## Hypothesis 1 

#This is an experiment to study whether there is a relationship between the electricity cost 
#and the sales of EV in the United States in 2017

#Analysis Plan:
# ● As both sales and costs are quantitative data, we will fit a linear regression model 
#   to predict whether there is an effect of cost on EV sales.  
# ● We will also perform a hypothesis test for the regression coefficient of ‘cost’ to test
#   whether it is zero, i.e. no significant difference in the sales due to electricity costs. 
#--------------------------------------------------------------------------------------------
library(ggplot2)

# Get the dataset --------------------------------------------------------------
input <- read.csv("Hypothesis_1_Dataset.csv")
population <- read.csv("US_State_Population_2018.csv")
# ------------------------------------------------------------------------------

# Summary statistic-------------------------------------------------------------
summary(input$Electricity_Price)
summary(input$EV_Registration)
summary(input$Group)
# ------------------------------------------------------------------------------

# Generate the histograms-------------------------------------------------------
qplot(input$Electricity_Price, 
      geom="histogram", 
      fill=I("brown"), 
      main="Electricity Price (cents/kWh) in 2017",
      xlab="Electricity Price",
      ylab="Count", binwidth=1)
# -----------------------------------------------------------------------------

 
# Plot the data ---------------------------------------------------------------
plot(input$Electricity_Price,
     input$EV_Registration,
     xlab = "Electricity Price", 
     ylab = "EV Registration",
     main="EV Registration and Electricity Price in 2017")
#------------------------------------------------------------------------------
# Plot the data ---------------------------------------------------------------
qqplot(input$Electricity_Price, 
       input$EV_Registration, 
       xlab = "Electricity Price", 
       ylab = "EV Registration")

# Plot the data for normality
fit=lm(EV_Registration ~ Electricity_Price, data = input)
qqnorm(fit$residuals)
qqline(fit$residuals)

#Plot the data ---------------------------------------------------------------
par(mfrow=c(2,2), mar=c(6,4,2,5))
plot(EV_by_Population ~ Electricity_Price, data = elPrice_EV_data)
plot(log(EV_by_Population) ~ Electricity_Price, data = elPrice_EV_data)
abline(fit2)
# -----------------------------------------------------------------------------

# Test effect of electricity price on the EV registration and the normalised EV registration
summary(lm(EV_Registration ~ Electricity_Price, data = elPrice_EV_data))
fit = lm(EV_by_Population ~ Electricity_Price, data = elPrice_EV_data)
summary(lm(EV_Registration ~ Electricity_Price + Population, data = elPrice_EV_data))
summary(fit)
plot(fit)

library(car)
bc = boxCox(fit, plotit = F)
bc$x[which.max(bc$y)]
fit2 = lm(log(EV_by_Population) ~ Electricity_Price, data = elPrice_EV_data)
summary(fit2)
plot(fit2)

glm.fit = glm(EV_Registration ~ Electricity_Price, data = elPrice_EV_data, family = "poisson")
anova(glm(EV_Registration ~ Electricity_Price, data = elPrice_EV_data, family = "poisson"), glm(EV_Registration ~ 1, data = elPrice_EV_data, family = "poisson"), test = "Chisq")
