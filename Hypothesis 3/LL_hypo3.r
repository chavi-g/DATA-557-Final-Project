# EV Project
# Hypothesis 3

mydata <- read.csv("cleaned_data_v2.csv")
head(mydata)

# This data is normalized and transformed, below are the fields:
# - County
# - County.of.sales
# - Gross.Domestic.Product
# - Mean.Real.Wages
# - Per.Capita.Personal.Income
# - Unemployment.Rate
# - Population
# - Sales.by.Population
# - GDP.by.Population
# - Sales.by.Personal.Income

#------------------------------------------------------------------------------
# Perform data exploration
pairs(mydata)
plot(x= mydata$Count.of.sales, y=mydata$Gross.Domestic.Product)
plot(x= mydata$Count.of.sales, y=mydata$Population)
plot(x= mydata$Count.of.sales, y=mydata$Sales.by.Personal.Income)
plot(x= mydata$Population, y=mydata$Sales.by.Personal.Income)
# We can see the strong relationship between: 
# - Count.of.sales AND GDP
# - Count.of.sales AND Population
# - Count.of.sales AND Sales.by.Personal.Income
# - Population AND Sales.by.Personal.Income

# Check normality
qqplot(mydata$Count.of.sales, mydata$Gross.Domestic.Product)      # linear
qqplot(mydata$Count.of.sales, mydata$Sales.by.Population)
qqplot(mydata$Count.of.sales, mydata$Mean.Real.Wages)
qqplot(mydata$Count.of.sales, mydata$Per.Capita.Personal.Income)
qqplot(mydata$Count.of.sales, mydata$Unemployment.Rate)
qqplot(mydata$Count.of.sales, mydata$GDP.by.Population)
qqplot(mydata$Gross.Domestic.Product, mydata$Sales.by.Population)
qqplot(mydata$Population, mydata$Sales.by.Personal.Income)        # linear
# We can see that both [Mean.Real.Wages] and [Unemployment.Rate] are not the best choice for data normality
# The normality test showing potential linearity:
# - [Count.of.sales] and [GDP]
# - [population] and [Sales.by.Personal.Income]

# Check correlation
cor(mydata$Count.of.sales, mydata$Per.Capita.Personal.Income)           # 0.7417717 is high
cor(mydata$Count.of.sales, mydata$Population)                           # 0.9442727 is high
cor(mydata$Count.of.sales, mydata$Mean.Real.Wages)                      # 0.4049112
cor(mydata$Count.of.sales, mydata$Unemployment.Rate)                    # -0.3570404
cor(mydata$Count.of.sales, mydata$Gross.Domestic.Product)               # 0.9916383 is high
cor(mydata$Count.of.sales, mydata$Sales.by.Population)                  # 0.4962921
cor(mydata$Count.of.sales, mydata$GDP.by.Population)                    # 0.5548067
cor(mydata$Gross.Domestic.Product, mydata$Per.Capita.Personal.Income)   # 0.7140942 is high
cor(mydata$Gross.Domestic.Product, mydata$Population)                   # 0.9366266 is high
# Based on the correlation scores, we can see there is a strong relationship
# - [Count.of.sales] and [Per.Capita.Personal.Income]
# - [Count.of.sales] and [Population]
# - [Count.of.sales] and [GDP]
# - [GDP] and [Per.Capita.Personal.Income]
# - [GDP] and [Population]
# Therefore, we these will be the good predictors for the EV sales model
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 1 predictor / [Count.of.sales] >>> [GDP]
model_1 <- lm(Count.of.sales ~ Gross.Domestic.Product, data = mydata)
summary(model_1)
plot(model_1)
# R-squared is high but this does not mean the model is always a good model
# R-squared:  0.9833
# p-value:    2.2e-16
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 1 predictor / [Count.of.sales] >>> [Per.Capita.Personal.Income]
model_2 <- lm(Count.of.sales ~ Per.Capita.Personal.Income, data = mydata)
summary(model_2)
plot(model_2)
# This is a good model
# R-squared:  0.5502
# p-value:    6.552e-08
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 1 predictor / [Count.of.sales] >>> [Population]
model_3 <- lm(Count.of.sales ~ Population, data = mydata)
summary(model_3)
plot(model_3)
# R-squared is high but this does not mean the model is always a good model
# R-squared:  0.8917
# p-value:    2.2e-16
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 1 predictor / [Count.of.sales] >>> [GDP.by.Population]   
model_4 <- lm(Count.of.sales ~ GDP.by.Population, data = mydata)
summary(model_4)
plot(model_4)
# This is a good model
# R-squared:  0.3078
# p-value:    0.000247
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 1 predictor / [Count.of.sales] >>> [Sales.by.Population]   
model_5 <- lm(Count.of.sales ~ Sales.by.Population, data = mydata)
summary(model_5)
plot(model_5)
# This is a good model
# R-squared:  0.2463
# p-value:    0.001312
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 2 predictors / [Count.of.sales] >>> [GDP + Per.Capital.Personal.Income]
model_6 <- lm(Count.of.sales ~ Gross.Domestic.Product + Per.Capita.Personal.Income, data = mydata)
summary(model_6)
plot(model_6)
# R-squared is high but this does not mean the model is always a good model
# R-squared:  0.9857
# p-value:    2.2e-16
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 2 predictors / [Count.of.sales] and [GDP + Population]
model_7 <- lm(Count.of.sales ~ Gross.Domestic.Product + Population, data = mydata)
summary(model_7)
plot(model_7)
# R-squared is high but this does not mean the model is always a good model
# R-squared:  0.9853
# p-value:    2.2e-16
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 2 predictors / [Count.of.sales] >>> [Per.Capita.Personal.Income + Population]
model_8 <- lm(Count.of.sales ~ Per.Capita.Personal.Income + Population, data = mydata)
summary(model_8)
plot(model_8)
# R-squared is high but this does not mean the model is always a good model
# R-squared:  0.9059
# p-value:    2.2e-16
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 2 predictor / [Count.of.sales] >>> [GDP.by.Population + Sales.by.Population]
model_9 <- lm(Count.of.sales ~ GDP.by.Population + Sales.by.Population, data = mydata)
summary(model_9)
plot(model_9)
# This is a good model
# R-squared:  0.4807
# p-value:    7.533e-06
#------------------------------------------------------------------------------

# After testing nine different models, we can see there are four good models
# that yield the best linear regression model:
# 1 predictor / [Count.of.sales] >>> [Per.Capita.Personal.Income]
# 1 predictor / [Count.of.sales] >>> [GDP.by.Population]  
# 1 predictor / [Count.of.sales] >>> [Sales.by.Population]  
# 2 predictor / [Count.of.sales] >>> [GDP.by.Population + Sales.by.Population]

# Perhaph, we can try different technique with random model, or glm, or something else...
# but there is doubt they will get any better since the sample size is too small to yield any valuable insights / effects.
# Maybe ... we can try to use some descriptive statistic to visualize the EV and its indicators similar like this visualization
# as a talking point to add more narative to the presentation and report in supporting with our main statistical tests.
# https://towardsdatascience.com/how-to-make-beautiful-small-multiple-us-maps-in-r-ad7e557cd463

# Couple points i think that will be helpful in performing the EV sale experiment
# that we should consider for future study beyond our current limitation:
# Point 1:
# - EV car is cheap on the electricity price but expensive on the MSRP price
# - California and Washington state are the top two states with highest EV registration
#   in the state. What are the common characteristic between these two states: 
#   CA is the silicon valley in the South where we have large tech company and big corporation while
#   WA is also the silicon valley in the Pacific Northwest with many big tech corporation.
#   Perhaps, we should check if the salary income of the tech workers has any contribution/effect in buying the EVs
# - We currently dont have the salary of the tech workers in this experiment. This is our limitation
#
# Point 2:
# - We should consider to compare the gas price in the test. As electricity price does not change much 
#   but the gas price changes seasonality (ex gas price is higher in summer). Is there any correlation
#   of EV sales surge in the summer or when the gas price increases?
#   
# Point 3:
# - Since our datasets are small for all three hypothesis. It is tough to find a good statistical model
#   to perform the testing due to small sample size.
# - There are not much historical data that provides a whole comprehensive EV detail for all 50 states
#   and all counties within each state. This is our limitation. 


# Good read
# https://theicct.org/sites/default/files/publications/ICCT_EV_surge_US_cities_20190610.pdf









