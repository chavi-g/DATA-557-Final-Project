* This is an experiment to study whether there is a relationship between the electricity cost and the sales of EV in the United States in 2017 *
•	Null hypothesis: 
There is no difference of the electricity price (group A and B) on the effect of EV sales 
•	Alternative hypothesis: 
There is difference of the electricity price (group A and B) on the effect of EV sales 

## Analysis Plan:
•	Based on the historical data of state electricity profiles across 50 States (+1 District of Columbia) for the past 7 years, there is a clear distinction of two group electricity prices (cents/kWh) across US. Therefore, we transform the data to separate it into two groups: A & B for our experiment.
o	Group A: electricity price less than 10 cents per kWh
o	Group B: electricity price more than 10 cents per kWh
•	As both sales and costs are quantitative data, we will fit a linear regression model 
to predict whether there is an effect of cost on EV sales.  
•	We will also perform a hypothesis test for the regression coefficient of ‘cost’ to test whether it is zero, i.e. no significant difference in the sales due to electricity costs
