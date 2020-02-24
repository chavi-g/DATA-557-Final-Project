### Analysis for Washington counties

data <- read.csv("final_data_3.csv")

#Data Cleaning
data2 <- data[which(data$Unemployment.Rate!='#N/A'),]
data2$Count.of.sales <- as.numeric(as.character(data2$Count.of.sales))
data2$Gross.Domestic.Product..All.Industries <- as.numeric(as.character(data2$Gross.Domestic.Product..All.Industries))
data2$Unemployment.Rate <- as.numeric(as.character(data2$Unemployment.Rate))
data2$Mean.Real.Wages <- as.numeric(as.character(data2$Mean.Real.Wages))
data2$Per.Capita.Personal.Income <- as.numeric(as.character(data2$Per.Capita.Personal.Income))
write.csv(data2, "cleaned_data.csv")

cor(data2[,c(2,3,7,8,9)])

fit1 <- lm(Count.of.sales ~ Gross.Domestic.Product..All.Industries, data = data2)
summary(fit1)
plot(fit1)

fit2<- lm(Count.of.sales ~ Gross.Domestic.Product..All.Industries+Per.Capita.Personal.Income+Unemployment.Rate, data = data2)
summary(fit2)
plot(fit2)

fit3 <- lm(Count.of.sales ~ Per.Capita.Personal.Income+Unemployment.Rate, data = data2)
summary(fit3)
plot(fit3)

anova(fit2, fit1)


