#Geography 411 – Spring 2026 
#Homework #4- Regression 
#Kalyn Wolters

#SAT Model

sat <- read.csv("SAT.csv")
sat

#making a scatterplot
plot(sat$SAT, sat$GPA, xlab = "SAT Score", ylab = "GPA")
satModel <- lm(GPA ~ SAT, data = sat)

summary(satModel)

#adding a regression line
plot(sat$SAT, sat$GPA, xlab = "SAT Score", ylab = "GPA")
abline(reg = satModel)

#Diagnostic Plots
plot(satModel)


#Retrieve important quantities
coefficients(satModel)
residuals(satModel)
fitted.values(satModel)

#Mosquito Model

mosquitos <- read.csv("mosquitos.csv")
summary(mosquitos)

#Estimate model and scatter plot
mosqModLinear <- lm(AegPupae ~ DevelopmentSites, data = mosquitos)
summary(mosqModLinear)

plot(mosquitos$DevelopmentSites, mosquitos$AegPupae)
abline(reg = mosqModLinear)

#Diagnostic plots
plot(mosqModLinear)

coefficients(mosqModLinear)
residuals(mosqModLinear)
fitted.values(mosqModLinear)

#Outlier Section

#Run the model without the outlier
mosqModLinear30 <- lm(AegPupae ~ DevelopmentSites, data = mosquitos[-30,])
summary(mosqModLinear30)

#Visualize the difference in models with and without the outlier
plot(mosquitos$DevelopmentSites, mosquitos$AegPupae)
points(mosquitos$DevelopmentSites[30], mosquitos$AegPupae[30], col = "red")
abline(reg = mosqModLinear)
abline(reg = mosqModLinear30, col = "red")

#Assess the diagnostic plots of the model without outlier
plot(mosqModLinear30)

#Log Model

hist(log(mosquitos$AegPupae))
mosqModLog <- lm(log(AegPupae) ~ DevelopmentSites, data = mosquitos)
summary(mosqModLog)
plot(mosquitos$DevelopmentSites, log(mosquitos$AegPupae))
abline(reg = mosqModLog)

#Diagnostic plots
plot(mosqModLog)


#Log-Log Model
hist(log(mosquitos$DevelopmentSites))

#Estimate the log-log model
mosqModLogLog <- lm(log(AegPupae) ~ log(DevelopmentSites), data = mosquitos)
summary(mosqModLogLog)

#Plot estimated model on scatter plot
plot(log(mosquitos$DevelopmentSites), log(mosquitos$AegPupae), xlab = "Developement Sites", ylab = "Pupae")
abline(reg = mosqModLogLog)

#Assess diagnostic plots 
plot(mosqModLogLog)

