#Greenhouse Data

#10/07/2017 Growth Measurements
growth_data<-read.csv("OctoberGrowth.csv")
growth_data
summary(growth_data)
str(growth_data)
summary(aov(growth_data$Leaf.Number~growth_data$Treatment))
anova(lm(growth_data$Leaf.Number~growth_data$Treatment))

