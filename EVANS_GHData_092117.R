GHData <- read_csv("GHData.csv")
View(GHData)
library(tidyr)
library(dplyr)
library(ggplot2)
#for faceted boxplots
GHData <- GHData%>%
  select(Treatment:LeafNumber)%>%
  gather("Attribute", "Measurement", Heightmm : LeafNumber)%>%
  mutate(Attribute = as.factor(Attribute),
         Species = ifelse(Species == "BB", "Blueberry", "Low Bush Cranberry"))

GHData$Treatment <- factor(GHData$Treatment, levels = c("UD", "SD", "MD", "Sterile")) 
GHData$Attribute <- factor(GHData$Attribute, labels = c("Height (mm)", "Leaf Number"))


### Boxplots
ggplot(data = GHData, aes(x = Treatment, y = (Measurement)))+
  geom_boxplot(outlier.alpha=0)+
  scale_y_log10() +
  facet_grid(Attribute~Species, scales="free")+
  geom_jitter(height = 0, width = 0.1, alpha =1, aes(col = Treatment))+
  theme(axis.title.y=element_blank())+
  theme(strip.background = element_blank())

###Statistics###
##BB
BlueberryTest=subset(GHData,Species=="Blueberry")
Bmodel=(aov(BlueberryTest$Measurement~BlueberryTest$Treatment))
summary(Bmodel)
#with log trans
BlueberryTest=subset(GHData,Species=="Blueberry")
TBmodel=(aov(log10(BlueberryTest$Measurement)~BlueberryTest$Treatment))
summary(TBmodel)
TukeyHSD(TBmodel)

##LBC
CranberryTest=subset(GHData,Species=="Low Bush Cranberry")
Cmodel=(aov(CranberryTest$Measurement~CranberryTest$Treatment))
summary(Cmodel)

#with log trans
CranberryTest=subset(GHData,Species=="Low Bush Cranberry")
TCmodel=(aov(log10(CranberryTest$Measurement)~CranberryTest$Treatment))
summary(TCmodel)
TukeyHSD(TCmodel)


