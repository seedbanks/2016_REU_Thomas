##########################################################
# 3 way ANOVA 
# Growth Chamber Experiment 
# data from: Documents< IU Biodiversity< GCH_CO2.csv
#########################################################
setwd("~/Documents/IU BioDiversity")
#clear environment
rm(list=ls())

CO2<-read.csv("GCH_CO2.csv", header=T)
summary(CO2)

#make variables factors
rpf=factor(CO2$rpf)
rpf<-factor(rpf, labels=c("rpf+", "rpf-"))
> is.factor(rpf)
[1] TRUE
> levels(rpf)
[1] "rpf+" "rpf-"

soil=factor(CO2$soil)
soil<-factor(soil, labels=c("sterile", "live"))
soil
is.factor(soil)
levels(soil)

plant=factor(CO2$plant)
plant<-factor(plant, labels=c("plant", "without plant"))
plant
is.factor(plant)
levels(plant)

#While I couldn't do an ANOVA for all weeks yet, I wanted to at least look at them individually
#ran 3-way ANOVA for each week seperately

week.1aov<- aov(week.1~rpf*soil*plant, data=CO2)
summary(week.1aov)
#week.1 shows only soil and soil:plant interaction are significant (<0.05)
week.2aov<- aov(week.2~rpf*soil*plant, data=CO2)
> summary(week.2aov)
#week.2 shows only soil as significant
week.3aov<- aov(week.3~rpf*soil*plant, data=CO2)
> summary(week.3aov)
#soil and plant are significant
week.4aov<- aov(week.4~rpf*soil*plant, data=CO2)
> summary(week.4aov)
# rpf:soil:plant interactions are significant
week.5aov<- aov(week.5~rpf*soil*plant, data=CO2)
> summary(week.5aov)
#plant is significant