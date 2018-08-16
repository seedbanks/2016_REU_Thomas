################################################################################
#                                                                              #
#  Bacterial Respiration Microcosm Experiment Anaylsis                         #
#  Written By:                                                                 #
#  Last Update: 06 June 2016                                                   #
#                                                                              #
#  Use this file to determine CO2 concentration based on IRGA output           #
#                                                                              #
#  Future Changes:                                                             #
#      1. Make analysis automated similar to PreSens workflow                  #
#                                                                              #
################################################################################

setwd('~/Dropbox/Greenhouse Effects Experiment/analyses')
rm(list=ls())

library(data.table)
library(plyr)

################################################################################

CO2Reading <- read.csv("../data/CO2RespirationAllData.csv")
CO2Reading$RPF.Concentration <- factor(CO2Reading$RPF.Concentration)
CO2Reading <- na.omit(CO2Reading)
attach(CO2Reading)
View(CO2Reading)

dat <- cbind(CO2Reading[,2], CO2Reading[,6])

dat.1 <- cbind(CO2Reading[,5], CO2Reading[,6])

RpfCO2 <- ddply(CO2Reading, .(Time), summarize, mean=mean(Expected.CO2.ppm.peaks))
RpfCO2
subset(RpfCO2, factor=="24", select="mean")

anova(lm(Time~Expected.CO2.ppm.peaks, data=CO2Reading))
anova(lm(RPF.Concentration~Expected.CO2.ppm.peaks, data=CO2Reading))

boxplot(RPF.Concentration~Expected.CO2.ppm.peaks, data=CO2Reading, 
        main="Rpf effect on CO2 Level",
        xlab="RPF Concentration", ylab="Expected CO2 (ppm)")








