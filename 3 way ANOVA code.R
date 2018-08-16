##########################################################
# 3 way ANOVA 
# Growth Chamber Experiment 
# data from: GCH_CO2.csv
#########################################################
rm(list=ls())
require(reshape)

CO2<-read.csv("GCH_CO2.csv", header=T)
summary(CO2)

CO2.m <- melt(CO2, id = c("Sample.ID", "rpf", "soil", "plants", "Hour"))

CO2.l <- as.data.frame(CO2.m)
colnames(CO2.l) <- c("ID", "rpf", "soil", "plant", "time", "week", "resp")
CO2.l$week <- as.numeric(gsub("Week.", "", CO2.l$week))

CO2.l$rpf <- as.factor(CO2.l$rpf)
CO2.l$soil <- as.factor(CO2.l$soil)
CO2.l$plant <- as.factor(CO2.l$plant)

#repeated measures ANOVA
ctrl <- lmeControl(opt="optim")
rmANOVA <-lme(resp ~ (rpf * week) + plant, random = ~ 1|ID, 
              + data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 1)], control=ctrl)
rmANOVA
summary(rmANOVA)

#effects of rpf on respiration per week
#rpf1= rpf+, rpf2=rpf - (control)
plot(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$rpf == 1), ], pch = 22, bg = "red")
points(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$rpf == 2), ], pch = 21, bg = "gray")

#effects of soil on respiration per week
# 1-sterile soil 2- live soil
plot(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 1), ], pch = 22, bg = "red")
points(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 2), ], pch = 21, bg = "gray")

#effects of plants/no plants on respiration per week
#1-plants 2-no plants
plot(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$plant == 1), ], pch = 22, bg = "red")
points(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$plant == 2), ], pch = 21, bg = "gray")
#rpf with soil 
plot(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 1 & 
                                               CO2.l$rpf == 1), ], pch = 22, bg = "red")
points(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 1 & 
                                          CO2.l$rpf == 2), ], pch = 21, bg = "gray")

#Rpf has an effect on sterile soil during the first week of the experiment
par(mar = c(5, 6, 3, 1))
# Start Plot
plot(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 1 & 
                                               CO2.l$rpf == 1), ], pch = 22, bg = "red",
     xlab = "", ylab = "", ylim = c(0, 1e5), yaxt = "n", las = 1)
# Add Second Points
points(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 1 & 
                                                 CO2.l$rpf == 2), ], pch = 21, bg = "gray")
mtext("Effects of Rpf with Sterile Soil", side = 3, cex = 1.5, line = 1)
mtext(expression(paste("CO"[2], " Respiration")), side = 2, line = 4, cex = 1.5)
mtext("Week", side = 1, line = 3, cex = 1.5)
legend("topright", c("rpf+", "rpf-"), pch = c(22, 21), pt.bg = c("red", "gray"), bty = "n")

 axis(2, at = c(0, 20000, 40000, 60000, 80000, 100000), 
      labels = c(0,expression("2" %*% "10"^4),
                 expression("4"%*% "10"^4),expression("6"%*% "10"^4),
                 expression("8"%*% "10"^4),expression("1" %*% "10"^5)),
      las = 1)
 box(lwd = 2)
