############################
#Growth Chamber Experiment Graphs
#Peyton Thomas
# data used: GCH_CO2.l
############################



##################################################################################################
#SOIL EFFECTS 
#is there a difference in effects between types of soil?
# Rpf+ with Sterile and Live soil
# Yes, Sterile soil has higher resp than live significantly over the first 3 weeks
par(mar = c(5, 6, 3, 1))
plot(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 1 & 
                                               CO2.l$rpf == 1), ], pch = 22, bg = "red",
     xlab = "", ylab = "", ylim = c(0, 1e5), yaxt = "n", las = 1, cex = 1.5)

mtext("Effects of Rpf+ with Soil Type", side = 3, cex = 1.5, line = 1)
mtext(expression(paste("CO"[2], " Respiration")), side = 2, line = 4, cex = 1.5)
mtext("Week", side = 1, line = 3, cex = 1.5)


axis(2, at = c(0, 20000, 40000, 60000, 80000, 100000), 
     labels = c(0,expression("2" %*% "10"^4),
                expression("4"%*% "10"^4),expression("6"%*% "10"^4),
                expression("8"%*% "10"^4),expression("1" %*% "10"^5)),
     las = 1)
box(lwd = 2)
legend("topright", c("Sterile", "Live"), pch = c(22, 21), pt.bg = c("red", "gray"), bty = "n")

# Add Second Points
points(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 2 & 
                                                 CO2.l$rpf == 1), ], pch = 21, bg = "gray", cex = 1.5)



#Is there a significant difference between +/- treatments in sterile soil?
#Effects of Rpf+/- with Sterile Soil
#Yes, Rpf+ has an effect that is different from Rpf-, only in the first week
par(mar = c(5, 6, 3, 1))
plot(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 1 & 
                                               CO2.l$rpf == 1), ], pch = 22, bg = "red",
     xlab = "", ylab = "", ylim = c(0, 1e5), yaxt = "n", las = 1, cex=1.5)
# Add Second Points
points(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 1 & 
                                                 CO2.l$rpf == 2), ], pch = 21, bg = "gray", cex=1.5)
mtext("Effects of Rpf with Simple ", side = 3, cex = 1.5, line = 1)
mtext(expression(paste("CO"[2], " Respiration")), side = 2, line = 4, cex = 1.5)
mtext("Week", side = 1, line = 3, cex = 1.5)
legend("topright", c("Rpf+", "Rpf-"), pch = c(22, 21), pt.bg = c("red", "gray"), bty = "n")

axis(2, at = c(0, 20000, 40000, 60000, 80000, 100000), 
     labels = c(0,expression("2" %*% "10"^4),
                expression("4"%*% "10"^4),expression("6"%*% "10"^4),
                expression("8"%*% "10"^4),expression("1" %*% "10"^5)),
     las = 1)
box(lwd = 2)

var.test(resp~soil, data=CO2.l)
t.test(resp~soil, data=CO2.l, var.equal=F)
#Does Rpf significantly increase CO2 resp with live soil?
#No, there is no difference between +/- treatments with live soil
par(mar = c(5, 6, 3, 1))
plot(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 2 & 
                                               CO2.l$rpf == 1), ], pch = 22, bg = "blue",
     xlab = "", ylab = "", ylim = c(0, 1e5), yaxt = "n", las = 1)
# Add Second Points
points(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 2 & 
                                                 CO2.l$rpf == 2), ], pch = 21, bg = "gray")
mtext("Effects of Rpf with Live", side = 3, cex = 1.5, line = 1)
mtext(expression(paste("CO"[2], " Respiration")), side = 2, line = 4, cex = 1.5)
mtext("Week", side = 1, line = 3, cex = 1.5)
legend("topright", c("Rpf+", "Rpf-"), pch = c(22, 21), pt.bg = c("blue", "gray"), bty = "n")

axis(2, at = c(0, 20000, 40000, 60000, 80000, 100000), 
     labels = c(0,expression("2" %*% "10"^4),
                expression("4"%*% "10"^4),expression("6"%*% "10"^4),
                expression("8"%*% "10"^4),expression("1" %*% "10"^5)),
     las = 1)
box(lwd = 2)

#What does the rpf+ sterile and rpf+ live look like together?
#Sterile is still higher, and you can see that Rpf+ does have an effect on sterile
par(mar = c(5, 6, 3, 1))
plot(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 1 & 
                                               CO2.l$rpf == 2), ], pch = 22, bg = "red",
     xlab = "", ylab = "", ylim = c(0, 1e5), yaxt = "n", las = 1)
# Add Second Points
points(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$soil == 2 & 
                                                 CO2.l$rpf == 2), ], pch = 21, bg = "gray")
mtext("Rpf- with sterile and live soil", side = 3, cex = 1.5, line = 1)
mtext(expression(paste("CO"[2], " Respiration")), side = 2, line = 4, cex = 1.5)
mtext("Week", side = 1, line = 3, cex = 1.5)
legend("topright", c("Sterile", "Live"), pch = c(22, 21), pt.bg = c("red", "gray"), bty = "n")

axis(2, at = c(0, 20000, 40000, 60000, 80000, 100000), 
     labels = c(0,expression("2" %*% "10"^4),
                expression("4"%*% "10"^4),expression("6"%*% "10"^4),
                expression("8"%*% "10"^4),expression("1" %*% "10"^5)),
     las = 1)
box(lwd = 2)











#############################################################################################
#PLANT EFFECTS
#Is there a difference in effects between plants and no plants?
#No, there is no significant difference between plants and no plants
par(mar = c(5, 6, 3, 1))
plot(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$plant == 1 & 
                                               CO2.l$rpf == 1), ], pch = 22, bg = "green",
     xlab = "", ylab = "", ylim = c(0, 1e5), yaxt = "n", las = 1)
# Add Second Points
points(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$plant == 2 & 
                                                 CO2.l$rpf == 1), ], pch = 21, bg = "orange")
mtext("Effects of Rpf with and without Plants", side = 3, cex = 1.5, line = 1)
mtext(expression(paste("CO"[2], " Respiration")), side = 2, line = 4, cex = 1.5)
mtext("Week", side = 1, line = 3, cex = 1.5)
legend("topright", c("Plants", "No plants"), pch = c(22, 21), pt.bg = c("green", "orange"), bty = "n")

axis(2, at = c(0, 20000, 40000, 60000, 80000, 100000), 
     labels = c(0,expression("2" %*% "10"^4),
                expression("4"%*% "10"^4),expression("6"%*% "10"^4),
                expression("8"%*% "10"^4),expression("1" %*% "10"^5)),
     las = 1)
box(lwd = 2)

#Is there an effect with plants between +/- treatments?
# The first week, Rpf+ had an effect
par(mar = c(5, 6, 3, 1))
plot(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$plant == 1 & 
                                               CO2.l$rpf == 1), ], pch = 22, bg = "green",
     xlab = "", ylab = "", ylim = c(0, 1e5), yaxt = "n", las = 1)
# Add Second Points
points(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$plant == 1 & 
                                                 CO2.l$rpf == 2), ], pch = 21, bg = "orange")
mtext("Effects of Rpf with  Plants", side = 3, cex = 1.5, line = 1)
mtext(expression(paste("CO"[2], " Respiration")), side = 2, line = 4, cex = 1.5)
mtext("Week", side = 1, line = 3, cex = 1.5)
legend("topright", c("Rpf+", "Rpf-"), pch = c(22, 21), pt.bg = c("green", "orange"), bty = "n")

axis(2, at = c(0, 20000, 40000, 60000, 80000, 100000), 
     labels = c(0,expression("2" %*% "10"^4),
                expression("4"%*% "10"^4),expression("6"%*% "10"^4),
                expression("8"%*% "10"^4),expression("1" %*% "10"^5)),
     las = 1)
box(lwd = 2)

#Is there an effect without plants between +/- treatments?
#No, there is no difference in effects
par(mar = c(5, 6, 3, 1))
plot(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$plant == 2 & 
                                               CO2.l$rpf == 1), ], pch = 22, bg = "green",
     xlab = "", ylab = "", ylim = c(0, 1e5), yaxt = "n", las = 1)
# Add Second Points
points(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & CO2.l$plant == 2 & 
                                                 CO2.l$rpf == 2), ], pch = 21, bg = "orange")
mtext("Effects of Rpf without Plants", side = 3, cex = 1.5, line = 1)
mtext(expression(paste("CO"[2], " Respiration")), side = 2, line = 4, cex = 1.5)
mtext("Week", side = 1, line = 3, cex = 1.5)
legend("topright", c("Rpf+", "Rpf-"), pch = c(22, 21), pt.bg = c("green", "orange"), bty = "n")

axis(2, at = c(0, 20000, 40000, 60000, 80000, 100000), 
     labels = c(0,expression("2" %*% "10"^4),
                expression("4"%*% "10"^4),expression("6"%*% "10"^4),
                expression("8"%*% "10"^4),expression("1" %*% "10"^5)),
     las = 1)
box(lwd = 2)

##################################################################################
#Effects of Rpf throughout time
#Which is more reliable, 24 or 48 hour readings?
#48 hour readings show a more stable trend.
par(mar = c(5, 6, 3, 1))
plot(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 24 &
                                               CO2.l$rpf == 1), ], pch = 22, bg = "green",
     xlab = "", ylab = "", ylim = c(0, 1e5), yaxt = "n", las = 1)
# Add Second Points
points(resp ~ jitter(week), data = CO2.l[which(CO2.l$time == 48 & 
                                                 CO2.l$rpf == 1), ], pch = 21, bg = "orange")
mtext("Effects of Rpf on time", side = 3, cex = 1.5, line = 1)
mtext(expression(paste("CO"[2], " Respiration")), side = 2, line = 4, cex = 1.5)
mtext("Week", side = 1, line = 3, cex = 1.5)
legend("topright", c("24", "48"), pch = c(22, 21), pt.bg = c("green", "orange"), bty = "n")

axis(2, at = c(0, 20000, 40000, 60000, 80000, 100000), 
     labels = c(0,expression("2" %*% "10"^4),
                expression("4"%*% "10"^4),expression("6"%*% "10"^4),
                expression("8"%*% "10"^4),expression("1" %*% "10"^5)),
     las = 1)
box(lwd = 2)

################
#T-Tests
#does Rpf+/- have signifcant effect on respiration
var.test(resp~rpf, data=CO2.l)
#variances are not equal, p less than 0.05
t.test(resp~rpf, data=CO2.l, var.equal=F)
#No, there is no variance 


#does soil have sig effect on resp?
var.test(resp~soil, data=CO2.l)
#var are not equal
t.test(resp~soil, data=CO2.l, var.equal=F)
#yes, sterile soil is shown to be significantly higher in resp than live

#do plants have sig effect on resp?
var.test(resp~plant, data=CO2.l)
#var are equal
t.test(resp~plant, data=CO2.l, var.equal=T)
#no, they are no significantly different. Plants have no effect on CO2 resp