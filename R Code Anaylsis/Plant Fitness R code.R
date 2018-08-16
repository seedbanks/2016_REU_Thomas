#######################
# Plant Fitness Analysis
#data: Plant Fitness.csv
########################
rm(list=ls())

plantfitness<-read.csv("Plant Fitness.csv", header=T)

plantfitness$rpf <- factor(plantfitness$rpf, levels=c(2,1), labels=c("rpf-", "rpf+"))
plantfitness$soil <- factor(plantfitness$soil, levels=c(2,1), labels=c("complex", "simple"))
summary(plantfitness)

###################################################
#Testing each variable independently 

abiomass<-aov(abiomass~rpf*soil, data=plantfitness)
abiomass
summary(abiomass)
TukeyHSD(abiomass)
#soil is significant at 0.000125, along with two interactions

bbiomass<-aov(bbiomass~rpf*soil, data=plantfitness)
bbiomass
summary(bbiomass)
TukeyHSD(bbiomass)
#rpf is significant at 0.0168, two interactions

flowercount<-aov(flower.count~rpf*soil, data=plantfitness)
flowercount
summary(flowercount)
TukeyHSD(flowercount)
#There is no significant effects or interactions on flower count

seedcount<-aov(seed.count~rpf*soil, data=plantfitness)
seedcount
summary(seedcount)
TukeyHSD(seedcount)
#no significant factors or interactions 

SLA<-aov(SLA~rpf*soil, data=plantfitness)
SLA
summary(SLA)
TukeyHSD(SLA)
#no significant factors or interactions

height<-aov(height~rpf*soil, data=plantfitness)
height
summary(height)
TukeyHSD(height)
#no significant factors or interactions

dweight<-aov(dweight~rpf*soil, data=plantfitness)
dweight
summary(dweight)
TukeyHSD(dweight)
#no significant factors or interactions

wweight<-aov(wweight~rpf*soil, data=plantfitness)
wweight
summary(wweight)
TukeyHSD(wweight)
#rpf and soil are significant, as well as two interactions

shoot.root<-aov(shoot.root~rpf*soil, data=plantfitness)
shoot.root
summary(shoot.root)
TukeyHSD(shoot.root)
#soil is significant

total.biomass<-aov(total.biomass~rpf*soil, data=plantfitness)
total.biomass
summary(total.biomass)
TukeyHSD(total.biomass)
#rpf is significant at 0.00286, with two interactions

anova<-aov(abiomass~rpf*soil*bbiomass*wweight, data=plantfitness)
TukeyHSD(anova)
#soil was significant at 0.000926, and some interactions

anova2<-aov(bbiomass~rpf*soil*abiomass*wweight, data=plantfitness)
summary(anova2)
TukeyHSD(anova2)
#rpf is significant at 0.0474, but no interactions

anova3<-aov(wweight~rpf*soil*abiomass*bbiomass, data=plantfitness)
summary(anova3)
TukeyHSD(anova3)
#rpf and soil are significant at 0.01304 and 0.00221 respectively, two sig interactions

################################################################
#boxplot of Total Biomass
boxplot(total.biomass~soil*rpf, data=plantfitness, notch=FALSE, ylab="", xlab="", 
        col=c("blue", "gray"), ylim = c(0, 2.5), yaxt = "n", las = 1, names=c("", "", "", ""))
mtext("Total Biomass of Plants", side = 3, cex = 1.5, line = 1)
mtext("Biomass (g)", side = 2, line = 4, cex = 1.5)
mtext("Rpf-                                     Rpf+", side = 1, line = 1, cex = 1)
mtext("Treatment", side = 1, line = 3, cex = 1.5)
legend("bottomright", title="Microbial Community Type", c("Simple", "Complex"), fill=c("blue", "gray"), 
       horiz=TRUE, cex=0.8, bty = "n")
box(lwd = 2)
axis(2, at = c(0, 0.5, 1.0, 1.5, 2.0, 2.5), 
     labels = c(0, 0.5, 1.0, 1.5, 2.0, 2.5),
     las = 1)


#is Total biomass different between sterile and live soil?
var.test(total.biomass~soil, data=plantfitness)
#variance is equal becuase p is greater than 0,05
t.test(total.biomass~soil, data=plantfitness, var.equal=T)
#no the groups aren't different

#is total biomass different between +/- rpf treatments?
var.test(total.biomass~rpf, data=plantfitness)
#p-value is greater than 0.05, therefore the variances are equal
t.test(total.biomass~rpf, data=plantfitness, var.equal=T)
#p-value less than 0.05, therefore the groups are different

######boxplot of Above Ground Biomass
boxplot(abiomass~soil*rpf, data=plantfitness, notch=FALSE, ylab="", xlab="", 
        col=c("green", "gray"), ylim = c(0, 2.5), yaxt = "n", las = 1, names=c("", "", "", ""))
mtext("Aboveground Biomass of Plants", side = 3, cex = 1.5, line = 1)
mtext("Biomass (g)", side = 2, line = 4, cex = 1.5)
mtext("Rpf-                                     Rpf+", side = 1, line = 1, cex = 1)
mtext("Treatment", side = 1, line = 3, cex = 1.5)
legend("topleft", title="Microbial Community Type", c("Simple", "Complex"), fill=c("green", "gray"), horiz=TRUE, cex=0.8, bty = "n")
box(lwd = 2)
axis(2, at = c(0, 0.5, 1.0, 1.5, 2.0, 2.5), 
     labels = c(0, 0.5, 1.0, 1.5, 2.0, 2.5),
     las = 1)


#is above biomass different with +/- treatments?
#NO its not
var.test(abiomass~rpf, data=plantfitness)
#p-value over 0.05,variances are equal 
t.test(abiomass~rpf, data=plantfitness, var.equal=T)
#no, the groups are not different

#is above biomass different between soil type?
#YES
var.test(abiomass~soil, data=plantfitness)
#var are equal
t.test(abiomass~soil, data=plantfitness, var.equal=T)
#p-value<<0.05, groups are different. Above ground biomass is sig higher than live groups





######boxplot of Below Ground Biomass
boxplot(bbiomass~soil*rpf, data=plantfitness, notch=FALSE, ylab="", xlab="", 
        col=c("brown", "gray"), ylim = c(0, 1.5), yaxt = "n", las = 1, names=c("", "", "", ""))
mtext("Belowground Biomass of Plants", side = 3, cex = 1.5, line = 1)
mtext("Biomass (g)", side = 2, line = 4, cex = 1.5)
mtext("Rpf-                                     Rpf+", side = 1, line = 1, cex = 1)
mtext("Treatment", side = 1, line = 3, cex = 1.5)
legend("topleft", title="Microbial Community Type", c("Simple", "Complex"), 
       fill=c("brown", "gray"), horiz=TRUE, cex=0.8, bty = "n")
box(lwd = 2)
axis(2, at = c(0, 0.5, 1.0, 1.5), 
     labels = c(0, 0.5, 1.0, 1.5),
     las = 1)



#is below ground biomass different with +/- treatments?
#Yes 
var.test(bbiomass~rpf, data=plantfitness)
#p-value over 0.05,variances are not equal 
t.test(bbiomass~rpf, data=plantfitness, var.equal=F)
#yes, the groups are different p-value-0.02191 and rpf- has greater below ground biomass

#is below ground different with soil type?
#NO
var.test(bbiomass~soil, data=plantfitness)
#no, variances are not equal
t.test(bbiomass~soil, data=plantfitness, var.equal=F)
#no, there is no difference between groups



#######boxplot of wet weight
boxplot(wweight~soil*rpf, data=plantfitness, notch=FALSE, ylab="", xlab="", 
        col=c("yellow", "gray"), ylim = c(0, 0.75), yaxt = "n", las = 1, names=c("", "", "", ""))
mtext("Weight of Leaves", side = 3, cex = 1.5, line = 1)
mtext("Weight (g)", side = 2, line = 4, cex = 1.5)
mtext("Rpf-                                     Rpf+", side = 1, line = 1, cex = 1)
mtext("Treatment", side = 1, line = 3, cex = 1.5)
legend("topright", title="Microbial Community Type", c("Simple", "Complex"), fill=c("yellow", "gray"), horiz=TRUE, cex=0.8, bty="n")
box(lwd = 2)
axis(2, at = c(0, 0.25, 0.5, 0.75), 
     labels = c(0, 0.25, 0.5, 0.75),
     las = 1)


#is rpf treatment different between groups?
#YES
var.test(wweight~rpf, data=plantfitness)
#yes variances are equal
t.test(wweight~rpf, data=plantfitness, var.equal=T)
#yes, there is a sig differences 0.03708, rpf- has greater weight

#is soil type different between groups?
#YES
var.test(wweight~soil, data=plantfitness)
#p-value above 0.05, yes variances are equal 
t.test(wweight~soil, data=plantfitness, var.equal=T)
#yes, sterile samples had a higher leaf weight than live