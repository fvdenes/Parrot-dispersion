# Seed dispersion Cacatuas #
library(survival)

cacatuas <- read.csv("C:/Users/voeroesd/Dropbox/EBD/Parrot seed dispersion/Distancias cacatuas.csv")
str(cacatuas)
summary(cacatuas)
cacatuas$Bird
levels(cacatuas$Bird)
levels(cacatuas$Plant)

# Compare dispersions of different plants by C. galerita and C. banksii
gale_banks <- cacatuas[-which(cacatuas$Bird=="Eolophus roseicapillus"),]
str(gale_banks)
gale_banks$Bird<-as.factor(as.character(gale_banks$Bird))
gale_banks$Plant<-as.factor(as.character(gale_banks$Plant))


## A plot with dispersion for both birds irrespective of plant
disp.both <- Surv(gale_banks$Distance, gale_banks$Min0Exact1)
disp.both

sfit.both <- survfit(formula = disp.both ~ 1)
sfit.both
str(sfit.both)

summary(sfit.both)

# identifying groups:
sfit <- survfit(formula =disp.both ~ gale_banks$Bird)
sfit
str(sfit)
summary(sfit)


plot(sfit,main="", xlab="Distance (m)", ylab="Dispersal probability", col=c("royalblue","azure4"), conf.int=T, mark.time=T,lwd=1.5,mark=16)
legend(500,0.8,legend=c(expression(italic("C. galerita")),  expression(italic("C. banksii"))),lty=c(1,1), col=c("royalblue","azure4"),lwd=1.5)


# estimate mean dispersal distances for each species separately
gale <- cacatuas[which(cacatuas$Bird=="Cacatua galerita"),]
banks <- cacatuas[which(cacatuas$Bird=="Calyptorhynchus banksii"),]

disp.gale <- Surv(gale$Distance, gale$Min0Exact1)
disp.banks <- Surv(banks$Distance, banks$Min0Exact1)

sfit.gale <- survfit(formula = disp.gale ~ 1)
sfit.banks <- survfit(formula = disp.banks ~ 1)

plot(sfit.gale,main="Kaplan-Meier estimate with 95% confidence bounds", xlab="Distance (m)", ylab="Dispersal function")
hist(gale$Distance,  breaks = seq(0,600,10), xlab="",main="", xlim=c(0,600),ylim=c(0,1000),col="grey")
print(sfit.gale,print.rmean=TRUE, rmean=560) # mean dispersal distance is so high because there are very few exact dispersal distances (represented in the plot where the curve shifts down on the Y-axis)

plot(sfit.banks,main="Kaplan-Meier estimate with 95% confidence bounds", xlab="Distance (m)", ylab="Dispersal function")
print(sfit.banks,print.rmean=TRUE, rmean="common")

# By plant species
table(gale_banks$Plant,gale_banks$Bird)

p1<-subset(gale,Plant=="Araucaria bidwillii")
p2<-subset(gale,Plant=="Archontophoenix alexandrae")
p3<-subset(gale,Plant=="Pinus radiata")

p4<-subset(banks,Plant=="sorgo")
p5<-subset(banks,Plant=="Terminalia")
p6<-subset(banks,Plant=="Terminalia catappa")
p7<-subset(banks,Plant=="tomatito")

### Testing differences between all plants:
# Cacatua galerita x Araucatia bidwillii

par(mfrow=c(3,2))
s1 <- Surv(p1$Distance,p1$Min0Exact1)
sfit1 <- survfit(formula=s1~1)
print(sfit1, print.rmean = TRUE, rmean="common")

plot(sfit1,main="Kaplan-Meier estimate with 95% confidence bounds", xlab="Distance (m)", ylab="Dispersal function")
text(130,0.8,labels=expression(italic("C. galerita - A. bidwillii")))
hist(p1$Distance,  breaks = seq(0,160,10), xlab="",main="", xlim=c(0,200),ylim=c(0,100),col="grey")

# Cacatua galerita x Archontophoenix alexandrae
s2 <- Surv(p2$Distance,p2$Min0Exact1)
sfit2 <- survfit(formula=s2~1)
print(sfit2, print.rmean = TRUE, rmean="common") # all distances are minimum dispersal distances, so mean is equal to the highest recorded distance
plot(sfit2,main="Kaplan-Meier estimate with 95% confidence bounds", xlab="Distance (m)", ylab="Dispersal function")
text(280,0.8,labels=expression(italic("C. galerita - A. alexandrae")))
hist(p2$Distance,  breaks = seq(0,350,10), xlab="",main="", xlim=c(0,300),ylim=c(0,1000),col="grey")

# Cacatua galerita x Pinus radiata
s3 <- Surv(p3$Distance,p3$Min0Exact1)
sfit3 <- survfit(formula=s3~1)
print(sfit3, print.rmean = TRUE, rmean="common")
plot(sfit3,main="Kaplan-Meier estimate with 95% confidence bounds", xlab="Distance (m)", ylab="Dispersal function")
text(450,0.4,labels=expression(italic("C. galerita - P. radiata")))
hist(p3$Distance,  breaks = seq(0,600,50), xlab="",main="", xlim=c(0,600),ylim=c(0,100),col="grey")

par(mfrow=c(4,2))
# Calyptorhynchus banksii x sorgo
s4 <- Surv(p4$Distance,p4$Min0Exact1)
sfit4 <- survfit(formula=s4~1)
print(sfit4, print.rmean = TRUE, rmean="common")
plot(sfit4,main="Kaplan-Meier estimate with 95% confidence bounds", xlab="Distance (m)", ylab="Dispersal function")
text(200,0.4,labels=expression(italic("C. banksii- Sorghum sp.")), lwd=1.5)
hist(p4$Distance,  breaks = seq(0,260,10), xlab="",main="", xlim=c(0,260),ylim=c(0,100),col="grey")

# Calyptorhynchus banksii x Terminalia
s5 <- Surv(p5$Distance,p5$Min0Exact1)
sfit5 <- survfit(formula=s5~1)
print(sfit5, print.rmean = TRUE, rmean="common")
plot(sfit5,main="Kaplan-Meier estimate with 95% confidence bounds", xlab="Distance (m)", ylab="Dispersal function")
text(45,0.8,labels=expression(italic("C. banksii - Terminalia sp.")))
hist(p5$Distance,  breaks = seq(0,50,10), xlab="",main="", xlim=c(0,50),ylim=c(0,100),col="grey")

# Calyptorhynchus banksii x Terminalia catappa
s6 <- Surv(p6$Distance,p6$Min0Exact1)
sfit6 <- survfit(formula=s6~1)
print(sfit6, print.rmean = TRUE, rmean="common")
plot(sfit6,main="Kaplan-Meier estimate with 95% confidence bounds", xlab="Distance (m)", ylab="Dispersal function")
text(315,0.8,labels=expression(italic("C. banksii - Terminalia catappa")))
hist(p6$Distance,  breaks = seq(0,350,35), xlab="",main="", xlim=c(0,350),ylim=c(0,300),col="grey")

# Calyptorhynchus banksii x tomatito
s7 <- Surv(p7$Distance,p7$Min0Exact1)
sfit7 <- survfit(formula=s7~1)
print(sfit7, print.rmean = TRUE, rmean="common")
plot(sfit7,main="Kaplan-Meier estimate with 95% confidence bounds", xlab="Distance (m)", ylab="Dispersal function")
text(105,0.8,labels=expression(italic("C. banksii - tomatito")))
hist(p7$Distance,  breaks = seq(0,120,10), xlab="",main="", xlim=c(0,120),ylim=c(0,100),col="grey")

# Mean dispersal distances



#### Plots ####
jpeg("Cgalerita_Cbanskii.jpg", width=10, height=7, units="in",res=300)


plot(sfit,main="", xlab="Distance (m)", ylab="Dispersal probability", col=c("royalblue","azure4"), conf.int=T, mark.time=T,lwd=1.5,mark=16)
legend(450,0.8,legend=c(expression(italic("C. galerita")),  expression(italic("C. banksii"))),lty=c(1,1), col=c("royalblue","azure4"),lwd=1.5)

dev.off()

jpeg("Cgalerita.jpg", width=10, height=7, units="in",res=300)
par(
  mfrow=c(3,2),
  mar=c(2,4,2,1),
  mgp=c(2,0.5,0)
)

plot(sfit1,main="", xlab="Distance (m)", ylab="Dispersal function")
text(130,0.8,labels=expression(italic("C. galerita - A. bidwillii")))
hist(p1$Distance,  breaks = seq(0,160,10), xlab="",main="", xlim=c(0,200),ylim=c(0,100),col="grey")

plot(sfit2,main="", xlab="Distance (m)", ylab="Dispersal function")
text(280,0.8,labels=expression(italic("C. galerita - A. alexandrae")))
hist(p2$Distance,  breaks = seq(0,350,10), xlab="",main="", xlim=c(0,300),ylim=c(0,1000),col="grey")

plot(sfit4,main="", xlab="Distance (m)", ylab="Dispersal function")
text(200,0.4,labels=expression(italic("C. banksii- Sorghum sp.")), lwd=1.5)
hist(p4$Distance,  breaks = seq(0,260,10), xlab="",main="", xlim=c(0,260),ylim=c(0,100),col="grey")

dev.off()


jpeg("Cbanksii.jpg", width=12, height=10, units="in",res=300)
par(
  mfrow=c(4,2),
  mar=c(2,5,2,1),
  mgp=c(2,0.5,0)
)

plot(sfit4,main="", xlab="Distance (m)", ylab="Dispersal function")
text(200,0.4,labels=expression(italic("C. banksii- Sorghum sp.")), lwd=1.5)
hist(p4$Distance,  breaks = seq(0,260,10), xlab="",main="", xlim=c(0,260),ylim=c(0,100),col="grey")

plot(sfit5,main="", xlab="Distance (m)", ylab="Dispersal function")
text(45,0.8,labels=expression(italic("C. banksii - Terminalia sp.")))
hist(p5$Distance,  breaks = seq(0,50,10), xlab="",main="", xlim=c(0,50),ylim=c(0,100),col="grey")

plot(sfit6,main="", xlab="Distance (m)", ylab="Dispersal function")
text(290,0.8,labels=expression(italic("C. banksii - Terminalia catappa")))
hist(p6$Distance,  breaks = seq(0,350,35), xlab="",main="", xlim=c(0,350),ylim=c(0,300),col="grey")

plot(sfit7,main="", xlab="Distance (m)", ylab="Dispersal function")
text(105,0.8,labels=expression(italic("C. banksii - tomatito")))
hist(p7$Distance,  breaks = seq(0,120,10), xlab="",main="", xlim=c(0,120),ylim=c(0,100),col="grey")

dev.off()