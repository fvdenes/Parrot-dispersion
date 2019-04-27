# Seed dispersion Anodorhynchus #
library(survival)

#guacas <- read.csv("~/Dropbox/Pos-doc/Parrot seed dispersion/Distancias guacas_3.csv")
guacas <- read.csv("C:/Users/voeroesd/Dropbox/EBD/Parrot seed dispersion/Distancias guacas_3.csv")
guacas <- read.csv("~/Dropbox/EBD/Parrot seed dispersion/Distancias guacas_3.csv")
colnames(guacas)[3]<-"Plant"
colnames(guacas)[4]<-"Distance"
str(guacas)
summary(guacas)

guacas$Ave
levels(guacas$Ave)

#create Surv object with right-truncation for each species: ####
hya_primary<- guacas[which(guacas$Ave=="A.hyacinthinus"&guacas$Primary1Secondary2==1),]
str(hya_primary)
disp.hya.primary <- Surv(hya_primary$Distance, hya_primary$Min0Exact1)
disp.hya.primary

hya_secondary<- guacas[which(guacas$Ave=="A.hyacinthinus"&guacas$Primary1Secondary2==2),]
str(hya_secondary)
disp.hya.secondary <- Surv(hya_secondary$Distance, hya_secondary$Min0Exact1)
disp.hya.secondary

lear <- guacas[which(guacas$Ave=="A.leari"),]
disp.lear <- Surv(lear$Distance, lear$Min0Exact1)
disp.lear

disp.both <- Surv(guacas$Distance, guacas$Min0Exact1)
disp.both

# Generate the Kaplan-Meier estimate of the survival function for A. hyacinthinus, primary dispersions: ####
sfit.hya.primary <- survfit(formula = disp.hya.primary ~ 1)
sfit.hya.primary
str(sfit.hya.primary)

summary(sfit.hya.primary)

plot(sfit.hya.primary,main="Kaplan-Meier estimate with 95% confidence bounds", xlab="Distance (m)", ylab="Dispersal function")

print(sfit.hya.primary, print.rmean = TRUE, rmean="common")

# Generate the Kaplan-Meier estimate of the survival function for A. hyacinthinus, secondary (dispersions: ####
## WE LATER SWITCHED TERNIMOLOGY FOR THE PAPER< SECONDARY BECAME TERTIARY
sfit.hya.secondary <- survfit(formula = disp.hya.secondary ~ 1)
sfit.hya.secondary
str(sfit.hya.secondary)

summary(sfit.hya.secondary)

plot(sfit.hya.secondary,main="Kaplan-Meier estimate with 95% confidence bounds", xlab="Distance (m)", ylab="Dispersal function")

print(sfit.hya.secondary, print.rmean = TRUE, rmean="common")

# Generate the Kaplan-Meier estimate of the survival function for A. leari:
sfit.lear <- survfit(formula = disp.lear ~ 1)
sfit.lear
str(sfit.lear)

summary(sfit.lear)

plot(sfit.lear,main="Kaplan-Meier estimate with 95% confidence bounds", xlab="Distance (m)", ylab="Dispersal function")

print(sfit.lear, print.rmean = TRUE, rmean="common")

# Generate the Kaplan-Meier estimate of the survival function for both species: ####
# first create a new column identifying lear and 1ary and 2ary hyacinthinus
guacas$Ave2<-NA
guacas$Ave2[which(guacas$Ave=="A.leari")]<-"A.leari"
guacas$Ave2[which(guacas$Ave=="A.hyacinthinus"&guacas$Primary1Secondary2==1)]<-"A.hyacinthinus_primary"
guacas$Ave2[which(guacas$Ave=="A.hyacinthinus"&guacas$Primary1Secondary2==2)]<-"A.hyacinthinus_secondary"


sfit.both <- survfit(formula = disp.both ~ 1)
sfit.both
str(sfit.both)

summary(sfit.both)

plot(sfit.both,main="Kaplan-Meier estimate with 95% confidence bounds", xlab="Distance (m)", ylab="Dispersal function")

# identifying groups:
sfit2 <- survfit(formula =disp.both ~ guacas$Ave2)
sfit2
str(sfit2)
summary(sfit2)

print(sfit2,print.rmean=TRUE)
print(sfit2,print.rmean=TRUE, rmean="common")
print(sfit2,print.rmean=TRUE, rmean="individual")
print(sfit2,print.rmean=TRUE, rmean=quantile(guacas[,4],0.95, na.rm=T)) # using arbitrary limit equal to the 95% quantile of pooled observations

str(lear)
quantile(hya[,4],0.95, na.rm=T)
quantile(lear[,4],0.95, na.rm=T)
quantile(guacas[,4],0.95, na.rm=T)
max(lear[,4])


plot(sfit2,main="", xlab="Distance (m)", ylab="Dispersal probability", col=c("royalblue","steelblue1","azure4"), conf.int=T, mark.time=T,lwd=1.5,mark=16)
legend(1200,0.8,legend=c("Hyacinth macaw - primary","Hyacinth macaw - secondary", "Lear's macaw"),lty=c(1,1), col=c("royalblue","steelblue1","azure4"),lwd=1.5)

## Test differences between two curves (G-rho family of tests, Fleming y Harrington (1982)) ####
# rho= 0 log-rank test(more weight on higher values of distance)
# rho= 1 generalized wilcoxon test(more weight on lower values of distance)
# rho= 1.5 Tarone-Ware test(in between log-rank and wilcoxon)

# hyancinth (primary) vs. lear ####
disp.both2<-disp.both[which(guacas$Ave2=="A.hyacinthinus_primary"|guacas$Ave2=="A.leari")]
guacas2<-guacas[which(guacas$Ave2=="A.hyacinthinus_primary"|guacas$Ave2=="A.leari"),]

svdiff1 <- survdiff(formula =disp.both2 ~ guacas2$Ave2, rho=1)
svdiff1



### Testing differences between all plants with n>5 for A. hyacinthinus, for primary and secondary dispersal: ####
table(guacas$Ave2,guacas$Plant)

#A. hyacinthinus primary ####

A.tot <- Surv(hya_primary$Distance[which(hya_primary$Plant=="Acrocomia totai")],hya_primary$Min0Exact1[which(hya_primary$Plant=="Acrocomia totai")])
sfit.A.tot <- survfit(formula=A.tot~1)
print(sfit.A.tot, print.rmean = TRUE, rmean="common")
 # proportion of dispersal edistances >100m

A.bar <- Surv(hya_primary$Distance[which(hya_primary$Plant=="Attalea barreirensis")],hya_primary$Min0Exact1[which(hya_primary$Plant=="Attalea barreirensis")])
sfit.A.bar <- survfit(formula=A.bar~1)
print(sfit.A.bar, print.rmean = TRUE, rmean="common")

A.eic <- Surv(hya_primary$Distance[which(hya_primary$Plant=="Attalea eichleri")],hya_primary$Min0Exact1[which(hya_primary$Plant=="Attalea eichleri")])
sfit.A.eic <- survfit(formula=A.eic~1)
print(sfit.A.eic, print.rmean = TRUE, rmean="common")

A.pha <- Surv(hya_primary$Distance[which(hya_primary$Plant=="Attalea phalerata")],hya_primary$Min0Exact1[which(hya_primary$Plant=="Attalea phalerata")])
sfit.A.pha <- survfit(formula=A.pha~1)
print(sfit.A.pha, print.rmean = TRUE, rmean="common")

M.fle <- Surv(hya_primary$Distance[which(hya_primary$Plant=="Mauritia flexulosa")],hya_primary$Min0Exact1[which(hya_primary$Plant=="Mauritia flexulosa")])
sfit.M.fle <- survfit(formula=M.fle~1)
print(sfit.M.fle, print.rmean = TRUE, rmean="common")



disp.hya.primary2<-disp.hya.primary[which(hya_primary$Plant=="Acrocomia totai"|hya_primary$Plant=="Attalea barreirensis"|hya_primary$Plant=="Attalea eichleri"|hya_primary$Plant=="Attalea phalerata"|hya_primary$Plant=="Mauritia flexulosa")]
hya_primary2<-hya_primary[which(hya_primary$Plant=="Acrocomia totai"|hya_primary$Plant=="Attalea barreirensis"|hya_primary$Plant=="Attalea eichleri"|hya_primary$Plant=="Attalea phalerata"|hya_primary$Plant=="Mauritia flexulosa"),]

(all_plants_hya_primary<- survdiff(formula =disp.hya.primary2 ~ hya_primary2$Plant, rho=1))


#A. hyacinthinus secondary ####
A.tot2 <- Surv(hya_secondary$Distance[which(hya_secondary$Plant=="Acrocomia totai")],hya_secondary$Min0Exact1[which(hya_secondary$Plant=="Acrocomia totai")])
sfit.A.tot2 <- survfit(formula=A.tot2~1)
print(sfit.A.tot2, print.rmean = TRUE, rmean="common")


# test difference in primary vs'secondary A. totai dispersions by hyacinthinus: ####
disp.both3<-disp.both[which(guacas$Ave2=="A.hyacinthinus_primary"|guacas$Ave2=="A.hyacinthinus_secondary")]
guacas3<-guacas[which(guacas$Ave2=="A.hyacinthinus_primary"|guacas$Ave2=="A.hyacinthinus_secondary"),]

disp.both4<-disp.both3[which(guacas3$Plant=="Acrocomia totai")]
guacas4<-guacas3[which(guacas3$Plant=="Acrocomia totai"),]

svdiff2 <- survdiff(formula =disp.both4 ~ guacas4$Ave2, rho=1)
svdiff2




### Testing differences between all plants with n>5 for A. leari: ####
J.mol <- Surv(lear$Distance[which(lear$Plant=="Jatropha mollisima")],lear$Min0Exact1[which(lear$Plant=="Jatropha mollisima")])
sfit.J.mol <- survfit(formula=J.mol~1)
print(sfit.J.mol, print.rmean = TRUE, rmean="common")

P.pac <- Surv(lear$Distance[which(lear$Plant=="Pilosocereus pachycladus")],lear$Min0Exact1[which(lear$Plant=="Pilosocereus pachycladus")])
sfit.P.pac <- survfit(formula=P.pac~1)
print(sfit.P.pac, print.rmean = TRUE, rmean="common")

S.cor <- Surv(lear$Distance[which(lear$Plant=="Syagrus coronatus")],lear$Min0Exact1[which(lear$Plant=="Syagrus coronatus")])
sfit.S.cor <- survfit(formula=S.cor~1)
print(sfit.S.cor, print.rmean = TRUE, rmean="common")



disp.lear2<-disp.lear[which(lear$Plant=="Jatropha mollisima"|lear$Plant=="Pilosocereus pachycladus"|lear$Plant=="Syagrus coronatus")]
lear2<-lear[which(lear$Plant=="Jatropha mollisima"|lear$Plant=="Pilosocereus pachycladus"|lear$Plant=="Syagrus coronatus"),]
(all_plants_lear<- survdiff(formula =disp.lear2 ~ lear2$Plant, rho=1))

# Proportion of dispersal distances >100m ####
## A. hyacinthinus (primary dispersion)
### All plants 
length(which(hya_primary$Distance>100))/length(hya_primary$Distance)

### A. totai
length(which(hya_primary$Distance[which(hya_primary$Plant=="Acrocomia totai")]>100))/length(hya_primary$Distance[which(hya_primary$Plant=="Acrocomia totai")])

### A. barreirensis
length(which(hya_primary$Distance[which(hya_primary$Plant=="Attalea barreirensis")]>100))/length(hya_primary$Distance[which(hya_primary$Plant=="Attalea barreirensis")])

### A. eichleri
length(which(hya_primary$Distance[which(hya_primary$Plant=="Attalea eichleri")]>100))/length(hya_primary$Distance[which(hya_primary$Plant=="Attalea eichleri")])

### "Attalea phalerata"
length(which(hya_primary$Distance[which(hya_primary$Plant=="Attalea phalerata")]>100))/length(hya_primary$Distance[which(hya_primary$Plant=="Attalea phalerata")])

### "Mauritia flexulosa"
length(which(hya_primary$Distance[which(hya_primary$Plant=="Mauritia flexulosa")]>100))/length(hya_primary$Distance[which(hya_primary$Plant=="Mauritia flexulosa")])

## A. hyacinthinus (tertiary dispersion)
### All plants 
length(which(hya_secondary$Distance>100))/length(hya_secondary$Distance)

### A. totai
length(which(hya_secondary$Distance[which(hya_secondary$Plant=="Acrocomia totai")]>100))/length(hya_secondary$Distance[which(hya_secondary$Plant=="Acrocomia totai")])

## A. leari (primary)
length(which(lear$Distance[which(lear$Plant=="Syagrus coronatus")]>100))/length(lear$Distance[which(lear$Plant=="Syagrus coronatus")])

# Figures ####
### Macaw species  ####
jpeg("anodorhynchus_dispersion_hist.jpg", width=8, height=6, units="in",res=300)
#layout(matrix(c(1,1,2,2,1,1,2,2,3,3,3,3), 3,4, byrow = T))
layout(matrix(c(1,2,3,4,4,4), 2,3, byrow = T))
par(
  mar=c(2,4,2,1),
  mgp=c(2,0.5,0)
)

hist(guacas$Distance[which(guacas$Ave2=="A.hyacinthinus_primary")],  breaks = seq(0,1620,32.4), xlab="",main="", xlim=c(0,1750),ylim=c(0,600),col="grey",axes=F)
axis(1,seq(0,1750,250))
axis(2,seq(0,600,100))
abline(v= 195.3,col=c("royalblue"),lwd=1.5)
text(1750,600,expression(bold("a")),cex=1.3)
mtext(expression("Distance (m)"),1,1.8,cex=0.7)
mtext("Hyacinth macaw - primary",3,-1.5,cex=0.7)


arrows(1603.8,10,1603.8,30,length=0.05,code=1)
arrows(1506.6,10,1506.6,30,length=0.05,code=1)
arrows(1020,10,1020,30,length=0.05,code=1)
arrows(793.8,10,793.8,30,length=0.05,code=1)
arrows(599.4,10,599.4,30,length=0.05,code=1)
arrows(502.2,10,502.2,30,length=0.05,code=1)
arrows(402.2,10,402.2,30,length=0.05,code=1)

legend(800,400,legend=c("Mean"),lty=c(1),col=c("royalblue"),cex=0.8,bty = "n",xjust=1)

hist(guacas$Distance[which(guacas$Ave2=="A.hyacinthinus_secondary")],  breaks = seq(0,1620,32.4), xlab="",main="", xlim=c(0,1750),ylim=c(0,600),col="grey",axes=F)
axis(1,seq(0,1750,250))
axis(2,seq(0,600,100))
abline(v= 55.19,col=c("steelblue1"),lwd=1.5)
text(1750,600,expression(bold("b")),cex=1.3)
mtext(expression("Distance (m)"),1,1.8,cex=0.7)
mtext("Hyacinth macaw - secondary",3,-1.5,cex=0.7)




legend(800,400,legend=c("Mean"),lty=c(1),col=c("royalblue"),cex=0.8,bty = "n",xjust=1)


hist(guacas$Distance[which(guacas$Ave=="A.leari")], breaks = "FD", xlab="",main="", mgp=c(2,0.5,0), col="grey")
abline(v=874.5,col=c("azure4"),lwd=1.5)
text(1000,135,expression(bold("c")),cex=1.3)
mtext(expression("Distance (m)"),1,1.8,cex=0.7)
mtext("Lear's macaw",3,-1.5,cex=0.7)

legend(800,90,legend=c("Mean"),lty=c(1),col=c("azure4"),cex=0.8,bty = "n",xjust=1)

par(
  mar=c(3.75,4,1.5,2)
)


plot(sfit2,main="", xlab="Distance (m)", ylab="Dispersal probability", col=c("royalblue","steelblue1","azure4"), conf.int=T, mark.time=T,lwd=1.5,mark=16)
legend(1200,0.8,legend=c("Hyacinth macaw - primary","Hyacinth macaw - secondary", "Lear's macaw"),lty=c(1,1), col=c("royalblue","steelblue1","azure4"),lwd=1.5)
text(1600,0.90,expression(bold("d")),cex=1.3)

dev.off()

### Plant type ####
jpeg("anodorhynchus_dispersion_hist2.jpg", width=8, height=6, units="in",res=300)
#layout(matrix(c(1,1,2,2,1,1,2,2,3,3,3,3), 3,4, byrow = T))
layout(matrix(c(1,1,2,2,3,3,3,3), 2,4, byrow = T))
par(
  mar=c(2,4,2,1),
  mgp=c(2,0.5,0)
)

hist(hya$Distance[which(hya$plant_group=="tall")],  breaks = seq(0,1620,32.4), xlab="",main="", xlim=c(0,1000),ylim=c(0,20),col="grey",axes=F)
axis(1,seq(0,1000,250))
axis(2,seq(0,20,5))
abline(v= 182.4,col=c("darkolivegreen3"),lwd=1.5)
text(1000,20,expression(bold("a")),cex=1.3)
mtext(expression("Distance (m)"),1,1.8,cex=0.7)
mtext(expression("Tall group"),3,-1.5,cex=0.7)


legend(500,10,legend=c("Mean"),lty=c(1),col=c("darkolivegreen3"),cex=0.8,bty = "n",xjust=1)


p1 <- hist(hya$Distance[which(hya$plant_group=="short")], breaks = seq(0,1620,32.4), xlab="",main="", mgp=c(2,0.5,0),xlim=c(0,1750),ylim=c(0,600),axes=F, col="grey")
abline(v=79.4,col=c("darkgreen"),lwd=1.5)
axis(1,seq(0,1750,250))
axis(2,seq(0,600,100))
text(1750,570,expression(bold("b")),cex=1.3)
mtext(expression("Distance (m)"),1,1.8,cex=0.7)
mtext(expression("Short group"),3,-1.5,cex=0.7)

legend(500,400,legend=c("Mean"),lty=c(1),col=c("darkgreen"),cex=0.8,bty = "n",xjust=1)

cbind(p1$density,p1$mids)

arrows(1603.8,10,1603.8,30,length=0.05,code=1)
arrows(1506.6,10,1506.6,30,length=0.05,code=1)
arrows(793.8,10,793.8,30,length=0.05,code=1)
arrows(599.4,10,599.4,30,length=0.05,code=1)
arrows(502.2,10,502.2,30,length=0.05,code=1)


par(
  mar=c(3.75,4,1.5,2)
)

plot(sfit_planttype,main="", xlab="Distance (m)", ylab="Dispersal probability", col=c("darkgreen","darkolivegreen3"), conf.int=T, mark.time=T,lwd=1.5,mark=16, mgp=c(2,0.5,0))
legend(500,0.8,legend=c(expression("Short group"),  expression("Tall group")),lty=c(1,1), col=c("darkgreen","darkolivegreen3"),lwd=1.5,bty = "n")
text(1600,0.80,expression(bold("c")),cex=1.3)
dev.off()



### Macaw and Plant type (no plant histogram) ####
jpeg("anodorhynchus_dispersion_hist3.jpg", width=8, height=8, units="in",res=300)
#layout(matrix(c(1,1,2,2,1,1,2,2,3,3,3,3), 3,4, byrow = T))
layout(matrix(c(1,1,2,2,3,3,3,3,4,4,4,4), 3,4, byrow = T))
par(
  mar=c(2,4,2,1),
  mgp=c(2,0.5,0)
)

hist(guacas$Distance[which(guacas$Ave=="A.hyacinthinus")],  breaks = seq(0,1620,32.4), xlab="",main="", xlim=c(0,1750),ylim=c(0,600),col="grey",axes=F)
axis(1,seq(0,1750,250))
axis(2,seq(0,600,100))
abline(v= 147,col=c("royalblue"),lwd=1.5)
text(1750,600,expression(bold("a")),cex=1.3)
mtext(expression("Distance (m)"),1,1.8,cex=0.7)
mtext(expression(italic("A. hyacinthinus")),3,-1.5,cex=0.7)


arrows(1603.8,10,1603.8,30,length=0.05,code=1)
arrows(1506.6,10,1506.6,30,length=0.05,code=1)
arrows(793.8,10,793.8,30,length=0.05,code=1)
arrows(599.4,10,599.4,30,length=0.05,code=1)
arrows(502.2,10,502.2,30,length=0.05,code=1)


legend(500,400,legend=c("Mean"),lty=c(1),col=c("royalblue"),cex=0.8,bty = "n",xjust=1)


hist(guacas$Distance[which(guacas$Ave=="A.leari")], breaks = "FD", xlab="",main="", mgp=c(2,0.5,0), col="grey")
abline(v=874.5,col=c("azure4"),lwd=1.5)
text(1000,135,expression(bold("b")),cex=1.3)
mtext(expression("Distance (m)"),1,1.8,cex=0.7)
mtext(expression(italic("A. leari")),3,-1.5,cex=0.7)

legend(800,90,legend=c("Mean"),lty=c(1),col=c("azure4"),cex=0.8,bty = "n",xjust=1)

par(
  mar=c(3.75,4,1.5,2)
)

plot(sfit2,main="", xlab="Distance (m)", ylab="Dispersal probability", col=c("royalblue","azure4"), conf.int=T, mark.time=T,lwd=1.5,mark=16, mgp=c(2,0.5,0))
legend(1200,0.8,legend=c(expression(italic("A. hyacinthinus")),  expression(italic("A. leari"))),lty=c(1,1), col=c("royalblue","azure4"),lwd=1.5,bty = "n")
text(1600,0.80,expression(bold("c")),cex=1.3)

par(
  mar=c(3.5,4,0.75,2)
)

plot(sfit_planttype,main="", xlab="Distance (m)", ylab="Dispersal probability", col=c("darkgreen","darkolivegreen3"), conf.int=T, mark.time=T,lwd=1.5,mark=16, mgp=c(2,0.5,0))
legend(500,0.8,legend=c(expression("Short group"),  expression("Tall group")),lty=c(1,1), col=c("darkgreen","darkolivegreen3"),lwd=1.5,bty = "n")
text(1600,0.80,expression(bold("d")),cex=1.3)

dev.off()
