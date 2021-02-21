rm(list = ls())

#Data cleaning and importing data. In my data I skipped first 40 rows.
################################################################################
################################################################################
#Data set for each city.
################################################################################
################################################################################

Harare <- read.table("Harare.dat", skip=40, header = F)
# View(Harare)

Huye <- read.table("Huye.dat", skip=40, header = F)

Iringa <- read.table("Iringa.dat", skip=40, header = F)

Johanesburg <- read.table("Johanesburg.dat", skip=40, header = F)

Jos<- read.table("Jos.dat", skip=40, header = F)

Kamembe <- read.table("Kamembe.dat", skip=40, header = F)

Kayonza <- read.table("Kayonza.dat", skip=40, header = F)

Kamembe <- read.table("Kamembe.dat", skip=40, header = F)

Kigali <- read.table("Kigali.dat", skip=40, header = F)

Koulikoro <- read.table("Koulikoro.dat", skip=40, header = F)

Lagos <- read.table("Lagos.dat", skip=40, header = F)

################################################################################
#combining precipitation of each city.
################################################################################
# newdata<- read.csv("yves dataset.csv", skip=40, header = F)
# View(newdata)
# years <- newdata[,3]
# View(years)

combinedcities <- cbind(Harare$V5,Huye$V5,Iringa$V5,Johanesburg$V5,Jos$V5,Kamembe$V5,
                        Kayonza$V5,Kigali$V5,Koulikoro$V5,Lagos$V5)

View(combinedcities)
colnames(combinedcities) <- c("Harare","Huye","Iringa","Johanesburg","Jos","Kamembe",
                                  "Kayonza","Kigali","Koulikoro","Lagos")
rownames(combinedcities) <- Harare[,1]

library(xtable)

xtable(summary(combinedcities))

################################################################################
#Cluster Analysis
################################################################################

which(is.na(combinedcities))

# Standardization
combinedcities_scaled <-scale(combinedcities) 
rownames(combinedcities_scaled) <- Harare[,1]

distance <- dist(t(combinedcities_scaled))
distance_1 <- dist(combinedcities_scaled)
################################################################################
#clustering - CA 
################################################################################
# par(mfrow = c(3,1))
clustering1 <- hclust(distance,method = "single")
plot(clustering1,hang=-2,main = "Single Linkage Dendogram",xlab = "",ylab = "",sub = "")
cutree(clustering1, 4)
rect.hclust(clustering1, k =4, border = 'red')

clustering2 <- hclust(distance,method = "average")
plot(clustering2,hang=-2,main = "Average Linkage Dendogram")
cutree(clustering2, 4)
rect.hclust(clustering1, k =4, border = 'red')

clustering3 <- hclust(distance,method = "ward.D2")
plot(clustering3,hang=-2,main = "Ward Algorithm Dendogram")
cutree(clustering3, 4)
rect.hclust(clustering1, k =4, border = 'red')
# trial <- hclust(distance_1,method = "ward.D2")
# plot(trial,hang = -2)



################################################################################
# PCA analysis
################################################################################
library(corrplot)
library(ggplot2)
corrplot(cor(combinedcities))

plot(combinedcities)
boxplot(combinedcities)
summary(combinedcities)
#check PCA eligibility
mean(cor(combinedcities))

PCA <- princomp(combinedcities)
PCA$loadings

View(combinedcities)
# write.csv(combinedcities,file = "yvesbonheur_mugiraneza_RMC1.csv")
################################################################################
############____________----------------------------------------__________######
library(psych)

## Unrotated PCA
pc1 <- principal(combinedcities_scaled, nfactors = 2, rotate = "none")

pc1 # the full model to see the loadings, Eigenvalues  (SS loadings) and variance accounted for
xtable(pc1$loadings)
## screeplot
# png('screeplot.png')
# par(mfrow = c(2,1))
pve <- 100* pc1$values/sum(pc1$values) 
plot(pc1$values, type ="o" , ylab ="Eigenvalues " , xlab ="Principal Component" ,
     col ="blue", main = 'screeplot',width=34)
plot (cumsum(pve) , type ="o" , ylab ="Cumulative PVE " , xlab ="Principal Component " , col ="brown3 ",
      main = 'Percentage Variance Explained (PVE) plot')
# dev.off()


## Rotated PCA after extracting two components
pc2 <- principal(combinedcities_scaled, nfactors = 2, rotate = "varimax") 

scores<- as.data.frame(pc2$scores)  #scores after rotation
View(scores)

# png('screeplot.png')
par(mfrow = c(1,2))
pve1 <- 100* pc2$values/sum(pc2$values) 
plot(pc2$values, type ="o" , ylab ="Eigenvalues " , xlab ="Principal Component" ,
     col ="blue", main = 'screeplot')
plot (cumsum(pve1) , type ="o" , ylab ="Cumulative PVE " , xlab ="Principal Component " , col ="brown3 ",
      main = 'Percentage Variance Explained (PVE) plot')

pc2$loadings

#png('pca_scores_rotated1.png')

scores <- cbind(scores,seq(as.Date('1960-1-16'),to=as.Date('2010-12-16'),by='1 month'))
colnames(scores) <- c("RC1","RC2","Num")
#   seq(1:612)

#-----------------------------------------------------------------------------##
par(mfrow=c(2,1))
plot(scores$RC1[1:153]~scores$Num[1:153],type ="o", xlab = 'Date', ylab='RC1', main='Rotated Component 1',xaxt='none')
plot(scores$RC1[153:306]~rownames(scores[153:306,]),type ="o", xlab = 'Date', ylab='RC1', main='Rotated Component 1',xaxt='none')
plot(scores$RC1[306:459]~rownames(scores[306:459,]),type ="o", xlab = 'Date', ylab='RC1', main='Rotated Component 1',xaxt='none')
plot(scores$RC1[459:612]~rownames(scores[459:612,]),type ="o", xlab = 'Date', ylab='RC1', main='Rotated Component 1',xaxt='none')

abline(h=0)
#--------------------------------------------------------------------------####
plot(scores$RC1[1:153]~scores$Num[1:153],type ="o", xlab = '1960-1972', ylab='RC1', main='Rotated Component 1',xaxt='none')
abline(h=0)
plot(scores$RC1[153:306]~scores$Num[153:306],type ="o", xlab = '1972-1984', ylab='RC1', main='Rotated Component 1',xaxt='none')
abline(h=0)
plot(scores$RC1[306:459]~scores$Num[306:459],type ="o", xlab = '1984-1996', ylab='RC1', main='Rotated Component 1',xaxt='none')
abline(h=0)
plot(scores$RC1[459:612]~scores$Num[459:612],type ="o", xlab = '1996-2010', ylab='RC1', main='Rotated Component 1',xaxt='none')
abline(h=0)
# library(ggplot2)

ggplot(scores[1:153,],aes(x=Num[1:153],y=RC1[1:153]))+
  geom_line()+
  geom_point()+
  scale_x_date(limit = c(as.Date("1960-1-16"),as.Date("1972-9-16")))+
  ggtitle("Rotated Component 1")

datexline1 <- scores$Num[153]
dataxline11 <- scores$Num[306]
ggplot(scores[153:306,],aes(x=Num[153:306],y=RC1[153:306]))+
  geom_line()+
  geom_point()+
  scale_x_date(limit = c(as.Date("1972-09-16"),as.Date("1985-06-16")))+
  ggtitle("Rotated Component 1")

# ggplot(scores[1:153,],aes(x=Num[1:153],y=RC1[1:153]))+
#   geom_line()+
#   geom_point()+
#   scale_x_date(limit = c(as.Date("1960-1-16"),as.Date("1972-9-16")))+
#   ggtitle("Rotated Component 1")
# 
# ggplot(scores[1:153,],aes(x=Num[1:153],y=RC1[1:153]))+
#   geom_line()+
#   geom_point()+
#   scale_x_date(limit = c(as.Date("1960-1-16"),as.Date("1972-9-16")))+
#   ggtitle("Rotated Component 1")

