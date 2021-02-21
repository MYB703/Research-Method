rm(list = ls())

#Import of Data.
setwd("/home/patricia/RESEARCH METHODS FOR CLIMATE SCIENCE")
data <- read.csv('TMN.csv')
View(data)

rownames(data) <- data[,1]
#new_data2=data[,-1]
new_data=data[-1:-2] 
View(new_data)


class(new_data) 
dim(new_data) 
names(new_data)
str(new_data)
anyNA(new_data)


###################-------------------###################
#############                                 ###########
#                    Cluster Analysis                   #
#############                                 ###########
###################-------------------###################


## standardizing and preparing the dataset
new_data_scaled <- scale(new_data)                # standardizing the variables
new_data_scaled.dist <- dist(new_data_scaled)     # distance computation with year as observation
new_data_scaled.distT <- dist(t(new_data_scaled)) # distance computation with year as variables


# performing hierarchical clustering of the observations (variables) using Ward method,
#single and average linkage 
ward = hclust(new_data_scaled.distT,method='ward.D2')
single= hclust(new_data_scaled.distT,method='single')
average = hclust(new_data_scaled.distT,method='average')

png('cluster_dendogram_variable.png') 
par(mfrow = c(1,3))
plot(ward, main = 'Wards Method',xlab =" ", sub ="", hang = -2)
rect.hclust(ward, k =4, border = 'red') ## selecting three clusters
plot(single, main = 'Single Linkage',xlab =" ", sub ="", hang = -2)
rect.hclust(single, k =4, border = 'red') ## selecting three clusters
plot(average, main = 'Average Linkage',xlab =" ", sub ="", hang = -2)
rect.hclust(average, k =4, border = 'red') ## selecting three clusters



cutree (ward,4)
cutree (single,4)
cutree (average,4)
dev.off()


###################---------------------------###################
#################                                 ###############
#                 Principal Componenet Analysis                 #
#################                                 ###############
###################---------------------------###################

# please install psych package by removing the # before the command on the next line
#install.packages('psych')

library(psych)

## Unrotated PCA
pc1 <- principal(new_data_scaled, nfactors = 10, rotate = "none")

pc1 # the full model to see the loadings, Eigenvalues  (SS loadings) and variance accounted for

## screeplot
png('screeplot.png')
par(mfrow = c(1,2))
pve <- 100* pc1$values/sum(pc1$values) 
plot(pc1$values, type ="o" , ylab ="Eigenvalues " , xlab ="Principal Component" ,
     col ="blue", main = 'screeplot')
plot (cumsum(pve) , type ="o" , ylab ="Cumulative PVE " , xlab ="Principal Component " , col ="brown3 ",
      main = 'Percentage Variance Explained (PVE) plot')
dev.off()


## Rotated PCA after extracting two components
pc2 <- principal(new_data_scaled, nfactors = 2, rotate = "varimax") 

scores<- as.data.frame(pc2$scores)  #scores after rotation

#png('pca_scores_rotated1.png')

par(mfrow=c(1,2))
plot(scores$RC1[1:153]~rownames(scores[1:153,]),type ="o", xlab = 'Date', ylab='RC1', main='Rotated Component 1',xaxt='none')
plot(scores$RC1[153:306]~rownames(scores[153:306,]),type ="o", xlab = 'Date', ylab='RC1', main='Rotated Component 1',xaxt='none')
plot(scores$RC1[306:459]~rownames(scores[306:459,]),type ="o", xlab = 'Date', ylab='RC1', main='Rotated Component 1',xaxt='none')
plot(scores$RC1[459:612]~rownames(scores[459:612,]),type ="o", xlab = 'Date', ylab='RC1', main='Rotated Component 1',xaxt='none')


