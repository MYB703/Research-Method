
###################-------------------###################
#############                                 ###########
#               Preprosessing the Data                  #
#############                                 ###########
###################-------------------###################

rm(list=ls()) ## this command clears the history
## setwd("/home/alabi") ## set your working directiory

data<-my_data <- read.csv("Data.csv") ## importing the data

rownames(data) <- data[,1]  ## Setting rownames

new_data=data[,-1]  ## removing the first column because it only serves as our row names

class(new_data) # checking your data class

dim(new_data) # getting the dimension of your data

names(new_data) # getting the column names

str(new_data) # checking data structure

anyNA(new_data) # checking for missing values

summary(new_data) # summary of the data

View(new_data) # view your data

###################-------------------###################
#############                                 ###########
#                    Cluster Analysis                   #
#############                                 ###########
###################-------------------###################


## standardizing and preparing the dataset
new_data_scaled <- scale(new_data) ## standardizing the variables
new_data_scaled.dist <- dist(new_data_scaled) # distance computation with year as observation
new_data_scaled.distT <- dist(t(new_data_scaled)) # distance computation with year as variables



# performing hierarchical clustering of the observations (variables) using Ward method,
#single and average linkage 
ward = hclust(new_data_scaled.distT,method='ward.D2')
single= hclust(new_data_scaled.distT,method='single')
average = hclust(new_data_scaled.distT,method='average')

## Plotting the dendogram
par(mfrow = c(1 ,3))
plot(ward, main = 'Wards Method',xlab =" ", sub ="")
rect.hclust(ward, k =3, border = 'red') ## selecting three clusters
plot(single, main = 'Single Linkage',xlab =" ", sub ="")
rect.hclust(single, k =3, border = 'red') ## selecting three clusters
plot(average, main = 'Average Linkage',xlab =" ", sub ="")
rect.hclust(average, k =3, border = 'red') ## selecting three clusters

## cutting the trees to see the groups selected above
cutree (ward,3)
cutree (single,3)
cutree (average,3)

# performing hierarchical clustering of the observations (years) using Ward method,single 
# and average linkage 
wardT = hclust(new_data_scaled.dist,method='ward.D2')
singleT = hclust(new_data_scaled.dist,method='single')
averageT = hclust(new_data_scaled.dist,method='average')

par(mfrow = c(3 ,1))
plot(wardT, main = 'Wards Method',xlab =" ", sub ="")
rect.hclust(wardT, k =3, border = 'red') ## selecting three clusters
plot(singleT, main = 'Single Linkage',xlab =" ", sub ="")
rect.hclust(singleT, k =4, border = 'red') ## selecting four clusters
plot(averageT, main = 'Average Linkage',xlab =" ", sub ="")
rect.hclust(averageT, k =5, border = 'red') ## selecting five clusters

## cutting the trees to see the groups selected above
cutree (wardT,3)
cutree (singleT,4)
cutree (averageT,5)



###################---------------------------###################
#################                                 ###############
#                 Principal Componenet Analysis                 #
#################                                 ###############
###################---------------------------###################

# please install psych package by removing the # before the command on the next line
## install.packages('psych')
require(psych)

## Unrotated PCA
pc1 <- principal(new_data_scaled, nfactors = 16, rotate = "none")


pc1 # the full model to see the loadings, Eigenvalues  (SS loadings) and variance accounted for

## screeplot
par(mfrow = c(1,2))
pve <- 100* pc1$values/sum(pc1$values) 
plot(pc1$values, type ="o" , ylab ="Eigenvalues " , xlab ="Principal Component" ,
     col ="blue", main = 'screeplot')
plot (cumsum(pve) , type ="o" , ylab ="Cumulative PVE " , xlab ="Principal Component " , col ="brown3 ",
      main = 'Percentage Variance Explained (PVE) plot')


## Rotated PCA after extracting four components
pc2 <- principal(new_data_scaled, nfactors = 4, rotate = "varimax") 

scores<- as.data.frame(pc2$scores)  #scores after rotation

#plotting some of the scores
plot(scores$RC1~rownames(scores),type ="o", xlab = 'year', ylab='RC1', main='Rotated Component 1')
plot(scores$RC2~rownames(scores),type ="o", xlab = 'year', ylab='RC2', main='Rotated Component 2')


###################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#              Autocorrelation and Cross correlation            #
#################                                 ###############
###################---------------------------###################

rm(list = ls()) 
##setwd('/home/alabi/Review_Phase/Block 3/Climate statistics') # please change  to your 
                                                             # preferred working directory

# please install psych package by removing the # before the install commands below
# install.packages('forecast')
# install.packages('TSA')
# install.packages('biwavelet')

#install.packages('forecast')
#install.packages('TSA')
#install.packages('biwavelet')

require(forecast)
require(TSA)
require(biwavelet)

## Data importation
kano<-read.csv('Kano.csv')

## Data preparation
class(kano)
dim(kano)
names(kano)
str(kano)
anyNA(kano)
View(kano)
summary(kano)

needed_data <-kano[, c(2,6)] # precipitation and temperature


### converting data to time series
prep <- ts(needed_data$PRED, start = c(1960,1), end = c(2010,12), frequency = 12)
temp <- ts(needed_data$TMPD, start = c(1960,1), end = c(2010,12), frequency = 12)

## ploting the series, Note: always Zoom to see the chart clearly
par(mfcol = c(2,1))
plot(prep, ylab = 'Monthly precipitation', main = 'Time Series plot of Precipitation', col='black')
plot(temp, ylab = 'Monthly Temperature', main = 'Time Series plot of Temperature', col='black')

### Autocorrelation plot
par(mfcol = c(1,2))
forecast::Acf(prep, lag.max = 36)
forecast::Acf(temp, lag.max = 36)


### cross-correlation plot
par(mfcol = c(1,1))
forecast::Ccf(prep,temp)


###################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#                         Fourier Analysis                      #
#################                                 ###############
###################---------------------------###################
par(mfcol = c(1,2))
p1 <- periodogram(prep)
p <- periodogram(temp)


## converting the frequency to periods
period_Temp <- 1/p$freq
period_Prep <- 1/p1$freq

par(mfcol = c(1,2))
plot(period_Temp, p$spec, type = 'b', xlim = c(2,13), xlab = 'period', ylab = 'periodogram',
     main = 'Temperature')

plot(period_Prep, p1$spec, type = 'b', xlim = c(2,13), xlab = 'period', ylab = 'periodogram',
     main = 'Precipitation')



###################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#             Wavelet Analysis and Wavelet Coherence            #
#################                                 ###############
###################---------------------------###################

## Wavelt
timelong=seq(1960, 2011-1/12, 1/12)  # indexing
wavelet=wt(cbind(timelong,prep),dj=0.1,mother="morlet",max.scale=16) #wavelet
wavelet1=wt(cbind(timelong,temp),dj=0.1,mother="morlet",max.scale=16) #wavelet

## plot of wavelet
par(mfcol = c(1,1))
par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
xx=plot(wavelet,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='Precipitation')

#par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
yy=plot(wavelet1,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='Temperature')


# wavelet coherence calculation
wcoh=wtc(cbind(timelong,prep),cbind(timelong,temp),dj=0.1,mother="morlet",max.scale=16) 

## plot of wavelet coherence
par(mfcol = c(1,1))
par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1) #To allow for colour bar to be included
plot(wcoh,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
     lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=T,ylab="Period(Year)", xlab = "Time(Year)",
     main='Wavelet Coherence') #plotting



# Arrows pointing to the right mean that x and y are in phase.

#Arrows pointing to the left mean that x and y are in anti-phase.

#Arrows pointing up mean that y leads x by π/2.

#Arrows pointing down mean that x leads y by π/2. 
