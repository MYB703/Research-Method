
###################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#              Autocorrelation and Cross correlation            #
#################                                 ###############
###################---------------------------###################

rm(list = ls()) 
##setwd('C:/Users/Babatunde Abiodun/Documents/Academic/Lectures/WASCAL/Codes/TimeSeries') # please change  to your 
                                                             # preferred working directory
data_to_use <- read.csv("yvesbonheur_mugiraneza_RMC1.csv")
# View(data_to_use)
potential <- data_to_use[,-1]
# View(potential)
# please install psych package by removing the # before the install commands below
# install.packages('forecast',dependencies=T)
# install.packages('TSA')
# install.packages('biwavelet') 
potential_scaled <- scale(potential)
# View(potential_scaled)
library(psych)
pc1 <- principal(potential_scaled,nfactors=2,rotate = "varimax")
scores1 <- as.data.frame(pc1$scores)
# View(scores1)
dates <- seq(as.Date('1960-1-16'),to=as.Date('2010-12-16'),by="1 month")
scores1 <- cbind(scores1,dates)
# scores1 <- scores1[,-c(3,4)]

plot(scores1$RC1[1:153]~scores1$dates[1:153],type ="o",xlab ="Year",ylab = "RC1",main ="Scores for Rotated Component 1960-1972" )
abline(h=0)
plot(scores1$RC1[153:306]~scores1$dates[153:306],type ="o",xlab ="Year",ylab = "RC1",main ="Scores for Rotated Component 1973-1985" )
abline(h=0)
plot(scores1$RC1[306:459]~scores1$dates[306:459],type ="o",xlab ="Year",ylab = "RC1",main ="Scores for Rotated Component 1986-1998" )
abline(h=0)
plot(scores1$RC1[459:612]~scores1$dates[459:612],type ="o",xlab ="Year",ylab = "RC1",main ="Scores for Rotated Component 1998-2010" )
abline(h=0)

# plot(scores$RC1[1:153]~scores$Num[1:153],type ="o", xlab = 'Date', ylab='RC1', main='Rotated Component 1',xaxt='none')
# plot(scores$RC1[153:306]~rownames(scores[153:306,]),type ="o", xlab = 'Date', ylab='RC1', main='Rotated Component 1',xaxt='none')
# plot(scores$RC1[306:459]~rownames(scores[306:459,]),type ="o", xlab = 'Date', ylab='RC1', main='Rotated Component 1',xaxt='none')
# plot(scores$RC1[459:612]~rownames(scores[459:612,]),type ="o", xlab = 'Date', ylab='RC1', main='Rotated Component 1',xaxt='none')


library(forecast)
library(TSA)
library(biwavelet)

## Data importation
# kano<-read.csv('Kano.txt')
# 
# ## Data RC1aration
# class(kano)
# dim(kano)
# names(kano)
# str(kano)
# anyNA(kano)
# View(kano)
# summary(kano)

needed_data <-scores1 # precipitation and RC2erature

# View(needed_data)
### converting data to time series
RC1 <- ts(needed_data$RC1, start = c(1960,1), end = c(2010,12), frequency = 12)
RC2 <- ts(needed_data$RC2, start = c(1960,1), end = c(2010,12), frequency = 12)


## ploting the series, Note: always Zoom to see the chart clearly
# png('timeseries_plot.png')
par(mfcol = c(2,1))
plot(RC1, ylab = 'RC1', main = 'Time Series plot of RC1', col='red')
lowess_value <- lowess(RC1)
lines(lowess_value,col="blue")

plot(RC2, ylab = 'RC2', main = 'Time Series plot of RC2', col='blue')
lowess_value <- lowess(RC1)
lines(lowess_value,col="red")
# dev.off()

detrend1 <- diff(needed_data$RC1,differences = 12)
detrend2 <- diff(needed_data$RC2,differences = 12)

RC1.detrend <- ts(detrend1, start = c(1960,1), end = c(2010,12), frequency = 12)
RC2.detrend <- ts(detrend2, start = c(1960,1), end = c(2010,12), frequency = 12) 

par(mfcol= c(2,1))
plot(RC1.detrend, ylab = 'RC1', main = 'Detrended Time Series plot of RC1', col='purple')
lowess_value <- lowess(RC1.detrend)
lines(lowess_value,col="red")

plot(RC2.detrend, ylab = 'RC2', main = 'Detrended Time Series plot of RC2', col='blue')
lowess_value <- lowess(RC2.detrend)
lines(lowess_value,col="red")

### Autocorrelation plot
# png('Auto_correlation.png')
par(mfcol = c(1,2))
forecast::Acf(RC1.detrend, lag.max = 36)
forecast::Acf(RC2, lag.max = 36)
# dev.off()


### cross-correlation plot
# png('cross_correlation.png')
par(mfcol = c(1,1))
forecast::Ccf(RC1,RC2)
# dev.off()

###################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#                         Fourier Analysis                      #
#################                                 ###############
###################---------------------------###################
# png('power_spectrum_Freq.png')
par(mfcol = c(1,2))
p1 <- periodogram(RC1)
p <- periodogram(RC2)
# dev.off()


## converting the frequency to periods
# png('power_spectrum_period.png')
period_RC2 <- 1/p$freq
period_RC1 <- 1/p1$freq

par(mfcol = c(1,2))
plot(period_RC2, p$spec, type = 'b', xlim = c(2,13), xlab = 'period', ylab = 'power',
     main = 'RC2')

plot(period_RC1, p1$spec, type = 'b', xlim = c(2,13), xlab = 'period', ylab = 'power',
     main = 'RC1')
# dev.off()

###################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#             Wavelet Analysis and Wavelet Coherence            #
#################                                 ###############
###################---------------------------###################

## Wavelet
timelong=seq(1960, 2011-1/12, 1/12)  # indexing
wavelet=wt(cbind(timelong,RC1),dj=0.1,mother="morlet",max.scale=16) #wavelet
wavelet1=wt(cbind(timelong,RC2),dj=0.1,mother="morlet",max.scale=16) #wavelet

## plot of wavelet
png('wavelet.png')
par(mfcol = c(1,1))
par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
xx=plot(wavelet,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='Precipitation')

#par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
yy=plot(wavelet1,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='RC2erature')
dev.off()


# wavelet coherence calculation
wcoh=wtc(cbind(timelong,RC1),cbind(timelong,RC2),dj=0.1,mother="morlet",max.scale=16) 

## plot of wavelet coherence
png('wavelet_coherence.png')
par(mfcol = c(1,1))
par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1) #To allow for colour bar to be included
plot(wcoh,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
     lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=T,ylab="Period(Year)", xlab = "Time(Year)",
     main='Wavelet Coherence') #plotting
dev.off()



# Arrows pointing to the right mean that x and y are in phase.

#Arrows pointing to the left mean that x and y are in anti-phase.

#Arrows pointing up mean that y leads x by π/2.

#Arrows pointing down mean that x leads y by π/2. 
