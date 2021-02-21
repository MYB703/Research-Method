
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

library(forecast)
library(TSA)
library(biwavelet)

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
png('timeseries_plot.png')
prep <- ts(needed_data$PRED, start = c(1960,1), end = c(2010,12), frequency = 12)
temp <- ts(needed_data$TMPD, start = c(1960,1), end = c(2010,12), frequency = 12)
dev.off()

## ploting the series, Note: always Zoom to see the chart clearly
png('Auto_correlation.png')
par(mfcol = c(2,1))
plot(prep, ylab = 'Monthly precipitation', main = 'Time Series plot of Precipitation', col='black')
plot(temp, ylab = 'Monthly Temperature', main = 'Time Series plot of Temperature', col='black')
dev.off()

### Autocorrelation plot
png('Auto_correlation.png')
par(mfcol = c(1,2))
forecast::Acf(prep, lag.max = 36)
forecast::Acf(temp, lag.max = 36)
dev.off()


### cross-correlation plot
png('cross_correlation.png')
par(mfcol = c(1,1))
forecast::Ccf(prep,temp)
dev.off()

###################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#                         Fourier Analysis                      #
#################                                 ###############
###################---------------------------###################
png('periodogram.png')
par(mfcol = c(1,2))
p1 <- periodogram(prep)
p <- periodogram(temp)
dev.off()


## converting the frequency to periods
png('frequency_gram.png')
period_Temp <- 1/p$freq
period_Prep <- 1/p1$freq

par(mfcol = c(1,2))
plot(period_Temp, p$spec, type = 'b', xlim = c(2,13), xlab = 'period', ylab = 'periodogram',
     main = 'Temperature')

plot(period_Prep, p1$spec, type = 'b', xlim = c(2,13), xlab = 'period', ylab = 'periodogram',
     main = 'Precipitation')
dev.off()

###################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#             Wavelet Analysis and Wavelet Coherence            #
#################                                 ###############
###################---------------------------###################

## Wavelet
timelong=seq(1960, 2011-1/12, 1/12)  # indexing
wavelet=wt(cbind(timelong,prep),dj=0.1,mother="morlet",max.scale=16) #wavelet
wavelet1=wt(cbind(timelong,temp),dj=0.1,mother="morlet",max.scale=16) #wavelet

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
        main='Temperature')
dev.off()


# wavelet coherence calculation
wcoh=wtc(cbind(timelong,prep),cbind(timelong,temp),dj=0.1,mother="morlet",max.scale=16) 

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
