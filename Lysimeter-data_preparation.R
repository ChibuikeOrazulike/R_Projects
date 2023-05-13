library(ISI.Toolbox)


#======================================================
# Reading data for Study Project
#======================================================
setwd("D:/BjornProject/DataEmmanuel")
getwd()

#======================================================
# Importing the lysimeter data frame
#======================================================
w.sample1 <- read.table("D:/BjornProject/DataEmmanuel/Weight_Lysimeter-I.csv",
                     header = T, sep = ",") #change the input csv file
colnames(w.sample1)<-c("date", "w.I1")
w.sample1$date <-strptime(w.sample1$date, format="%d/%m/%Y %H:%M") #, format= "%d.%m.%Y %H:")
head(w.sample1, 10)
#======================================================
# Converting lysimeter data frame to XTS (Time Series Format)
#======================================================
library(xts)
W.xts1 = xts(x = w.sample1, order.by = w.sample1$date)

#======================================================
# Importing the soil moisture data frame
#======================================================
w.wet1 = read.table("D:/BjornProject/DataEmmanuel/Soil_Moisture_Wet.csv",
                    header = T, sep = ",")
colnames(w.wet1)<-c("Time", "w.I1", "w.I2", "w.I3")
w.wet1$Time = strptime(w.wet1$Time, format="%d/%m/%Y %H:%M") #, format= "%d.%m.%Y %H:")

#======================================================
# Converting soil moisture data frame to XTS (Time Series Format)
#======================================================
library(xts)
W.xts2 = xts(x = w.wet1, order.by = w.wet1$Time)

#======================================================
# Removing the lysimeter duplicate time from the time series
#======================================================
?DeleteDuplicteTime
w.data2 = DeleteDuplicteTime(W.xts1)
#======================================================
# Removing the Soil Moisture duplicate time from the time series
#======================================================
?DeleteDuplicteTime
w.data3 = DeleteDuplicteTime(W.xts2)

#======================================================
# Removing Null values from the lysimeter data# 
#======================================================
w.nullexc = DeleteNAs(w.data2$w.I1, Print = T, return_index = FALSE)
w.nullexc

#======================================================
# Removing Null values from the soil moisture data# 
#======================================================
w.nullexc2 = DeleteNAs(w.data3$w.I1, Print = T, return_index = FALSE) #select the right w.I1/w.I2/w.I3
w.nullexc2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Outlier removal by GradFilter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
w.Gradfilter = GradFilter(w.nullexc, abs_grad = 15, NormalizedGrad = TRUE)
?GradFilter()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Smoothing of the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
w.datasmooth = round(Smooth(w.Gradfilter, window_width = 7))
head(w.datasmooth)
DynPlot(cbind(w.nullexc, w.datasmooth,  w.nullexc2$w.I1), Axis = c(1,1,2), Labels=c("Original","Smooth", "Soil moisture"), colset = c(1,2,3)) #change to the appropriate soil moisture column

#======================================================
# Exporting the time series data 
#======================================================
WriteXTStoTXT(w.datasmooth, DirName = "D:/BjornProject/DataEmmanuel/Routputs/Lysimeter1.csv") #rename the output

# Winsorize()
# CutTimeSeries()
# quantile()
