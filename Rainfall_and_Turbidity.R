library(ISI.Toolbox)
library(IETD) #https://cran.r-project.org/web/packages/IETD/IETD.pdf

#======================================================
# Reading data for Study Project
#======================================================
setwd("C:/Users/rojasgom/Nextcloud/Shared/SS22_SP_Turbidity/Data2021/")
#======================================================
# Rainfall
#======================================================
LO = ReadTabletoXTS("Lockwitz14-22_rain.txt", head=T,TimeZone = "Etc/GMT-1") #Lockwitz rainfall gauge
LO = DeleteDuplicteTime(LO)
start(LO) #"2014-01-01 00:05:00 +01"
end(LO) #"2022-05-01 +01"

#==================================================================================  
#Extraction of independent rainfall events from a sub-daily time series using IETD
#==================================================================================

LO_2 = CutTimeSeries(LO,"2018-01-01 00:00","2022-05-01 00:00") 
LO_df = data.frame(date=as.POSIXct(LO_2),mm=coredata(LO_2))

#Extraction of independent rainfall events. 5h independant event. 5 mm rainfall depth
#IETD	: The minimum rainless period or dry period (hours) to be considered between two independent rainfall events.
#Thres: A rainfall depth threshold to define slight rainfall events (default value 0.5).

LO_event = drawre(LO_df,IETD=5,Thres=0.0)#min depth is 1mm per rainfall event
summary(LO_event$Rainfall_Characteristics)
View(LO_event$Rainfall_Characteristics)
eventsLO =LO_event[[1]]
#WriteXTStoTXT(eventsLO,"LO_Rain_events_2018_2022.csv")

#======================================================
# MS6
#======================================================
#Turbidity using SCAN sensor in MS6
MS6 = ReadTabletoXTS("scan_MEZ_Turb_MS6.txt",header=T,sep = ",", format="%Y-%m-%d %H:%M",
                     TimeZone="Etc/GMT-1") #Always use Etc/GMT-1 time zone!
MS6 = round(MS6,2)
head(MS6)
tail(MS6)
summary(MS6)
# Samples taken during rainfall events
# You should pre-process the MS6_KR.csv file for doing the average of the TS that are separated by /
# e.g. 23.09.2020 14:23,93482,92693/95264,5859

MS6_gs = ReadTabletoXTS("MS6_KR.csv",header=T,sep = ",", format="%d.%m.%Y %H:%M",
                        TimeZone="Etc/GMT-1")
View(MS6_gs)
summary(MS6_gs)

# Example for joining TS and Turbidity
TS_Turb = na.omit(cbind(MS6_gs[,1],MS6))
View(TS_Turb)
DynPlot(MS6["2021"])

#======================================================
# Elements
#======================================================

elem_MS6_less63 = ReadTabletoXTS("metals_river_less63MS6.txt",header=T,sep = ",", format="%Y-%m-%d %H:%M",
                     TimeZone="Etc/GMT-1") #Always use Etc/GMT-1 time zone!
head(elem_MS6_less63)
elem_MS6_less63 =elem_MS6_less63[,-c(1:4)]
elem_MS6_less63 =round(elem_MS6_less63,2)

elem_MS6_more63 = ReadTabletoXTS("metals_river_more63MS6.txt",header=T,sep = ",", format="%Y-%m-%d %H:%M",
                                 TimeZone="Etc/GMT-1") #Always use Etc/GMT-1 time zone!
head(elem_MS6_more63)
elem_MS6_more63 =elem_MS6_more63[,-c(1:4)]
elem_MS6_more63 =round(elem_MS6_more63,2)

# Example Elements and Turbidity
elem_less63MS6_Turb = na.omit(cbind(elem_MS6_less63,MS6))
View(elem_less63MS6_Turb)
start(elem_less63MS6_Turb)
end(elem_less63MS6_Turb)

DynPlot(MS6["2021-05/09"])
DynPlot(cbind(MS6["2021-08-23"],elem_MS6_less63$Pb_µg_g["2021-08-23"]))
DynPlot(cbind(MS6["2021-07-13"],elem_MS6_less63$Pb_µg_g["2021-07-13"],MS6_gs$TS.fein...63µm.["2021-07-13"]))



########################
# MS4
########################

## Elements in MS4: here we don't have elements measured in more than 63 microm 
elem_MS4_less63 = ReadTabletoXTS("metals_river_less63MS4.txt",header=T,sep = ",", format="%Y-%m-%d %H:%M",
                                 TimeZone="Etc/GMT-1") #Always use Etc/GMT-1 time zone!
head(elem_MS4_less63)
elem_MS4_less63 =elem_MS4_less63[,-c(1:4)]
elem_MS4_less63 =round(elem_MS4_less63,2)
View(elem_MS4_less63)

#Other functions in ISI.Toolbox that might be interesting for you:
# For checking the "help" just write ?NameFunction, e.g. ?CutTimeSeries() and hit enter
# CutTimeSeries()
# WriteXTStoTXT()