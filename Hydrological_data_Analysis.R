getwd()
setwd("D:/Shortcourse/TASK1")
library(ISI.Toolbox)

csv.files <- list.files(path = "D:/Shortcourse/TASK1", pattern = "tas_day_MPI-ESM-MR_rcp85", full.names =TRUE, recursive = FALSE)
P1 <- read.table("D:/Shortcourse/TASK1/tas_day_MPI-ESM-MR_rcp85_r1i1P1_19760101-20051231.csv",
                        header = T, sep = ",", dec = ".")
colnames(P1) <- c("Date", "Temp")
P1$Date <-strptime(P1$Date, format = "%Y-%m-%d") 
head(P1, 10)
P1$Corrected <- round(P1$Temp - 273.15)

P2 <- read.table("D:/Shortcourse/TASK1/tas_day_MPI-ESM-MR_rcp85_r1i1P1_20310101-20601231.csv",
                 header = T, sep = ",", dec = ".")
colnames(P2) <- c("Date", "Temp")
P2$Date <-strptime(P2$Date, format = "%Y-%m-%d") 
P2$Corrected <- round(P2$Temp - 273.15)


P3 <- read.table("D:/Shortcourse/TASK1/tas_day_MPI-ESM-MR_rcp85_r1i1P1_20710101-21001231.csv",
                 header = T, sep = ",", dec = ".")
colnames(P3) <- c("Date", "Temp")
P3$Date <-strptime(P3$Date, format = "%Y-%m-%d") 
head(P3, 10)
P3$Corrected <- round(P3$Temp - 273.15)
summary(P3)

listed_data <- list(Period1 = P1, Period2= P2, Period3 = P3)
str(listed_data)
head(listed_data$Period1)

# =========================PLOTTING THE GRAPHS DIFFERENTLY===============================================================
par(mfrow=(c(1,3)))
plot(y =listed_data$Period1$Corrected, x= listed_data$Period1$Date, type = "l",col = "red", xlab = "Dates",
     ylab = "Temp", main = "Historical")

plot(listed_data$Period2$Corrected, x= listed_data$Period2$Date, type = "l",col = "blue", xlab = "Dates",
     ylab = "Temp", main = "Period 1")
plot(listed_data$Period3$Corrected, x= listed_data$Period3$Date, type = "l",col = "black", xlab = "Dates",
     ylab = "Temp", main = "Perio 2")
# =========================PLOTTING ON THE SAME GRAPH===============================================================
plot(listed_data$Period1$Corrected, type = "l",col = "red", xlab = "Dates",
     ylab = "Temp", main = "TAS")
lines(listed_data$Period2$Corrected, type = "l", col = "blue", pch=10, cex=3)
lines(listed_data$Period3$Corrected, type = "l", col = "black", pch=10, cex=3)

#===========================PLOTTING ON THE SAME GRAPH==============================================================
par(mfrow=(c(1,3)))
boxplot(P1$Corrected)
boxplot(P2$Corrected)
boxplot(P3$Corrected)

#===========================PLOTTING THE HISTOGRAM==============================================================
par(mfrow=(c(1,3)))
hist(P1$Corrected, breaks = 25)
hist(P2$Corrected, breaks = 25)
hist(P3$Corrected, breaks = 25)

#============================================Using xts tool======================================================
library(xts)
P1.xts = xts(x = P1, order.by = P1$Date)
P1.xts
?xts
P2.xts = xts(x = P2, order.by = P2$Date)
P3.xts = xts(x = P3, order.by = P3$Date)

#================================Aggregation of the monthly data=========================================================
library(xts)
library(lubridate)
monthly.P1 <- round(apply.monthly(P1.xts$Corrected, mean),3)
head(monthly.P1)
monthly.P2 <- round(apply.monthly(P2.xts$Corrected, mean),3)
head(monthly.P2)
monthly.P3 <- round(apply.monthly(P3.xts$Corrected, mean),3)
head(monthly.P3)
DynPlot(cbind(monthly.P1$Corrected, monthly.P2$Corrected,  monthly.P3$Corrected), Axis = c(1,1,2), Labels=c("Historical","Period 1", "Period 2"), colset = c(1,2,3)) #change to the appropriate lysimeter column

#================================Aggregation of the weekly data=========================================================

Weekly.P1 <- round(apply.weekly(P1.xts$Corrected, mean),3)
head(Weekly.P1)
Weekly.P2 <- round(apply.weekly(P2.xts$Corrected, mean),3)
head(Weekly.P2)
Weekly.P3 <- round(apply.weekly(P3.xts$Corrected, mean),3)
head(Weekly.P3)
DynPlot(cbind(Weekly.P1$Corrected, Weekly.P2$Corrected,  Weekly.P3$Corrected), Axis = c(1,1,2), Labels=c("Historical","Period 1", "Period 3"), colset = c(1,2,3)) #change to the appropriate lysimeter column
#================================Aggregation of the quarterly data=========================================================

Quarterly.P1 <- round(apply.quarterly(P1.xts$Corrected, mean),3)
head(Quarterly.P1)
Quarterly.P2 <- round(apply.quarterly(P2.xts$Corrected, mean),3)
head(Quarterly.P2)
Quarterly.P3 <- round(apply.quarterly(P3.xts$Corrected, mean),3)
head(Quarterly.P3)
DynPlot(cbind(Quarterly.P1$Corrected, Quarterly.P2$Corrected,  Quarterly.P3$Corrected), Axis = c(1,1,2), Labels=c("Historical","Period 1", "Period 3"), colset = c(1,2,3)) 

#================================Aggregation of the yearly data=========================================================
Yearly.P1 <- round(apply.yearly(P1.xts$Corrected, mean),3)
head(Yearly.P1)
Yearly.P2 <- round(apply.yearly(P2.xts$Corrected, mean),3)
head(Yearly.P2)
Yearly.P3 <- round(apply.yearly(P3.xts$Corrected, mean),3)
head(Yearly.P3)
DynPlot(cbind(Yearly.P1$Corrected, Yearly.P2$Corrected,  Yearly.P3$Corrected), Axis = c(1,1,2), Labels=c("Historical","Period 1", "Period 3"), colset = c(1,2,3))

#================================t-test statistics======================================================================
P1_P2stat <-  t.test(P1$Corrected, P2$Corrected, conf.level = 0.95)
P1_P2stat
P1_P3stat <- t.test(P1$Corrected, P2$Corrected, conf.level = 0.95)
P1_P3stat
P2_P3stat <- t.test(P2$Corrected, P3$Corrected, conf.level = 0.95)
P2_P3stat

#================================ANOVA TEST=============================================================================
ggplot(P1, aes( x = Date, y = Corrected)) +
  geom_point(color="firebrick2") +
  labs(x = "GDP per capita ($)",
       y = "Life expectancy (years)",
       color= "Continent",
       size = "Population",
       title = "GDP vs Life expectancy") +
  geom_smooth(method = "lm", color= "purple2") +
  scale_x_log10() +
  theme_light(base_size = 12)



                                                          #Exercise2(Based on periods)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(terra)

Net_files <- list.files("D:/Shortcourse/Exercise_II/tas/rcp85", full.names =TRUE, recursive = FALSE)
Net_files
Pe1 <- "D:/Shortcourse/Exercise_II/tas/rcp85/tas_day_MPI-ESM-MR_historical_r1i1p1_19760101-20051231_DE.nc"
Pe2 <- "D:/Shortcourse/Exercise_II/tas/rcp85/tas_day_MPI-ESM-MR_rcp85_r1i1p1_20310101-20601231_DE.nc"
Pe3 <- "D:/Shortcourse/Exercise_II/tas/rcp85/tas_day_MPI-ESM-MR_rcp85_r1i1p1_20710101-21001231_DE.nc"
#======================================combining the raster files========de=======================================================
library(terra)
Rast_file <- terra::rast(c(Pe1, Pe2, Pe3))
plot(Rast_file)
Rast_file
#================================TERRA AS ARRAY=============================================================================
Raster_array <- terra::as.array(Rast_file)
head(Raster_array, 6)
#===============================CONVERTING TO DEGREE CELCIUS=============================================================================
Raster_array <- terra::as.array(Rast_file)
array_corrected <- round((Raster_array-273.15),2)
array_corrected
plot(array_corrected)
w <- c(mean(array_corrected[ , ,1:10958]), mean(array_corrected[ , ,10959:21917]), mean(array_corrected[ , ,21917:32873]))
w 
#================================Apply mean on the array data=============================================================================

P1_mean <- apply(array_corrected[ , ,1:10958], MARGIN = c(1,2), mean, na.rm = TRUE)
P2_mean <- apply(array_corrected[ , ,10959:21917], MARGIN = c(1,2), mean, na.rm = TRUE)
P3_mean <- apply(array_corrected[ , ,21917:32873], MARGIN = c(1,2), mean, na.rm = TRUE)
#=================================Converting back to raster=============================================================================
P1_raster <- terra::rast(P1_mean, extent = Rast_file, crs = "WGS84")
P2_raster <- terra::rast(P2_mean, extent = Rast_file, crs = "WGS84")
P3_raster <- terra::rast(P3_mean, extent = Rast_file, crs = "WGS84")


P2_raster
par(mfrow=(c(2,2)))
plot(P1_raster, main = "Historical")
plot(P2_raster, main = "Near Future")
plot(P3_raster, main = "Far future")


                                                      #Exercise2 Based on seasons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(sf)
require(tidyverse)
require(raster)
Net_files <- list.files("D:/Shortcourse/Exercise_II/tas/rcp85", full.names =TRUE, recursive = FALSE)
Net_files
Pe1 <- "D:/Shortcourse/Exercise_II/tas/rcp85/tas_day_MPI-ESM-MR_historical_r1i1p1_19760101-20051231_DE.nc"
Pe2 <- "D:/Shortcourse/Exercise_II/tas/rcp85/tas_day_MPI-ESM-MR_rcp85_r1i1p1_20310101-20601231_DE.nc"
Pe3 <- "D:/Shortcourse/Exercise_II/tas/rcp85/tas_day_MPI-ESM-MR_rcp85_r1i1p1_20710101-21001231_DE.nc"
#======================================combining the raster files========de=======================================================
library(terra)
Rast_file <- terra::rast(c(Pe1, Pe2, Pe3))
plot(Rast_file)
Rast_file
comb_sf <- terra::as.data.frame(Rast_file, xy = TRUE)
comb_sf
colnames(comb_sf) <- c("x", "y", as.character(time(Rast_file)))
head(comb_sf)
?pivot_longer
pivotted <- pivot_longer(comb_sf, cols = !c("x","y"), names_to = "date")
pivotted
colnames(comb_sf)
pivotted$celcius <- round(pivotted$value - 273.15, 2)
pivotted
pivotted$date <- as.Date(pivotted$date) 
pivotted
Firstquater <- pivotted %>% mutate(month=month(date)) %>% filter(month %in% c(12, 1, 2))
meanFirstQ <- Firstquater %>% group_by(x,y) %>% summarize(cell_value = mean(celcius))    

Secondquater <- pivotted %>% mutate(month=month(date)) %>% filter(month %in% c(3, 4, 5))
meanSecondQ <- Secondquater %>% group_by(x,y) %>% summarize(cell_value = mean(celcius)) 

Thirdquater <- pivotted %>% mutate(month=month(date)) %>% filter(month %in% c(6, 7, 8))
meanThirdQ <- Thirdquater %>% group_by(x,y) %>% summarize(cell_value = mean(celcius))

Fourthquater <- pivotted %>% mutate(month=month(date)) %>% filter(month %in% c(9, 10, 11))
meanfourthQ <- Fourthquater %>% group_by(x,y) %>% summarize(cell_value = mean(celcius)) 

#1st plot
firstplot<- ggplot(meanFirstQ)+
  geom_tile(mapping = aes(x,y,fill= cell_value)) +
  scale_fill_viridis_c(begin = 0,
                       end = 1)+
  coord_sf() +
  labs(x=NULL, y=NULL, fill="TAS.",
       title = "WINTER")
  theme_light(base_size = 8) 

#2nd Plot
  secondplot<- ggplot(meanSecondQ)+
    geom_tile(mapping = aes(x,y,fill= cell_value)) +
    scale_fill_viridis_c(begin = 0,
                         end = 1)+
    coord_sf() +
    labs(x=NULL, y=NULL, fill="TAS",
         title = "SPRING")
  theme_light(base_size = 8) 

#3rd Plot
  thirdplot<- ggplot(meanThirdQ)+
    geom_tile(mapping = aes(x,y,fill= cell_value)) +
    scale_fill_viridis_c(begin = 0,
                         end = 1)+
    coord_sf() +
    labs(x=NULL, y=NULL, fill="TAS",
         title = "SUMMER")
  theme_light(base_size = 8) 
  
#4th Plot
  fourthplot<- ggplot(meanfourthQ)+
    geom_tile(mapping = aes(x,y,fill= cell_value)) +
    scale_fill_viridis_c(begin = 0,
                         end = 1)+
    coord_sf() +
    labs(x=NULL, y=NULL, fill="TAS",
         title = "FALL")
  theme_light(base_size = 8) 
#combined plot
  library(ggpubr)
combplot <- ggarrange(firstplot, secondplot, thirdplot,fourthplot + 
                        font("x.text", size = 10),
                      ncol = 2, nrow = 2) 
combplot

                                                      #Exercise3
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(terra)

list.files("D:\\Shortcourse\\Exercise_III\\ta\\rcp85", full.names =TRUE, recursive = FALSE)
setwd("D:\\Shortcourse\\Exercise_III\\ta\\rcp85")
Per1 <- "D:\\Shortcourse\\Exercise_III\\ta\\rcp85\\ta_mean_MPI-ESM-MR_historical_r1i1p1_19760101-20051231.nc"
Per2 <- "D:\\Shortcourse\\Exercise_III\\ta\\rcp85\\ta_mean_MPI-ESM-MR_rcp85_r1i1p1_20310101-20601231.nc"
Per3 <- "D:\\Shortcourse\\Exercise_III\\ta\\rcp85\\ta_mean_MPI-ESM-MR_rcp85_r1i1p1_20710101-21001231.nc"


r.rast <- terra::rast(Per1)
r.rast
r.crds <- crds(r.rast)


indexlon <- which.min(abs(unique(r.crds[,1]) - 13.5782))
indexlat <- which.min(abs(unique(r.crds[,2]) - 50.9867))

r.rast <- round (r.rast - 273.15, digits = 3)
r.rast
Profile <- r.rast[indexlat, indexlon, ]
str(Profile)
Profile.frame <- terra::as.data.frame(Rast_file, xy = TRUE)
colnames(Profile.frame) <- c("x", "y", as.character(time(Rast_file)))
head(Profile.frame)
Pivoted.Pro <- pivot_longer(comb_sf, cols = !c("x","y"), names_to = "date")
Pivoted.Pro
colnames(comb_sf)














