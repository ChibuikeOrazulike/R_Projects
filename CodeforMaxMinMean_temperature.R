require(tidyverse)
require(lubridate)
library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)
library(RNetCDF)
library(dplyr)
#====================================SETTING THE WORKING DIRECTORY AND IMPORTING THE FILE=================================================
setwd("D:/DESCARGAS/STUDY_PROJECT_GERMANY/ERA5-Land") #SET to a folder where you are working on
Temp <- nc_open("Temp_2014-2022.nc") #ASSIGN the netcdf file for the year that you downloaded for user in the analysis
#It is better that you rename all the downloaded netcdf file, for example as "Temp_2011-2021.nc" for years 2011-2021, so that you can easily change only the date 

#====================================CHECKING THE COORDINATE AND EXTRACTING THE FILE==============================================
lat <- ncvar_get(Temp, "latitude") #dont change
lon <- ncvar_get(Temp, "longitude") #dont change
time <- ncvar_get(Temp, "time") #dont change
#to see the dimension of variables
dim(time)
dim(lat)
dim(lon)
#The above operation is not compulsory but it will help you you to visualize
Temp.array <- ncvar_get(Temp, "t2m")
Temp.slice <- Temp.array [11,11, ]#SLICE/EXTRACT the variables (TEMPERATURE) for this array. 3 is array for lon and 2 is array for latitude
#The index code above[3,2, ] was got from panoply, please check your panoply to confirm. This is very important to get the exact coordinate

#====================================PUTTING THE EXTRACTED DATA IN THE DATA FRAME AND ARRANGING IT==============================================
y <- data.frame(Temp.slice)#dont change
colnames(y) <- c("Temp")#dont change
y$Celcius <- round((y$Temp-273.15),2)#dont change(Here I converted the temperature to Celcius, the format recognised by Aquacrop)
y$Temp <- NULL #dont change(here I deleted the column in Kelvin and left with only converted celcius value)

y$Date <- seq(ymd_hm("2014-01-01 00:00"), ymd_hm("2022-08-01 00:00"), by = "hours")
# For the Above, "CHANGE" to the year that you are working on. In this line, I used netcdf file starting from "2011-01-01 00:00" and ended with "2021-12-31 23:00"
#Just change only the dates above

#====================================GETTING THE MEAN, MAX AND MIN TEMPERATURE==============================================
Tdaily_mean <- y %>% group_by(Day = as.Date(Date)) %>% dplyr::summarise(mean_value = round(mean(Celcius),2))#dont change
Tdaily_max <- y %>% group_by(Day = as.Date(Date)) %>% dplyr::summarise(max_value = max(Celcius))#dont change
Tdaily_min  <- y %>% group_by(Day = as.Date(Date)) %>% dplyr::summarise(min_value = min(Celcius))#dont change
write.csv(Tdaily_mean, file = "Tdaily_mean_2014-2022.csv")#RENAME properly to the year( In this case, 2011-2021)oNLY CHANGE THE YEAR
write.csv(Tdaily_max, file = "Tdaily_max_2014-2022.csv")#RENAME properly to the year( In this case, 2011-2021)oNLY CHANGE THE YEAR
write.csv(Tdaily_min, file = "Tdaily_min_2014-2022.csv")#RENAME properly to the year( In this case, 2011-2021)oNLY CHANGE THE YEAR

#GO TO THE HOME DIRECTORY AND VIEW THE FILE. DO THE SAME FOR OTHER YEARS. 
##I WILL SHARE THE SCRIPT WHERE I JOINED THE DIFFERNT TIME SERIES FOR THE ENTIRE YEARS INTO ONE TOMORROW. I NEED TO PUT COMMENTS ON THEM TOO. 


#====================================JOINING THE YEARS TOGETHER==============================================

Allfilesmean <- list.files(path = "D:/DESCARGAS/STUDY_PROJECT_GERMANY/ERA5-Land", pattern = "Tdaily_mean", full.names =TRUE, recursive = FALSE)#This searches for all files called Tdaily.mean in your directory
Total_daily_mean <- do.call(rbind, lapply(Allfilesmean, function(x) read.csv(x, stringsAsFactors = FALSE)))#This mergers the Daily mean Temperatures for all the years

Allfilesmax <- list.files(path = "D:/DESCARGAS/STUDY_PROJECT_GERMANY/ERA5-Land", pattern = "Tdaily_max", full.names =TRUE, recursive = FALSE)#This searches for all files called Tdaily.max in your directory
Total_daily_max <- do.call(rbind, lapply(Allfilesmax, function(x) read.csv(x, stringsAsFactors = FALSE)))#This mergers the Daily max Temperatures for all the years

Allfilesmin <- list.files(path = "D:/DESCARGAS/STUDY_PROJECT_GERMANY/ERA5-Land", pattern = "Tdaily_min", full.names =TRUE, recursive = FALSE)#This searches for all files called Tdaily.min in your directory
Total_daily_min <- do.call(rbind, lapply(Allfilesmin, function(x) read.csv(x, stringsAsFactors = FALSE)))#This mergers the Daily min Temperatures for all the years

#====================================SAVING THE MERGED TEMPERATURE DAT==============================================
write.csv(Total_daily_mean, file = "MeanDailyTemp_1981-2022.csv")#RENAME properly to the year( In this case, 2011-2021)oNLY CHANGE THE YEAR
write.csv(Total_daily_max, file = "MaxDailyTemp_max_1981-2022.csv")#RENAME properly to the year( In this case, 2011-2021)oNLY CHANGE THE YEAR
write.csv(Total_daily_min, file = "MaxDailyTemp_1981-2022.csv")#RENAME properly to the year( In this case, 2011-2021)oNLY CHANGE THE YEAR

#====================================MAKING THE BOXPLOT FOR EACH OF THE YEAR IN THE DATA==============================================

Total_daily_mean$Year <- year(ymd(Total_daily_mean$Day))
Total_daily_mean%>% ggplot(aes(factor(Year), mean_value)) +
  geom_boxplot() 








