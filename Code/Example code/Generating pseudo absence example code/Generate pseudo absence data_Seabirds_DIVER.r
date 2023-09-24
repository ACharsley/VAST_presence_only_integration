##############################################################################################################################
## 
## This script generates pseudo-absence data for seabirds based on DIVER low level aerial survey data
##
##############################################################################################################################
rm(list=ls())

######## Define working directories
InputFiles = "F:/Seabird_analysis/Input"
SpatialGrids = "F:/Seabird_analysis/Shapefiles"
OutputFiles = "F:/Seabird_analysis/Output"

######## Load seabird presence data
setwd(InputFiles)
dat=read.csv("Seabirds_DIVER_data.csv",sep=";")
dim(dat)
names(dat)
plot(dat$Longitude,dat$Latitude) 
dat_3 <- dat

######## Generate pseudo-absence data for the DIVER low level bird survey and May, 
######## and concatenate presences and pseudo-absences for this monitoring program
#### Load the fine spatial grid for the U.S. Gulf of Mexico (GOM)
setwd(SpatialGrids)
gridGOM=read.csv("Spatial_grid_Seabirds_DIVER_5.csv",sep=";")
dim(gridGOM)
names(gridGOM)
plot(gridGOM$Longitude,gridGOM$Latitude) 

#### Select data 
dat_3_5 <- dat_3[dat_3$Month=="5",] 
dim(dat_3_5)
plot(dat_3_5$Longitude,dat_3_5$Latitude) 
nb_datapoints <- dim(dat_3_5)[1]

#### Generate pseudo-absence data for the DIVER low level bird survey #3 and May
pseudo_absence <-c()
pseudo_absence$ID <- sample(gridGOM$ID, (nb_datapoints*10), replace = TRUE)
pseudo_absence$Longitude=pseudo_absence$Latitude=c()
for (i in 1:length(pseudo_absence$ID)){
	pseudo_absence$Longitude[i] <- gridGOM$Longitude[gridGOM$ID%in%pseudo_absence$ID[i]]
	pseudo_absence$Latitude[i] <- gridGOM$Latitude[gridGOM$ID%in%pseudo_absence$ID[i]]
}
plot(pseudo_absence$Longitude,pseudo_absence$Latitude) 

#### Concatenate presences and pseudo-absences for the DIVER low level bird survey #3 and May
finaldata_3_5<-c()
finaldata_3_5$Longitude<-c(dat_3_5$Longitude,pseudo_absence$Longitude)
finaldata_3_5$Latitude<-c(dat_3_5$Latitude,pseudo_absence$Latitude)
finaldata_3_5$Month<-rep(5,length(dat_3_5$Longitude))
finaldata_3_5$Monitoring_program<-rep(2,length(dat_3_5$Longitude))
finaldata_3_5$Presence<-c(rep(1,length(dat_3_5$Longitude)),rep(0,length(pseudo_absence$Longitude)))
plot(finaldata_3_5$Longitude,finaldata_3_5$Latitude) 
finaldata_3_5<-as.data.frame(finaldata_3_5)

######## Generate pseudo-absence data for the DIVER low level bird survey and June, 
######## and concatenate presences and pseudo-absences for this monitoring program
#### Load the fine spatial grid for the U.S. Gulf of Mexico (GOM)
setwd(SpatialGrids)
gridGOM=read.csv("Spatial_grid_Seabirds_DIVER_6.csv",sep=";")
dim(gridGOM)
names(gridGOM)
plot(gridGOM$Longitude,gridGOM$Latitude) 

#### Select data 
dat_3_6 <- dat_3[dat_3$Month=="6",] 
dim(dat_3_6)
plot(dat_3_6$Longitude,dat_3_6$Latitude) 
nb_datapoints <- dim(dat_3_6)[1]

#### Generate pseudo-absence data for the DIVER low level bird survey and June
pseudo_absence <-c()
pseudo_absence$ID <- sample(gridGOM$ID, (nb_datapoints*10), replace = TRUE)
pseudo_absence$Longitude=pseudo_absence$Latitude=c()
for (i in 1:length(pseudo_absence$ID)){
	pseudo_absence$Longitude[i] <- gridGOM$Longitude[gridGOM$ID%in%pseudo_absence$ID[i]]
	pseudo_absence$Latitude[i] <- gridGOM$Latitude[gridGOM$ID%in%pseudo_absence$ID[i]]
}
plot(pseudo_absence$Longitude,pseudo_absence$Latitude) 

#### Concatenate presences and pseudo-absences for the DIVER low level bird survey and June
finaldata_3_6<-c()
finaldata_3_6$Longitude<-c(dat_3_6$Longitude,pseudo_absence$Longitude)
finaldata_3_6$Latitude<-c(dat_3_6$Latitude,pseudo_absence$Latitude)
finaldata_3_6$Month<-rep(6,length(dat_3_6$Longitude))
finaldata_3_6$Monitoring_program<-rep(2,length(dat_3_6$Longitude))
finaldata_3_6$Presence<-c(rep(1,length(dat_3_6$Longitude)),rep(0,length(pseudo_absence$Longitude)))
plot(finaldata_3_6$Longitude,finaldata_3_6$Latitude) 
finaldata_3_6<-as.data.frame(finaldata_3_6)

######## Generate pseudo-absence data for the DIVER low level bird survey and July, 
######## and concatenate presences and pseudo-absences for this monitoring program
#### Load the fine spatial grid for the U.S. Gulf of Mexico (GOM)
setwd(SpatialGrids)
gridGOM=read.csv("Spatial_grid_Seabirds_DIVER_7.csv",sep=";")
dim(gridGOM)
names(gridGOM)
plot(gridGOM$Longitude,gridGOM$Latitude) 

#### Select data 
dat_3_7 <- dat_3[dat_3$Month=="7",] 
dim(dat_3_7)
plot(dat_3_7$Longitude,dat_3_7$Latitude) 
nb_datapoints <- dim(dat_3_7)[1]

#### Generate pseudo-absence data for monitoring program #3 and July
pseudo_absence <-c()
pseudo_absence$ID <- sample(gridGOM$ID, (nb_datapoints*10), replace = TRUE)
pseudo_absence$Longitude=pseudo_absence$Latitude=c()
for (i in 1:length(pseudo_absence$ID)){
	pseudo_absence$Longitude[i] <- gridGOM$Longitude[gridGOM$ID%in%pseudo_absence$ID[i]]
	pseudo_absence$Latitude[i] <- gridGOM$Latitude[gridGOM$ID%in%pseudo_absence$ID[i]]
}
plot(pseudo_absence$Longitude,pseudo_absence$Latitude) 

#### Concatenate presences and pseudo-absences for the DIVER low level bird survey and July
finaldata_3_7<-c()
finaldata_3_7$Longitude<-c(dat_3_7$Longitude,pseudo_absence$Longitude)
finaldata_3_7$Latitude<-c(dat_3_7$Latitude,pseudo_absence$Latitude)
finaldata_3_7$Month<-rep(7,length(dat_3_7$Longitude))
finaldata_3_7$Monitoring_program<-rep(2,length(dat_3_7$Longitude))
finaldata_3_7$Presence<-c(rep(1,length(dat_3_7$Longitude)),rep(0,length(pseudo_absence$Longitude)))
plot(finaldata_3_7$Longitude,finaldata_3_7$Latitude) 
finaldata_3_7<-as.data.frame(finaldata_3_7)

######## Generate pseudo-absence data for the DIVER low level bird survey and August, 
######## and concatenate presences and pseudo-absences for this monitoring program
#### Load the fine spatial grid for the U.S. Gulf of Mexico (GOM)
setwd(SpatialGrids)
gridGOM=read.csv("Spatial_grid_Seabirds_DIVER_8.csv",sep=";")
dim(gridGOM)
names(gridGOM)
plot(gridGOM$Longitude,gridGOM$Latitude) 

#### Select data 
dat_3_8 <- dat_3[dat_3$Month=="8",] 
dim(dat_3_8)
plot(dat_3_8$Longitude,dat_3_8$Latitude) 
nb_datapoints <- dim(dat_3_8)[1]

#### Generate pseudo-absence data for the DIVER low level bird survey and August
pseudo_absence <-c()
pseudo_absence$ID <- sample(gridGOM$ID, (nb_datapoints*10), replace = TRUE)
pseudo_absence$Longitude=pseudo_absence$Latitude=c()
for (i in 1:length(pseudo_absence$ID)){
	pseudo_absence$Longitude[i] <- gridGOM$Longitude[gridGOM$ID%in%pseudo_absence$ID[i]]
	pseudo_absence$Latitude[i] <- gridGOM$Latitude[gridGOM$ID%in%pseudo_absence$ID[i]]
}
plot(pseudo_absence$Longitude,pseudo_absence$Latitude) 

#### Concatenate presences and pseudo-absences for the DIVER low level bird survey and August
finaldata_3_8<-c()
finaldata_3_8$Longitude<-c(dat_3_8$Longitude,pseudo_absence$Longitude)
finaldata_3_8$Latitude<-c(dat_3_8$Latitude,pseudo_absence$Latitude)
finaldata_3_8$Month<-rep(8,length(dat_3_8$Longitude))
finaldata_3_8$Monitoring_program<-rep(2,length(dat_3_8$Longitude))
finaldata_3_8$Presence<-c(rep(1,length(dat_3_8$Longitude)),rep(0,length(pseudo_absence$Longitude)))
plot(finaldata_3_8$Longitude,finaldata_3_8$Latitude) 
finaldata_3_8<-as.data.frame(finaldata_3_8)

######## Generate pseudo-absence data for the DIVER low level bird survey and September, 
######## and concatenate presences and pseudo-absences for this monitoring program
#### Load the fine spatial grid for the U.S. Gulf of Mexico (GOM)
setwd(SpatialGrids)
gridGOM=read.csv("Spatial_grid_Seabirds_DIVER_9.csv",sep=";")
dim(gridGOM)
names(gridGOM)
plot(gridGOM$Longitude,gridGOM$Latitude) 

#### Select data 
dat_3_9 <- dat_3[dat_3$Month=="9",] 
dim(dat_3_9)
plot(dat_3_9$Longitude,dat_3_9$Latitude) 
nb_datapoints <- dim(dat_3_9)[1]

#### Generate pseudo-absence data for the DIVER low level bird survey and September
pseudo_absence <-c()
pseudo_absence$ID <- sample(gridGOM$ID, (nb_datapoints*10), replace = TRUE)
pseudo_absence$Longitude=pseudo_absence$Latitude=c()
for (i in 1:length(pseudo_absence$ID)){
	pseudo_absence$Longitude[i] <- gridGOM$Longitude[gridGOM$ID%in%pseudo_absence$ID[i]]
	pseudo_absence$Latitude[i] <- gridGOM$Latitude[gridGOM$ID%in%pseudo_absence$ID[i]]
}
plot(pseudo_absence$Longitude,pseudo_absence$Latitude) 

#### Concatenate presences and pseudo-absences for the DIVER low level bird survey and September
finaldata_3_9<-c()
finaldata_3_9$Longitude<-c(dat_3_9$Longitude,pseudo_absence$Longitude)
finaldata_3_9$Latitude<-c(dat_3_9$Latitude,pseudo_absence$Latitude)
finaldata_3_9$Month<-rep(9,length(dat_3_9$Longitude))
finaldata_3_9$Monitoring_program<-rep(2,length(dat_3_9$Longitude))
finaldata_3_9$Presence<-c(rep(1,length(dat_3_9$Longitude)),rep(0,length(pseudo_absence$Longitude)))
plot(finaldata_3_9$Longitude,finaldata_3_9$Latitude) 
finaldata_3_9<-as.data.frame(finaldata_3_9)

######## Generate pseudo-absence data for the DIVER low level bird survey and October, 
######## and concatenate presences and pseudo-absences for this monitoring program
#### Load the fine spatial grid for the U.S. Gulf of Mexico (GOM)
setwd(SpatialGrids)
gridGOM=read.csv("Spatial_grid_Seabirds_DIVER_10.csv",sep=";")
dim(gridGOM)
names(gridGOM)
plot(gridGOM$Longitude,gridGOM$Latitude) 

#### Select data 
dat_3_10 <- dat_3[dat_3$Month=="10",] 
dim(dat_3_10)
plot(dat_3_10$Longitude,dat_3_10$Latitude) 
nb_datapoints <- dim(dat_3_10)[1]

#### Generate pseudo-absence data for the DIVER low level bird survey and October
pseudo_absence <-c()
pseudo_absence$ID <- sample(gridGOM$ID, (nb_datapoints*10), replace = TRUE)
pseudo_absence$Longitude=pseudo_absence$Latitude=c()
for (i in 1:length(pseudo_absence$ID)){
	pseudo_absence$Longitude[i] <- gridGOM$Longitude[gridGOM$ID%in%pseudo_absence$ID[i]]
	pseudo_absence$Latitude[i] <- gridGOM$Latitude[gridGOM$ID%in%pseudo_absence$ID[i]]
}
plot(pseudo_absence$Longitude,pseudo_absence$Latitude) 

#### Concatenate presences and pseudo-absences for the DIVER low level bird survey and October
finaldata_3_10<-c()
finaldata_3_10$Longitude<-c(dat_3_10$Longitude,pseudo_absence$Longitude)
finaldata_3_10$Latitude<-c(dat_3_10$Latitude,pseudo_absence$Latitude)
finaldata_3_10$Month<-rep(10,length(dat_3_10$Longitude))
finaldata_3_10$Monitoring_program<-rep(2,length(dat_3_10$Longitude))
finaldata_3_10$Presence<-c(rep(1,length(dat_3_10$Longitude)),rep(0,length(pseudo_absence$Longitude)))
plot(finaldata_3_10$Longitude,finaldata_3_10$Latitude) 
finaldata_3_10<-as.data.frame(finaldata_3_10)

######## Generate pseudo-absence data for the DIVER low level bird survey and November, 
######## and concatenate presences and pseudo-absences for this monitoring program
#### Load the fine spatial grid for the U.S. Gulf of Mexico (GOM)
setwd(SpatialGrids)
gridGOM=read.csv("Spatial_grid_Seabirds_DIVER_11.csv",sep=";")
dim(gridGOM)
names(gridGOM)
plot(gridGOM$Longitude,gridGOM$Latitude) 

#### Select data 
dat_3_11 <- dat_3[dat_3$Month=="11",] 
dim(dat_3_11)
plot(dat_3_11$Longitude,dat_3_11$Latitude) 
nb_datapoints <- dim(dat_3_11)[1]

#### Generate pseudo-absence data for the DIVER low level bird survey and November
pseudo_absence <-c()
pseudo_absence$ID <- sample(gridGOM$ID, (nb_datapoints*10), replace = TRUE)
pseudo_absence$Longitude=pseudo_absence$Latitude=c()
for (i in 1:length(pseudo_absence$ID)){
	pseudo_absence$Longitude[i] <- gridGOM$Longitude[gridGOM$ID%in%pseudo_absence$ID[i]]
	pseudo_absence$Latitude[i] <- gridGOM$Latitude[gridGOM$ID%in%pseudo_absence$ID[i]]
}
plot(pseudo_absence$Longitude,pseudo_absence$Latitude) 

#### Concatenate presences and pseudo-absences for the DIVER low level bird survey and November
finaldata_3_11<-c()
finaldata_3_11$Longitude<-c(dat_3_11$Longitude,pseudo_absence$Longitude)
finaldata_3_11$Latitude<-c(dat_3_11$Latitude,pseudo_absence$Latitude)
finaldata_3_11$Month<-rep(11,length(dat_3_11$Longitude))
finaldata_3_11$Monitoring_program<-rep(2,length(dat_3_11$Longitude))
finaldata_3_11$Presence<-c(rep(1,length(dat_3_11$Longitude)),rep(0,length(pseudo_absence$Longitude)))
plot(finaldata_3_11$Longitude,finaldata_3_11$Latitude) 
finaldata_3_11<-as.data.frame(finaldata_3_11)

######## Generate pseudo-absence data for the DIVER low level bird survey and December, 
######## and concatenate presences and pseudo-absences for this monitoring program
#### Load the fine spatial grid for the U.S. Gulf of Mexico (GOM)
setwd(SpatialGrids)
gridGOM=read.csv("Spatial_grid_Seabirds_DIVER_12.csv",sep=";")
dim(gridGOM)
names(gridGOM)
plot(gridGOM$Longitude,gridGOM$Latitude) 

#### Select data 
dat_3_12 <- dat_3[dat_3$Month=="12",] 
dim(dat_3_12)
plot(dat_3_12$Longitude,dat_3_12$Latitude) 
nb_datapoints <- dim(dat_3_12)[1]

#### Generate pseudo-absence data for the DIVER low level bird survey and December
pseudo_absence <-c()
pseudo_absence$ID <- sample(gridGOM$ID, (nb_datapoints*10), replace = TRUE)
pseudo_absence$Longitude=pseudo_absence$Latitude=c()
for (i in 1:length(pseudo_absence$ID)){
	pseudo_absence$Longitude[i] <- gridGOM$Longitude[gridGOM$ID%in%pseudo_absence$ID[i]]
	pseudo_absence$Latitude[i] <- gridGOM$Latitude[gridGOM$ID%in%pseudo_absence$ID[i]]
}
plot(pseudo_absence$Longitude,pseudo_absence$Latitude) 

#### Concatenate presences and pseudo-absences for the DIVER low level bird survey and December
finaldata_3_12<-c()
finaldata_3_12$Longitude<-c(dat_3_12$Longitude,pseudo_absence$Longitude)
finaldata_3_12$Latitude<-c(dat_3_12$Latitude,pseudo_absence$Latitude)
finaldata_3_12$Month<-rep(12,length(dat_3_12$Longitude))
finaldata_3_12$Monitoring_program<-rep(2,length(dat_3_12$Longitude))
finaldata_3_12$Presence<-c(rep(1,length(dat_3_12$Longitude)),rep(0,length(pseudo_absence$Longitude)))
plot(finaldata_3_12$Longitude,finaldata_3_12$Latitude) 
finaldata_3_12<-as.data.frame(finaldata_3_12)

######## Save the presence-absence datasets that you generated in a .csv file
finaldata <- rbind(finaldata_3_5,finaldata_3_6,finaldata_3_7,finaldata_3_8,finaldata_3_9,
	finaldata_3_10,finaldata_3_11,finaldata_3_12)
dim(finaldata)
names(finaldata)
setwd(OutputFiles)
write.csv(finaldata,'PresenceAbsence_Seabirds_DIVER.csv',row.names=F)

