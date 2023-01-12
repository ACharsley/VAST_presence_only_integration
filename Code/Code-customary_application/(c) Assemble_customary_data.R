###################################################
##              Create customary data            ##
##                                               ##
##                Anthony Charsley               ##
##                  October 2022                 ##
###################################################

# This code formats the customary data obtained from Maori partners 
# and attaches it to the NZFFD Waitaki data.

###################################################

rm(list=ls())


##############
#  Packages  #
##############

library(tidyverse)


#################
#  Directories  #
#################

data_waitaki_dir <- "./Data/Waitaki"
fig_dir <- "./Data/Figures"
raw_data <- "./Data/raw_data"


###################
#  Load datasets  #
###################

##Network
network <- readRDS(file.path(data_waitaki_dir, "Waitaki_network.rds"))

network_to_join <- network %>% 
  select(CatName, nzsegment, parent_s, child_s, dist_s, Lon, Lat)

##NZFFD observations
NZFFD_data <- readRDS(file.path(data_waitaki_dir, "Waitaki_NZFFD_obs.rds"))

##Habitat data
hab <- readRDS(file.path(data_waitaki_dir, "Waitaki_REC_covs.rds"))


###########################
#   Load customary data   #
###########################

#Load data and format
cust_data <- read_csv(file.path(raw_data, "FINAL customary fisheries data for Waitaki.csv"))
cust_data <- cust_data %>% 
  rename("nzsegment"=`nzsegment number (if multiple numbers best matches then do these on different rows)`,
         "Data_source"=`Data source`, "Data_value"=Longfin) %>%
  select(nzsegment, Year, Data_source, Data_value) %>%
  filter(!is.na(nzsegment), !is.na(Data_value))


table(cust_data$Data_value) #Check the levels of p/a data
cust_data$Data_value <- ifelse(cust_data$Data_value=="Present", 1, 0) #Transform to binary

table(cust_data$Data_value, cust_data$Year) #table of data across time
table(cust_data$Data_value, cust_data$Data_source) #table of data across the source data


#cust_data <- cust_data %>% filter(Year==1880) #Leave this out as I will use all customary data in simulations


#I'll make the assumption that all present-day customary catch has similar catchability
#and that all historical catch (1880) have similar catchability.

cust_data$Fishmethod <- ifelse(cust_data$Data_source=="Ngai Tahu pre-1880 transcripts", "Historical_hinaki", "Presentday_fyke")

table(cust_data$Data_value, cust_data$Fishmethod) #table of data across the fishing method data

##   Join network   ##
cust_data <- inner_join(cust_data, network_to_join, by="nzsegment")

cust_data <- cust_data %>%
  rename("parent_i"=parent_s, "child_i"=child_s, "dist_i"=dist_s) %>%
  mutate(Data_type="Customary")

table(cust_data$Data_value, cust_data$Year) #table of data across time


##########
#  Save  #
##########

saveRDS(cust_data, file.path(data_waitaki_dir, "Waitaki_NZFFD_cust_obs.rds"))

