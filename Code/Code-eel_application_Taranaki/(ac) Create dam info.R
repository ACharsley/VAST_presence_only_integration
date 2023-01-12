###################################
##  Create dam information data  ##
###################################


rm(list=ls())

library(tidyverse)
library(proj4)
library(readxl)


#####################
##   Directories   ##
#####################

data_dir <- "./Data"
raw_data_dir <- "./Data/raw_data"
data_taranaki_dir <- "./Data/Taranaki"
fig_dir <- file.path(data_taranaki_dir, "Figures")


#######################
##   Load dam data   ##
#######################

## Taranaki_dams_joined.xlsx is a dataset which I generated in ArcGIS.
## This data set was created by joining dam location/year data with barrier locations
## in the Taranaki region.
dam_data <- read_excel(file.path(data_taranaki_dir, "Taranaki_dams_joined.xlsx"))

colnames(dam_data)

dam_data_to_join <- dam_data %>% 
  select(Distance_km, CatName, nzsegment, child_s, parent_s, NZDAM_ID,
         NAME_OF_DAM, DATE) %>%
  mutate(CatName_ID = paste0(CatName, "_",NZDAM_ID)) #Ensures that a barrier isn't lost when incorrectly assigned to multiple catchments

#Rename DATE to Year_barrier_finished
dam_data_to_join <- dam_data_to_join %>% rename(Year_barrier_finished = DATE)

#########################
## Examine the repeats ##
#########################

# # Find the minimum distance between a dam and a barrier, and identify the dam ID
# damID_mindistance <- tapply(dam_data_to_join$Distance_km, dam_data_to_join$CatName_ID, min)
# 
# # Subset according to min distance and CatName_ID
# dam_data_to_join <- dam_data_to_join %>% 
#   filter((Distance_km %in% damID_mindistance) & (CatName_ID %in% names(damID_mindistance)))


# Find the maximum distance between a dam and a barrier, and identify the dam ID
tapply(dam_data_to_join$Distance_km, dam_data_to_join$CatName_ID, max)

## All barriers are close to a 'NZ Dam' ID. Therefore, I'll keep all the repeats and assume the 
## barrier is blocking/interferring with both waterways.


#########################
## Fix NAs in Dam date ##
#########################
#Years with NAs that information couldn't be found on was set to the median year


#Waiaua - Oaonui Stream_NZD367
dam_data_to_join$Year_barrier_finished[dam_data_to_join$CatName_ID == "Oaonui Stream_NZD367"] <- round(median(1978:2021))

#Timaru - Timaru Stream_NZD366
dam_data_to_join$Year_barrier_finished[dam_data_to_join$CatName_ID == "Timaru Stream_NZD366"] <- round(median(1978:2021))

#Timaru - Stony River (Hangatahua)_NZD366
dam_data_to_join$Year_barrier_finished[dam_data_to_join$CatName_ID == "Stony River (Hangatahua)_NZD366"] <- round(median(1978:2021))

# NA (no dam name) - Mimi River_NZD397
dam_data_to_join$Year_barrier_finished[dam_data_to_join$CatName_ID == "Mimi River_NZD397"] <- round(median(1978:2021))

#Waitara River - Waitara River_NZD370
dam_data_to_join$Year_barrier_finished[dam_data_to_join$CatName_ID == "Waitara River_NZD370"] <- 1924 #source: https://en.wikipedia.org/wiki/Motukawa_Power_Station

#Waitara - Waingongoro River_NZD370
dam_data_to_join$Year_barrier_finished[dam_data_to_join$CatName_ID == "Waingongoro River_NZD370"] <- 1903 #Source: https://www.scoop.co.nz/stories/BU2104/S00347/normanby-power-station-gets-green-light.htm



###################################
## Find segments upstream of dam ##
###################################


## Load network to extract habitat covariates
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))


dam_list <- list()

for(i in 1:nrow(dam_data_to_join)){
  
  print(paste0("Dam ", i))
  
  ind_dam_location_data <- network %>% filter(child_s %in% dam_data_to_join$child_s[i])
  
  next_upparent <- network %>% filter(parent_s %in% ind_dam_location_data$child_s)
  dam_and_up <- rbind(ind_dam_location_data, next_upparent)
  
  GO <- TRUE
  counter <- 0
  
  while(GO){ #Go until convergence
    
    counter <- counter + 1
    print(counter)
    
    next_upparent <- network %>% filter(parent_s %in% next_upparent$child_s)
    
    if(nrow(next_upparent)==0){GO <- FALSE}
    
    dam_and_up <- rbind(dam_and_up, next_upparent)
    
  }
  
  dam_and_up$Year_barrier_finished <- dam_data_to_join$Year_barrier_finished[i]
  
  dam_list[[i]] <- dam_and_up 
  
}

dam_data_full <- do.call(rbind, dam_list)

length(unique(dam_data_full$child_s)) == nrow(dam_data_full)


#####################################
##  Join dam data to network data  ##
#####################################

net_to_join <- network %>% 
  filter(!(child_s %in% dam_data_full$child_s)) %>%
  mutate(Year_barrier_finished=NA)

dam_data_final <- rbind(net_to_join, dam_data_full)


dd <- ggplot(dam_data_final) +
  geom_point(data = dam_data_final, aes(x = Lon, y = Lat, colour = !is.na(Year_barrier_finished)), alpha=0.6) +
  #geom_point(data = damhere, aes(x = long, y = lat, col="Barrier")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Areas affected by barriers to upstream movement \nTaranaki, NZ") + 
  guides(colour=guide_legend(title="Affected by barrier"))

ggsave(file.path(fig_dir, "Areas_affected_by_barriers.png"), dd, width=10, height = 10)

all(network$child_s %in% dam_data_final$child_s) #unique ID same in both so can overwrite save

## Save ##
saveRDS(dam_data_final, file.path(data_taranaki_dir, "Taranaki_network.rds"))






