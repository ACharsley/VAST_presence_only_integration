###################################################
##       Assemble NZFFD Waitaki catchment        ##
##            presence/absence data              ##
##                                               ##
##                Anthony Charsley               ##
##                  October 2022                 ##
###################################################

# This code assembles presence/absence 
# data from the NZFFD.

###################################################

rm(list=ls())


#################
#  Directories  #
#################

data_waitaki_dir <- "./Data/Waitaki"
raw_data <- "./Data/raw_data"


##############
#  Packages  #
##############

library(tidyverse)


#################################
#  Load NZFFD data and network  #
#################################

#Network
network <- readRDS(file.path(data_waitaki_dir, "Waitaki_network.rds"))
network_to_join <- network %>% filter(parent_s!=0)
network_to_join <- network_to_join %>% filter(FWENZ_isLake!=TRUE) #This takes out lakes

#Observations
#NZFFD <- read.csv(file.path(raw_data, "nzffdms.csv")) # Downloaded from https://nzffdms.niwa.co.nz/search on 7/10/22
NZFFD <- read.csv(file.path(raw_data, "data-1665108945016.csv")) # Received from Jane Robbins on 7/10/22

table(NZFFD$y[NZFFD$y>=1967], NZFFD$m[NZFFD$y>=1967])


########################################################
#  Sorting out NZFFD records into single rows/NZreach  #
########################################################

NZFFD <- NZFFD[-(which(is.na(NZFFD$y))),]     #checked these NA in the orginial NZFFD and they are true missing dates, so they are removed here
NZFFD <- NZFFD[-(which(NZFFD$nzreach <=9)),]   #removing records that never got assigned NZreach numbers

DATA <- NZFFD[!duplicated(NZFFD$card),]
SPECIES<-unique(NZFFD$spcode)
tmp<-split(NZFFD$spcode,NZFFD$card)

res <- lapply(tmp,function(x) match(SPECIES,x,nomatch=0)>0)
res <- do.call("rbind",res)
res <- data.frame(res)
names(res) <- SPECIES

DATA <- DATA[order(DATA$card),]
DATA<-cbind(DATA,res)

nrow(DATA)
length(unique(DATA$card))

rm(tmp,res)


##############
#  Join REC  #
##############

obs <- left_join(DATA, network_to_join, by="nzsegment")
#obs <- inner_join(DATA, network_to_join, by="nzsegment") #I THINK IT SHOULD BE THIS CODE INSTEAD
obs <- obs[(grepl("aitaki", obs$CatName)),] #These are the nzsegments in the Waitaki Catchment

obs <- obs %>% select(nzsegment, Lat, Lon, child_s, parent_s, dist_s, y, catchname, org, fishmeth, effort, pass, angdie) %>% #All fish species
  rename("child_i"=child_s, "parent_i"=parent_s, "dist_i"=dist_s, "Year"=y) #%>%
  #filter(Year >= 1967) #To capture only data from 1967 to present

colSums(is.na(obs))


###########################
## Fixing fishing methods
###########################

table(obs$fishmeth, useNA = "ifany")

fishingdat <- read.csv(file.path(raw_data, "Table of fishingmethods and factors.csv"))

obs <- left_join(obs, fishingdat, by=c("fishmeth"="NZFFD.abbreviation"))
table(obs$Method.groupings, useNA = "ifany")

obs <- obs %>%
  select(-c(fishmeth, Method.Description)) %>% 
  rename("fishmeth"=Method.groupings) 

table(NZFFD_data$angdie, NZFFD_data$fishmeth)
#Visual method won't work


# Keep only ef, net, trap and visual
obs <- obs %>% 
  filter(fishmeth=="Electric fishing" | fishmeth=="Net" | fishmeth=="Trap" | fishmeth=="Visual")



#########################
## Fixing organisations
#########################

table(obs$org, useNA = "ifany")

orgdat <- read.csv(file.path(raw_data, "organisation table and groupings.csv"))

obs <- left_join(obs, orgdat, by=c("org"="abbreviation"))
table(obs$grouping, useNA = "ifany")

obs <- obs %>%
  select(-c(org, name, description)) %>% 
  rename("org"=grouping) 

# #remove individuals, unknowns and nas
# obs <- obs %>%
#   filter(org!="individuals" & org!="unk" & org!="unknown" & !is.na(org))

#Keep only council, doc, niwa and university (removed the rest as they had less than 30 observations or org was NA)
obs <- obs %>%
  filter(org=="council" | org=="doc" | org=="niwa" | org=="university")


##########
#  Save  #
##########

saveRDS(obs, file.path(data_waitaki_dir, "Waitaki_NZFFD_obs.rds"))
