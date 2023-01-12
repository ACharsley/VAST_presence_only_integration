###################################################
##     Assemble NZFFD presence/absence data      ##
##           for the Taranaki region             ##
##                                               ##
##               Anthony Charsley                ##
##                November 2022                  ##
###################################################

# This code assembles presence/absence 
# data from the NZFFD.

###################################################

rm(list=ls())


#################
#  Directories  #
#################

raw_data <- "./Data/raw_data"
data_taranaki_dir <- "./Data/Taranaki"


##############
#  Packages  #
##############

library(tidyverse)


#################################
#  Load NZFFD data and network  #
#################################

#Network
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))
network_to_join <- network %>% filter(parent_s!=0, FWENZ_isLake!=TRUE) #This takes out lakes

#Observations
NZFFD_raw <- read.csv(file.path(raw_data, "nzffdms.csv")) # Downloaded from https://nzffdms.niwa.co.nz/search on 12/01/23
#NZFFD_old <- read.csv(file.path(raw_data, "data-1665108945016.csv")) # Received from Jane Robbins on 7/10/22

#table(NZFFD$y[NZFFD$y>=1967], NZFFD$m[NZFFD$y>=1967])



###############
# Format data #
###############

names(NZFFD_raw)

NZFFD <- NZFFD_raw %>%
  select(nzffdRecordNumber, eventDate, institution, samplingPurpose, waterBody, catchmentNumber, catchmentName,
         eastingNZTM, northingNZTM, decimalLongitude, decimalLatitude, recSegment, 
         downstreamBarrier, 
         
         siteReachLength, siteAverageWidth, waterTemperature, waterConductivity, 
         waterDissolvedOxygenPercent, waterDissolvedOxygenPPM, waterPH, waterSalinity, habitatFlowPercent,
         habitatSubstratePercent, habitatInstreamCoverPresent, habitatRiparianVegPercent, samplingMethod,
         samplingProtocol, 
         
         EfmNumberOfPasses, EfmMinutes, EfmArea, NetsTrapsTotalNumber, NetsTrapsBaited, NetsTrapsMeshSize,
         NetsTrapsDayNight, NetsTrapsAverageSetTime,
         
         taxonName, taxonCommonName, taxonRemarks, totalCount, present, minLength, maxLength)


#Ensure there is a year variable
NZFFD$Year <- as.numeric(substr(NZFFD$eventDate, start = 1, stop = 4))
NZFFD <- NZFFD %>% relocate(Year, .after = nzffdRecordNumber)
NZFFD <- NZFFD[-(which(is.na(NZFFD$Year))),]     #checked these NA in the original NZFFD and they are true missing dates, so they are removed here

#Rename REC segments
NZFFD <- NZFFD %>% rename("nzsegment" = "recSegment")

#Ensure the REC segments are correct
NZFFD <- NZFFD[-(which(NZFFD$nzsegment <=9)),]   #removing records that never got assigned NZreach numbers


##########################
# Fixing fishing methods #
##########################

table(NZFFD$samplingMethod, useNA = "ifany")

# fishingdat <- read.csv(file.path(raw_data, "Table of fishingmethods and factors.csv"))
# 
# obs <- left_join(obs, fishingdat, by=c("fishmeth"="NZFFD.abbreviation"))
# table(obs$Method.groupings, useNA = "ifany")
# 
# obs <- obs %>%
#   select(-c(fishmeth, Method.Description)) %>% 
#   rename("fishmeth"=Method.groupings) 
# 
# table(obs$angdie, obs$fishmeth)
# table(obs$angaus, obs$fishmeth)
# 
# # Keep only ef, net, trap and visual
# obs <- obs %>% 
#   filter(fishmeth=="Electric fishing" | fishmeth=="Net" | fishmeth=="Trap" | fishmeth=="Visual")


# Keep only ef, net, trap and visual
NZFFD <- NZFFD %>% 
  filter(samplingMethod=="Electric fishing - Backpack" | #Electric fishing
           samplingMethod=="Electric fishing - Bank generator or mains" |
           samplingMethod=="Electric fishing - Boat" | 
           samplingMethod=="Electric fishing - combination of nets and electric fishing" |
           samplingMethod=="Electric fishing - combination of nets traps and electric fishing" |
           samplingMethod=="Electric fishing - Combination of traps and electric fishing" | 
           samplingMethod=="Electric fishing - Type unknown" |
           
           samplingMethod=="Fyke net - Fyke net including minifykes" | #Nets
           samplingMethod=="Fyke net - Mini" |
           samplingMethod=="Fyke net - Standard" |
           samplingMethod=="Fyke net - Super" |
           samplingMethod=="Other net - Drop" |
           samplingMethod=="Other - hinaki" |
           samplingMethod=="Other net - Hand net" |
           samplingMethod=="Other net - Plankton" |
           samplingMethod=="Other net - Push net" |
           samplingMethod=="Other net - Seine" |
           samplingMethod=="Other net - Set net" |
           samplingMethod=="Other net - Unknown type of net" |
           samplingMethod=="Other net - Whitebait" |
           samplingMethod=="Other net - Whitebait scoop" |
           samplingMethod=="Other net - Whitebait set" |
           
           samplingMethod=="Gill net - Multi mesh" | #Traps
           samplingMethod=="Gill net - Single mesh" |
           samplingMethod=="Gill net - Trammel" |
           samplingMethod=="Traps - Bait trap (Killwell)"|
           samplingMethod=="Traps - Box trap" |
           samplingMethod=="Traps - combination of Kilwell and Gee minnow traps" |
           samplingMethod=="Traps - Combination of traps" |
           samplingMethod=="Traps - Fry trap" |
           samplingMethod=="Traps - G minnow (coarse mesh, baited)" |
           samplingMethod=="Traps - G minnow (coarse mesh, unbaited)" |
           samplingMethod=="Traps - G minnow (fine mesh, baited)" |
           samplingMethod=="Traps - G minnow (fine mesh, unbaited)" |
           samplingMethod=="Traps - Gee minnow" |
           samplingMethod=="Traps - Hoop trap" |
           samplingMethod=="Traps - Unknown type of trap")

# Set Method
NZFFD$FishMethod <- ifelse(NZFFD$samplingMethod=="Electric fishing - Backpack" | #Electric fishing
                             NZFFD$samplingMethod=="Electric fishing - Bank generator or mains" |
                             NZFFD$samplingMethod=="Electric fishing - Boat" | 
                             NZFFD$samplingMethod=="Electric fishing - combination of nets and electric fishing" |
                             NZFFD$samplingMethod=="Electric fishing - combination of nets traps and electric fishing" |
                             NZFFD$samplingMethod=="Electric fishing - Combination of traps and electric fishing" | 
                             NZFFD$samplingMethod=="Electric fishing - Type unknown", "Electric fishing", 
                           ifelse(NZFFD$samplingMethod=="Fyke net - Fyke net including minifykes" | #Nets
                                    NZFFD$samplingMethod=="Fyke net - Mini" |
                                    NZFFD$samplingMethod=="Fyke net - Standard" |
                                    NZFFD$samplingMethod=="Fyke net - Super" |
                                    NZFFD$samplingMethod=="Other net - Drop" |
                                    NZFFD$samplingMethod=="Other - hinaki" |
                                    NZFFD$samplingMethod=="Other net - Hand net" |
                                    NZFFD$samplingMethod=="Other net - Plankton" |
                                    NZFFD$samplingMethod=="Other net - Push net" |
                                    NZFFD$samplingMethod=="Other net - Seine" |
                                    NZFFD$samplingMethod=="Other net - Set net" |
                                    NZFFD$samplingMethod=="Other net - Unknown type of net" |
                                    NZFFD$samplingMethod=="Other net - Whitebait" |
                                    NZFFD$samplingMethod=="Other net - Whitebait scoop" |
                                    NZFFD$samplingMethod=="Other net - Whitebait set", "Net",
                                  ifelse(NZFFD$samplingMethod=="Gill net - Multi mesh" | #Traps
                                           NZFFD$samplingMethod=="Gill net - Single mesh" |
                                           NZFFD$samplingMethod=="Gill net - Trammel" |
                                           NZFFD$samplingMethod=="Traps - Bait trap (Killwell)"|
                                           NZFFD$samplingMethod=="Traps - Box trap" |
                                           NZFFD$samplingMethod=="Traps - combination of Kilwell and Gee minnow traps" |
                                           NZFFD$samplingMethod=="Traps - Combination of traps" |
                                           NZFFD$samplingMethod=="Traps - Fry trap" |
                                           NZFFD$samplingMethod=="Traps - G minnow (coarse mesh, baited)" |
                                           NZFFD$samplingMethod=="Traps - G minnow (coarse mesh, unbaited)" |
                                           NZFFD$samplingMethod=="Traps - G minnow (fine mesh, baited)" |
                                           NZFFD$samplingMethod=="Traps - G minnow (fine mesh, unbaited)" |
                                           NZFFD$samplingMethod=="Traps - Gee minnow" |
                                           NZFFD$samplingMethod=="Traps - Hoop trap" |
                                           NZFFD$samplingMethod=="Traps - Unknown type of trap", "Trap", "Other")))

NZFFD <- NZFFD %>% select(-c("samplingMethod")) %>% filter(FishMethod != "Other")






## NEED TO FIX BELOW: ##

########################
# Fixing organisations #
########################

table(NZFFD$institution, useNA = "ifany")

orgdat <- read.csv(file.path(raw_data, "organisation table and groupings.csv"))

obs <- left_join(obs, orgdat, by=c("org"="abbreviation"))
table(obs$grouping, useNA = "ifany")

obs <- obs %>%
  select(-c(org, name, description)) %>% 
  rename("org"=grouping) 

# #remove individuals, unknowns and nas
# obs <- obs %>%
#   filter(org!="individuals" & org!="unk" & org!="unknown" & !is.na(org))

#Keep only consultants, council, doc, fish&game, niwa and university (removed the rest as they had less than 30 observations or org was NA)
obs <- obs %>%
  filter(org=="consultants" | org=="council" | org=="doc" | org=="fish&game" | org=="niwa" | org=="university")




# ########################################################
# #  Sorting out NZFFD records into single rows/NZreach  #
# ########################################################
# 
# NZFFD <- NZFFD[-(which(is.na(NZFFD$y))),]     #checked these NA in the original NZFFD and they are true missing dates, so they are removed here
# NZFFD <- NZFFD[-(which(NZFFD$nzreach <=9)),]   #removing records that never got assigned NZreach numbers
# 
# DATA <- NZFFD[!duplicated(NZFFD$card),]
# SPECIES<-unique(NZFFD$spcode)
# tmp<-split(NZFFD$spcode,NZFFD$card)
# 
# res <- lapply(tmp,function(x) match(SPECIES,x,nomatch=0)>0)
# res <- do.call("rbind",res)
# res <- data.frame(res)
# names(res) <- SPECIES
# 
# DATA <- DATA[order(DATA$card),]
# DATA<-cbind(DATA,res)
# 
# nrow(DATA)
# length(unique(DATA$card))
# 
# rm(tmp,res)


##################################
# Encounter / non-encounter data #
##################################

# Sorting out NZFFD records into single records

DATA <- NZFFD[!duplicated(NZFFD$nzffdRecordNumber),]
SPECIES<-unique(NZFFD$taxonName)
tmp<-split(NZFFD$taxonName,NZFFD$nzffdRecordNumber)

res <- lapply(tmp,function(x) match(SPECIES,x,nomatch=0)>0)
res <- do.call("rbind",res)
res <- data.frame(res)
names(res) <- SPECIES

DATA <- DATA[order(DATA$nzffdRecordNumber),]
DATA<-cbind(DATA,res)

nrow(DATA)
length(unique(DATA$nzffdRecordNumber))

rm(tmp,res)


# ###########################
# # Abundance data from TRC #
# ###########################
# 
# abundance_data_TRC <- NZFFD %>% filter(institution == "Taranaki Regional Council")




##############
#  Join REC  #
##############

obs <- inner_join(DATA, network_to_join, by="nzsegment")

obs <- obs %>% select(nzsegment, Lat, Lon, child_s, parent_s, dist_s, y, catchname, org, fishmeth, effort, pass, 
                      angdie, angaus) %>% #All fish species
  rename("child_i"=child_s, "parent_i"=parent_s, "dist_i"=dist_s, "Year"=y) #%>%
#filter(Year >= 1967) #To capture only data from 1967 to present

colSums(is.na(obs))


###############
# Format data #
###############

#Convert longfin eel FALSE/TRUE to 0/1
obs$angdie <- factor(obs$angdie, levels = c("FALSE", "TRUE"))
obs$angdie <- as.numeric(obs$angdie) - 1

#Convert shortfin eel FALSE/TRUE to 0/1
obs$angaus <- factor(obs$angaus, levels = c("FALSE", "TRUE"))
obs$angaus <- as.numeric(obs$angaus) - 1


#Format final data
obs <- obs %>% select(-c("effort", "pass"))



################################
# Examine the data across time #
################################

addmargins(table(obs$angdie, obs$Year)) #longfin eel

addmargins(table(obs$angaus, obs$Year)) #shortfin eel

# Remove the years 1966 and 1969 as these years only have 1 observation
# and are 10 years earlier than the rest of the data set. Also remove
# the year 2022 as this year is incomplete at the time of the analysis

obs <- obs %>% filter(!(Year %in% c(1966, 1969, 2022)))



##########
#  Save  #
##########

saveRDS(obs, file.path(data_taranaki_dir, "Taranaki_NZFFD_obs.rds"))
