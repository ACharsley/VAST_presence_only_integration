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

data_dir <- "./Data_processed"
raw_data_dir <- "./Data_raw"
data_taranaki_dir <- "./Data_processed/Taranaki"


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
NZFFD_raw <- read.csv(file.path(raw_data_dir, "nzffdms.csv")) # Downloaded from https://nzffdms.niwa.co.nz/search on 12/01/23
#NZFFD_old <- read.csv(file.path(raw_data_dir, "data-1665108945016.csv")) # Received from Jane Robbins on 7/10/22

#table(NZFFD$y[NZFFD$y>=1967], NZFFD$m[NZFFD$y>=1967])



###############
# Format data #
###############

names(NZFFD_raw)

# These variables may be needed so keep for now
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

#Remove records without an nzsegment identifier
NZFFD <- NZFFD %>% filter(!is.na(nzsegment))


##########################
# Fixing fishing methods #
##########################

table(NZFFD$samplingMethod, useNA = "ifany")

# fishingdat <- read.csv(file.path(raw_data_dir, "Table of fishingmethods and factors.csv"))
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

table(NZFFD$FishMethod, useNA = "ifany")

# NZFFD_TRC <- NZFFD %>% 
#   filter(institution == "Taranaki Regional Council") %>% 
#   select("nzffdRecordNumber", "Year", "eventDate", "institution", "samplingPurpose", "waterBody",
#          "catchmentNumber", "catchmentName", "decimalLongitude", "decimalLatitude", "nzsegment",
#          "taxonName","totalCount", "present", "minLength", "maxLength", "FishMethod")
# 
# 
# write_csv(NZFFD_TRC, file.path(data_taranaki_dir, "NZFFD_TRC.csv"))


########################
# Fixing organisations #
########################

table(NZFFD$institution, useNA = "ifany")

write_csv(data.frame("Names"=names(table(NZFFD$institution, useNA = "ifany"))), 
          file = file.path(data_dir, "NZFFD_institutions.csv"))

#Read organisation data with grouping information
orgdat <- read.csv(file.path(raw_data_dir, "organisation table and groupings_v2.csv"))

#Join NZFFD data and organisation information
NZFFD <- left_join(NZFFD, orgdat, by=c("institution"="name"))
table(NZFFD$grouping, useNA = "ifany")

NZFFD <- NZFFD %>%
  select(-c(abbreviation, description)) %>% 
  rename("org"=grouping) 

# #remove individuals, unknowns and nas
# obs <- obs %>%
#   filter(org!="individuals" & org!="unk" & org!="unknown" & !is.na(org))
# orgdat <- read.csv(file.path(raw_data_dir, "organisation table and groupings.csv"))
# 
# obs <- left_join(obs, orgdat, by=c("org"="abbreviation"))
# table(obs$grouping, useNA = "ifany")
# 
# obs <- obs %>%
#   select(-c(org, name, description)) %>% 
#   rename("org"=grouping) 
# 
# # #remove individuals, unknowns and nas
# # obs <- obs %>%
# #   filter(org!="individuals" & org!="unk" & org!="unknown" & !is.na(org))
# 
# #Keep only consultants, council, doc, fish&game, niwa and university (removed the rest as they had less than 30 observations or org was NA)
# obs <- obs %>%
#   filter(org=="consultants" | org=="council" | org=="doc" | org=="fish&game" | org=="niwa" | org=="university")


################
# Data to keep #
################


#1. Subset by area and join REC (this automatically subsets to Taranaki region as network_to_join only has Taranaki data)
NZFFD_REC_joined <- inner_join(NZFFD, network_to_join, by="nzsegment")


#2. Select variables to keep
NZFFD_REC_joined <- NZFFD_REC_joined %>% 
  select(nzffdRecordNumber, nzsegment, Lat, Lon, catchmentName, catchmentNumber, #Variables related to identifability and location
         child_s, parent_s, dist_s, #variables derived for stream network modelling
         Year, org, FishMethod, #variables specific to sampling
         EfmNumberOfPasses, EfmMinutes, EfmArea, NetsTrapsTotalNumber, #variables related to sampling effort
         NetsTrapsBaited, NetsTrapsMeshSize, NetsTrapsDayNight,NetsTrapsAverageSetTime,
         taxonName, taxonCommonName, taxonRemarks, totalCount, present, #fish species present, count variables
         minLength, maxLength) %>% # fish remarks
  rename("child_i"=child_s, "parent_i"=parent_s, "dist_i"=dist_s) #%>%
#filter(Year >= 1967) #To capture only data from 1967 to present

#relocate
NZFFD_REC_joined <- NZFFD_REC_joined %>%
  relocate(nzffdRecordNumber, nzsegment, Lat, Lon, catchmentName, catchmentNumber, #Variables related to identifability and location
           child_i, parent_i, dist_i, #variables derived for stream network modelling
           Year, org, FishMethod, #variables specific to sampling
           EfmNumberOfPasses, EfmMinutes, EfmArea, NetsTrapsTotalNumber, #variables related to sampling effort
           NetsTrapsBaited, NetsTrapsMeshSize, NetsTrapsDayNight,NetsTrapsAverageSetTime,
           taxonName, taxonCommonName, taxonRemarks, totalCount, present, #fish species present, count variables
           minLength, maxLength)


#3. Subset by fishing method
#Keep only ef, net, trap and visual
NZFFD_REC_joined <- NZFFD_REC_joined %>% filter(FishMethod != "Other")


#4. Subset by organisation
table(NZFFD_REC_joined$org, useNA = "ifany")

#Keep all but individuals (sampling protocols unlikely followed), unknown (cannot verify sampler) and NA (cannot verify sampler)
NZFFD_REC_joined <- NZFFD_REC_joined %>%
  filter(!(org=="individuals" | org=="unknown" | is.na(org)))


#Examine missingness
colSums(is.na(NZFFD_REC_joined))

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


## Continue from here: ##

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
