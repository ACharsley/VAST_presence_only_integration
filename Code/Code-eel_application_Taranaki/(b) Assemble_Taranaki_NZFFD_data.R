###################################################
##     Assemble NZFFD presence/absence data      ##
##           for the Taranaki region             ##
##                                               ##
##               Anthony Charsley                ##
##                January 2023                   ##
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
NZFFD <- NZFFD[-(which(is.na(NZFFD$Year))),]

#Rename REC segments
NZFFD <- NZFFD %>% rename("nzsegment" = "recSegment")

#Ensure the REC segments are correct
NZFFD <- NZFFD[-(which(NZFFD$nzsegment <=9)),]   #removing records that never got assigned nzsegment numbers

#Remove records without an nzsegment identifier
NZFFD <- NZFFD %>% filter(!is.na(nzsegment))


##########################
# Fixing fishing methods #
##########################

table(NZFFD$samplingMethod, useNA = "ifany")

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
#table(NZFFD$FishMethod, NZFFD$samplingMethod, useNA = "ifany") #use this to check - creates big table

#This code was run to examine TRC data when in contact with TRC (20/01/23)
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

#Ran this to get a list of institutions to match up with groupings (and therefore creating "organisation table and groupings_v2.csv")
# write_csv(data.frame("Names"=names(table(NZFFD$institution, useNA = "ifany"))), 
#           file = file.path(data_dir, "NZFFD_institutions.csv"))

#Read organisation data with grouping information
orgdat <- read.csv(file.path(raw_data_dir, "organisation table and groupings_v2.csv"))

#Join NZFFD data and organisation information
NZFFD <- left_join(NZFFD, orgdat, by=c("institution"="name"))

table(NZFFD$grouping, useNA = "ifany")

NZFFD <- NZFFD %>%
  select(-c(abbreviation, description)) %>% 
  rename("org"=grouping) 


################
# Data to keep #
################


#1. Subset by area and join REC (this automatically subsets to Taranaki region as network_to_join only has Taranaki data)
NZFFD_REC_joined <- inner_join(NZFFD, network_to_join, by="nzsegment")


#2. Select variables to keep
NZFFD_REC_joined <- NZFFD_REC_joined %>% 
  select(nzffdRecordNumber, nzsegment, Lat, Lon, catchmentName, catchmentNumber, #Variables related to identifability and location
         child_s, parent_s, dist_s, #variables derived for stream network modelling
         Year, org, institution, FishMethod, #variables specific to sampling (NOTE: institution kept as it is needed later)
         EfmNumberOfPasses, EfmMinutes, EfmArea, NetsTrapsTotalNumber, #variables related to sampling effort
         NetsTrapsBaited, NetsTrapsMeshSize, NetsTrapsDayNight,NetsTrapsAverageSetTime,
         taxonName, taxonCommonName, taxonRemarks, totalCount, present, #fish species present, count variables
         minLength, maxLength) %>% # fish remarks
  rename("child_i"=child_s, "parent_i"=parent_s, "dist_i"=dist_s)

#relocate
NZFFD_REC_joined <- NZFFD_REC_joined %>%
  relocate(nzffdRecordNumber, nzsegment, Lat, Lon, catchmentName, catchmentNumber, #Variables related to identifability and location
           child_i, parent_i, dist_i, #variables derived for stream network modelling
           Year, org, institution, FishMethod, #variables specific to sampling
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


##########################
# Create effort variable #
##########################

## This needs to be one per nzffdRecordNumber i.e., fishing occasion 
## (also important so that it makes sense when encounter/non-encounter data is made)


##also use min/max length here 



###########################
# Abundance data from TRC #
###########################

#Build TRC abundance data
NZFFD_abund_TRC <- NZFFD_REC_joined %>% #This is data that is collected by TRC and is NOT NA in total count
  filter((institution=="Taranaki Regional Council" & !is.na(totalCount)))

#check:
table(NZFFD_abund_TRC$institution, is.na(NZFFD_abund_TRC$totalCount))

table(NZFFD_abund_TRC$FishMethod, useNA = "ifany")

table(NZFFD_abund_TRC$taxonName)


#when longfin eel weren't found, it doesn't necessarily mean they weren't present, for example
#another target species could have been of interest


#Subset data to keep only longfin eel data
NZFFD_abund_TRC_lf <- NZFFD_abund_TRC %>% 
  filter(taxonName == "Anguilla dieffenbachii")

#Examine data
table(NZFFD_abund_TRC_lf$Year)


#Check if each sampling event is unique or if any need to be combined
length(unique(NZFFD_abund_TRC_lf$nzffdRecordNumber))
nrow(NZFFD_abund_TRC_lf)
## Both equal so ok


#Keep only variables of interest
colnames(NZFFD_abund_TRC_lf)
NZFFD_abund_TRC_lf <- NZFFD_abund_TRC_lf %>%
  select(nzffdRecordNumber, nzsegment, Lat, Lon, catchmentName, catchmentNumber, #Variables related to identifability and location
         child_i, parent_i, dist_i, #variables derived for stream network modelling
         Year, org, institution, FishMethod, #variables specific to sampling
         #Put an effort variable here
         taxonName, totalCount) #longfin eel count


##################################
# Encounter / non-encounter data #
##################################

#Build p/a data 
NZFFD_pa <- NZFFD_REC_joined %>%  #we assume these nzffdRecordNumber's are when abundance sampling was conducted
  filter(!(nzffdRecordNumber%in% NZFFD_abund_TRC$nzffdRecordNumber))

#check:
table(NZFFD_pa$institution, is.na(NZFFD_pa$totalCount))


#1. Fix 'present' variable
NZFFD_pa$present <- as.numeric(ifelse(NZFFD_pa$present == 'true', 1, 0)) #If present than one, if not the zero
NZFFD_pa$totalCount <- ifelse(is.na(NZFFD_pa$totalCount), 0, NZFFD_pa$totalCount) #If NA than count is zero

NZFFD_pa$pa <- ifelse((NZFFD_pa$present + NZFFD_pa$totalCount) > 0, TRUE, FALSE) #derive presence/absence (pa) variable

#Remove variables I don't need
NZFFD_pa <- NZFFD_pa %>% select(-c(present, totalCount))


#2. Remove any FALSE's as these will be naturally generated when a species doesn't appear with a nzffdRecordNumber
NZFFD_pa <- NZFFD_pa %>% filter(pa!=FALSE)


# Sorting out NZFFD records into single records
DATA <- NZFFD_pa[!duplicated(NZFFD_pa$nzffdRecordNumber),]
SPECIES<-unique(NZFFD_pa$taxonName)
tmp<-split(NZFFD_pa$taxonName,NZFFD_pa$nzffdRecordNumber)

res <- lapply(tmp,function(x) match(SPECIES,x,nomatch=0)>0)
res <- do.call("rbind",res)
res <- data.frame(res)
names(res) <- SPECIES

DATA <- DATA[order(DATA$nzffdRecordNumber),]
NZFFD_pa_final<-cbind(DATA,res)

nrow(NZFFD_pa_final)
length(unique(NZFFD_pa_final$nzffdRecordNumber))

rm(tmp,res)


#Select rows of interest
colnames(NZFFD_pa_final)

NZFFD_pa_final <- NZFFD_pa_final %>%
  select(nzffdRecordNumber, nzsegment, Lat, Lon, catchmentName, catchmentNumber, #Variables related to identifability and location
         child_i, parent_i, dist_i, #variables derived for stream network modelling
         Year, org, institution, FishMethod, #variables specific to sampling
         #Put an effort variable here
         `Anguilla dieffenbachii`, `Anguilla australis`) #fish species present


###############
# Format data #
###############

#Convert longfin eel FALSE/TRUE to 0/1
NZFFD_pa_final$`Anguilla dieffenbachii` <- factor(NZFFD_pa_final$`Anguilla dieffenbachii`, levels = c("FALSE", "TRUE"))
NZFFD_pa_final$`Anguilla dieffenbachii` <- as.numeric(NZFFD_pa_final$`Anguilla dieffenbachii`) - 1

#Convert shortfin eel FALSE/TRUE to 0/1
NZFFD_pa_final$`Anguilla australis` <- factor(NZFFD_pa_final$`Anguilla australis`, levels = c("FALSE", "TRUE"))
NZFFD_pa_final$`Anguilla australis` <- as.numeric(NZFFD_pa_final$`Anguilla australis`) - 1



################################
# Examine the data across time #
################################

#Abundance data
addmargins(table(NZFFD_pa_final$Year))

colnames(pa_table)[pa_table["Sum",] < 30]

# Keep all years for now, can remove later

#Examine missingness
colSums(is.na(NZFFD_abund_TRC_lf))


#Presence/absence data
pa_table <- addmargins(table(NZFFD_pa_final$`Anguilla dieffenbachii`, NZFFD_pa_final$Year)) ; pa_table #longfin eel
addmargins(table(NZFFD_pa_final$`Anguilla australis`, NZFFD_pa_final$Year)) #shortfin eel

colnames(pa_table)[pa_table["Sum",] < 30]

# Keep all years for now, can remove later

#Examine missingness
colSums(is.na(NZFFD_pa_final))


##########
#  Save  #
##########

saveRDS(NZFFD_pa_final, file.path(data_taranaki_dir, "Taranaki_NZFFD_pa_data.rds"))
saveRDS(NZFFD_abund_TRC_lf, file.path(data_taranaki_dir, "Taranaki_NZFFD_abund_data.rds"))

