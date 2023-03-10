###################################################
##     Assemble NZFFD presence/absence data      ##
##           for the Taranaki region             ##
##                                               ##
##               Anthony Charsley                ##
##                 March 2023                    ##
###################################################

# This code assembles presence/absence 
# data from the NZFFD.

###################################################

rm(list=ls())


#################
#  Directories  #
#################

raw_data_dir <- "./Data_raw/Eel_application_Taranaki"
data_taranaki_dir <- "./Data_processed/Taranaki"
fig_dir <- "./Data_processed/Taranaki/Figures"


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


###############
# Format data #
###############

names(NZFFD_raw) #Note present variable isn't actually p/a

# These variables may be needed so keep for now
NZFFD <- NZFFD_raw %>%
  select(nzffdRecordNumber, eventDate, institution, samplingPurpose, waterBody, catchmentNumber, catchmentName,
         eastingNZTM, northingNZTM, decimalLongitude, decimalLatitude, recSegment, 
         downstreamBarrier, 
         
         # siteReachLength, siteAverageWidth, waterTemperature, waterConductivity,
         # waterDissolvedOxygenPercent, waterDissolvedOxygenPPM, waterPH, waterSalinity, habitatFlowPercent,
         # habitatSubstratePercent, habitatInstreamCoverPresent, habitatRiparianVegPercent, 
         samplingMethod, samplingProtocol, samplingPurpose,

         # EfmNumberOfPasses, EfmMinutes, EfmArea, NetsTrapsTotalNumber, NetsTrapsBaited, NetsTrapsMeshSize,
         # NetsTrapsDayNight, NetsTrapsAverageSetTime,
         
         taxonName, taxonCommonName, taxonRemarks, totalCount, minLength, maxLength)


#Ensure there is a year variable
NZFFD$Year <- as.numeric(substr(NZFFD$eventDate, start = 1, stop = 4))
NZFFD <- NZFFD %>% relocate(Year, .after = nzffdRecordNumber)
NZFFD <- NZFFD[-(which(is.na(NZFFD$Year))),]

#Rename recsegment variable
NZFFD <- NZFFD %>% rename("nzsegment" = "recSegment")

#Ensure the REC segments are correct
NZFFD <- NZFFD[-(which(NZFFD$nzsegment <=9)),]   #removing records that never got assigned nzsegment numbers
NZFFD <- NZFFD %>% filter(!is.na(nzsegment)) #Remove records without an nzsegment identifier



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


########################
# Fixing organisations #
########################

table(NZFFD$institution, useNA = "ifany")

#Ran this to get a list of institutions to match up with groupings (and therefore creating "organisation table and groupings_v2.csv")
write_csv(data.frame("Names"=names(table(NZFFD$institution, useNA = "ifany"))),
          file = file.path(data_taranaki_dir, "NZFFD_institutions.csv"))

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


#1. Subset by area and join REC (this automatically subsets to Taranaki region as network_to_join only 
#   has Taranaki data and also excludes lakes).
NZFFD_REC_joined <- inner_join(NZFFD, network_to_join, by="nzsegment")


#2. Select variables to keep
NZFFD_REC_joined <- NZFFD_REC_joined %>% 
  select(nzffdRecordNumber, nzsegment, Lat, Lon, catchmentName, catchmentNumber, #Variables related to identifability and location
         child_s, parent_s, dist_s, #variables derived for stream network modelling
         Year, org, institution, FishMethod, #variables specific to sampling (NOTE: institution kept as it is needed later)
         samplingPurpose,
         # EfmNumberOfPasses, EfmMinutes, EfmArea, NetsTrapsTotalNumber, #variables related to sampling effort
         # NetsTrapsBaited, NetsTrapsMeshSize, NetsTrapsDayNight,NetsTrapsAverageSetTime,
         taxonName, taxonCommonName, taxonRemarks, totalCount, #fish species present, count variables
         minLength, maxLength) %>% # fish remarks
  rename("child_i"=child_s, "parent_i"=parent_s, "dist_i"=dist_s)

#relocate
NZFFD_REC_joined <- NZFFD_REC_joined %>%
  relocate(nzffdRecordNumber, nzsegment, Lat, Lon, catchmentName, catchmentNumber, #Variables related to identifability and location
           child_i, parent_i, dist_i, #variables derived for stream network modelling
           Year, org, institution, FishMethod, #variables specific to sampling
           samplingPurpose,
           # EfmNumberOfPasses, EfmMinutes, EfmArea, NetsTrapsTotalNumber, #variables related to sampling effort
           # NetsTrapsBaited, NetsTrapsMeshSize, NetsTrapsDayNight,NetsTrapsAverageSetTime,
           taxonName, taxonCommonName, taxonRemarks, totalCount, #fish species present, count variables
           minLength, maxLength)



########################################################################
# Split data into encounter/non-encounter data and encounter-only data #
########################################################################

## 1. Build encounter/non-encounter data
#     Abundance data only has non-zero abundance and we will therefore implement zeros by assuming 
#     that any fish not found have zero abundance

#Build 'abundance' data using 'totalCount'
NZFFD_abund <- NZFFD_REC_joined %>% 
  filter((!is.na(totalCount)))

# Remove all fishing method except electric fishing
# NZFFD_abund <- NZFFD_abund %>% filter(FishMethod != "Other")
NZFFD_abund <- NZFFD_abund %>% filter(FishMethod == "Electric fishing")

#Remove consultants, individuals, unknown, and NA
NZFFD_abund <- NZFFD_abund %>%
  filter((org=="cawthron" | org=="council" | org=="doc" | org=="fish_and_game" | org=="niwa" | org=="university"))


#check:
table(NZFFD_abund$institution, is.na(NZFFD_abund$totalCount))

table(NZFFD_abund$FishMethod, useNA = "ifany")

table(NZFFD_abund$taxonName)

table(NZFFD_abund$Year)


####################################################################
# Convert abundance data by sorting NZFFD records into single rows # 
####################################################################

NZFFD_ene <- NZFFD_abund

## removing zeros as these will be manually generated
NZFFD_ene <- NZFFD_ene %>% filter(totalCount > 0)


NZFFD_ene <- NZFFD_ene[!duplicated(NZFFD_ene$nzffdRecordNumber),]
SPECIES<-unique(NZFFD_ene$taxonName)
tmp<-split(NZFFD_ene$taxonName, NZFFD_ene$nzffdRecordNumber)

res <- lapply(tmp,function(x) match(SPECIES,x,nomatch=0)>0)
res <- do.call("rbind",res)
res <- data.frame(res)
names(res) <- SPECIES

NZFFD_ene <- NZFFD_ene[order(NZFFD_ene$nzffdRecordNumber),]
NZFFD_ene<-cbind(NZFFD_ene,res)

nrow(NZFFD_ene)
length(unique(NZFFD_ene$nzffdRecordNumber))
## Both equal so ok

rm(tmp,res)

#Select rows of interest
colnames(NZFFD_ene)

NZFFD_ene <- NZFFD_ene %>%
  select(nzffdRecordNumber, nzsegment, Lat, Lon, catchmentName, catchmentNumber, #Variables related to identifability and location
         child_i, parent_i, dist_i, #variables derived for stream network modelling
         Year, org, institution, FishMethod, #variables specific to sampling
         #Put an effort variable here
         `Anguilla dieffenbachii`) #fish species encounter

table(NZFFD_ene$Year, NZFFD_ene$`Anguilla dieffenbachii`)

#Convert longfin eel FALSE/TRUE to 0/1
NZFFD_ene$`Anguilla dieffenbachii` <- factor(NZFFD_ene$`Anguilla dieffenbachii`, levels = c("FALSE", "TRUE"))
NZFFD_ene$`Anguilla dieffenbachii` <- as.numeric(NZFFD_ene$`Anguilla dieffenbachii`) - 1



###################################################
# Encounter data converted to encounter-only data #
###################################################
# The encounter-only data can consist of 'NA' counts (i.e., no abundance records) and/or any 
# samples taken using nets, traps or any other sampling method and/or samples taken by consultant, 
# individuals, unknown or NA organisation records.

#Build encounter-only (eo) data 
NZFFD_eo <- NZFFD_REC_joined %>%  #filter out abundance sampling using nzffdRecordNumber
  filter(!(nzffdRecordNumber %in% NZFFD_ene$nzffdRecordNumber))

NZFFD_eo <- NZFFD_eo %>% filter(taxonName == "Anguilla dieffenbachii")

#check
table(NZFFD_eo$Year)

## Format data equivalently to NZFFD data
NZFFD_eo <- NZFFD_eo %>%
  select(nzffdRecordNumber, nzsegment, Lat, Lon, catchmentName, catchmentNumber, #Variables related to identifability and location
         child_i, parent_i, dist_i, #variables derived for stream network modelling
         Year, org, institution, FishMethod) %>% #variables specific to sampling
  mutate("Anguilla dieffenbachii"=1)



################################
# Examine the data across time #
################################

#abundance data
addmargins(table(NZFFD_ene$Year))

# Keep all years for now, can remove later

#Examine missingness
colSums(is.na(NZFFD_ene))


#encounter/non-encounter data
ene_table <- addmargins(table(NZFFD_ene$`Anguilla dieffenbachii`, NZFFD_ene$Year)) ; ene_table #longfin eel

colnames(ene_table)[ene_table["Sum",] < 30]
# Keep all years for now, can remove later



##########
#  Save  #
##########

saveRDS(NZFFD_ene, file.path(data_taranaki_dir, "Taranaki_NZFFD_ene_data.rds"))
saveRDS(NZFFD_eo, file.path(data_taranaki_dir, "Taranaki_encounter_only_lf_data.rds"))

# saveRDS(NZFFD_abund_TRC_lf, file.path(data_taranaki_dir, "Taranaki_NZFFD_abund_data.rds"))
# write.csv(NZFFD_abund_TRC_lf, row.names = F, file = file.path(data_taranaki_dir, "Taranaki_NZFFD_abund_data.csv"))


###################
# Plot e/n-e data #
###################

Data_to_plot <- NZFFD_ene
Data_to_plot$encounter <- ifelse(round(Data_to_plot$`Anguilla dieffenbachii`)==1, "Encounter", "Non-encounter")

#Taranaki network by year
tab <- table(Data_to_plot$encounter, Data_to_plot$Year)
years <- unique(Data_to_plot$Year)[order(unique(Data_to_plot$Year))]
data_text <- data.frame("Year"= years, label=paste0(tab[1,], "/", tab[2,]),
                        x=174, y=-39.8)


catchmap <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_point(aes(x = Lon, y = Lat, col = encounter), alpha = 0.6) +
  facet_wrap(.~Year) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel NZFFD encounter/non-encounter observations by year") +
  guides(color = guide_legend(title = "")) +
  scale_colour_manual(values = c("#E41A1C", "#377EB8")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.5)),
        axis.text.x = element_text(angle = 90))

catchmap <- catchmap +geom_text(
  data = data_text,
  mapping = aes(x = x, y = y, label = label)
)

ggsave(file.path(fig_dir, "Taranaki_lf_observations_byYear.png"), catchmap, height = 12, width = 15)


###################################################

# Taranaki catchment

l2 <- lapply(1:nrow(network), function(x){
  parent <- network$parent_s[x]
  find <- network %>% filter(child_s == parent)
  if(nrow(find)>0) out <- cbind.data.frame(network[x,], 'Lon2'=find$Lon, 'Lat2'=find$Lat)
  if(nrow(find)==0) out <- cbind.data.frame(network[x,], 'Lon2'=NA, 'Lat2'=NA)
  
  return(out)
})
l2 <- do.call(rbind, l2)


catchmap2 <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = encounter), size=3, alpha = 0.6) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel NZFFD encounter/non-encounter observations") +
  guides(color = guide_legend(title = "")) +
  scale_colour_manual(values = c("#E41A1C", "#377EB8")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.8)))

ggsave(file.path(fig_dir, "Taranaki_lf_observations.png"), catchmap2, height = 12, width = 15)



############################
# Plot encounter-only data #
############################

Data_to_plot_eo <- NZFFD_eo
Data_to_plot_eo$present <- ifelse(round(Data_to_plot_eo$`Anguilla dieffenbachii`)==1, "Encounter", "Non-encounter")

#Load network data
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))

#Taranaki network by year
tab <- table(Data_to_plot_eo$present, Data_to_plot_eo$Year)
years <- unique(Data_to_plot_eo$Year)[order(unique(Data_to_plot_eo$Year))]
data_text <- data.frame("Year"= years, label=tab[1,],
                        x=174, y=-39.8)


catchmap <- ggplot(Data_to_plot_eo) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_point(aes(x = Lon, y = Lat, col = present), alpha = 0.6) +
  facet_wrap(.~Year) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel NZFFD encounter-only observations by year") +
  guides(color = guide_legend(title = "")) +
  scale_colour_manual(values = "#E41A1C") +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.5)),
        axis.text.x = element_text(angle = 90))

catchmap <- catchmap +geom_text(
  data = data_text,
  mapping = aes(x = x, y = y, label = label)
)

ggsave(file.path(fig_dir, "Taranaki_encounter_only_observations_byYear.png"), catchmap, height = 12, width = 15)


###################################################

# Taranaki catchment

catchmap2 <- ggplot(Data_to_plot_eo) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = present), size=3, alpha = 0.6) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel NZFFD encounter-only observations") +
  guides(color = guide_legend(title = "")) +
  scale_colour_manual(values = "#E41A1C") +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.8)))

ggsave(file.path(fig_dir, "Taranaki_encounter_only_observations.png"), catchmap2, height = 12, width = 15)




