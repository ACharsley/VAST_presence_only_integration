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

data_dir <- "./Data_processed"
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






# 
# 
# #3. Subset by fishing method
# #Keep only ef, net, trap and visual
# NZFFD_REC_joined <- NZFFD_REC_joined %>% filter(FishMethod != "Other")
# 
# 
# #4. Subset by organisation
# table(NZFFD_REC_joined$org, useNA = "ifany")
# 
# #Keep all but individuals (sampling protocols unlikely followed), unknown (cannot verify sampler) and NA (cannot verify sampler)
# NZFFD_REC_joined <- NZFFD_REC_joined %>%
#   filter(!(org=="individuals" | org=="unknown" | is.na(org)))
# 
# 
# #Examine missingness
# colSums(is.na(NZFFD_REC_joined))
# 
# 







##########################
# Create effort variable #
##########################

## This needs to be one per nzffdRecordNumber i.e., fishing occasion 
## (also important so that it makes sense when encounter/non-encounter data is made)


##also use min/max length here 



########################################################################
# Split data into encounter/non-encounter data and encounter-only data #
########################################################################


#Abundance data only has non-zero abundance and we will therefore implement zeros by assuming 
#       that any fish not found have zero abundance

#Build TRC abundance data
NZFFD_abund <- NZFFD_REC_joined %>% 
  filter((!is.na(totalCount)))

# Remove 'other' fishing method
NZFFD_abund <- NZFFD_abund %>% filter(FishMethod != "Other")


#Remove consultants, individuals, unknown, and NA
NZFFD_abund <- NZFFD_abund %>%
  filter(!(org=="consultants" | org=="individuals" | org=="unknown" | is.na(org)))


#check:
table(NZFFD_abund$institution, is.na(NZFFD_abund$totalCount))

table(NZFFD_abund$FishMethod, useNA = "ifany")

table(NZFFD_abund$taxonName)

table(NZFFD_abund$Year)


####################################################################
# Convert abundance data by sorting NZFFD records into single rows # 
####################################################################

NZFFD_pa <- NZFFD_abund

## removing zeros as these will be manually generated
NZFFD_pa <- NZFFD_pa %>% filter(totalCount > 0)


NZFFD_pa <- NZFFD_pa[!duplicated(NZFFD_pa$nzffdRecordNumber),]
SPECIES<-unique(NZFFD_pa$taxonName)
tmp<-split(NZFFD_pa$taxonName, NZFFD_pa$nzffdRecordNumber)

res <- lapply(tmp,function(x) match(SPECIES,x,nomatch=0)>0)
res <- do.call("rbind",res)
res <- data.frame(res)
names(res) <- SPECIES

NZFFD_pa <- NZFFD_pa[order(NZFFD_pa$nzffdRecordNumber),]
NZFFD_pa<-cbind(NZFFD_pa,res)

nrow(NZFFD_pa)
length(unique(NZFFD_pa$nzffdRecordNumber))
## Both equal so ok

rm(tmp,res)

#Select rows of interest
colnames(NZFFD_pa)

NZFFD_pa <- NZFFD_pa %>%
  select(nzffdRecordNumber, nzsegment, Lat, Lon, catchmentName, catchmentNumber, #Variables related to identifability and location
         child_i, parent_i, dist_i, #variables derived for stream network modelling
         Year, org, institution, FishMethod, #variables specific to sampling
         #Put an effort variable here
         `Anguilla dieffenbachii`) #fish species present

table(NZFFD_pa$Year, NZFFD_pa$`Anguilla dieffenbachii`)


##################################################
# Encounter data converted to presence-only data #
##################################################
# The encounter-only data can consist of 'NA' counts not without any abundance records and any 
# consultant, individual, unknown or NA organisation records, AND any 'other' fishing methods

#Build presence-only (po) data 
NZFFD_po <- NZFFD_REC_joined %>%  #filter out abundance sampling using nzffdRecordNumber
  filter(!(nzffdRecordNumber %in% NZFFD_pa$nzffdRecordNumber))

NZFFD_po <- NZFFD_po %>% filter(taxonName == "Anguilla dieffenbachii")

#check
table(NZFFD_po$Year)





###############
# Format data #
###############

#Convert longfin eel FALSE/TRUE to 0/1
NZFFD_pa$`Anguilla dieffenbachii` <- factor(NZFFD_pa$`Anguilla dieffenbachii`, levels = c("FALSE", "TRUE"))
NZFFD_pa$`Anguilla dieffenbachii` <- as.numeric(NZFFD_pa$`Anguilla dieffenbachii`) - 1


################################
# Examine the data across time #
################################

#abundance data
addmargins(table(NZFFD_abund_TRC_lf$Year))

# Keep all years for now, can remove later

#Examine missingness
colSums(is.na(NZFFD_abund_TRC_lf))


#Presence/absence data
pa_table <- addmargins(table(NZFFD_pa$`Anguilla dieffenbachii`, NZFFD_pa$Year)) ; pa_table #longfin eel

colnames(pa_table)[pa_table["Sum",] < 30]

# Keep all years for now, can remove later

#Examine missingness
colSums(is.na(NZFFD_pa))


##########
#  Save  #
##########

saveRDS(NZFFD_pa, file.path(data_taranaki_dir, "Taranaki_NZFFD_pa_data.rds"))
saveRDS(NZFFD_abund_TRC_lf, file.path(data_taranaki_dir, "Taranaki_NZFFD_abund_data.rds"))

write.csv(NZFFD_abund_TRC_lf, row.names = F, file = file.path(data_taranaki_dir, "Taranaki_NZFFD_abund_data.csv"))


#################
# Plot p/a data #
#################

Data_to_plot <- NZFFD_pa
Data_to_plot$present <- ifelse(round(Data_to_plot$`Anguilla dieffenbachii`)==1, "Present", "Absent")

#Taranaki network by year
tab <- table(Data_to_plot$present, Data_to_plot$Year)
years <- unique(Data_to_plot$Year)[order(unique(Data_to_plot$Year))]
data_text <- data.frame("Year"= years, label=tab[1,],
                        x=174, y=-39.8)


catchmap <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_point(aes(x = Lon, y = Lat, col = present), alpha = 0.6) +
  facet_wrap(.~Year) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel NZFFD presence/absence observations by year") +
  guides(color = guide_legend(title = "")) +
  scale_colour_manual(values = c("#377EB8", "#E41A1C")) +
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
  geom_point(aes(x = Lon, y = Lat, col = present), size=3, alpha = 0.6) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel NZFFD presence/absence observations") +
  guides(color = guide_legend(title = "")) +
  scale_colour_manual(values = c("#377EB8", "#E41A1C")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.8)))

ggsave(file.path(fig_dir, "Taranaki_lf_observations.png"), catchmap2, height = 12, width = 15)

