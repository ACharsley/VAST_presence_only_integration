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

raw_data_dir <- "./Data_raw"
data_taranaki_dir <- "./Data_processed"
fig_dir <- "./Data_processed/Figures"


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
#NZFFD_raw <- read.csv(file.path(raw_data_dir, "nzffdms_12_01_23.csv")) # Downloaded from https://nzffdms.niwa.co.nz/search on 12/01/23
NZFFD_raw <- read.csv(file.path(raw_data_dir, "nzffdms_18_12_23.csv")) # Downloaded from https://nzffdms.niwa.co.nz/search on 18/12/23


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


#Set year and month variable
NZFFD$Year <- as.numeric(substr(NZFFD$eventDate, start = 1, stop = 4))
NZFFD$Month <- as.numeric(substr(NZFFD$eventDate, start = 6, stop = 7))
NZFFD$Day <- as.numeric(substr(NZFFD$eventDate, start = 9, stop = 10))

NZFFD <- NZFFD %>% relocate(Year, Month, Day, .after = nzffdRecordNumber)
NZFFD <- NZFFD[-(which(is.na(NZFFD$Year))),]


#Investigating years vs months vs days:
summary(NZFFD$Year)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1901    1998    2007    2005    2017    2023
summary(NZFFD$Month)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   2.000   4.000   5.648   9.000  12.000     973 
summary(NZFFD$Day)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    1.0    8.0   16.0   15.5   23.0   31.0    4566 


#Rename recsegment variable
NZFFD <- NZFFD %>% 
  dplyr::rename("nzsegment" = "recSegment")

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

# #Ran this if I need to get a list of institutions to match up with groupings (and therefore creating "organisation table and groupings_v2.csv")
# write_csv(data.frame("Names"=names(table(NZFFD$institution, useNA = "ifany"))),
#            file = file.path(raw_data_dir, "NZFFD_institutions.csv"))

#Read organisation data with grouping information
#I built v4 on 18/12/2023 with updated orgs (will need to do it again with new versions of the NZFFD)
orgdat <- read.csv(file.path(raw_data_dir, "organisation table and groupings_v4.csv")) 

#Join NZFFD data and organisation information
NZFFD <- left_join(NZFFD, orgdat, by=c("institution"="name"))

table(NZFFD$grouping, useNA = "ifany")

NZFFD <- NZFFD %>%
  select(-c(abbreviation, description)) %>% 
  rename("org"=grouping) 



##########################
# Make new time variable #
##########################

## Season of observation
NZFFD$Season <- ifelse(NZFFD$Month %in% c(12,1,2), "Summer",
                       ifelse(NZFFD$Month %in% c(3,4,5), "Autumn",
                              ifelse(NZFFD$Month %in% c(6,7,8), "Winter", 
                                     ifelse(NZFFD$Month %in% c(9,10,11), "Spring", NA))))
table(NZFFD$Season, useNA = "ifany")# 895 NA's

## Year_Season variable
#This ensures the summer 12th month is connected to the following year
NZFFD$YearV2 <- ifelse(NZFFD$Month == 12, NZFFD$Year + 1, NZFFD$Year)

NZFFD$Year_season <- paste0(NZFFD$YearV2, "_", NZFFD$Season)
table(NZFFD$Year_season, useNA = "ifany") # 895 NA's

# ## For implementation in VAST
# NZFFD$Time <- NZFFD$YearV2 + ifelse(NZFFD$Month %in% c(12,1,2), 0,
#                                     ifelse(NZFFD$Month %in% c(3,4,5), 0.25,
#                                            ifelse(NZFFD$Month %in% c(6,7,8), 0.5, 
#                                                   ifelse(NZFFD$Month %in% c(9,10,11), 0.75, NA))))
# table(NZFFD$Time, useNA = "ifany") # 895 NA's

## Hydrological year (June to May, i.e., "2020" is June 2020 to May 2021)
NZFFD$Year_hydro <- ifelse(NZFFD$Month %in% c(1,2,3,4,5), NZFFD$Year - 1, 
                           ifelse(NZFFD$Month %in% c(6,7,8,9,10,11,12), NZFFD$Year, NA))
table(NZFFD$Year_hydro, useNA = "ifany") # 895 NA's

## Remove variables not needed
NZFFD$YearV2 <- NULL



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
         Year, Year_hydro, Year_season, Season, Month, Day, #time variables
         org, institution, FishMethod, #variables specific to sampling
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
           Year, Year_hydro, Year_season, Season, Month, Day, #time variables
           org, institution, FishMethod, #variables specific to sampling
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

# Remove all fishing method except electric fishing, net and trap
NZFFD_abund <- NZFFD_abund %>% filter(FishMethod == "Electric fishing"| FishMethod == "Net" | FishMethod == "Trap")

#Remove consultants, individuals, unknown, and NA
NZFFD_abund <- NZFFD_abund %>%
  filter((org=="cawthron" | org=="council" | org=="doc" | org=="fish_and_game" | org=="niwa" | org=="university"))

#Examine the sampling purpose and remove if not appropriate
table(NZFFD_abund$samplingPurpose, useNA = "ifany")
##Remove sampling purposes that target species and
##therefore are more likely biased towards one species
NZFFD_abund <- NZFFD_abund %>%
  filter(!(samplingPurpose == "Abundance specific species" | 
             samplingPurpose == "Fish salvage" | 
             samplingPurpose == "Lengths and abundance specific species" |
             samplingPurpose == "Other" |
             samplingPurpose == "Presence or absence specific species")) #removes 156

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

DATA <- NZFFD_ene[!duplicated(NZFFD_ene[c("nzffdRecordNumber")]),]
SPECIES<-unique(NZFFD_ene$taxonName)
tmp<-split(NZFFD_ene$taxonName, NZFFD_ene$nzffdRecordNumber)

res <- lapply(tmp,function(x) match(SPECIES,x,nomatch=0)>0)
res <- do.call("rbind",res)
res <- data.frame(res)
names(res) <- SPECIES
res$nzffdRecordNumber <- as.numeric(rownames(res))

DATA <- DATA[order(DATA$nzffdRecordNumber),]
#DATA<-cbind(DATA,res)
DATA <- full_join(DATA, res, by="nzffdRecordNumber")

nrow(DATA)
length(unique(DATA$nzffdRecordNumber))

#Overwrite NZFFD_ene data
NZFFD_ene <- DATA

rm(tmp,res,DATA)


#Select rows of interest
colnames(NZFFD_ene)

NZFFD_ene <- NZFFD_ene %>%
  select(nzffdRecordNumber, nzsegment, Lat, Lon, catchmentName, catchmentNumber, #Variables related to identifability and location
         child_i, parent_i, dist_i, #variables derived for stream network modelling
         Year, Year_hydro, Year_season, Season, Month, Day, #time variables
         org, institution, FishMethod, #variables specific to sampling
         #Put an effort variable here
         `Anguilla dieffenbachii`) #fish species encounter


#Convert longfin eel FALSE/TRUE to 0/1
NZFFD_ene$`Anguilla dieffenbachii` <- factor(NZFFD_ene$`Anguilla dieffenbachii`, levels = c("FALSE", "TRUE"))
NZFFD_ene$`Anguilla dieffenbachii` <- as.numeric(NZFFD_ene$`Anguilla dieffenbachii`) - 1

#Add Data_source variable
NZFFD_ene$Data_source <- ifelse(NZFFD_ene$FishMethod == "Electric fishing", "Structured_EF", 
                        ifelse(NZFFD_ene$FishMethod %in% c("Net", "Trap"), "Structured_NetTrap", NA))



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

## removing zeros
NZFFD_eo <- NZFFD_eo %>% filter(totalCount > 0 | is.na(totalCount)) #none removed

#Remove duplicate presence-only data 
#only real impact on final data is removing 3 obs from 2020 (year_hydro), 1 obs removed from 1921 (doesn't impact final data)
NZFFD_eo <- NZFFD_eo[!duplicated(NZFFD_eo[c("nzffdRecordNumber")]),]

## Format data equivalently to NZFFD data
NZFFD_eo <- NZFFD_eo %>%
  select(nzffdRecordNumber, nzsegment, Lat, Lon, catchmentName, catchmentNumber, #Variables related to identifability and location
         child_i, parent_i, dist_i, #variables derived for stream network modelling
         Year, Year_hydro, Year_season, Season, Month, Day, #time variables
         org, institution, FishMethod) %>% #variables specific to sampling
  mutate("Anguilla dieffenbachii"=1, "Data_source" = "Unstructured")



##############################
# Select final time variable #
#############################


#presence/absence data
table(NZFFD_ene$Year_hydro, useNA = "ifany")
# 1965 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 
# 1   35   26   15   41    5    5   27   14   19    6    5   50   39    6    9   27   13   22   48    8   40   38   17   16   11    2 
# 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 <NA> 
#   5   27   32   30  118    2   64    2   15    2    7   61   20   13   19    6   15    3    5

#Remove all but Year_hyrdo and rename Year_hydro to Year
NZFFD_ene <- NZFFD_ene %>%
  select(-c("Year", "Year_season", "Season", "Month", "Day")) %>%
  rename("Year" = "Year_hydro")

#remove missing year data
NZFFD_ene <- NZFFD_ene %>% 
  filter(!is.na(Year)) # 5 removed

table(NZFFD_ene$Year, useNA = "ifany") 


#presence-only data
table(NZFFD_eo$Year_hydro, useNA = "ifany")

#Remove all but Year_hyrdo
NZFFD_eo <- NZFFD_eo %>%
  select(-c("Year", "Year_season", "Season", "Month", "Day")) %>%
  rename("Year" = "Year_hydro")

table(NZFFD_eo$Year, useNA = "ifany") #No data to remove
# 1921 1960 1978 1979 1980 1981 1984 1986 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2007 2008 2010 
# 2    1    3   26   16    1    2    2   13   37   20   11   15    9   19   40   18   20    7   46   11    2   16    2   14   15   28 
# 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 
# 24    5    7    6   13   24   20    6   28   17   33   10
  


################################
# Examine the data across time #
################################

#Very few observations prior to 1978 for both data sets so remove, remove 2023 as incomplete
NZFFD_ene <- NZFFD_ene %>% filter(Year >= 1978 & Year <= 2022)
NZFFD_eo <- NZFFD_eo %>% filter(Year >= 1978 & Year <= 2022)


#encounter/non-encounter data
ene_table <- addmargins(table(NZFFD_ene$`Anguilla dieffenbachii`, NZFFD_ene$Year)) ; ene_table #longfin eel

colnames(ene_table)[ene_table["Sum",] < 30]
# Keep all remaining years for now, can remove later



#######################
# Examine missingness #
#######################

colSums(is.na(NZFFD_ene))
colSums(is.na(NZFFD_eo))



##########
#  Save  #
##########

saveRDS(NZFFD_ene, file.path(data_taranaki_dir, "Taranaki_NZFFD_ene_data.rds"))
saveRDS(NZFFD_eo, file.path(data_taranaki_dir, "Taranaki_encounter_only_lf_data.rds"))

# saveRDS(NZFFD_abund_TRC_lf, file.path(data_taranaki_dir, "Taranaki_NZFFD_abund_data.rds"))
# write.csv(NZFFD_abund_TRC_lf, row.names = F, file = file.path(data_taranaki_dir, "Taranaki_NZFFD_abund_data.csv"))


# ###################
# # Plot e/n-e data #
# ###################
# 
# Data_to_plot <- NZFFD_ene
# Data_to_plot$encounter <- ifelse(round(Data_to_plot$`Anguilla dieffenbachii`)==1, "Encounter", "Non-encounter")
# 
# #Taranaki network by year
# tab <- table(Data_to_plot$encounter, Data_to_plot$Year)
# years <- unique(Data_to_plot$Year)[order(unique(Data_to_plot$Year))]
# data_text <- data.frame("Year"= years, label=paste0(tab[1,], "/", tab[2,]),
#                         x=174, y=-39.8)
# 
# 
# catchmap <- ggplot(Data_to_plot) +
#   geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
#   geom_point(aes(x = Lon, y = Lat, col = encounter), alpha = 0.6) +
#   facet_wrap(.~Year) +
#   xlab("Longitude") + ylab("Latitude") +
#   ggtitle("Longfin eel NZFFD encounter/non-encounter observations by year") +
#   guides(color = guide_legend(title = "")) +
#   scale_colour_manual(values = c("#E41A1C", "#377EB8")) +
#   theme_bw(base_size = 14) +
#   theme(axis.text = element_text(size = rel(0.5)),
#         axis.text.x = element_text(angle = 90))
# 
# catchmap <- catchmap +geom_text(
#   data = data_text,
#   mapping = aes(x = x, y = y, label = label)
# )
# 
# ggsave(file.path(fig_dir, "Taranaki_lf_observations_byYear.png"), catchmap, height = 12, width = 15)
# 
# 
# ###################################################
# 
# # Taranaki catchment
# 
# l2 <- lapply(1:nrow(network), function(x){
#   parent <- network$parent_s[x]
#   find <- network %>% filter(child_s == parent)
#   if(nrow(find)>0) out <- cbind.data.frame(network[x,], 'Lon2'=find$Lon, 'Lat2'=find$Lat)
#   if(nrow(find)==0) out <- cbind.data.frame(network[x,], 'Lon2'=NA, 'Lat2'=NA)
#   
#   return(out)
# })
# l2 <- do.call(rbind, l2)
# 
# 
# catchmap2 <- ggplot(Data_to_plot) +
#   geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
#   geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
#   geom_point(aes(x = Lon, y = Lat, col = encounter), size=3, alpha = 0.6) +
#   xlab("Longitude") + ylab("Latitude") +
#   ggtitle("Longfin eel NZFFD encounter/non-encounter observations") +
#   guides(color = guide_legend(title = "")) +
#   scale_colour_manual(values = c("#E41A1C", "#377EB8")) +
#   theme_bw(base_size = 14) +
#   theme(axis.text = element_text(size = rel(0.8)))
# 
# ggsave(file.path(fig_dir, "Taranaki_lf_observations.png"), catchmap2, height = 12, width = 15)
# 
# 
# 
# ############################
# # Plot encounter-only data #
# ############################
# 
# Data_to_plot_eo <- NZFFD_eo
# Data_to_plot_eo$present <- ifelse(round(Data_to_plot_eo$`Anguilla dieffenbachii`)==1, "Encounter", "Non-encounter")
# 
# #Load network data
# network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))
# 
# #Taranaki network by year
# tab <- table(Data_to_plot_eo$present, Data_to_plot_eo$Year)
# years <- unique(Data_to_plot_eo$Year)[order(unique(Data_to_plot_eo$Year))]
# data_text <- data.frame("Year"= years, label=tab[1,],
#                         x=174, y=-39.8)
# 
# 
# catchmap <- ggplot(Data_to_plot_eo) +
#   geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
#   geom_point(aes(x = Lon, y = Lat, col = present), alpha = 0.6) +
#   facet_wrap(.~Year) +
#   xlab("Longitude") + ylab("Latitude") +
#   ggtitle("Longfin eel NZFFD encounter-only observations by year") +
#   guides(color = guide_legend(title = "")) +
#   scale_colour_manual(values = "#E41A1C") +
#   theme_bw(base_size = 14) +
#   theme(axis.text = element_text(size = rel(0.5)),
#         axis.text.x = element_text(angle = 90))
# 
# catchmap <- catchmap +geom_text(
#   data = data_text,
#   mapping = aes(x = x, y = y, label = label)
# )
# 
# ggsave(file.path(fig_dir, "Taranaki_encounter_only_observations_byYear.png"), catchmap, height = 12, width = 15)
# 
# 
# ###################################################
# 
# # Taranaki catchment
# 
# catchmap2 <- ggplot(Data_to_plot_eo) +
#   geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
#   geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
#   geom_point(aes(x = Lon, y = Lat, col = present), size=3, alpha = 0.6) +
#   xlab("Longitude") + ylab("Latitude") +
#   ggtitle("Longfin eel NZFFD encounter-only observations") +
#   guides(color = guide_legend(title = "")) +
#   scale_colour_manual(values = "#E41A1C") +
#   theme_bw(base_size = 14) +
#   theme(axis.text = element_text(size = rel(0.8)))
# 
# ggsave(file.path(fig_dir, "Taranaki_encounter_only_observations.png"), catchmap2, height = 12, width = 15)




