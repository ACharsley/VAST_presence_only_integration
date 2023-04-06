###################################################
##         Create data by running all code       ##
##                                               ##
##               Anthony Charsley                ##
##                  March 2023                   ##
###################################################

rm(list = ls())

#Create Taranaki stream network (Taranaki_network_aa.rds)
source("./Code/Create_study_data/(aa) Create_Taranaki_network.R", echo=T)
rm(list = ls())

#Examine dam locations and build Taranaki_dam_locations.csv to evaluate
source("./Code/Create_study_data/(ab) Dam data.R", echo=T)
rm(list = ls())

#Create dam information and add to taranaki stream network (Taranaki_network.rds)
source("./Code/Create_study_data/(ac) Create dam info.R", echo=T)
rm(list = ls())

#Assemble the NZFFD data for the Taranaki region
source("./Code/Create_study_data/(b) Assemble_Taranaki_NZFFD_data.R", echo=T)
rm(list = ls())

#Assemble the Habitat data for the Taranaki region
source("./Code/Create_study_data/(c) Assemble_Taranaki_habitat_data.R", echo=T)
rm(list = ls())

#Perform habitat suitability modelling
source("./Code/Create_study_data/(d) Build_habitat_suitability_model.R", echo=T)
rm(list = ls())

#Generate pseudo-absence data
source("./Code/Create_study_data/(e) Generate_pseudo-absence_data.R", echo=T)
rm(list = ls())

#Combine data and create downstream data
source("./Code/Create_study_data/(f) Combine_and_create_ds_data.R", echo=T)
rm(list = ls())
