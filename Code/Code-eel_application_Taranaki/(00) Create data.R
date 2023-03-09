###################################################
##         Create data by running all code       ##
##                                               ##
##               Anthony Charsley                ##
##                February 2022                  ##
###################################################

rm(list = ls())

#Create Taranaki stream network (Taranaki_network_aa.rds)
source("./Code/Code-eel_application_Taranaki/(aa) Create_Taranaki_network.R")
rm(list = ls())

#Examine dam locations and build Taranaki_dam_locations.csv to evaluate
source("./Code/Code-eel_application_Taranaki/(ab) Dam data.R")
rm(list = ls())

#Create dam information and add to taranaki stream network (Taranaki_network.rds)
source("./Code/Code-eel_application_Taranaki/(ac) Create dam info.R")
rm(list = ls())

#Assemble the NZFFD data for the Taranaki region
source("./Code/Code-eel_application_Taranaki/(b) Assemble_Taranaki_NZFFD_data.R")
rm(list = ls())

#Assemble the Habitat data for the Taranaki region
source("./Code/Code-eel_application_Taranaki/(c) Assemble_Taranaki_habitat_data.R")
rm(list = ls())

#Assemble the presence-only data
source("./Code/Code-eel_application_Taranaki/(d) Assemble_Taranaki_presence-only_data.R")
rm(list = ls())

#Perform habitat suitability modelling
source("./Code/Code-eel_application_Taranaki/(e) Build_habitat_suitability_model")
rm(list = ls())

#Generate pseudo-absence data
source("./Code/Code-eel_application_Taranaki/(f) Generate_pseudo-absence_data.R")
rm(list = ls())

#Combine data and create downstream data
source("./Code/Code-eel_application_Taranaki/(g) Combine_and_create_ds_data.R")
rm(list = ls())
