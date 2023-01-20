###################################################
##      Assemble presence-only data for the      ##
##                 Taranaki region               ##
##                                               ##
##               Anthony Charsley                ##
##                 January 2022                  ##
###################################################

# This code assembles presence-only data, currently using
# abundance data from the NZFFD collected by Taranaki
# Regional Council.

###################################################



rm(list=ls())


#################
#  Directories  #
#################

data_taranaki_dir <- "./Data_processed/Taranaki"


##############
#  Packages  #
##############

library(tidyverse)


###############
#  Load data  #
###############

#NZFFD abundance data
NZFFD_abund_TRC_lf <- readRDS(file.path(data_taranaki_dir, "Taranaki_NZFFD_abund_data.rds"))


########################
# Set data to presence #
########################

presence_only_lf_data <- NZFFD_abund_TRC_lf %>%
  filter(totalCount > 0) %>%
  select(-c(taxonName, totalCount))


#############
# Save data #
#############

saveRDS(presence_only_lf_data, file.path(data_taranaki_dir, "Taranaki_presence_only_lf_data.rds"))
