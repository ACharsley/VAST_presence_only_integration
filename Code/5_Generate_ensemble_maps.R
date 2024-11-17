###################################################
##              Generate ensemble maps           ##
##                                               ##
##               Anthony Charsley                ##
##                 October 2024                  ##
###################################################


rm(list=ls())



##############
#  Packages  #
##############

library(tidyverse)



#################
#  Directories  #
#################

processed_data_path <- file.path(getwd(), "Data_processed")

model_path <- paste0(getwd(), "/Models")

# Taranaki_data_model (structured data only)
mod_struc_only_dir <- file.path(model_path, "Taranaki_data_model_SE")

# 1a (random 1n)
mod_rand_dir <- file.path(model_path, "Model_1a_SE")

# 2a (unsuitable habitats 1n)
mod_unsuithab_dir <- file.path(model_path, "Model_2a_SE")

# 3a and 3b (near roads 1n and 5n)
mod_nearroads1n_dir <- file.path(model_path, "Model_3a_SE")
mod_nearroads5n_dir <- file.path(model_path, "Model_3b_SE")

# 4a (unsuitable habitats and near roads 1n )
mod_unsuithab_nearroads_dir <- file.path(model_path, "Model_4a_SE")



####################################
# Load model POE, SE's and extract #
####################################

# Taranaki_data_model (structured data only)
load(file.path(mod_struc_only_dir, "Probability_of_encounter.RData"))
POE_mod_struc_only <- Probability_of_encounter

load(file.path(mod_struc_only_dir, "POC_SE.RData"))
SE_mod_struc_only <- POC_SE
SE_mod_struc_only <- sum(SE_mod_struc_only, na.rm = T)
rm(Probability_of_encounter) ; rm(POC_SE)


# 1a (random 1n)
load(file.path(mod_rand_dir, "Probability_of_encounter.RData"))
POE_mod_rand <- Probability_of_encounter

load(file.path(mod_rand_dir, "POC_SE.RData"))
SE_mod_rand <- POC_SE
SE_mod_rand <- sum(SE_mod_rand, na.rm = T)
rm(Probability_of_encounter) ; rm(POC_SE)


# 2a (unsuitable habitats 1n)
load(file.path(mod_unsuithab_dir, "Probability_of_encounter.RData"))
POE_mod_unsuithab <- Probability_of_encounter

load(file.path(mod_unsuithab_dir, "POC_SE.RData"))
SE_mod_unsuithab <- POC_SE
SE_mod_unsuithab <- sum(SE_mod_unsuithab, na.rm = T)
rm(Probability_of_encounter) ; rm(POC_SE)


# 3a and 3b (near roads 1n and 5n)
load(file.path(mod_nearroads1n_dir, "Probability_of_encounter.RData"))
POE_mod_nearroads1n <- Probability_of_encounter

load(file.path(mod_nearroads1n_dir, "POC_SE.RData"))
SE_mod_nearroads1n <- POC_SE
SE_mod_nearroads1n <- sum(SE_mod_nearroads1n, na.rm = T)
rm(Probability_of_encounter) ; rm(POC_SE)

load(file.path(mod_nearroads5n_dir, "Probability_of_encounter.RData"))
POE_mod_nearroads5n <- Probability_of_encounter

load(file.path(mod_nearroads5n_dir, "POC_SE.RData"))
SE_mod_nearroads5n <- POC_SE
SE_mod_nearroads5n <- sum(SE_mod_nearroads5n, na.rm = T)
rm(Probability_of_encounter) ; rm(POC_SE)


# 4a (unsuitable habitats and near roads 1n )
load(file.path(mod_unsuithab_nearroads_dir, "Probability_of_encounter.RData"))
POE_mod_unsuithab_nearroads <- Probability_of_encounter

load(file.path(mod_unsuithab_nearroads_dir, "POC_SE.RData"))
SE_mod_unsuithab_nearroads <- POC_SE
SE_mod_unsuithab_nearroads <- sum(SE_mod_unsuithab_nearroads, na.rm = T)
rm(Probability_of_encounter) ; rm(POC_SE)



#################################
# Load cross validation results #
#################################

# Taranaki_data_model (structured data only)
load(file.path(model_path, "Taranaki_data_model", "CV", "CV_results.RData"))
AUC_mod_struc_only <- median(cv_list$AUC$estimates)
rm(cv_list)

# 1a (random 1n)
load(file.path(model_path, "Model_1a", "CV", "CV_results.RData"))
AUC_mod_rand <- median(cv_list$AUC$estimates)
rm(cv_list)

# 2a (unsuitable habitats 1n)
load(file.path(model_path, "Model_2a", "CV", "CV_results.RData"))
AUC_mod_unsuithab <- median(cv_list$AUC$estimates)
rm(cv_list)

# 3a and 3b (near roads 1n and 5n)
load(file.path(model_path, "Model_3a", "CV", "CV_results.RData"))
AUC_mod_nearroads1n <- median(cv_list$AUC$estimates)
rm(cv_list)

load(file.path(model_path, "Model_3b", "CV", "CV_results.RData"))
AUC_mod_nearroads5n <- median(cv_list$AUC$estimates)
rm(cv_list)

# 4a (unsuitable habitats and near roads 1n )
load(file.path(model_path, "Model_4a", "CV", "CV_results.RData"))
AUC_mod_unsuithab_nearroads <- median(cv_list$AUC$estimates)
rm(cv_list)



#####################
# Calculate weights #
#####################

# Weight 1
w1_mod_struc_only <- AUC_mod_struc_only / (AUC_mod_struc_only + AUC_mod_rand + AUC_mod_unsuithab + AUC_mod_nearroads1n + 
                                             AUC_mod_nearroads5n + AUC_mod_unsuithab_nearroads)

w1_mod_rand <- AUC_mod_rand / (AUC_mod_struc_only + AUC_mod_rand + AUC_mod_unsuithab + AUC_mod_nearroads1n + 
                                             AUC_mod_nearroads5n + AUC_mod_unsuithab_nearroads)

w1_mod_unsuithab <- AUC_mod_unsuithab / (AUC_mod_struc_only + AUC_mod_rand + AUC_mod_unsuithab + AUC_mod_nearroads1n + 
                                 AUC_mod_nearroads5n + AUC_mod_unsuithab_nearroads)

w1_mod_nearroads1n <- AUC_mod_nearroads1n / (AUC_mod_struc_only + AUC_mod_rand + AUC_mod_unsuithab + AUC_mod_nearroads1n + 
                                           AUC_mod_nearroads5n + AUC_mod_unsuithab_nearroads)

w1_mod_nearroads5n <- AUC_mod_nearroads5n / (AUC_mod_struc_only + AUC_mod_rand + AUC_mod_unsuithab + AUC_mod_nearroads1n + 
                                               AUC_mod_nearroads5n + AUC_mod_unsuithab_nearroads)

w1_mod_unsuithab_nearroads <- AUC_mod_unsuithab_nearroads / (AUC_mod_struc_only + AUC_mod_rand + AUC_mod_unsuithab + AUC_mod_nearroads1n + 
                                               AUC_mod_nearroads5n + AUC_mod_unsuithab_nearroads)


w1_mod_struc_only + w1_mod_rand + w1_mod_unsuithab + w1_mod_nearroads1n +
  w1_mod_nearroads5n + w1_mod_unsuithab_nearroads


# Weight 2
w2_mod_struc_only <- 1 - SE_mod_struc_only / (SE_mod_struc_only + SE_mod_rand + SE_mod_unsuithab + SE_mod_nearroads1n + 
                                                SE_mod_nearroads5n + SE_mod_unsuithab_nearroads)

w2_mod_rand <- 1 - SE_mod_rand / (SE_mod_struc_only + SE_mod_rand + SE_mod_unsuithab + SE_mod_nearroads1n + 
                                                SE_mod_nearroads5n + SE_mod_unsuithab_nearroads)

w2_mod_unsuithab <- 1 - SE_mod_unsuithab / (SE_mod_struc_only + SE_mod_rand + SE_mod_unsuithab + SE_mod_nearroads1n + 
                                    SE_mod_nearroads5n + SE_mod_unsuithab_nearroads)

w2_mod_nearroads1n <- 1 - SE_mod_nearroads1n / (SE_mod_struc_only + SE_mod_rand + SE_mod_unsuithab + SE_mod_nearroads1n + 
                                              SE_mod_nearroads5n + SE_mod_unsuithab_nearroads)

w2_mod_nearroads5n <- 1 - SE_mod_nearroads5n / (SE_mod_struc_only + SE_mod_rand + SE_mod_unsuithab + SE_mod_nearroads1n + 
                                                  SE_mod_nearroads5n + SE_mod_unsuithab_nearroads)

w2_mod_unsuithab_nearroads <- 1 - SE_mod_unsuithab_nearroads / (SE_mod_struc_only + SE_mod_rand + SE_mod_unsuithab + SE_mod_nearroads1n + 
                                                  SE_mod_nearroads5n + SE_mod_unsuithab_nearroads)


# Final weights
w_mod_struc_only <- (w1_mod_struc_only + w2_mod_struc_only)/2
w_mod_rand <- (w1_mod_rand + w2_mod_rand)/2
w_mod_unsuithab <- (w1_mod_unsuithab + w2_mod_unsuithab)/2
w_mod_nearroads1n <- (w1_mod_nearroads1n + w2_mod_nearroads1n)/2
w_mod_nearroads5n <- (w1_mod_nearroads5n + w2_mod_nearroads5n)/2
w_mod_unsuithab_nearroads <- (w1_mod_unsuithab_nearroads + w2_mod_unsuithab_nearroads)/2



