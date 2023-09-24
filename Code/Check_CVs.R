################################
# Check CVs have been produced #
################################


rm(list=ls())


##############
#  Scenario  #
##############

scenarios <-   c("Taranaki data",
                 "1a", "1b", "1c", 
                 #"1d", #failed model
                 "2a", "2b", 
                 #"2c", "2d", #failed model
                 "3a", "3b", "3c", 
                 #"3d", #failed model
                 "4a", "4b", "4c", "4d")

network_type <- "full"

CV_failed <- list()

for(sce in 1:length(scenarios)){
  
  #################
  #  Directories  #
  #################
  
  model_path <- paste0(getwd(), "/Models")
  
  if(scenarios[sce] == "Taranaki data"){
    if(network_type == "downstream"){
      path <- file.path(model_path, "Taranaki_data_model_ds")
    }
    if(network_type == "full"){
      path <- file.path(model_path, "Taranaki_data_model")
    }
  }else{
    if(network_type == "downstream"){
      path <- file.path(model_path, paste0("Model_", scenarios[sce], "_ds"))
    }
    if(network_type == "full"){
      path <- file.path(model_path, paste0("Model_", scenarios[sce]))
    }
  }
  
  #Set a cv path
  path_cv <- file.path(path, "CV")
  
  n_fold <- 10
  i <- 1 #counter
  for(k in 1:n_fold){
    #Set cv k-fold path
    path_cv_k <- file.path(path_cv, paste0("Fold_", k))
    
    #If file doesn't exist save
    if(!file.exists(file.path(path_cv_k, paste0("Save.rds")))){
      CV_failed[[scenarios[sce]]][i] <- k
      i <- i + 1
    }
    
  }
  
}

CV_failed

# $`3c`
# [1]  1  2  3  5  6  7  9 10
# All CVs failed but two. Lapack routine dgesv: system is exactly singular

# $`4c`
# [1]  1  2  3  4  5  6  7  8  9 10
# All CVs failed.

# $`4d`
# [1] 1 2 3 4 6 8 9
# All CVs failed but three.


