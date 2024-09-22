###################################################
##         Generate performance metrics for      ##
##              simulation experiment            ##
##                                               ##
##               Anthony Charsley                ##
##                September 2024                 ##
###################################################



rm(list=ls())



##############
#  Packages  #
##############

library(tidyverse)



#################
#  Directories  #
#################

EM_path <- paste0(getwd(), "/Models/EMs")



####################
#  Call functions  #
####################

source(paste0(getwd(), "/Code/funcs.R"))



########################
#  Modelling scenario  #
########################

# Operating models
scenarios <- c("OM_1a", #"OM_1b", #these didn't converge
               "OM_2a", #"OM_2b",
               "OM_3a", "OM_3b",
               "OM_4a" #,"OM_4b"
               )

# Estimating models
data_sample <- c("sample_rand1n",
                 "sample_rand5n",
                 "sample_unsuithab1n",
                 "sample_unsuithab5n",
                 "sample_nearroads1n",
                 "sample_nearroads5n",
                 "sample_unsuithab_nearroads1n",
                 "sample_unsuithab_nearroads5n")

# Replications
reps <- c(1:10)

#Objects to save performance metrics in 
PM_list <- list()


start_time <- Sys.time()
# Operating model
for(sce in scenarios){ # sce = "OM_1a"
  
  print(sce)
  
  # Estimating model
  for(mod in data_sample){ # mod = "sample_rand1n"
    
    ############################
    # Load operating model POE #
    ############################
    
    save_OM_path <- file.path(getwd(), "Models", paste0("Model_",sce))
    load(file.path(save_OM_path, "Probability_of_encounter.RData"))
    POE_OM <- Probability_of_encounter ; rm(Probability_of_encounter)
    
    ####
    
    i <- 1 #Counter for storing objects
    
    # Replicate
    for(r in reps){ # r = 1
      
      # Path with simulation study model results 
      save_EM_path <- file.path(EM_path, paste0("EM_output_", sce), paste0("Rep_",r,"_",mod))
      
      
      ################################
      # Generate performance metrics #
      ################################
      
      tryCatch({
        load(file.path(save_EM_path, "Probability_of_encounter.RData"))
        POE_EM <- Probability_of_encounter ; rm(Probability_of_encounter)
        
        
        
        ## 1. Pearson's correlation coefficient
        PM_list[["Pearson"]][[paste0(sce, "_", mod)]][[i]] <- diag(cor(POE_OM, POE_EM, method = "pearson"))
        
        ## 2. Spearman's rank correlation
        PM_list[["Spearman"]][[paste0(sce, "_", mod)]][[i]] <- diag(cor(POE_OM, POE_EM, method = "spearman"))
        
        ## 3. RMSE (yearly)
        PM_list[["RMSE"]][[paste0(sce, "_", mod)]][[i]] <- sqrt(colMeans((POE_EM - POE_OM)^2))
        
        ## 4. Average error (yearly)
        PM_list[["AVE"]][[paste0(sce, "_", mod)]][[i]] <- colMeans(POE_EM - POE_OM)
        
        ## 5. Model bias (intercept of regression)
        mod_df <- data.frame("obs" = as.vector(POE_OM), 
                             "pred" = as.vector(POE_EM), 
                             "year" = rep(c(1978:2022), each=nrow(POE_OM)))
        mod_cal <- lm(obs ~ as.factor(year) + pred, data = mod_df)
        #summary(mod_cal)
        PM_list[["Bias"]][[paste0(sce, "_", mod)]][[i]] <- unname(mod_cal$coefficients["(Intercept)"])
        
        ## 6. Model spread (slope of regression) 
        PM_list[["Spread"]][[paste0(sce, "_", mod)]][[i]] <- unname(mod_cal$coefficients["pred"])
        
        ## 7. Reliability of slope
        mod_true <- lm(obs ~ as.numeric(year), data = mod_df)
        #summary(mod_true)
        
        mod_est <- lm(pred ~ as.numeric(year), data = mod_df)
        #summary(mod_est)
        
        PM_list[["Trend_rel"]][[paste0(sce, "_", mod)]][[i]] <- unname(mod_est$coefficients["as.numeric(year)"] - 
                                                                       mod_true$coefficients["as.numeric(year)"])
        
        #Update counter:
        i <- i+1
      },
      error=function(e) e) #If something fails then TryCatch will return an error then move to the next
    }
  }
}
time = Sys.time() - start_time ; print(paste0("Performance metrics run time: ", time))



########
# Save #
########

saveRDS(PM_list, file.path(EM_path, "Perf_metrics.RDS"))

