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
# library(ROCR) #auc function



####################
#  Call functions  #
####################

source(paste0(getwd(), "/Code/funcs.R"))



#################
#  Directories  #
#################

EM_path <- paste0(getwd(), "/Models/EMs")
# VAST_input_data_dir <- paste0(getwd(), "/Data_processed/VAST_input_data")



# ##########################
# #  Load VAST input data  #
# ##########################
# 
# VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data_OMs.rds"))



########################
#  Modelling scenario  #
########################

## Arguments to determine what OM to generate results for ##
task_id = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")) #task_id=1

if(task_id == 1){scenario = "OM_1a"}
if(task_id == 2){scenario = "OM_2a"}
if(task_id == 3){scenario = "OM_3a"}
if(task_id == 4){scenario = "OM_3b"}
if(task_id == 5){scenario = "OM_4a"}

print(scenario)

# Estimating models to loop over
data_sample <- c("sample_rand1n",
                 "sample_rand2n",
                 "sample_rand5n",
                 
                 "sample_unsuithab1n",
                 "sample_unsuithab2n",
                 "sample_unsuithab5n",
                 
                 "sample_nearroads1n",
                 "sample_nearroads2n",
                 "sample_nearroads5n",
                 
                 "sample_unsuithab_nearroads1n",
                 "sample_unsuithab_nearroads2n",
                 "sample_unsuithab_nearroads5n")

# Replications
reps <- c(1:100)



####################
# Generate metrics #
####################

#Objects to save performance metrics in 
PM_list <- list()


start_time <- Sys.time()

print("Rep not found or failed to generate metric for:")

# Estimating model
for(mod in data_sample){ # mod = "sample_rand5n"
  
  ############################
  # Load operating model POE #
  ############################
  
  save_OM_path <- file.path(getwd(), "Models", paste0("Model_",scenario))
  load(file.path(save_OM_path, "POE_list.RData"))
  POE_OM <- POE_list$Probability_of_encounter ; POE_OM_SE <- POE_list$SE ; rm(POE_list)
  
  ####
  
  i <- 1 #Counter for storing objects
  
  # Replicate
  for(r in reps){ # r = 39
    
    # Path with simulation study model results 
    save_EM_path <- file.path(EM_path, paste0("EM_output_", scenario), paste0("Rep_",r,"_",mod))
    #save_EM_path <- file.path(EM_path, paste0("EM_output_", scenario,"_v1"), paste0("Rep_",r,"_",mod))
    
    ################################
    # Generate performance metrics #
    ################################
    
    tryCatch({
      load(file.path(save_EM_path, "POE_list.RData"))
      POE_EM <- POE_list$Probability_of_encounter ; POE_EM_SE <- POE_list$SE ; rm(POE_list)
      
      
      ## 1. Model bias (intercept of regression)
      mod_df <- data.frame("POE_OM" = logit(ifelse(as.vector(POE_OM)==0,0.0001,
                                                   ifelse(as.vector(POE_OM)==1,0.9999,as.vector(POE_OM)))), 
                           "POE_EM" = logit(ifelse(as.vector(POE_EM)==0,0.0001,
                                                   ifelse(as.vector(POE_EM)==1,0.9999,as.vector(POE_EM)))), 
                           "year" = rep(c(1978:2022), each=nrow(POE_OM)))
      
      #mod_cal1 <- lm(POE_OM ~ as.factor(year) + POE_EM, data = mod_df)
      mod_cal1 <- lm(POE_OM ~ POE_EM, data = mod_df)
      #summary(mod_cal1)
      PM_list[["Bias"]][[mod]][[i]] <- unname(mod_cal1$coefficients["(Intercept)"])
      
      
      ## 2. Model spread (slope of regression) 
      PM_list[["Spread"]][[mod]][[i]] <- unname(mod_cal1$coefficients["POE_EM"])
      
      
      ## 3. Model bias - JT metric (gradient of regression)
      #mod_cal2 <- lm(POE_EM ~ as.factor(year) + POE_OM, data = mod_df)
      mod_cal2 <- lm(POE_EM ~ POE_OM, data = mod_df)
      #summary(mod_cal2)
      PM_list[["Bias_JT"]][[mod]][[i]] <- unname(mod_cal2$coefficients["POE_OM"])
      
      
      
      # ################ testing without logit transformation #####################
      # ## Model bias (intercept of regression)
      # mod_df2 <- data.frame("POE_OM" = as.vector(POE_OM), 
      #                      "POE_EM" = as.vector(POE_EM), 
      #                      "year" = rep(c(1978:2022), each=nrow(POE_OM)))
      # 
      # 
      # mod_cal3 <- lm(POE_OM ~ as.factor(year) + POE_EM, data = mod_df2)
      # #mod_cal3 <- lm(POE_OM ~ POE_EM, data = mod_df)
      # #summary(mod_cal3)
      # PM_list[["Bias_2"]][[mod]][[i]] <- unname(mod_cal3$coefficients["(Intercept)"])
      # 
      # 
      # ## Model spread (slope of regression) 
      # PM_list[["Spread_2"]][[mod]][[i]] <- unname(mod_cal3$coefficients["POE_EM"])
      # 
      # 
      # ## Model bias - JT metric (gradient of regression)
      # mod_cal4 <- lm(POE_EM ~ as.factor(year) + POE_OM, data = mod_df2)
      # #mod_cal4 <- lm(POE_EM ~ POE_OM, data = mod_df)
      # #summary(mod_cal4)
      # PM_list[["Bias_JT_2"]][[mod]][[i]] <- unname(mod_cal4$coefficients["POE_OM"])
      # ###########################################################################
      
      
      ## 4. RMSE (yearly)
      PM_list[["RMSE"]][[mod]][[i]] <- sqrt(mean((POE_EM - POE_OM)^2)) #sqrt(colMeans((POE_EM - POE_OM)^2))
      
      
      ## 5. 50% confidence interval coverage
      UCI_50 <- (POE_EM + 0.674 * POE_EM_SE)
      LCI_50 <- (POE_EM - 0.674 * POE_EM_SE)
      cover50 <- (POE_OM <= UCI_50 & POE_OM >= LCI_50) + 0
      cover50 <- (sum(cover50)/length(cover50))*100
      PM_list[["Coverage_50"]][[mod]][[i]] <- cover50
      
      # ## same calculation but across the mean
      # UCI_mt_50 <- (colMeans(POE_EM) + 0.674 * colMeans(POE_EM_SE))
      # LCI_mt_50 <- (colMeans(POE_EM) - 0.674 * colMeans(POE_EM_SE))
      # cover_mt_50 <- (colMeans(POE_OM) <= UCI_mt_50 & colMeans(POE_OM) >= LCI_mt_50) + 0
      # cover_mt_50 <- (sum(cover_mt_50)/length(cover_mt_50))*100
      # PM_list[["Coverage_mt_50"]][[mod]][[i]] <- cover_mt_50
      
      
      ## 6. 80% confidence interval coverage
      UCI_80 <- (POE_EM + 1.282 * POE_EM_SE)
      LCI_80 <- (POE_EM - 1.282 * POE_EM_SE)
      cover80 <- (POE_OM <= UCI_80 & POE_OM >= LCI_80) + 0
      cover80 <- (sum(cover80)/length(cover80))*100
      PM_list[["Coverage_80"]][[mod]][[i]] <- cover80
      
      
      ## 7. 95% confidence interval coverage
      UCI_95 <- (POE_EM + 1.960 * POE_EM_SE)
      LCI_95 <- (POE_EM - 1.960 * POE_EM_SE)
      cover95 <- (POE_OM <= UCI_95 & POE_OM >= LCI_95) + 0
      cover95 <- (sum(cover95)/length(cover95))*100
      PM_list[["Coverage_95"]][[mod]][[i]] <- cover95
      
      
      # ## 8. AUC
      # ## Extract the POE estimate for the EM at each OM observation ##
      # rows <- as.character(VAST_input_data[[scenario]]$Child_i) #this is the POE site (row in data)
      # columns <- as.character(VAST_input_data[[scenario]]$Year) #this is the POE
      # coords <- cbind(rows,columns)
      # 
      # EM_R_i <- POE_EM[coords]
      # ##
      # ## Extract OM observations ##
      # OM_obs <- round(VAST_input_data[[scenario]]$Catch_KG)
      # OM_obs <- factor(as.logical(OM_obs))
      # levels(OM_obs) <- c("FALSE", "TRUE") #ensure the levels are F/T
      # ##
      # ## Calculate AUC
      # # Format predictions and test data for calculating AUC
      # pred_AUC <- prediction(EM_R_i, OM_obs)
      # 
      # #Area under the receiver operator characteristic curve
      # AUC <- performance(pred_AUC, "auc")@y.values #find and store AUC values
      # ##
      # 
      # PM_list[["AUC"]][[mod]][[i]] <- AUC[[1]]
      
      
      #Update counter:
      i <- i+1
    },
    error=function(e) {
      print(paste0("OM: ",scenario,", data_sample: ",mod,", rep: ",r))
      e
      }
    ) #If something fails then TryCatch will return an error then move to the next
  }
}


time = Sys.time() - start_time ; print(paste0("Performance metrics run time: ", time))



########
# Save #
########

saveRDS(PM_list, file.path(EM_path, paste0("Perf_metrics_",scenario,".RDS")))

