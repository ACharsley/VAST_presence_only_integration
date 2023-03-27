###################################################
##  Longfin eel models for the Taranaki region   ##
##                                               ##
##              Anthony Charsley                 ##
##               February 2023                   ##
###################################################

# This code builds a models with presence/absence 
# data and presence-only/pseudo-absence data
#

###########################################


rm(list=ls())


##############
#  Packages  #
##############

library(tidyverse)
library(VAST)


#################
#  Directories  #
#################

data_dir <- "./Data_processed"
raw_data_dir <- "./Data_raw"
data_taranaki_dir <- "./Data_processed/Taranaki"
fig_dir <- "./Data_processed/Taranaki/Figures"
pseudoabsence_data_dir <- "./Data_processed/Taranaki/Pseudo_absence_data"


# ####################
# #  Call functions  #
# ####################
# 
# source("./Code/Code-eel_application_Taranaki/funcs.R")


###################
#  Load datasets  #
###################

data_indicator <- c("Taranaki data", 
                    "Sample1a", "Sample1b", "Sample1c", "Sample1d", 
                    "Sample2a", "Sample2b", "Sample2c", "Sample2d")[1]

print(data_indicator)

## Set data ##
if(data_indicator == "Taranaki data") load(file.path(data_taranaki_dir, "Taranaki_data.RData")) ; taranaki_data <- Taranaki_data_with_ds
if(data_indicator == "Sample1a"){ load(file.path(data_taranaki_dir, "Taranaki_data_1a.RData")) ; taranaki_data <- Taranaki_data_1a_with_ds}
if(data_indicator == "Sample1b"){ load(file.path(data_taranaki_dir, "Taranaki_data_1b.RData")) ; taranaki_data <- Taranaki_data_1b_with_ds}
if(data_indicator == "Sample1c"){ load(file.path(data_taranaki_dir, "Taranaki_data_1c.RData")) ; taranaki_data <- Taranaki_data_1c_with_ds}
if(data_indicator == "Sample1d"){ load(file.path(data_taranaki_dir, "Taranaki_data_1d.RData")) ; taranaki_data <- Taranaki_data_1d_with_ds}

#Spatially biased data here

####



##Network
network <- taranaki_data$network

#Format network data
Network_sz = network %>% select(parent_s,child_s,dist_s)
Network_sz_LL = network %>% select(parent_s, child_s, dist_s, Lat, Lon)

##Habitat data
X_gctp <- taranaki_data$X_gctp

##NZFFD / presence-only / pseudo-absence data
obs <- taranaki_data$obs


########################
# Set data up for VAST #
########################

addmargins(table(obs$FishMethod, obs$org, useNA = "ifany"))
#NOTE: There are very limited net and trapping data. I will therefore
#      remove these observations. In addition, I will assume that pseudo-absence
#      data was 'electric fished'.

## 1. Build data type variable

obs$Data_type <- ifelse(obs$org %in% c("presence_only", "pseudo_absence"), "PO_PA", "Presence_absence")

fct_count(obs$Data_type)


## 2. Examine fishing methods

##Set up 'gear'
## Fishing Method:
fct_count(obs$FishMethod)

#Remove Net and trap data
obs <- obs %>% filter(!(FishMethod %in% c("Net", "Trap")))

#Convert NA's (pseudo-absences) to "Electric fishing"
obs[is.na(obs$FishMethod), "FishMethod"] <- "Electric fishing"


## NOTE: Fishing method does not need to be included as a catchability covariate
##       as all methods of fishing is electric fishing.



## 3. Examine fish samplers

## Organisations
fct_count(obs$org)

#Reduce to two groups: 
#     - council_doc_niwa (Council, doc and NIWA - similar target species, sampling protocols likely to be followed) 
#     - others (consultants, fish&game and university - mixed catchability)

obs$org <- fct_collapse(obs$org,
                        CouncilDocNiwa = c("council","niwa","doc"),
                        Other = c("cawthron","consultants","fish_and_game","university")) #,po_pa = c("presence_only", "pseudo_absence")


## NOTE: I will assume that presence-only/pseudo-absence data is equally sampled by 'CouncilDOCNiwa' and other

#Remove "presence_only", "pseudo_absence" data
obs_po <- obs %>% filter(org == "presence_only")
obs_pa <- obs %>% filter(org == "pseudo_absence")

obs_filtered <- obs %>% filter(!(org %in% c("presence_only", "pseudo_absence")))

rm(obs) #drop original obs dataset to net get confused

#Shuffle "presence_only" data and "pseudo_absence" data, and set new org values
set.seed(27012023)
obs_po[sample(nrow(obs_po)), "org"] <- rep(c("CouncilDocNiwa", "Other"), length.out=nrow(obs_po))
obs_pa[sample(nrow(obs_pa)), "org"] <- rep(c("CouncilDocNiwa", "Other"), length.out=nrow(obs_pa))

#Reconnect all datasets
obs <- rbind(obs_filtered, obs_po, obs_pa)

obs <- obs %>% droplevels()

fct_count(obs$org)


## 4. Examine Years of data
addmargins(table(obs$`Anguilla dieffenbachii`, obs$Year))
#addmargins(table(obs$Year, obs$`Anguilla dieffenbachii`))


table(obs$Year, obs$Data_type) #Some years, data type doesn't overlap. Is that an issue?


##NOTE: I have left years for now until I see how the models run. I will come back to this.

#However, remove 2022 as it's incomplete
obs <- obs %>% filter(Year != 2022)


## Examine data

## Add method_organisation variable
table(obs$FishMethod, obs$org, obs$Data_type, useNA = "ifany")



## 5. Create final dataset

#Data for longfin eel catch
Data_Geostat = data.frame(Lon=obs$Lon,
                          Lat=obs$Lat,
                          Child_i=obs$child_i,
                          Year=obs$Year,
                          Catch_KG=obs$`Anguilla dieffenbachii`, 
                          Data_type=obs$Data_type,
                          Sampler = obs$org,
                          Length = obs$dist_i) 

set.seed(22)
Data_Geostat[,'Catch_KG'] = Data_Geostat[,'Catch_KG'] * exp(1e-3*rnorm(nrow(Data_Geostat)))


##Final data set
pander::pandoc.table( Data_Geostat[1:6,], digits=6 ) #table of the first 6 observations

###############
###############


########################
#  Habitat covariates  #
########################

REC_covs <- c("std_log_Dist2Coast", "std_StreamOrder", "std_sinuosity", "std_segslpmean", "std_log_seg_ro_mm", 
              "std_loc_elev", "std_loc_rnvar", "std_loc_rd100", "std_local_twarm", "std_log_DSDIST2LAK", "std_FWENZ_dsMaxSlope",
              "std_FWENZ_dsAveSlope", "std_us_ind", "std_FWENZ_USLakePC", "std_FWENZ_segShade", "std_MeanFlowCumecs")


Barrier_covs <- c("std_Years_since_barrier", "Barrier_present") #NOTE: will only use one or the other, not both


# All covariate names
covars_all <- c(REC_covs, Barrier_covs)

n_p <- length(covars_all)

#Check they match the dimnames
all(covars_all == dimnames(X_gctp)[[4]])


#Create habitat data matrix at observation locations
n_i <- nrow(Data_Geostat)
yrs <- min(Data_Geostat$Year):max(Data_Geostat$Year)
n_t <- length(yrs)
hab_children <- as.numeric(rownames(X_gctp))

X_gctp <- X_gctp[,,as.character(yrs),,drop=F] # This ensures that if any years are dropped then X_gctp has the right years still


X_itp <- array(0, dim=c(n_i,n_t,n_p))
for(i in 1:n_i){
  for(p in 1:n_p){
    child_i <- Data_Geostat$Child_i[i]
    index <- which(hab_children == child_i)
    X_itp[i,,p] <- X_gctp[index,,as.character(yrs),p] #All categories are the same so dont need to loop by c
  }
}

#check habitat covariates are right
all(rownames(X_gctp) == network$child_s)
all(rownames(X_itp) == Data_Geostat$Child_i)



#############################
#  Catchability covariates  #
#############################

## Set variable for data type VAST input
table(Data_Geostat$Data_type, useNA = "ifany")

Data_Geostat$Data_type_input <- factor(ifelse(Data_Geostat$Data_type == "Presence_absence", "0", "1"))
table(Data_Geostat$Data_type_input, useNA = "ifany")


## Examine sampler (organisation) catchability covariate ##
table(Data_Geostat$Sampler, useNA = "ifany")


########
# Save #
########

if(data_indicator == "Taranaki data") save.image(file.path(data_taranaki_dir, paste0("general_inputs_taranaki.Rdata")))
if(data_indicator == "Sample1a") save.image(file.path(data_taranaki_dir, paste0("general_inputs_sample1a.Rdata")))
if(data_indicator == "Sample1b") save.image(file.path(data_taranaki_dir, paste0("general_inputs_sample1b.Rdata")))
if(data_indicator == "Sample1c") save.image(file.path(data_taranaki_dir, paste0("general_inputs_sample1c.Rdata")))
if(data_indicator == "Sample1d") save.image(file.path(data_taranaki_dir, paste0("general_inputs_sample1d.Rdata")))



#########
# Plots #
#########
if(data_indicator == "Taranaki data"){
  
  Data_to_plot <- obs
  Data_to_plot$present <- ifelse(round(Data_to_plot$`Anguilla dieffenbachii`)==1, "Present", "Absent")
  
  ## Load full network for plots
  raw_data_dir <- "./Data_raw"
  netfull <- readRDS(file.path(raw_data_dir, "NZ_network.rds"))
  
  #New Zealand map
  nzmap <- ggplot() +
    geom_point(data = netfull, aes(x = long, y = lat), pch = ".") +
    geom_point(data = network, aes(x = Lon, y = Lat), color = "red", pch = ".") +
    xlab("Longitude") + ylab("Latitude") +
    theme_bw(base_size = 14)
  
  ggsave(file.path(fig_dir, "Taranaki_on_NZ.png"), nzmap, height = 6, width = 5)
  
  ###################################################
  
  #Taranaki network by year
  
  tab <- table(Data_to_plot$present, Data_to_plot$Year)
  years <- unique(Data_to_plot$Year)[order(unique(Data_to_plot$Year))]
  
  data_text <- data.frame("Year"= years, label=paste0(tab[2,], "/", tab[1,]),
                          x=174, y=-39.8)
  
  
  catchmap <- ggplot(Data_to_plot) +
    #geom_point(data = network, aes(x = easting, y = northing), col = "gray") +
    #geom_point(aes(x = easting, y = northing, col = present), alpha = 0.6) +
    geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
    geom_point(aes(x = Lon, y = Lat, col = present), alpha = 0.6) +
    facet_wrap(.~Year) +
    #xlab("Easting") + ylab("Northing") +
    
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Longfin eel NZFFD presence/absence observations by year") +
    #scale_color_brewer(palette = "Set1") +
    guides(color = guide_legend(title = "")) +
    scale_colour_manual(values = c("#377EB8", "#E41A1C")) +
    #scale_colour_manual(values = c("#E41A1C", "green")) +
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
    # if(nrow(find)>0) out <- cbind.data.frame(network[x,], 'E2'=find$easting, 'N2'=find$northing)
    # if(nrow(find)==0) out <- cbind.data.frame(network[x,], 'E2'=NA, 'N2'=NA)
    
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
    #scale_colour_manual(values = c("#E41A1C", "#39B600")) +
    theme_bw(base_size = 14) +
    theme(axis.text = element_text(size = rel(0.8)))
  
  #ggsave(file.path(fig_dir, "Taranaki_lf_observations.png"), catchmap2, height = 12, width = 15)
  ggsave(file.path(fig_dir, "Taranaki_lf_observations_ds.png"), catchmap2, height = 12, width = 15)
  
  ###################################################
  ###################################################
}



###############
# Build model #
###############


#####################################################################
# Habitat unsuitability model - w/ covariates and catchability term #
#####################################################################
# Build HUM as follows:
#   - Using full network data
#   - With habitat covariates
#   - With 'sampler' as a cacthability covariate


library(tidyverse)
library(VAST)
library(DHARMa)

rm(list=ls())


##########################
#  Directories and paths #
##########################

data_dir <- "./Data_processed"
raw_data_dir <- "./Data_raw"
data_taranaki_dir <- "./Data_processed/Taranaki"
fig_dir <- "./Data_processed/Taranaki/Figures"
pseudoabsence_data_dir <- "./Data_processed/Taranaki/Pseudo_absence_data"

model_path <- "./Models"


#############
# Load data #
#############

data_indicator = "Taranaki data"

if(data_indicator == "Taranaki data") load(file.path(data_taranaki_dir, paste0("general_inputs_taranaki.Rdata")))


#####################
# Create model path #
#####################

if(data_indicator == "Taranaki data") path <- file.path(model_path, "HUM")

dir.create(path, showWarnings = FALSE)
path_figs <- file.path(path, "Figures")
dir.create(path_figs, showWarnings=FALSE)



####################
#  Call functions  #
####################

source("./Code/Code-eel_application_Taranaki/funcs.R")


#######################
# Set up model inputs #
#######################

# 1. Set data input
Data_inp <- Data_Geostat


# 2. Set covariate input
X1config_inp <- array(1, dim = c(1,n_p)) #initaially turn on all covariates with '1'
X1config_inp[,which(covars_all == "std_Years_since_barrier")] <- 0 #two barrier covariates ("std_Years_since_barrier" and "Barrier_present"). Turn one off

X1_formula_inp = X2_formula_inp = paste0("~",(paste0(covars_all, collapse = "+")))

#TURN OFF covariates in 2nd predictor
X2config_inp <- array(0, dim = c(1,n_p))


# 3. Set up catchability input
Q1_formula <- ~ Sampler
Q1config_k <- 1
Q2_formula <- ~ 0
Q2config_k <- NULL
catchability_data <- Data_inp[,c("Lat", "Lon", "Sampler")]


# 4. Set model settings
Version = "VAST_v14_0_1"

FieldConfig <- c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 0, "Epsilon2" = 0) #1 category
RhoConfig <- c("Beta1" = 2, "Beta2" = 3, "Epsilon1" = 1, "Epsilon2" = 0) #Remove RhoConfig[3] if necessary

ObsModel <- c(2,0)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1, "Calculate_effective_area" = 1)

bias_correct = TRUE

# 5. Make settings - this isn't required but will do for saving later
settings <- make_settings(n_x = nrow(Network_sz),
                          purpose = "index2",
                          Region = "Stream_network",
                          fine_scale = FALSE,
                          FieldConfig = FieldConfig,
                          RhoConfig = RhoConfig,
                          OverdispersionConfig = OverdispersionConfig,
                          ObsModel = ObsModel,
                          bias.correct = bias_correct,
                          Options = Options,
                          Version = Version)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1


############################
# Build extrapolation grid #
############################
input_grid <- data.frame("Lat" = Data_inp$Lat, 
                         "Lon" = Data_inp$Lon, 
                         "child_i" = Data_inp$Child_i, 
                         "Area_km2" = Data_inp$Length)

Extrapolation_List = make_extrapolation_info(Region = "stream_network", 
                                             input_grid = input_grid)


#############################
# Build spatial information #
#############################
Spatial_List = make_spatial_info(n_x = nrow(Network_sz),
                                 Lon_i = Data_inp$Lon,
                                 Lat_i = Data_inp$Lat,
                                 Extrapolation_List = Extrapolation_List,
                                 Method = "Stream_network",
                                 grid_size_km = 1,
                                 fine_scale = FALSE,
                                 Network_sz_LL = Network_sz_LL,
                                 DirPath = paste0(path, "/"),
                                 Save_Results = TRUE)


## Plot data and knots 
plot_data(Extrapolation_List = Extrapolation_List, 
          Spatial_List = Spatial_List, 
          Data_Geostat = Data_inp,
          PlotDir = paste0(path_figs, "/")) 


########################
# Build the TMB object #
########################
TmbData = make_data(b_i = as_units(Data_inp$Catch_KG, unitless), 
                    a_i = as_units(rep(1,nrow(Data_inp)), 'km'), 
                    t_i = Data_inp$Year,
                    
                    Version = Version,
                    FieldConfig = FieldConfig, 
                    OverdispersionConfig = OverdispersionConfig,
                    RhoConfig = RhoConfig, 
                    ObsModel_ez = ObsModel, 
                    Options = Options, 
                    
                    X_gctp = X_gctp,
                    X_itp = X_itp,
                    X1_formula = X1_formula_inp,
                    X1config_cp = X1config_inp,
                    X2_formula = X2_formula_inp,
                    X2config_cp = X2config_inp,
                    
                    catchability_data = catchability_data,
                    Q1_formula = Q1_formula,
                    Q2_formula = Q2_formula,
                    Q1config_k = Q1config_k,
                    Q2config_k = Q2config_k,
                    
                    spatial_list = Spatial_List,
                    Network_sz = Network_sz,
                    CheckForErrors = TRUE)


####################
# Build VAST model #
####################

TmbList = make_model(build_model = TRUE, 
                     TmbData = TmbData, 
                     RunDir = path,
                     Version = Version,
                     RhoConfig = RhoConfig,
                     Method = "Stream_network")

####################
# check parameters #
####################

Obj = TmbList[["Obj"]]
Obj$fn( Obj$par )
Obj$gr( Obj$par )

#####################################################
# Estimate fixed effects and predict random effects #
#####################################################
start_time <- Sys.time()
Opt = TMBhelper::fit_tmb(obj = Obj,
                         lower = TmbList[["Lower"]],
                         upper = TmbList[["Upper"]],
                         getsd = TRUE, 
                         savedir = path, 
                         bias.correct = bias_correct, 
                         newtonsteps = 3, 
                         bias.correct.control = list( sd = FALSE, split = NULL, nsplit = 1, vars_to_correct = c( "Index_cyl", "Index_ctl" ) ), 
                         getJointPrecision = TRUE) 
Report = Obj$report()
time = Sys.time() - start_time


####################################
# Save important model information #
####################################

Save = list( "Opt" = Opt, "Report" = Report, "ParHat" = Obj$env$parList( Opt$par ), "TmbData" = TmbData )
save(Save, file = file.path(path, "Save.RData"))

model_data <- Data_inp %>%
  select(Lat, Lon, Catch_KG, Year) %>%
  rename("Lat_i" = "Lat", "Lon_i" = "Lon", "b_i" = "Catch_KG", "t_i" = "Year") %>%
  mutate("a_i" = rep(1,nrow(Data_inp)), "v_i" = rep(0,nrow(Data_inp)),
         "c_iz" = rep(0,nrow(Data_inp))) %>%
  relocate("Lat_i", "Lon_i", "a_i", "v_i", "b_i", "t_i", "c_iz")


## Always double check this before running
Fit <- list( "data_frame" = model_data,
             "extrapolation_list" = Extrapolation_List,
             "spatial_list" = Spatial_List,
             "data_list" = TmbData,
             "tmb_list" = TmbList,
             "parameter_estimates" = Opt,
             "Report" = Report,
             "ParHat" = Obj$env$parList( Opt$par ),
             "year_labels" = c(min(model_data$t_i):max(model_data$t_i)),
             "years_to_plot" = which(c(min(model_data$t_i):max(model_data$t_i)) %in% unique(model_data$t_i)),
             "category_names" = NA,
             "settings" = settings,
             #"input_args" = input_args,
             "X1config_cp" = X1config_inp,
             "X2config_cp" = X2config_inp,
             "X_gctp" = X_gctp,
             "X_itp" = X_itp,
             #"covariate_data" = covariate_data,
             "X1_formula" = X1_formula_inp,
             "X2_formula" = X2_formula_inp,
             "Q1config_k" = Q1config_k,
             "Q2config_k" = Q2config_k,
             "catchability_data" = catchability_data,
             "Q1_formula" = Q1_formula,
             "Q2_formula" = Q2_formula,
             "total_time" = time)

save(Fit, file = file.path(path, "Fit.RData"))
#load(file.path(path, "Fit.RData"))


# Extract probability of encounter data
Probability_of_encounter<- matrix(Report$R1_gct, nrow = dim(Report$R1_gct)[1], ncol = dim(Report$R1_gct)[3],
                                  dimnames = list(Network_sz_LL$child_s, min(Fit$year_labels):max(Fit$year_labels)))
save(Probability_of_encounter, file = file.path(path, "Probability_of_encounter.RData"))
#load(file.path(path, "Probability_of_encounter.RData"))


###############
# Check model #
###############

######## Print the diagnostics generated during parameter estimation, and confirm that:
######## (1) no parameter is hitting an upper or lower bound and (2) the final gradient for each fixed-effect 
######## is close to zero (less than 0.0001). Also check model convergence via the Hessian (should be TRUE)
pander::pandoc.table( Opt$diagnostics[,c( 'Param', 'Lower', 'MLE', 'Upper', 'final_gradient' )] ) 
all( abs( Opt$diagnostics[,'final_gradient'] ) <1e-4 ) #### TRUE
all( eigen( Opt$SD$cov.fixed )$values >0 ) #### TRUE



######## Evaluate the model using DHARMa residuals
n_samples <- 1000
#Obj = TmbList$Obj
n_g_orig = Obj$env$data$n_g
Obj$env$data$n_g = 0
b_iz = matrix( NA, nrow = length( TmbData$b_i ), ncol = n_samples )
for ( zI in 1 : n_samples ) {
  if ( zI %% max( 1, floor( n_samples / 10 ) ) == 0 ) {
    message( "  Finished sample ", zI, " of ", n_samples )
  }
  b_iz[,zI] = simulate_data( fit = list( tmb_list = list( Obj = Obj ) ), type = 1 )$b_i
}
if ( any( is.na( b_iz ) ) ) {
  stop( "Check simulated residuals for NA values" )
}
b_iz <- as_units( b_iz, unitless )
dharmaRes = createDHARMa(simulatedResponse = b_iz, observedResponse = TmbData$b_i,
                         fittedPredictedResponse = Probability_of_encounter, integer = FALSE )
prop_lessthan_i = apply( as.numeric( b_iz ) < outer( as.numeric( TmbData$b_i ), 
                                                     rep( 1, n_samples ) ), MARGIN = 1, FUN = mean )
prop_lessthanorequalto_i = apply( as.numeric( b_iz ) <= outer( as.numeric( TmbData$b_i ), 
                                                               rep( 1, n_samples ) ), MARGIN = 1, FUN = mean )
PIT_i = runif( min = prop_lessthan_i, max = prop_lessthanorequalto_i, n = length( prop_lessthan_i ) )
dharmaRes$scaledResiduals = PIT_i

#Save residuals
save(dharmaRes, file = file.path(path, "dharmaRes.RData"))
#load(file.path(path, "dharmaRes.RData"))

## dharma plots ##
# Plot residuals on map
plot_residuals(residuals = dharmaRes$scaledResiduals, fit = fit, save_dir=path_figs,
               Data_inp = Data_inp, network=Network_sz_LL, coords="lat_long")

# Histogram of residuals #
val = dharmaRes$scaledResiduals
val[val == 0] = -0.01
val[val == 1] = 1.01

jpeg(file.path(path_figs, "Resid_hist.jpg"), width = 600, height = 600)
hist(val, 
     breaks = seq(-0.02, 1.02, len = 53),
     col = c("red",rep("lightgrey",50), "red"),
     #main = "Hist of DHARMa residuals",
     main = "",
     xlab = "Residuals (outliers are marked red)",
     cex.axis=1.5, cex.lab=1.5)
dev.off()
##
## QQ plot ##
jpeg(file.path(path_figs, "QQplot.jpg"), width = 600, height = 600)
gap::qqunif(dharmaRes$scaledResiduals,
            pch=2,
            bty="n", 
            logscale = F, 
            col = "black", 
            cex.axis=1.5, cex.lab=1.5 
            #main = "QQ plot residuals", 
            #cex.main = 1
)
dev.off()
##
####

#################################
# Plot probability of encounter #
#################################

## Yearly
plot_maps_network(plot_set = c(1), 
                  fit = Fit, 
                  Sdreport = Fit$parameter_estimates$SD, 
                  TmbData = Fit$data_list, 
                  spatial_list = Fit$spatial_list, 
                  DirName = path_figs, 
                  Panel = "category", 
                  PlotName = "POE_lf_yearly",
                  PlotTitle = "Longfin eel yearly probability of encounter in Taranaki, NZ",
                  cex = 0.5, 
                  Zlim = c(0,1), 
                  arrows=T, 
                  pch=15)


## Across time
plot_maps_network(plot_set = c(1), 
                  fit = Fit, 
                  Sdreport = Fit$parameter_estimates$SD, 
                  TmbData = Fit$data_list, 
                  spatial_list = Fit$spatial_list, 
                  DirName = path_figs, 
                  Panel = "Year", 
                  PlotName = "POE_lf",
                  PlotTitle = "Longfin eel yearly P.O.E in Taranaki, NZ",
                  cex = 0.75, 
                  Zlim = c(0,1), 
                  arrows=T, 
                  pch=15)




##############################
# Plot river length occupied #
##############################













##################################################################
##################################################################
