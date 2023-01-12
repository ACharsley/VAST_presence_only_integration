###############################################################################################################################
##
##  (11) This script runs and evaluates a spatially-varying catchability model fitted to TRAWL BT and COD BT data 
##  for spiny dogfish (SPD)
##  Here, the TRAWL BT data are assumed to be more reliable than the COD BT data
##  
###############################################################################################################################

######## Set the output directory
if ( !dir.exists( paste0( DIR$Output, "\\SPD_TRAWL_BT_COD_BT_Qvarying" ) ) ) {
	dir.create( paste0( DIR$Output, "\\SPD_TRAWL_BT_COD_BT_Qvarying" ) )
} 
OutputDir <- paste0( DIR$Output, "\\SPD_TRAWL_BT_COD_BT_Qvarying" )
setwd( OutputDir )

######## Load required R packages
library( VAST )
library( DHARMa )

######## Load required functions
source( file = make.filename( "DHARMa utilities.R", DIR$Functions ) ) 
source( file = make.filename( "plot utility functions.R", DIR$Functions ) ) 
source( file = make.filename( "utilities.R", DIR$Functions ) ) 
source( file = make.filename( "plot_population_index.R", DIR$Functions ) ) 

######## Load SPD data
load( make.filename( "SPD_Sp_data.RData", DIR$Input ) )
table( Sp_data$Dataset )
Sp_data <- Sp_data[!( Sp_data$Dataset == "Additional_TRAWL_BT" | 
	Sp_data$Dataset == "COD_MW" | Sp_data$Dataset == "COD_BLL" ),]
dim( Sp_data )
names( Sp_data )
#### [1] "Lon"           "Lat"           "Year"          "AreaSwept_km2"
#### [5] "Value"         "Dataset"       "Datatype"      "Vessel_ID"  

Sp_data$Vessel_ID <- droplevels( Sp_data$Vessel_ID )
Sp_data <- Sp_data[Sp_data$Year > 1990,]
years <- sort( unique( Sp_data$Year ) )
Year_i <- as.numeric( as.character( Sp_data$Year ) )
Lon_i <- Sp_data$Lon
Lat_i <- Sp_data$Lat
table( Sp_data$Dataset )
table( Sp_data$Datatype )
tapply( Sp_data[,'Value'], INDEX = list( Sp_data[,'Year'], Sp_data[,'Dataset'] ), FUN = mean )
Sp_data$Type <- factor( ifelse( Sp_data$Dataset == "TRAWL_BT", "0", "1" ) )
table( Sp_data$Type )

######## Load the prediction grid for SPD
load( make.filename( "Prediction_grid_SPD_10kmx10km.RData", DIR$Input ) )
dim( Prediction_grid )
names( Prediction_grid )
input_grid <- as.data.frame( cbind( Prediction_grid$Lon, Prediction_grid$Lat, Prediction_grid$Area_km2 ) )
names( input_grid ) <- c( "Lon", "Lat", "Area_km2" )
summary( input_grid$Lon )
summary( input_grid$Lat )

######## Define some VAST settings 
Version = get_latest_version( package = "VAST" )
Method = "Barrier"
grid_size_km = 25
n_x = 200
FieldConfig = matrix( data = c( Omega1 = "IID", Epsilon1 = "IID", Beta1 = "IID",  
	Omega2 = "IID", Epsilon2 = "IID", Beta2 = "IID" ), nrow = 3, ncol = 2 )
RhoConfig = c( Beta1 = 0, Beta2 = 0, Epsilon1 = 4, Epsilon2 = 4 ) 
OverdispersionConfig = c( Eta1 = 1, Eta2 = 1 )
ObsModel = c( 2, 1 )
fine_scale = TRUE

######## Decide which post-hoc calculations to include in VAST output
Options =  c( "SD_site_density" = 1, "SD_site_logdensity" = 0, "Calculate_Range" = 0, "Calculate_evenness" = 0, 
	"Calculate_effective_area" = 0, "Calculate_Cov_SE" = 0, "Calculate_Synchrony" = 0, 
	"Calculate_Coherence" = 0, "report_additional_variables" = TRUE, "treat_nonencounter_as_zero" = TRUE, 	
	"Range_fraction" = 0.2 )

######## Determine the study region 
Region = "User"

######## Determine strata within the study region 
strata.limits <- data.frame( STRATA = "All_areas" )

######## Define the extrapolation grid
Extrapolation_List = make_extrapolation_info( Region = Region, strata.limits = strata.limits, 
	input_grid = input_grid, max_cells = 2000 )

######## Generate the spatial information necessary for conducting spatio-temporal parameter estimation
setwd( OutputDir )
Spatial_List = make_spatial_info( grid_size_km = grid_size_km, n_x = n_x, fine_scale = fine_scale, Method = Method, 
	Lon = as.numeric( as.character( Sp_data[,"Lon"] ) ),
	Lat = as.numeric( as.character( Sp_data[, "Lat"] ) ), 
	Extrapolation_List = Extrapolation_List, Save_Results = TRUE, "knot_method" = "grid" )
Sp_data = cbind( Sp_data, knot_i = Spatial_List$knot_i )
save( Sp_data, file = "Sp_data.RData" )

######## Plot data and knots 
Sp_data$Year <- as.numeric( Sp_data$Year )
plot_data( Extrapolation_List = Extrapolation_List, Spatial_List = Spatial_List, Data_Geostat = Sp_data ) 

######## Build the TMB object
Q1_formula <- ~ Type
Q1config_k <- 3
Q2_formula <- ~ Type
Q2config_k <- 3
v_i <- as.numeric( Sp_data$Vessel_ID )
TmbData = make_data( "Version" = Version, "FieldConfig" = FieldConfig, "OverdispersionConfig" = OverdispersionConfig,
	"RhoConfig" = RhoConfig, "ObsModel" = ObsModel, "c_i" = rep( 0, nrow( Sp_data ) ), 
	"b_i" = as.numeric( as.character( Sp_data[, "Value"] ) ), 
	"a_i" = rep( 1, nrow( Sp_data ) ), 
	"v_i" = v_i - 1, 
	"s_i" = Sp_data[, "knot_i"] - 1, 
	"t_i" = as.numeric( as.character( Sp_data[, "Year"] ) ),
	"a_xl" = Spatial_List[["a_gl"]], "catchability_data" = Sp_data,
  	"Q1_formula" = Q1_formula, "Q2_formula" = Q2_formula, "Q1config_k" = Q1config_k, "Q2config_k" = Q2config_k,
	"MeshList" = Spatial_List$MeshList,
	"GridList" = Spatial_List$GridList, "Method" = Spatial_List$Method, "Options" = Options, 
	"CheckForErrors" = TRUE, "spatial_list" = Spatial_List )

######## Build the VAST model 
TmbList = make_model( "build_model" = TRUE, "TmbData" = TmbData, "RunDir" = OutputDir, "Version" = Version, 
	"RhoConfig" = RhoConfig, "loc_x" = Spatial_List$loc_x, "Method" = Method, "Use_REML" = FALSE )

######## Check parameters
Obj = TmbList[["Obj"]]
Obj$fn( Obj$par )
Obj$gr( Obj$par )

######## Estimate fixed effects and predict random effects
Opt = TMBhelper::fit_tmb( obj = Obj, lower = TmbList[["Lower"]], upper = TmbList[["Upper"]], 
	getsd = TRUE, savedir = OutputDir, bias.correct = TRUE, newtonsteps = 3, 
	bias.correct.control = list( sd = FALSE, split = NULL, nsplit = 1, vars_to_correct = c( "Index_cyl", "Index_ctl" ) ), 
	getJointPrecision = TRUE ) 
Report = Obj$report()
Save = list( "Opt" = Opt, "Report" = Report, "ParHat" = Obj$env$parList( Opt$par ), "TmbData" = TmbData )
save( Save, file = "Save.RData" )

######## Print the diagnostics generated during parameter estimation, and confirm that:
######## (1) no parameter is hitting an upper or lower bound and (2) the final gradient for each fixed-effect 
######## is close to zero (less than 0.0001). Also check model convergence via the Hessian (should be TRUE)
pander::pandoc.table( Opt$diagnostics[,c( 'Param', 'Lower', 'MLE', 'Upper', 'final_gradient' )] ) 
all( abs( Opt$diagnostics[,'final_gradient'] ) <1e-4 ) #### TRUE
all( eigen( Opt$SD$cov.fixed )$values >0 ) #### TRUE

######## Generate 100 predictions by sampling from the predictive distribution, and save the 
######## generated samples in .RData files
samples <- sample_variable( Sdreport = Opt$SD, Obj = Obj, variable_name = "D_gct",
	n_samples = 100, seed = sample( 1 : 1000, 1 ) )
save( samples, file = "Density_samples.RData" )
Index_samples <- sample_variable( Sdreport = Opt$SD, Obj = Obj, variable_name = "Index_gctl",
	n_samples = 100, seed = sample( 1 : 1000, 1 ) )
save( samples, file = "Index_samples.RData" )

######## Get region-specific settings for plots
MapDetails_List = make_map_info( "Region" = Region, "NN_Extrap" = Spatial_List$PolygonList$NN_Extrap, 
	"spatial_list" = Spatial_List, "Extrapolation_List" = Extrapolation_List )
save( MapDetails_List, file = "MapDetails_List.RData" )

######## Define some settings                                                  
Year_Set = seq( min( Sp_data[,'Year'] ), max( Sp_data[,'Year'] ) )
Years2Include = which( Year_Set %in% sort( unique( Sp_data[,'Year'] ) ) )

######## Extract spatial densities and save them in a .RData file
Density_1 = as.data.frame( Report$D_gct[,1,,drop = TRUE] )
colnames( Density_1 ) <- as.character( Year_Set[Years2Include] )
save( Density_1, file = "Spatial_density_estimates.RData" ) 

######## Extract spatial indices and save them in a .RData file
Index_1 = as.data.frame( Report$Index_gctl[,1,,,drop = TRUE] )
colnames( Index_1 ) <- as.character( Year_Set[Years2Include] )
save( Index_1, file = "Spatial_indices.RData" ) 

######## Extract the standard errors (SEs) associated with spatial indices, and save them in .RData files
Sdreport = Opt[["SD"]]
SE_Index_1 = SE_CPUE_1 = array( TMB::summary.sdreport( Sdreport )[which( 
	rownames( TMB::summary.sdreport( Sdreport ) ) == "Index_gctl" ),2], 
	dim = c( dim( Report$Index_gctl ) ), dimnames = list( NULL, NULL, NULL, NULL ) )[,1,,]
colnames( SE_Index_1 ) <- as.character( Year_Set[Years2Include] )
save( SE_Index_1, file = "Spatial_SE_indices.RData" ) 

######## Produce a biomass index and save it in a .RData file
Index = plot_population_index( TmbData = TmbData, Sdreport = Opt[["SD"]], Year_Set = Year_Set, 
	Years2Include = Years2Include, use_biascorr = TRUE, extrapolation_list = Extrapolation_List )
save( Index, file = "Index.RData" ) 

######## Evaluate the model using DHARMa residuals
n_samples <- 1000
Obj = TmbList$Obj
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
b_iz <- as_units( b_iz, 'kg' )
dharmaRes = create_DHARMa( simulatedResponse = b_iz, observedResponse = TmbData$b_i,
	fittedPredictedResponse = Report$D_i, integer = FALSE )
prop_lessthan_i = apply( as.numeric( b_iz ) < outer( as.numeric( TmbData$b_i ), 
	rep( 1, n_samples ) ), MARGIN = 1, FUN = mean )
prop_lessthanorequalto_i = apply( as.numeric( b_iz ) <= outer( as.numeric( TmbData$b_i ), 
	rep( 1, n_samples ) ), MARGIN = 1, FUN = mean )
PIT_i = runif( min = prop_lessthan_i, max = prop_lessthanorequalto_i, n = length( prop_lessthan_i ) )
dharmaRes$scaledResiduals = PIT_i

#### Save the DHARMa residuals 
save( dharmaRes, file = "dharmaRes.RData" )

#### Produce and save an histogram of DHARMa residuals
val = dharmaRes$scaledResiduals
val[val == 0] = -0.01
val[val == 1] = 1.01
jpeg( "Histogram_of_DHARMa_residuals.png", width = 6, height = 7, units = "in", res = 600 )
	hist( val, breaks = seq( -0.02, 1.02, len = 53 ), col = c( "red", rep( "lightgrey", 50 ), "red" ),
		main = "", xlab = "Residuals", cex.main = 2.5 )
dev.off()

#### Produce and save a QQ-plot of DHARMa residuals
jpeg( "QQplot_of_DHARMa_residuals.png", width = 6, height = 7, units = "in", res = 600 )
	gap::qqunif( dharmaRes$scaledResiduals, pch = 2, bty = "n", logscale = F, col = "black", cex = 0.6,
		main = "", cex.main = 2.5 )
dev.off()

