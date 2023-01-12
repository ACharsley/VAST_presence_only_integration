
#data_list should have a 'network', 'habitat', 'obs' slot
create_ds_data <- function(data_list){
  
  library(tidyverse)
  
  network <- data_list$network
  obs <- data_list$obs
  X_gctp <- data_list$X_gctp
  
  #################
  #################
  
  obs_child <- unique(obs$child_i)
  
  net_obs <- network %>% filter(child_s %in% obs_child)
  nextdown <- network %>% filter(child_s %in% net_obs$parent_s)
  save <- rbind.data.frame(net_obs,nextdown)
  for(i in 1:100){
    nextdown <- network %>% filter(child_s %in% nextdown$parent_s)
    save <- unique(rbind.data.frame(save, nextdown))
    print(nrow(save))
  }
  network_sub <- save
  
  # #Select nodes to keep for downstream data
  # habitat <- habitat %>% filter(child_s %in% network_sub$child_s)
  
  #Select nodes to keep for downstream data
  rows_to_keep <- dimnames(X_gctp)[[1]][dimnames(X_gctp)[[1]] %in% network_sub$child_s]
  X_gctp_ds <- X_gctp[rows_to_keep,,,,drop=FALSE]
  
  ## Rename nodes ##
  nodes <- unique(c(network_sub$child_s, network_sub$parent_s))
  inodes <- seq_along(nodes)
  
  #Rename nodes in network
  net_parents <- sapply(1:nrow(network_sub), function(x){
    if(network_sub$parent_s[x] != 0) new_node <- inodes[which(nodes == network_sub$parent_s[x])]
    if(network_sub$parent_s[x] == 0) new_node <- 0
    return(new_node)
  })
  net_children <- sapply(1:nrow(network_sub), function(x) inodes[which(nodes == network_sub$child_s[x])])
  
  network_sub$parent_s <- net_parents
  network_sub$child_s <- net_children
  
  #Rename nodes in observations
  obs_parents <- sapply(1:nrow(obs), function(x){
    if(obs$parent_i[x] != 0) new_node <- inodes[which(nodes == obs$parent_i[x])]
    if(obs$parent_i[x] == 0) new_node <- 0
    return(new_node)  
  })
  obs_children <- sapply(1:nrow(obs), function(x) inodes[which(nodes == obs$child_i[x])])
  
  obs_sub <- obs
  obs_sub$parent_i <- obs_parents
  obs_sub$child_i <- obs_children
  
  #browser()
  
  # #Rename nodes in habitat covariates
  # habitat_children <- sapply(1:nrow(habitat), function(x) inodes[which(nodes == habitat$child_s[x])])
  # 
  # habitat_sub <- habitat
  # habitat_sub$child_s <- habitat_children
  
  
  #Rename nodes in habitat covariates
  rownames_hab <- as.numeric(dimnames(X_gctp_ds)[[1]])
  hab_children <- sapply(1:nrow(X_gctp_ds), function(x) inodes[which(nodes == rownames_hab[x])])
  
  dimnames(X_gctp_ds)[[1]] <- hab_children
  
  X_gctp_ds <- X_gctp_ds[order(hab_children),,,,drop=FALSE]
  
  
  
  #Check all nodes in network
  all(obs_children %in% net_children)
  #all(habitat_children %in% net_children)
  all(dimnames(X_gctp_ds)[[1]] %in% net_children)
  
  data_list_all <- list("network" = network,
                        "X_gctp" = X_gctp,
                        "obs" = obs,
                        "network_ds" = network_sub,
                        "X_gctp_ds" = X_gctp_ds,
                        "obs_ds" = obs_sub)
  
  return(data_list_all)
  
}


make_spatial_info_AC = function( n_x,
                              Lon_i,
                              Lat_i,
                              Extrapolation_List,
                              knot_method = NULL,
                              Method = "Mesh",
                              anisotropic_mesh = NULL,
                              Kmeans = NULL,
                              grid_size_km = 50,
                              grid_size_LL = 1,
                              fine_scale = FALSE,
                              Network_sz_LL = NULL,
                              iter.max = 1000,
                              randomseed = 1,
                              nstart = 100,
                              DirPath = paste0(getwd(),"/"),
                              Save_Results = FALSE,
                              LON_intensity,
                              LAT_intensity,
                              backwards_compatible_kmeans = FALSE,
                              ... ){

  # Deprecated options
  if( Method=="Spherical_mesh" ){
    stop("Method=`Spherical_mesh` is not being maintained, but please write the package author if you need to explore this option")
  }
  if( Method == "Stream_network" & fine_scale == TRUE ){
    stop("Please use fine_scale=FALSE with stream network spatial model; feature fine_scale=TRUE not yet supported for stream network spatial model.")
  }

  # Backwards compatibility for when settings didn't include knot_method, such that settings$knot_method=NULL
  if(is.null(knot_method)) knot_method = "samples"

  # Backwards compatible option for different extrapolation grid
  if( missing(LON_intensity) & missing(LAT_intensity) ){
    if( knot_method=="samples" ){
      LON_intensity = Lon_i
      LAT_intensity = Lat_i
    }
    if( knot_method=="grid" ){
      which_rows = which( Extrapolation_List$Data_Extrap[,'Include']==TRUE & strip_units(Extrapolation_List[["Area_km2_x"]])>0 & strip_units(rowSums(Extrapolation_List[["a_el"]]))>0 )
      LON_intensity = Extrapolation_List$Data_Extrap[ which_rows, 'Lon']
      LAT_intensity = Extrapolation_List$Data_Extrap[ which_rows, 'Lat']
    }
    if( !(knot_method %in% c("samples","grid")) ) stop("`knot_method` must be either `samples` or `grid`")
  }

  # Convert to an Eastings-Northings coordinate system
  if( Method=="Spherical_mesh" ){
    loc_i = data.frame( 'Lon'=Lon_i, 'Lat'=Lat_i )
    # Bounds for 2D AR1 grid
    Grid_bounds = (grid_size_km/110) * apply(loc_e/(grid_size_km/110), MARGIN=2, FUN=function(vec){trunc(range(vec))+c(0,1)})

    # Calculate k-means centroids
    if(is.null(Kmeans)) Kmeans = make_kmeans(n_x=n_x,
                                             loc_orig=loc_i[,c("Lon", "Lat")], randomseed=randomseed, kmeans_purpose='spatial', backwards_compatible_kmeans=backwards_compatible_kmeans, ... )

    # Calculate grid for 2D AR1 process
    loc_grid = expand.grid( 'Lon'=seq(Grid_bounds[1,1],Grid_bounds[2,1],by=grid_size_LL), 'Lat'=seq(Grid_bounds[1,2],Grid_bounds[2,2],by=grid_size_LL) )
    Which = sort(unique(RANN::nn2(data=loc_grid, query=Extrapolation_List$Data_Extrap[which(strip_units(Extrapolation_List$Area_km2_x)>0),c('Lon','Lat')], k=1)$nn.idx[,1]))
    loc_grid = loc_grid[Which,]
    grid_num = RANN::nn2( data=loc_grid, query=loc_i, k=1)$nn.idx[,1]
  }
  if( Method %in% c("Mesh","Grid","Stream_network","Barrier") ){
    loc_i = project_coordinates( X=Lon_i, Y=Lat_i, projargs=Extrapolation_List$projargs )
    loc_intensity = project_coordinates( X=LON_intensity, Y=LAT_intensity, projargs=Extrapolation_List$projargs )
    colnames(loc_i) = colnames(loc_intensity) = c("E_km", "N_km")
    # Bounds for 2D AR1 grid
    Grid_bounds = grid_size_km * apply(Extrapolation_List$Data_Extrap[,c('E_km','N_km')]/grid_size_km, MARGIN=2, FUN=function(vec){trunc(range(vec))+c(0,1)})

    # Calculate k-means centroids
    if(is.null(Kmeans)) Kmeans = make_kmeans(n_x=n_x, loc_orig=loc_intensity[,c("E_km", "N_km")], randomseed=randomseed, nstart=nstart,
                                             DirPath=DirPath, Save_Results=Save_Results, backwards_compatible_kmeans=backwards_compatible_kmeans )
    NN_i = RANN::nn2( data=Kmeans[["centers"]], query=loc_i, k=1)$nn.idx[,1]

    # Calculate grid for 2D AR1 process
    loc_grid = expand.grid( 'E_km'=seq(Grid_bounds[1,1],Grid_bounds[2,1],by=grid_size_km), 'N_km'=seq(Grid_bounds[1,2],Grid_bounds[2,2],by=grid_size_km) )
    Which = sort(unique(RANN::nn2(data=loc_grid, query=Extrapolation_List$Data_Extrap[which(strip_units(Extrapolation_List$Area_km2_x)>0),c('E_km','N_km')], k=1)$nn.idx[,1]))
    loc_grid = loc_grid[Which,]
    grid_num = RANN::nn2( data=loc_grid, query=loc_i, k=1)$nn.idx[,1]
  }

  # Calc design matrix and areas
  if( Method=="Grid" ){
    knot_i = grid_num
    loc_x = loc_grid
  }
  if( Method %in% c("Mesh","Spherical_mesh","Barrier") ){
    knot_i = NN_i
    loc_x = Kmeans[["centers"]]
  }
  if( Method == "Stream_network" ){
    knot_i = Extrapolation_List$Data_Extrap[,"child_i"]
    loc_x = project_coordinates( X=Network_sz_LL[,"Lon"], Y=Network_sz_LL[,"Lat"], projargs=Extrapolation_List$projargs )
    colnames(loc_x) = c('E_km', 'N_km')
  }

  # Bookkeeping for extrapolation-grid
  if( fine_scale==FALSE ){
    loc_g = loc_x
  }
  if( fine_scale==TRUE ){
    loc_g = Extrapolation_List$Data_Extrap[ which(strip_units(Extrapolation_List$Area_km2_x)>0), c('E_km','N_km') ]
  }

  # Convert loc_x back to location in lat-long coordinates latlon_x
  #origargs = "+proj=longlat +ellps=WGS84"
  origargs = "+proj=longlat +datum=WGS84"
  latlon_x = project_coordinates( X=loc_x[,"E_km"], Y=loc_x[,"N_km"], projargs=origargs, origargs=Extrapolation_List$projargs )[,c("Y","X")]
  colnames(latlon_x) = c("Lat", "Lon")

  # Convert loc_g back to location in lat-long coordinates latlon_g
  latlon_g = project_coordinates( X=loc_g[,"E_km"], Y=loc_g[,"N_km"], projargs=origargs, origargs=Extrapolation_List$projargs )[,c("Y","X")]
  colnames(latlon_g) = c("Lat", "Lon")

  # Bundle lat-lon
  latlon_i = cbind( 'Lat'=Lat_i, 'Lon'=Lon_i )

  # Make mesh and info for anisotropy  SpatialDeltaGLMM::
  # Diagnose issues:  assign("Kmeans", Kmeans, envir = .GlobalEnv)
  if(Method != "Stream_network"){
    MeshList = make_mesh( Method=Method, loc_x=Kmeans$centers, loc_g=loc_g, loc_i=loc_i, Extrapolation_List=Extrapolation_List,
                          fine_scale=fine_scale, anisotropic_mesh=anisotropic_mesh, ... )
  }else{
    MeshList = make_mesh_AC( Method=Method, loc_x=loc_x, loc_g=loc_g, loc_i=loc_i, Extrapolation_List=Extrapolation_List,
                          fine_scale=fine_scale, anisotropic_mesh=anisotropic_mesh, ... )
  }

  # Deal with loc_s and latlon_s
  if( tolower(Method)=="mesh"){
    n_s = MeshList$anisotropic_spde$n.spde
    loc_s = MeshList$anisotropic_spde$mesh$loc[,1:2]
  }
  if( tolower(Method)=="grid"){
    n_s = nrow(loc_x)
    loc_s = loc_x
  }
  if( tolower(Method)=="spherical_mesh"){
    n_s = MeshList$isotropic_spde$n.spde
    loc_s = NA
  }
  if( tolower(Method)=="stream_network"){
    n_s = nrow(loc_x)
    loc_s = loc_x
  }
  if( tolower(Method)=="barrier"){
    n_s = MeshList$anisotropic_spde$n.spde
    loc_s = MeshList$anisotropic_spde$mesh$loc[,1:2]
  }
  if(is.na(as.vector(loc_s)[1])){
    latlon_s = NA
  }else{
    colnames(loc_s) = c("E_km", "N_km")
    latlon_s = project_coordinates( X=loc_s[,"E_km"], Y=loc_s[,"N_km"], projargs=origargs, origargs=Extrapolation_List$projargs )[,c("Y","X")]
    colnames(latlon_s) = c("Lat", "Lon")
  }

  # Make matrices for 2D AR1 process
  Dist_grid = dist(loc_grid, diag=TRUE, upper=TRUE)
  M0 = as( ifelse(as.matrix(Dist_grid)==0, 1, 0), "dgTMatrix" )
  M1 = as( ifelse(as.matrix(Dist_grid)==grid_size_km, 1, 0), "dgTMatrix" )
  M2 = as( ifelse(as.matrix(Dist_grid)==sqrt(2)*grid_size_km, 1, 0), "dgTMatrix" )
  if( Method=="Spherical_mesh" ) GridList = list("M0"=M0, "M1"=M1, "M2"=M2, "grid_size_km"=grid_size_LL)
  if( Method %in% c("Mesh","Grid","Stream_network","Barrier") ) GridList = list("M0"=M0, "M1"=M1, "M2"=M2, "grid_size_km"=grid_size_km)

  # Make projection matrices
  if( fine_scale==FALSE ){
    A_is = matrix(0, nrow=nrow(loc_i), ncol=n_s)
    A_is[ cbind(1:nrow(loc_i),knot_i) ] = 1
    A_is = as( A_is, "dgTMatrix" )
    A_gs = as( diag(n_x), "dgTMatrix" )
  }
  if( fine_scale==TRUE ){
    A_is = INLA::inla.spde.make.A( MeshList$anisotropic_mesh, loc=as.matrix(loc_i) )
    if( class(A_is)=="dgCMatrix" ) A_is = as( A_is, "dgTMatrix" )
    A_gs = INLA::inla.spde.make.A( MeshList$anisotropic_mesh, loc=as.matrix(loc_g) )
    if( class(A_gs)=="dgCMatrix" ) A_gs = as( A_gs, "dgTMatrix" )
    Check_i = apply( A_is, MARGIN=1, FUN=function(vec){sum(vec>0)})
    Check_g = apply( A_is, MARGIN=1, FUN=function(vec){sum(vec>0)})
    if( any(c(Check_i,Check_g) <= 0 ) ){
      # stop("Problem with boundary")
      # plot(MeshList$anisotropic_mesh)
      # points( x=loc_i[which(Check_i!=3),'E_km'], y=loc_i[which(Check_i!=3),'N_km'], col="red" )
    }
  }

  # Calculate areas
  if( Method != "Stream_network" ){
    PolygonList = calculate_knot_areas( loc_x=loc_x, Data_Extrap=Extrapolation_List[["Data_Extrap"]], a_el=Extrapolation_List[["a_el"]])
    if( fine_scale==FALSE ){
      a_gl = PolygonList[["a_xl"]]
    }
    if( fine_scale==TRUE ){
      a_gl = as.matrix(Extrapolation_List[["a_el"]][ which(strip_units(Extrapolation_List$Area_km2_x)>0), ])
    }
  }else{
    PolygonList = NULL
    dist_inp = Network_sz_LL[,"dist_s"]
    dist_inp[which(is.infinite(dist_inp))] <- 0
    a_gl = matrix(dist_inp, nrow=n_x)
  }

  # Moving
  if( fine_scale==TRUE | Method=="Stream_network" ){
    g_e = rep(NA, length(Extrapolation_List[["Area_km2_x"]]))
    g_e[ which(strip_units(Extrapolation_List[["Area_km2_x"]])>0) ] = 1:length(which(strip_units(Extrapolation_List[["Area_km2_x"]])>0))
  }else{
    g_e = PolygonList$NN_Extrap$nn.idx[,1]
    g_e[ which(strip_units(Extrapolation_List[["Area_km2_x"]])==0) ] = NA
  }

  # Return
  Return = list( "fine_scale"=fine_scale, "A_is"=A_is, "A_gs"=A_gs, "n_x"=n_x, "n_s"=n_s, "n_g"=nrow(a_gl), "n_i"=nrow(loc_i),
                 "MeshList"=MeshList, "GridList"=GridList, "a_gl"=a_gl, "a_xl"=a_gl, "Kmeans"=Kmeans, "knot_i"=knot_i,
                 "loc_i"=as.matrix(loc_i), "loc_x"=as.matrix(loc_x), "loc_g"=as.matrix(loc_g), "loc_s"=as.matrix(loc_s), "g_e"=g_e,
                 "Method"=Method, "PolygonList"=PolygonList, "NN_Extrap"=PolygonList$NN_Extrap, "knot_method"=knot_method,
                 "latlon_x"=latlon_x, "latlon_g"=latlon_g, "latlon_i"=latlon_i, "latlon_s"=latlon_s )
  class(Return) = "make_spatial_info"
  return( Return )
}




make_mesh_AC <-
  function( loc_x,
            loc_g,
            loc_i,
            Method,
            Extrapolation_List,
            anisotropic_mesh = NULL,
            fine_scale = FALSE,
            map_data,
            ...){

    #######################
    # Create the anisotropic SPDE mesh using 2D coordinates
    #######################

    # 2D coordinates SPDE
    if( is.null(anisotropic_mesh)){
      if( fine_scale==FALSE ){
        anisotropic_mesh = INLA::inla.mesh.create( loc_x, plot.delay=NULL, ...)
      }else{
        loc_z = rbind( loc_x, loc_g, loc_i )
        outer_hull = INLA::inla.nonconvex.hull( as.matrix(loc_z), convex = -0.05, concave = -0.05)
        anisotropic_mesh = INLA::inla.mesh.create( loc_x, plot.delay=NULL, boundary=outer_hull, ...)
      }
    }

    anisotropic_spde = INLA::inla.spde2.matern(anisotropic_mesh, alpha=2)

    # Pre-processing in R for anisotropy
    Dset = 1:2
    # Triangle info
    TV = anisotropic_mesh$graph$tv       # Triangle to vertex indexing
    V0 = anisotropic_mesh$loc[TV[,1],Dset]   # V = vertices for each triangle
    V1 = anisotropic_mesh$loc[TV[,2],Dset]
    V2 = anisotropic_mesh$loc[TV[,3],Dset]
    E0 = V2 - V1                      # E = edge for each triangle
    E1 = V0 - V2
    E2 = V1 - V0

    # Pre-processing for barriers
    # Barriers don't affect projection matrix A
    # Obtain polygon for water
    if( missing(map_data) ){
      map_data = rnaturalearth::ne_countries( scale=switch("medium", "low"=110, "medium"=50, "high"=10, 50) )
      attr(map_data,"proj4string") = sp::CRS("+proj=longlat +datum=WGS84")
    }

    # Calculate centroid of each triangle in mesh and convert to SpatialPoints
    n_triangles = length(anisotropic_mesh$graph$tv[,1])
    posTri = matrix(NA, nrow=n_triangles, ncol=2)
    for(tri_index in 1:n_triangles){
      temp = anisotropic_mesh$loc[ anisotropic_mesh$graph$tv[tri_index,], ]
      posTri[tri_index,] = colMeans(temp)[c(1,2)]
    }
    posTri = sp::SpatialPoints(posTri, proj4string=sp::CRS(Extrapolation_List$projargs) )
    posTri = sp::spTransform(posTri, CRSobj=map_data@proj4string )

    # Calculate set of triangles barrier.triangles with centroid over land
    if( Method == "Barrier" ){
      anisotropic_mesh_triangles_over_land = unlist(sp::over(map_data, posTri, returnList=TRUE))
    }else{
      anisotropic_mesh_triangles_over_land = vector()
    }
    #
    #plot( x=posTri@coords[,1], y=posTri@coords[,2], col=ifelse(1:n_triangles%in%triangles_over_land,"black","red") )

    # Create Barrier object if requested
    # Don't do this unless necessary, because it sometimes throws an error
    #Diagnose issues:  assign("anisotropic_mesh", anisotropic_mesh, envir = .GlobalEnv)
    # barrier_finite_elements = INLA:::inla.barrier.fem(mesh=anisotropic_mesh,
    #                                                   barrier.triangles=anisotropic_mesh_triangles_over_land)
    # barrier_list = list(C0 = barrier_finite_elements$C[[1]],
    #                     C1 = barrier_finite_elements$C[[2]],
    #                     D0 = barrier_finite_elements$D[[1]],
    #                     D1 = barrier_finite_elements$D[[2]],
    #                     I = barrier_finite_elements$I )
    # sp::plot( INLA::inla.barrier.polygon(anisotropic_mesh, triangles_over_land) )

    # Calculate Areas
    crossprod_fn = function(Vec1,Vec2) abs(det( rbind(Vec1,Vec2) ))
    Tri_Area = rep(NA, nrow(E0))
    for(i in 1:length(Tri_Area)) Tri_Area[i] = crossprod_fn( E0[i,],E1[i,] )/2   # T = area of each triangle

    ################
    # Add the isotropic SPDE mesh for spherical or 2D projection, depending upon `Method` input
    ################

    # Mesh and SPDE for different inputs
    if(Method %in% c("Mesh","Grid","Stream_network","Barrier")){
      loc_isotropic_mesh = loc_x
      isotropic_mesh = anisotropic_mesh
    }
    if(Method %in% c("Spherical_mesh")){
      loc_isotropic_mesh = INLA::inla.mesh.map(loc_x, projection="longlat", inverse=TRUE) # Project from lat/long to mesh coordinates
      isotropic_mesh = INLA::inla.mesh.create( loc_isotropic_mesh, plot.delay=NULL, ...)
    }
    isotropic_spde = INLA::inla.spde2.matern(isotropic_mesh, alpha=2)

    ####################
    # Return stuff
    ####################
    #if( isotropic_mesh$n != anisotropic_mesh$n ) stop("Check `Calc_Anisotropic_Mesh` for problem")

    Return = list("loc_x"=loc_x, "loc_isotropic_mesh"=loc_isotropic_mesh, "isotropic_mesh"=isotropic_mesh,
                  "isotropic_spde"=isotropic_spde, "anisotropic_mesh"=anisotropic_mesh, "anisotropic_spde"=anisotropic_spde,
                  "Tri_Area"=Tri_Area, "TV"=TV, "E0"=E0, "E1"=E1, "E2"=E2,
                  "anisotropic_mesh_triangles_over_land"=anisotropic_mesh_triangles_over_land,
                  #"barrier_list"=barrier_list
                  "barrier_list"=NULL
                  )
    return(Return)
  }



plot_residuals = function(residuals, fit, Data_inp, network,
                          Zlim=NULL, coords="lat_long", save_dir){
  
  if(coords == "northing_easting"){
    
    resid_data <- data.frame("Northing"=Data_inp$Northing, "Easting"=Data_inp$Easting, 
                             "Year"=Data_inp$Year, "Resid"=residuals)
    
    #xlim <- range(resid_data$Easting); ylim <- range(resid_data$Northing)
    xlim <- range(network$Easting); ylim <- range(network$Northing)
    
    if(is.null(Zlim)) inp_Zlim = quantile(resid_data$Resid, prob = c(0,1), na.rm=TRUE)
    
    
    p1 <- ggplot(resid_data) +
      geom_point(data = network, aes(x = easting, y = northing), col = "gray", alpha = 0.6) +
      geom_point(aes(x = Easting, y = Northing, col = Resid), alpha = 0.7) +
      facet_wrap(~Year) +
      #xlim(xlim) + ylim(ylim) +
      #scale_color_distiller(palette = "Spectral", limits = inp_Zlim) +
      scale_color_distiller(palette = "RdYlGn", limits = inp_Zlim, direction = 1) +
      coord_cartesian(xlim = xlim, ylim = ylim) +
      #scale_x_continuous(breaks=quantile(xct$E_km, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$E_km, prob=c(0.1,0.5,0.9)),0)) +
      
      #scale_color_brewer(palette = "Set1") +
      #scale_size_binned("Pearson Residual", range = c(0,3)) +
      xlab("Easting") + ylab("Northing") +
      #ggtitle("Residual plots by year") + 
      theme_bw(base_size = 14) +
      theme(legend.title = element_blank(),
            axis.text = element_text(size = rel(0.5)),
            axis.text.x = element_text(angle = 90))
    
  }
  
  
  if(coords == "lat_long"){
    
    resid_data <- data.frame("Lat"=Data_inp$Lat, "Lon"=Data_inp$Lon, 
                             "Year"=Data_inp$Year, "Resid"=residuals)
    
    xlim <- range(resid_data$Lon); ylim <- range(resid_data$Lat)
    #xlim <- range(network$long); ylim <- range(network$lat)
    
    if(is.null(Zlim)) inp_Zlim = quantile(resid_data$Resid, prob = c(0,1), na.rm=TRUE)
    
    
    p1 <- ggplot(resid_data) +
      geom_point(data = network, aes(x = Lon, y = Lat), col = "gray", alpha = 0.6) +
      geom_point(aes(x = Lon, y = Lat, col = Resid), alpha = 0.7) +
      facet_wrap(~Year) +
      #xlim(xlim) + ylim(ylim) +
      #scale_color_distiller(palette = "Spectral", limits = inp_Zlim) +
      scale_color_distiller(palette = "RdYlGn", limits = inp_Zlim, direction = 1) +
      coord_cartesian(xlim = xlim, ylim = ylim) +
      #scale_x_continuous(breaks=quantile(xct$E_km, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$E_km, prob=c(0.1,0.5,0.9)),0)) +
      
      #scale_color_brewer(palette = "Set1") +
      #scale_size_binned("Pearson Residual", range = c(0,3)) +
      xlab("Longitude") + ylab("Latitude") +
      #ggtitle("Residual plots by year") + 
      theme_bw(base_size = 14) +
      theme(legend.title = element_blank(),
            axis.text = element_text(size = rel(0.5)),
            axis.text.x = element_text(angle = 90))
    
    
  }
  
  
  ggsave(file.path(save_dir, "Residual_plot.png"), p1, width = 12, height = 10)
}





## Plot Number
# 1 = Probability of capture maps
# 2 = Log-expected positive catch rate
# 3 = Log-predicted density (product of encounter probability and positive catch rates)
# 4 = Total biomass across all categories (only useful in a multivariate model)
# 5 = Spatio-temporal variation in probability of capture
# 6 = Spatio-temporal variation in log-positive catch rates (when using a conventional delta-model)
# 7 = Linear predictor for probability of capture
# 8 = Linear predictor for positive catch rates
# 9 = Covariates that are included in the model (measured values)

# 10 = Individual covariate effects for probability of capture (taken from MR code)
# 11 = Individual covariate effects on positive catch rates (MR code = 16)

# 12 = Combined covariate effects for probability of capture (dont really get why its eta)
# 13 = Combined covariate effects for positive catch rates (dont really get why its eta)

# 14 = Spatial effects for probability of capture [Not working here - not sure why]
# 15 = Spatial variation for positive catch rates [Not working here - not sure why]

# 16 = Spatially-varying response for habitat covariates in 1st linear predictor
# 17 = Spatially-varying response for habitat covariates in 2nd linear predictor


plot_maps_network <-
  function(plot_set=1, 
           make_plot=TRUE,
           fit, 
           Sdreport=NULL,
           TmbData=NULL, 
           spatial_list=NULL, 
           DirName=NULL, 
           plot_value="estimate",
           quiet = FALSE,
           Panel="Category",
           category_names=NULL, 
           covar_names=NULL, 
           PlotName,
           PlotTitle,
           MapSizeRatio,
           years_to_plot,
           year_labels,
           Xlim=NULL, 
           Ylim=NULL, 
           Zlim = NULL,
           legend=TRUE, 
           #textmargin, 
           arrows=FALSE, 
           cex=0.5, 
           pch=19, 
           n_p, #This is the number of covariates YOU want to plot (not necessarily how many there are overall)
           which_np_touse, #This is which of the covariates you want to plot. E.g. If there are 7 covariates but only want to plot c(2,5) (n_p=2)
           which_cat_cov_toplot, #Which category to plot covariate plots for
           ...){
    
    ## local functions ##
    logsum = function(vec){ max(vec) + log(sum(exp(vec-max(vec)))) }
    
    extract_value = function( Sdreport, Report, Obj=fit$tmb_list$Obj, variable_name, plot_value="estimate", n_samples, sample_fixed=TRUE ){
      if( missing(Report) ){
        Report = Obj$report()
      }
      if( is.function(plot_value) ){
        if(missing(Obj)) stop("Must provide `Obj` for `extract_value(.)` in `plot_maps(.)` when specifying a function for argument `plot_value`")
        Var_r = sample_variable( Sdreport=Sdreport, Obj=Obj, variable_name=variable_name, n_samples=n_samples, sample_fixed=sample_fixed )
        Return = apply( Var_r, MARGIN=1:(length(dim(Var_r))-1), FUN=plot_value )
        if( any(dim(Return)!=dim(Report[[variable_name]])) ){
          stop("Check `extract_value(.)` in `plot_maps(.)`")
        }
      }else if( plot_value=="estimate" ){
        Return = Report[[variable_name]]
      }else stop("Check input `plot_value` in `plot_maps(.)`")
      return( Return )
      # apply( Var_r, MARGIN=c(2,4), FUN=function(mat){sum(abs(mat)==Inf)})
    }
    ####
    
    if(missing(PlotName)){stop("PlotName is missing without any default")}
    if(missing(PlotTitle)){stop("PlotTitle is missing without any default")}
    #if(missing(textmargin)){stop("textmargin is missing without any default")}
    
    if(missing(year_labels)){year_labels = fit$year_labels}
    if(missing(years_to_plot)){years_to_plot = fit$years_to_plot}
    Report <- fit$Report
    
    
    # Fill in missing inputs
    if( "D_xt" %in% names(Report)){
      # SpatialDeltaGLMM
      category_names = "singlespecies"
      Ncategories = length(category_names)
      Nyears = dim(Report$D_xt)[2]
    }
    if( "D_xct" %in% names(Report)){
      # VAST Version < 2.0.0
      if( is.null(category_names) ) category_names = 1:dim(Report$D_xct)[2]
      Ncategories = dim(Report$D_xct)[2]
      Nyears = dim(Report$D_xct)[3]
    }
    if( "D_xcy" %in% names(Report)){
      # VAST Version >= 2.0.0
      if( is.null(category_names) ) category_names = 1:dim(Report$D_xcy)[2]
      Ncategories = dim(Report$D_xcy)[2]
      Nyears = dim(Report$D_xcy)[3]
    }
    if( "D_gcy" %in% names(Report)){
      # VAST Version 8.0.0 through 9.3.0
      if( is.null(category_names) ) category_names = 1:dim(Report$D_gcy)[2]
      Ncategories = dim(Report$D_gcy)[2]
      Nyears = dim(Report$D_gcy)[3]
    }
    if( "D_gct" %in% names(Report)){
      # VAST Version >= 8.0.0
      if( is.null(category_names) ) category_names = 1:dim(Report$D_gct)[2]
      Ncategories = dim(Report$D_gct)[2]
      Nyears = dim(Report$D_gct)[3]
    }
    if("dhat_ktp" %in% names(Report)){
      # MIST Version <= 14
      if( is.null(category_names) ) category_names = 1:dim(Report$dhat_ktp)[3]
      Ncategories = dim(Report$dhat_ktp)[3]
      Nyears = dim(Report$dhat_ktp)[2]
    }
    if("dpred_ktp" %in% names(Report)){
      # MIST Version >= 15
      if( is.null(category_names) ) category_names = 1:dim(Report$dpred_ktp)[3]
      Ncategories = dim(Report$dpred_ktp)[3]
      Nyears = dim(Report$dpred_ktp)[2]
    }
    
    if( missing(MapSizeRatio) ){
      MapSizeRatio = c(3, 3)
    }
    
    
    # Errors
    if( Nyears != length(year_labels) ){
      stop("Problem with `year_labels`")
    }
    # if( Nyears != length(year_labels) & !any(plot_set %in% c(9,10))){
    #   stop("Problem with `year_labels`")
    # }
    
    if( Ncategories != length(category_names) ){
      stop("Problem with `category_names`")
    }
    
    
    
    
    
    
    
    # Extract elements [doesn't do anything at the moment]
    plot_names <- c("Probability of capture", "Spatio-temporal variation", "Linear predictor for probability of capture", 
                    "Covariates", "Covariate effects for probability of capture", "Individual ovariate effects for probability of capture", 
                    "Spatial effects for probability of captures", "Spatially-varying response for habitat covariates in 1st linear predictor")
    # if( is.null(textmargin)){
    #   textmargin <- c("Probability of encounter", "Density, ln(kg. per square km.)", "Density, ln(kg. per square km.)", "", "", "", "", "", "", "CV of density (dimensionless)", "Covariate value", "Density, ln(kg. per square km.)", "", "", "", "")
    # }
    
    
    
    
    # Loop through plots
    Return = NULL
    for(plot_num in plot_set){
      
      inp_Zlim <- Zlim
      
      # Extract elements
      Array_xct = NULL
      # plot_code <- c("probability_of_capture", "pos_catch", "ln_density", "", "", "epsilon_1", "epsilon_2",
      #                "linear_predictor_1", "linear_predictor_2", "density_CV", "covariates_1", "covariates_2", "total_density",
      #                "covariate_effects_1", "covariate_effects_2", "omega_1", "omega_2")[plot_num]
      
      
      # Extract matrix to plot
      if(plot_num==1){
        # Probability of capture
        if( quiet==FALSE ) message(" # plot_num 1: Plotting probability of capture maps")
        if("D_xt"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="R1_xt")
        if("D_xct"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="R1_xct")
        if("D_xcy"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="R1_xcy")
        if("D_gcy"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="R1_gcy")
        if("D_gct"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="R1_gct")
        if(any(c("dhat_ktp","dpred_ktp")%in%names(Report))) stop("Not implemented for SpatialVAM")
        message( "`plot_num=1` doesn't work well when using ObsModel[2]==1, because average area-swept doesn't generally match area of extrapolation-grid cells" )
      }
      
      if(plot_num==2){
        # Log-expected positive catch rate
        if( quiet==FALSE ) message(" # plot_num 2: Plotting positive catch rate maps")
        if("D_xt"%in%names(Report)) Array_xct = log( extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="R2_xt") )
        if("D_xct"%in%names(Report)) Array_xct = log( extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="R2_xct") )
        if("D_xcy"%in%names(Report)) Array_xct = log( extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="R2_xcy") )
        if("D_gcy"%in%names(Report)) Array_xct = log( extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="R2_gcy") )
        if("D_gct"%in%names(Report)) Array_xct = log( extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="R2_gct") )
        if(any(c("dhat_ktp","dpred_ktp")%in%names(Report)))  stop("Not implemented for SpatialVAM")
        message( "`plot_num=2` doesn't work well when using ObsModel[2]==1, because average area-swept doesn't generally match area of extrapolation-grid cells" )
      }
      
      if(plot_num==3){
        # Log-predicted density (product of encounter probability and positive catch rates)
        if( quiet==FALSE ) message(" # plot_num 3: Plotting density maps (in log-space)")
        if("D_xt"%in%names(Report)) Array_xct = log( extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="D_xt") )
        if("D_xct"%in%names(Report)) Array_xct = log( extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="D_xct") )
        if("D_xcy"%in%names(Report)) Array_xct = log( extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="D_xcy") )
        if("D_gcy"%in%names(Report)) Array_xct = log( extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="D_gcy") )
        if("D_gct"%in%names(Report)) Array_xct = log( extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="D_gct") )
        if("dhat_ktp" %in% names(Report)) Array_xct = aperm(extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="dhat_ktp")[,,cI],c(1,3,2))
        if("dpred_ktp" %in% names(Report)) Array_xct = aperm(extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="dpred_ktp")[,,cI],c(1,3,2))
      }
      
      if(plot_num==4){
        # Total density ("Dens")
        if( quiet==FALSE ) message(" # plot_num 4: Plotting total density")
        if("D_xt"%in%names(Report)) Array_xct = log(Report$D_xt)
        if("D_xct"%in%names(Report)) Array_xct = log(apply(Report$D_xct, FUN=sum, MARGIN=c(1,3)))
        if("D_xcy"%in%names(Report)) Array_xct = log(apply(Report$D_xcy, FUN=sum, MARGIN=c(1,3)))
        if("D_gcy"%in%names(Report)) Array_xct = log(apply(Report$D_gcy, FUN=sum, MARGIN=c(1,3)))
        if("D_gct"%in%names(Report)) Array_xct = log(apply(Report$D_gct, FUN=sum, MARGIN=c(1,3)))
        logsum = function(vec){ max(vec) + log(sum(exp(vec-max(vec)))) }
        if("dhat_ktp" %in% names(Report)) Array_xct = apply(aperm(Report$dhat_ktp,c(1,3,2)), FUN=logsum, MARGIN=c(1,3))
        if("dpred_ktp" %in% names(Report)) Array_xct = apply(aperm(Report$dpred_ktp,c(1,3,2)), FUN=logsum, MARGIN=c(1,3))
      }
      
      if(plot_num==5){
        # Epsilon for presence/absence
        if( quiet==FALSE ) message(" # plot_num 5: Plotting spatio-temporal effects (Epsilon) in 1st linear predictor")
        if("D_xt"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="Epsilon1_st")
        if("D_xct"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="Epsilon1_sct")
        if("D_xcy"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="Epsilon1_sct")
        if(any(c("D_gcy","D_gct")%in%names(Report))) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="Epsilon1_gct")
        if(any(c("dhat_ktp","dpred_ktp")%in%names(Report)))  stop("Not implemented for SpatialVAM")
      }
      
      if(plot_num==6){
        # Epsilon for positive values ("Eps_Pos")
        if( quiet==FALSE ) message(" # plot_num 7: Plotting spatio-temporal effects (Epsilon) in 2nd linear predictor")
        if("D_xt"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="Epsilon2_st")
        if("D_xct"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="Epsilon2_sct")
        if("D_xcy"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="Epsilon2_sct")
        if(any(c("D_gcy","D_gct")%in%names(Report))) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="Epsilon2_gct")
        if(any(c("dhat_ktp","dpred_ktp")%in%names(Report)))  stop("Not implemented for SpatialVAM")
      }
      
      if(plot_num==7){
        # Linear predictor for probability of capture
        if( quiet==FALSE ) message(" # plot_num 7: Plotting 1st predictor after action of link function")
        if("D_xt"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="P1_xt")
        if("D_xct"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="P1_xct")
        if("D_xcy"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="P1_xcy")
        if(any(c("D_gcy","D_gct")%in%names(Report))) stop("`plot_maps` not implemented for requested plot_num")
        if(any(c("dhat_ktp","dpred_ktp")%in%names(Report)))  stop("Not implemented for SpatialVAM")
      }
      
      if(plot_num==8){
        # Linear predictor for positive catch rates
        if( quiet==FALSE ) message(" # plot_num 8: Plotting 2nd predictor after action of link function")
        if("D_xt"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="P2_xt")
        if("D_xct"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="P2_xct")
        if("D_xcy"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Obj=Obj, Report=Report, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="P2_xcy")
        if(any(c("D_gcy","D_gct")%in%names(Report))) stop("`plot_maps` not implemented for requested plot_num")
        if(any(c("dhat_ktp","dpred_ktp")%in%names(Report)))  stop("Not implemented for SpatialVAM")
      }
      
      if(plot_num==9){
        # Covariates that are included in the model (measured values)
        if( quiet==FALSE ) message(" # plot_num 9: Plotting covariates for 1st linear predictor")
        if(is.null(TmbData)) stop( "Must provide `TmbData` to plot covariates" )
        #if(!("X_xtp" %in% names(TmbData))) stop( "Can only plot covariates for VAST version >= 2.0.0" )
        if("X_xtp"%in%names(TmbData)) Array_xct = aperm( TmbData$X_xtp, perm=c(1,3,2) )
        if("X_gtp"%in%names(TmbData)) Array_xct = aperm( TmbData$X_gtp, perm=c(1,3,2) )
        if("X_gctp"%in%names(TmbData)) Array_xct = aperm( array(TmbData$X_gctp[,1,,],dim(TmbData$X_gctp)[c(1,3,4)]), perm=c(1,3,2) )
        if("X1_gctp"%in%names(TmbData)) Array_xct = aperm( array(TmbData$X1_gctp[,1,,],dim(TmbData$X1_gctp)[c(1,3,4)]), perm=c(1,3,2) )
        #category_names = 1:dim(Array_xct)[2]
        category_names = covar_names
        
        # if(!missing(which_cat_cov_toplot)){
        #   Array_xct <- Array_xct[,which_cat_cov_toplot,years_to_plot,drop=FALSE]
        #   category_names <-  category_names[which_cat_cov_toplot]
        # }
        
      }
      
      if(plot_num==10){
        # Individual covariate effects for probability of capture
        if( quiet==FALSE ) message(" # plot_num 10: Plotting individual covariate effects for probability of capture")
        if(is.null(TmbData)) stop("Must provide `TmbData` to plot covariates.")
        #if(!("X_gtp" %in% names(TmbData))) stop( "Can only plot covariates for VAST version >= 2.0.0")
        if(!("X1_gctp" %in% names(TmbData))) stop( "Can only plot covariates for VAST version >= 3.6.0")
        
        if(is.null(covar_names)){stop("covar_names must be specified")}
        
        if(missing(n_p)){stop("n_p missing. Please specify.")}
        if(missing(which_np_touse)){stop("which_np_touse missing. Please specify.")}
        if(missing(which_cat_cov_toplot)){stop("which_cat_cov_toplot missing. Please specify.")}
        
        message(paste0("Covariates for category #", which_cat_cov_toplot, " is being plotted"))
        
        ####
        X <- TmbData$X1_gctp[,which_cat_cov_toplot,,]
        
        gamma1_cp <- Sdreport$par.fixed[names(Sdreport$par.fixed)=="gamma1_cp"]
        
        gamma1_cp <- gamma1_cp[seq(which_cat_cov_toplot, length(category_names)*n_p, by=length(category_names))]
        
        category_names <- paste0(covar_names, " for category: ", category_names[which_cat_cov_toplot])
        Array_xct <- array(NA, dim=c(dim(X)[1], n_p, dim(X)[2]))
        
        for(i in 1:n_p){
          eta <- gamma1_cp[i] * X[,,which_np_touse[i]]
          Array_xct[,i,] <- eta
        }
        
        #Array_xct <- Array_xct[,which_np_touse,]
        ####  
        
        
      }
      
      
      
      if(plot_num==11){
        # Individual covariate effects for positive catch rates
        if( quiet==FALSE ) message(" # plot_num 11: Plotting individual covariate effects for positive catch rates")
        if(is.null(TmbData)) stop("Must provide `TmbData` to plot covariates.")
        #if(!("X_gtp" %in% names(TmbData))) stop( "Can only plot covariates for VAST version >= 2.0.0")
        if(!("X2_gctp" %in% names(TmbData))) stop( "Can only plot covariates for VAST version >= 3.6.0")
        
        if(is.null(covar_names)){stop("covar_names must be specified")}
        
        
        if(missing(n_p)){stop("n_p missing. Please specify.")}
        if(missing(which_np_touse)){stop("which_np_touse missing. Please specify.")}
        
        message(paste0("Covariates for category #", which_cat_cov_toplot, " is being plotted"))
        
        ####
        X <- TmbData$X1_gctp[,which_cat_cov_toplot,,]
        
        gamma2_cp <- Sdreport$par.fixed[names(Sdreport$par.fixed)=="gamma2_cp"]
        
        gamma2_cp <- gamma2_cp[seq(which_cat_cov_toplot, length(category_names)*n_p, by=length(category_names))]
        
        category_names <- paste0(covar_names, " for category: ", category_names[which_cat_cov_toplot])
        Array_xct <- array(NA, dim=c(dim(X)[1], n_p, dim(X)[2]))
        
        for(i in 1:n_p){
          eta <- gamma2_cp[i] * X[,,which_np_touse[i]]
          Array_xct[,i,] <- eta
        }
        
        #Array_xct <- Array_xct[,which_np_touse,]
        ####
      }
      
      
      if(plot_num==12){
        # Combined covariate effects for probability of capture
        if( quiet==FALSE ) message(" # plot_num 12: Plotting covariate effects for 1st linear predictor")
        if("D_xt"%in%names(Report)) stop()
        if("D_xct"%in%names(Report)) stop()
        if("D_xcy"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Report=Report, Obj=Obj, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="eta1_xct")
        if(any(c("D_gcy","D_gct")%in%names(Report))) Array_xct = extract_value(Sdreport=Sdreport, Report=Report, Obj=Obj, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="eta1_gct")
        if("dhat_ktp" %in% names(Report)) stop()
        if("dpred_ktp" %in% names(Report)) stop()
      }
      
      if(plot_num==13){
        # Covariate effects for positive catch rates
        if( quiet==FALSE ) message(" # plot_num 13: Plotting covariate effects for 2nd linear predictor")
        if("D_xt"%in%names(Report)) stop()
        if("D_xct"%in%names(Report)) stop()
        if("D_xcy"%in%names(Report)) Array_xct = extract_value(Sdreport=Sdreport, Report=Report, Obj=Obj, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="eta2_xct")
        if(any(c("D_gcy","D_gct")%in%names(Report))) Array_xct = extract_value(Sdreport=Sdreport, Report=Report, Obj=Obj, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="eta2_gct")
        if("dhat_ktp" %in% names(Report)) stop()
        if("dpred_ktp" %in% names(Report)) stop()
      }
      
      if(plot_num==14){
        # Spatial effects for probability of capture
        if( quiet==FALSE ) message(" # plot_num ",plot_num,": plotting spatial effects (Omega) for 1st linear predictor")
        if("D_xt"%in%names(Report)) stop()
        if("D_xct"%in%names(Report)) stop()
        if("D_xcy"%in%names(Report)) Array_xct = Report$Omega1_sc %o% 1
        if(any(c("D_gcy","D_gct")%in%names(Report))) Array_xct = Report$Omega1_gc %o% 1
        if("dhat_ktp" %in% names(Report)) stop()
        if("dpred_ktp" %in% names(Report)) stop()
      }
      
      if(plot_num==15){
        # Spatial effects for positive catch rates
        if( quiet==FALSE ) message(" # plot_num ",plot_num,": plotting spatial effects (Omega) for 2nd linear predictor")
        if("D_xt"%in%names(Report)) stop()
        if("D_xct"%in%names(Report)) stop()
        if("D_xcy"%in%names(Report)) Array_xct = Report$Omega2_sc %o% 1
        if(any(c("D_gcy","D_gct")%in%names(Report))) Array_xct = Report$Omega2_gc %o% 1
        if("dhat_ktp" %in% names(Report)) stop()
        if("dpred_ktp" %in% names(Report)) stop()
      }
      
      if(plot_num==16){
        # Spatially-varying response for habitat covariates in 1st linear predictor
        if( quiet==FALSE ) message(" # plot_num ",plot_num,": plotting spatially-varying response to habitat covariates (Xi) for 1st linear predictor")
        if("D_xt"%in%names(Report)) stop()
        if("D_xct"%in%names(Report)) stop()
        if("D_xcy"%in%names(Report)) stop()
        if(any(c("D_gcy","D_gct")%in%names(Report))) Array_xct = extract_value(Sdreport=Sdreport, Report=Report, Obj=Obj, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="Xi1_gcp")
        if("dhat_ktp" %in% names(Report)) stop()
        if("dpred_ktp" %in% names(Report)) stop()
      }
      
      if(plot_num==17){
        # Spatially-varying response for density covariates in 1st linear predictor
        if( quiet==FALSE ) message(" # plot_num ",plot_num,": plotting spatially-varying response to density covariates (Xi) for 2nd linear predictor")
        if("D_xt"%in%names(Report)) stop()
        if("D_xct"%in%names(Report)) stop()
        if("D_xcy"%in%names(Report)) stop()
        if(any(c("D_gcy","D_gct")%in%names(Report))) Array_xct = extract_value(Sdreport=Sdreport, Report=Report, Obj=Obj, plot_value=plot_value, sample_fixed=sample_fixed, n_samples=n_samples, variable_name="Xi2_gcp")
        if("dhat_ktp" %in% names(Report)) stop()
        if("dpred_ktp" %in% names(Report)) stop()
      }
      
      
      if( is.null(Array_xct)) stop("Problem with `plot_num`")
      if( any(abs(Array_xct)==Inf) ) stop("plot_maps(.) has some element of output that is Inf or -Inf, please check results")
      # if( all(Years2Include %in% 1:dim(Array_xct)[3]) ){
      #   years_to_include = Years2Include
      # }else{
      #   years_to_include = 1:dim(Array_xct)[3]
      # }
      
      
      # require(ggplot2)
      if(all(is.null(spatial_list))) stop("add spatial_list (output from FishStatsUtils::make_spatial_info) to use ggplot2 plots.")
      
      
      
      # if(make_plot==TRUE){
      #   
      #   # Plot for each category
      #   if( tolower(Panel)=="category" ){
      #     if(length(dim(Array_xct))==2) Nplot = 1
      #     if(length(dim(Array_xct))==3) Nplot = dim(Array_xct)[2]
      #     
      #     Return = Array_xct
      #     
      #     for( cI in 1:Nplot){
      #       
      #       # if(length(dim(Array_xct))==2) Return = Mat_xt = Array_xct
      #       # if(length(dim(Array_xct))==3) Return = Mat_xt = array(as.vector(Array_xct[,cI,]),dim=dim(Array_xct)[c(1,3)])
      #       if(length(dim(Array_xct))==2) Mat_xt = Array_xct
      #       if(length(dim(Array_xct))==3) Mat_xt = array(as.vector(Array_xct[,cI,]),dim=dim(Array_xct)[c(1,3)])
      #       
      #       
      #       ## matrix is number of nodes by number of years
      #       if(is.null(dim(Mat_xt)))  n_t = 1 else n_t <- dim(Mat_xt)[2]
      #       if(n_t != length(year_labels)) stop("number of years in density array does not match Data_Geostat years")
      #       
      #       if(n_t > 1) {
      #         xct <- lapply(1:n_t, function(x){
      #           out <- data.frame('value'=Mat_xt[,x], 'year'=year_labels[x], spatial_list$loc_g, 'category'=category_names[cI])
      #           return(out)
      #         })
      #         xct <- do.call(rbind, xct)
      #       } else xct <- data.frame('value'=Mat_xt, 'year'=year_labels, spatial_list$loc_g, "category"=category_names[1])
      #       
      #       if(all(is.null(Xlim))) Xlim = c(min(xct$E_km),max(xct$E_km))
      #       if(all(is.null(Ylim))) Ylim = c(min(xct$N_km),max(xct$N_km))
      #       p <- ggplot(xct)
      #       if(arrows == TRUE){
      #         Network_sz_EN <- data.frame('parent_s'=fit$data_list$parent_s, 'child_s'=fit$data_list$child_s, fit$spatial_list$loc_g)
      #         l2 <- lapply(1:nrow(Network_sz_EN), function(x){
      #           parent <- Network_sz_EN$parent_s[x]
      #           find <- Network_sz_EN %>% filter(child_s == parent)
      #           if(nrow(find)>0) out <- cbind.data.frame(Network_sz_EN[x,], 'E2'=find$E_km, 'N2'=find$N_km)
      #           if(nrow(find)==0) out <- cbind.data.frame(Network_sz_EN[x,], 'E2'=NA, 'N2'=NA)
      #           # if(nrow(find)>0) out <- cbind.data.frame(Network_sz_EN[x,], 'long2'=find$long, 'lat2'=find$lat)
      #           # if(nrow(find)==0) out <- cbind.data.frame(Network_sz_EN[x,], 'long2'=NA, 'lat2'=NA)
      #           return(out)
      #         })
      #         l2 <- do.call(rbind, l2)
      #         p <- p + geom_segment(data=l2, aes(x = E_km,y = N_km, xend = E2, yend = N2), arrow=arrow(length=unit(0.2,"cm")), 
      #                               col="gray92")
      #       }
      #       if(is.null(Zlim)) inp_Zlim = quantile(xct$value, prob = c(0,1), na.rm=TRUE)
      #       p <- p +
      #         geom_point(aes(x = E_km, y = N_km, color = value), cex = cex, pch=pch) +
      #         scale_color_distiller(palette = "Spectral", limits = inp_Zlim) +
      #         coord_cartesian(xlim = Xlim, ylim = Ylim) +
      #         scale_x_continuous(breaks=quantile(xct$E_km, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$E_km, prob=c(0.1,0.5,0.9)),0)) +
      #         # guides(color=guide_legend(title=plot_codes[plot_num])) +
      #         facet_wrap(~year) + 
      #         mytheme() +
      #         xlab("Eastings") + ylab("Northings")
      #       
      #       #if(is.null(PlotName)){PlotName=plot_names[plot_num]}
      #       
      #       if(Nplot!=1) p <- p + ggtitle(wrapper(paste(PlotTitle, " - ", category_names[cI]), width = 50))
      #       if(Nplot==1) p <- p + ggtitle(wrapper(PlotTitle, width = 50))
      #       
      #       if(!is.null(DirName)){
      #         
      #         if(Nplot!=1) ggsave(file.path(DirName, paste0(PlotName, "_", cI, "_byCat.png")), p, width=8,height=8)
      #         if(Nplot==1) ggsave(file.path(DirName, paste0(PlotName, ".png")), p, width=8,height=8)
      #       }      
      #       if(is.null(DirName)){
      #         dev.new()
      #         print(p)
      #       }    
      #     }
      #   }
      #   
      #   # Plot for each year
      #   if( tolower(Panel)=="year" ){
      #     Nplot = length(years_to_plot)
      #     
      #     Return = Array_xct
      #     
      #     for( tI in 1:Nplot){
      #       if(length(dim(Array_xct))==2) Mat_xc = Array_xct[,years_to_plot[tI],drop=TRUE]
      #       if(length(dim(Array_xct))==3) Mat_xc = Array_xct[,,years_to_plot[tI],drop=TRUE]
      #       if(is.null(dim(Mat_xc)) & is.vector(Mat_xc)){
      #         Ncategories = 1
      #       } else { Ncategories = dim(Mat_xc)[2] }
      #       
      #       # Return = Mat_xc = array( as.vector(Mat_xc), dim=c(dim(Array_xct)[1],Ncategories)) # Reformat to make sure it has same format for everything
      #       Mat_xc = array( as.vector(Mat_xc), dim=c(dim(Array_xct)[1],Ncategories))
      #       
      #       
      #       ## matrix is number of nodes by number of years
      #       n_c <- dim(Mat_xc)[2]
      #       if(n_c > 1) {
      #         xct <- lapply(1:n_c, function(x){
      #           out <- data.frame('value'=Mat_xc[,x], 'year'=year_labels[years_to_plot[tI]], spatial_list$loc_g, 'category'=category_names[x])
      #           return(out)
      #         })
      #         xct <- do.call(rbind, xct)
      #       } else xct <- data.frame('value'=Mat_xc, 'year'=year_labels[years_to_plot[tI]], spatial_list$loc_g, "category"='')
      #       if(all(is.null(Xlim))) Xlim = c(min(xct$E_km),max(xct$E_km))
      #       if(all(is.null(Ylim))) Ylim = c(min(xct$N_km),max(xct$N_km))
      #       p <- ggplot(xct)
      #       if(arrows == TRUE){
      #         Network_sz_EN <- data.frame('parent_s'=fit$data_list$parent_s, 'child_s'=fit$data_list$child_s, fit$spatial_list$loc_g)
      #         l2 <- lapply(1:nrow(Network_sz_EN), function(x){
      #           parent <- Network_sz_EN$parent_s[x]
      #           find <- Network_sz_EN %>% filter(child_s == parent)
      #           if(nrow(find)>0) out <- cbind.data.frame(Network_sz_EN[x,], 'E2'=find$E_km, 'N2'=find$N_km)
      #           if(nrow(find)==0) out <- cbind.data.frame(Network_sz_EN[x,], 'E2'=NA, 'N2'=NA)
      #           # if(nrow(find)>0) out <- cbind.data.frame(Network_sz_EN[x,], 'long2'=find$long, 'lat2'=find$lat)
      #           # if(nrow(find)==0) out <- cbind.data.frame(Network_sz_EN[x,], 'long2'=NA, 'lat2'=NA)
      #           return(out)
      #         })
      #         l2 <- do.call(rbind, l2)
      #         p <- p + geom_segment(data=l2, aes(x = E_km,y = N_km, xend = E2, yend = N2), #arrow=arrow(length=unit(0.2,"cm")), 
      #                               col="gray92")
      #       }
      #       if(is.null(Zlim)) inp_Zlim = quantile(xct$value, prob = c(0,1), na.rm=TRUE)
      #       
      #       
      #       if(n_c > 1){
      #         
      #         p <- p +
      #           geom_point(aes(x = E_km, y = N_km, color = value), cex=cex, pch=pch) +
      #           scale_color_distiller(palette = "Spectral", limits = inp_Zlim) +
      #           coord_cartesian(xlim = Xlim, ylim = Ylim) +
      #           scale_x_continuous(breaks=quantile(xct$E_km, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$E_km, prob=c(0.1,0.5,0.9)),0)) +
      #           # guides(color=guide_legend(title=plot_codes[plot_num])) +
      #           facet_wrap(~category) + 
      #           mytheme() +
      #           xlab("Eastings") + ylab("Northings")
      #         
      #         width = 10; height = 5
      #         
      #       } 
      #       
      #       if(n_c == 1){
      #         
      #         p <- p +
      #           geom_point(aes(x = E_km, y = N_km, color = value), cex=cex, pch=pch) +
      #           scale_color_distiller(palette = "Spectral", limits = inp_Zlim) +
      #           coord_cartesian(xlim = Xlim, ylim = Ylim) +
      #           scale_x_continuous(breaks=quantile(xct$E_km, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$E_km, prob=c(0.1,0.5,0.9)),0)) +
      #           # guides(color=guide_legend(title=plot_codes[plot_num])) +
      #           #facet_wrap(~category) + 
      #           mytheme() +
      #           xlab("Eastings") + ylab("Northings")
      #         
      #         width = 6; height = 5
      #         
      #       }
      #       
      #       #if(is.null(PlotName)){PlotName=plot_names[plot_num]}
      #       
      #       if(Nplot!=1) p <- p + ggtitle(wrapper(paste(PlotTitle," - ", year_labels[years_to_plot[tI]]), width = 50))
      #       if(Nplot==1) p <- p + ggtitle(wrapper(PlotTitle, width = 50))
      #       
      #       if(!is.null(DirName)){
      #         
      #         if(Nplot!=1) ggsave(file.path(DirName, paste0(PlotName, "_", year_labels[years_to_plot[tI]], ".png")), p, width=width, height=height)
      #         if(Nplot==1) ggsave(file.path(DirName, paste0(PlotName, ".png")), p, width = width, height = height)
      #       }       
      #       if(is.null(DirName)){
      #         dev.new()
      #         print(p) 
      #       }  
      #     }
      #   }
      #   
      # }else{
      #   if(length(plot_set) > 1){print("Returned array will be for the last plot_set when plot_set > 1")}
      #   
      #   Return = Array_xct
      #   
      # }
      
      
      
      
      
      
      if(make_plot==TRUE){
        
        # Plot for each category
        if( tolower(Panel)=="category" ){
          if(length(dim(Array_xct))==2) Nplot = 1
          if(length(dim(Array_xct))==3) Nplot = dim(Array_xct)[2]
          
          Return = Array_xct
          
          for( cI in 1:Nplot){
            
            # if(length(dim(Array_xct))==2) Return = Mat_xt = Array_xct
            # if(length(dim(Array_xct))==3) Return = Mat_xt = array(as.vector(Array_xct[,cI,]),dim=dim(Array_xct)[c(1,3)])
            if(length(dim(Array_xct))==2) Mat_xt = Array_xct
            if(length(dim(Array_xct))==3) Mat_xt = array(as.vector(Array_xct[,cI,]),dim=dim(Array_xct)[c(1,3)])
            
            
            ## matrix is number of nodes by number of years
            if(is.null(dim(Mat_xt)))  n_t = 1 else n_t <- dim(Mat_xt)[2]
            if(n_t != length(year_labels)) stop("number of years in density array does not match Data_Geostat years")
            
            if(n_t > 1) {
              xct <- lapply(1:n_t, function(x){
                out <- data.frame('value'=Mat_xt[,x], 'year'=year_labels[x], spatial_list$latlon_g, 'category'=category_names[cI])
                return(out)
              })
              xct <- do.call(rbind, xct)
            } else xct <- data.frame('value'=Mat_xt, 'year'=year_labels, spatial_list$latlon_g, "category"=category_names[1])
            
            if(all(is.null(Xlim))) Xlim = c(min(xct$Lon),max(xct$Lon))
            if(all(is.null(Ylim))) Ylim = c(min(xct$Lat),max(xct$Lat))
            p <- ggplot(xct)
            if(arrows == TRUE){
              Network_sz_EN <- data.frame('parent_s'=fit$data_list$parent_s, 'child_s'=fit$data_list$child_s, fit$spatial_list$latlon_g)
              l2 <- lapply(1:nrow(Network_sz_EN), function(x){
                parent <- Network_sz_EN$parent_s[x]
                find <- Network_sz_EN %>% filter(child_s == parent)
                if(nrow(find)>0) out <- cbind.data.frame(Network_sz_EN[x,], 'E2'=find$Lon, 'N2'=find$Lat)
                if(nrow(find)==0) out <- cbind.data.frame(Network_sz_EN[x,], 'E2'=NA, 'N2'=NA)
                # if(nrow(find)>0) out <- cbind.data.frame(Network_sz_EN[x,], 'long2'=find$long, 'lat2'=find$lat)
                # if(nrow(find)==0) out <- cbind.data.frame(Network_sz_EN[x,], 'long2'=NA, 'lat2'=NA)
                return(out)
              })
              l2 <- do.call(rbind, l2)
              p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = E2, yend = N2), arrow=arrow(length=unit(0.2,"cm")), 
                                    col="gray92")
            }
            if(is.null(Zlim)) inp_Zlim = quantile(xct$value, prob = c(0,1), na.rm=TRUE)
            p <- p +
              geom_point(aes(x = Lon, y = Lat, color = value), cex = cex, pch=pch) +
              #scale_color_distiller(palette = "Spectral", limits = inp_Zlim) +
              scale_color_distiller(palette = "RdYlGn", limits = inp_Zlim, direction = 1) +
              coord_cartesian(xlim = Xlim, ylim = Ylim) +
              scale_x_continuous(breaks=quantile(xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$Lon, prob=c(0.1,0.5,0.9)),1)) +
              # guides(color=guide_legend(title=plot_codes[plot_num])) +
              facet_wrap(~year) + 
              mytheme() +
              xlab("Longitude") + ylab("Latitude")
            
            #if(is.null(PlotName)){PlotName=plot_names[plot_num]}
            
            if(Nplot!=1) p <- p + ggtitle(wrapper(paste(PlotTitle, " - ", category_names[cI]), width = 40))
            if(Nplot==1) p <- p + ggtitle(wrapper(PlotTitle, width = 40))
            
            if(!is.null(DirName)){
              
              if(Nplot!=1) ggsave(file.path(DirName, paste0(PlotName, "_", cI, "_byCat.png")), p, width=8,height=8)
              if(Nplot==1) ggsave(file.path(DirName, paste0(PlotName, ".png")), p, width=8,height=8)
            }      
            if(is.null(DirName)){
              dev.new()
              print(p)
            }    
          }
        }
        
        # Plot for each year
        if( tolower(Panel)=="year" ){
          Nplot = length(years_to_plot)
          
          Return = Array_xct
          
          for( tI in 1:Nplot){
            if(length(dim(Array_xct))==2) Mat_xc = Array_xct[,years_to_plot[tI],drop=TRUE]
            if(length(dim(Array_xct))==3) Mat_xc = Array_xct[,,years_to_plot[tI],drop=TRUE]
            if(is.null(dim(Mat_xc)) & is.vector(Mat_xc)){
              Ncategories = 1
            } else { Ncategories = dim(Mat_xc)[2] }
            
            # Return = Mat_xc = array( as.vector(Mat_xc), dim=c(dim(Array_xct)[1],Ncategories)) # Reformat to make sure it has same format for everything
            Mat_xc = array( as.vector(Mat_xc), dim=c(dim(Array_xct)[1],Ncategories))
            
            
            ## matrix is number of nodes by number of years
            n_c <- dim(Mat_xc)[2]
            if(n_c > 1) {
              xct <- lapply(1:n_c, function(x){
                out <- data.frame('value'=Mat_xc[,x], 'year'=year_labels[years_to_plot[tI]], spatial_list$latlon_g, 'category'=category_names[x])
                return(out)
              })
              xct <- do.call(rbind, xct)
            } else xct <- data.frame('value'=Mat_xc, 'year'=year_labels[years_to_plot[tI]], spatial_list$latlon_g, "category"='')
            if(all(is.null(Xlim))) Xlim = c(min(xct$Lon),max(xct$Lon))
            if(all(is.null(Ylim))) Ylim = c(min(xct$Lat),max(xct$Lat))
            p <- ggplot(xct)
            if(arrows == TRUE){
              Network_sz_EN <- data.frame('parent_s'=fit$data_list$parent_s, 'child_s'=fit$data_list$child_s, fit$spatial_list$latlon_g)
              l2 <- lapply(1:nrow(Network_sz_EN), function(x){
                parent <- Network_sz_EN$parent_s[x]
                find <- Network_sz_EN %>% filter(child_s == parent)
                if(nrow(find)>0) out <- cbind.data.frame(Network_sz_EN[x,], 'E2'=find$Lon, 'N2'=find$Lat)
                if(nrow(find)==0) out <- cbind.data.frame(Network_sz_EN[x,], 'E2'=NA, 'N2'=NA)
                # if(nrow(find)>0) out <- cbind.data.frame(Network_sz_EN[x,], 'long2'=find$long, 'lat2'=find$lat)
                # if(nrow(find)==0) out <- cbind.data.frame(Network_sz_EN[x,], 'long2'=NA, 'lat2'=NA)
                return(out)
              })
              l2 <- do.call(rbind, l2)
              p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = E2, yend = N2), #arrow=arrow(length=unit(0.2,"cm")), 
                                    col="gray92")
            }
            if(is.null(Zlim)) inp_Zlim = quantile(xct$value, prob = c(0,1), na.rm=TRUE)
            
            
            if(n_c > 1){
              
              p <- p +
                geom_point(aes(x = Lon, y = Lat, color = value), cex=cex, pch=pch) +
                #scale_color_distiller(palette = "Spectral", limits = inp_Zlim) +
                scale_color_distiller(palette = "RdYlGn", limits = inp_Zlim, direction = 1) +
                coord_cartesian(xlim = Xlim, ylim = Ylim) +
                scale_x_continuous(breaks=quantile(xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$Lon, prob=c(0.1,0.5,0.9)),1)) +
                # guides(color=guide_legend(title=plot_codes[plot_num])) +
                facet_wrap(~category) + 
                mytheme() +
                xlab("Longitude") + ylab("Latitude")
              
              width = 10; height = 5
              
            } 
            
            if(n_c == 1){
              
              p <- p +
                geom_point(aes(x = Lon, y = Lat, color = value), cex=cex, pch=pch) +
                #scale_color_distiller(palette = "Spectral", limits = inp_Zlim) +
                scale_color_distiller(palette = "RdYlGn", limits = inp_Zlim, direction = 1) +
                coord_cartesian(xlim = Xlim, ylim = Ylim) +
                scale_x_continuous(breaks=quantile(xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$Lon, prob=c(0.1,0.5,0.9)),1)) +
                # guides(color=guide_legend(title=plot_codes[plot_num])) +
                #facet_wrap(~category) + 
                mytheme() +
                xlab("Longitude") + ylab("Latitude")
              
              width = 6; height = 5
              
            }
            
            #if(is.null(PlotName)){PlotName=plot_names[plot_num]}
            
            if(Nplot!=1) p <- p + ggtitle(wrapper(paste(PlotTitle," - ", year_labels[years_to_plot[tI]]), width = 40))
            if(Nplot==1) p <- p + ggtitle(wrapper(PlotTitle, width = 40))
            
            if(!is.null(DirName)){
              
              if(Nplot!=1) ggsave(file.path(DirName, paste0(PlotName, "_", year_labels[years_to_plot[tI]], ".png")), p, width=width, height=height)
              if(Nplot==1) ggsave(file.path(DirName, paste0(PlotName, ".png")), p, width = width, height = height)
            }       
            if(is.null(DirName)){
              dev.new()
              print(p) 
            }  
          }
        }
        
      }else{
        if(length(plot_set) > 1){print("Returned array will be for the last plot_set when plot_set > 1")}
        
        Return = Array_xct
        
      }
      
      
      
      
      
      
      
      
    }
    
    return( invisible(Return) )
  }




wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}


mytheme <- function (base_size = 14, base_family = "") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.title.x = element_text(margin = margin(10,0,0,0)),
          #axis.title.x = element_text(vjust = -1.5),
          #axis.title.y = element_text(margin = margin(0,20,0,0)),
          #axis.title.y = element_text(vjust = -0.1),
          legend.title = element_blank(),
          axis.text = element_text(size = rel(0.5)),
          axis.text.x = element_text(angle = 90),
          axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey50"),
          panel.grid.major = element_line(colour = "grey90", size = 0.2),
          panel.grid.minor = element_line(colour = "grey98", size = 0.5),
          strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2))
}




# plot_range_index_riverlength <-
#   function( Sdreport,
#             Report,
#             TmbData,
#             year_labels = NULL,
#             years_to_plot = NULL,
#             strata_names = NULL,
#             PlotDir = paste0(getwd(),"/"),
#             FileName_COG = paste0(PlotDir,"/center_of_gravity.png"),
#             FileName_Area = paste0(PlotDir,"/Area.png"),
#             FileName_EffArea = paste0(PlotDir,"/Effective_Area.png"),
#             Znames = rep("",ncol(TmbData$Z_xm)),
#             use_biascorr = TRUE,
#             category_names = NULL,
#             interval_width = 1,
#             total_river_length,
#             ...){
#     
#     # Informative errors
#     if(is.null(Sdreport)) stop("Sdreport is NULL; please provide Sdreport")
#     
#     # Which parameters
#     if( "ln_Index_tl" %in% rownames(TMB::summary.sdreport(Sdreport)) ){
#       # SpatialDeltaGLMM
#       CogName = "mean_Z_tm"
#       EffectiveName = "effective_area_tl"
#       TmbData[['n_c']] = 1
#     }
#     if( "ln_Index_ctl" %in% rownames(TMB::summary.sdreport(Sdreport)) ){
#       # VAST Version < 2.0.0
#       CogName = "mean_Z_ctm"
#       EffectiveName = "effective_area_ctl"
#     }
#     if( "ln_Index_cyl" %in% rownames(TMB::summary.sdreport(Sdreport)) ){
#       # VAST Version >= 2.0.0
#       CogName = "mean_Z_cym"
#       EffectiveName = "effective_area_cyl"
#       TmbData[["n_t"]] = nrow(TmbData[["t_yz"]])
#     }
#     
#     # Default inputs
#     if( is.null(year_labels)) year_labels = 1:TmbData$n_t
#     if( is.null(years_to_plot) ) years_to_plot = 1:TmbData$n_t
#     if( is.null(strata_names) ) strata_names = 1:TmbData$n_l
#     if( is.null(category_names) ) category_names = 1:TmbData$n_c
#     Return = list( "year_labels"=year_labels )
#     
#     # Plot distribution shift and kernal-area approximation to area occupied if necessary outputs are available
#     if( !any(c("mean_Z_tm","mean_Z_ctm","mean_Z_cym") %in% names(Report)) ){
#       message( "To plot range-shifts and kernal-approximation to area occupied, please re-run with Options['Calculate_Range']=1" )
#     }else{
#       message( "Plotting center-of-gravity..." )
#       
#       # Extract index  (using bias-correctino if available and requested)
#       SD = TMB::summary.sdreport(Sdreport)
#       SD_mean_Z_ctm = array( NA, dim=c(unlist(TmbData[c('n_c','n_t','n_m')]),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) )
#       if( use_biascorr==TRUE && "unbiased"%in%names(Sdreport) ){
#         SD_mean_Z_ctm[] = SD[which(rownames(SD)==CogName),c('Est. (bias.correct)','Std. Error')]
#       }
#       if( !any(is.na(SD_mean_Z_ctm)) ){
#         message("Using bias-corrected estimates for center of gravity...")
#       }else{
#         message("Not using bias-corrected estimates for center of gravity...")
#         SD_mean_Z_ctm[] = SD[which(rownames(SD)==CogName),c('Estimate','Std. Error')]
#       }
#       
#       # Plot center of gravity
#       # Can't use `FishStatsUtils::plot_index` because of 2-column format
#       png( file=FileName_COG, width=6.5, height=TmbData$n_c*2, res=200, units="in")
#       par( mar=c(2,2,1,0), mgp=c(1.75,0.25,0), tck=-0.02, oma=c(1,1,0,1.5), mfrow=c(TmbData$n_c,dim(SD_mean_Z_ctm)[[3]]), ... )  #
#       for( cI in 1:TmbData$n_c ){
#         for( mI in 1:dim(SD_mean_Z_ctm)[[3]]){
#           Ybounds = (SD_mean_Z_ctm[cI,years_to_plot,mI,'Estimate']%o%rep(interval_width,2) + SD_mean_Z_ctm[cI,years_to_plot,mI,'Std. Error']%o%c(-interval_width,interval_width))
#           Ylim = range(Ybounds,na.rm=TRUE)
#           plot_lines(x=year_labels[years_to_plot], 
#                      y=SD_mean_Z_ctm[cI,years_to_plot,mI,'Estimate'], 
#                      ybounds=Ybounds, 
#                      col_bounds=rgb(1,0,0,0.2), 
#                      fn=plot,
#                      type="l", 
#                      lwd=2, 
#                      col="red", 
#                      bounds_type="shading", 
#                      ylim=Ylim, xlab="", ylab="", main="")
#           if( cI==1 ) mtext(side=3, text=Znames[mI], outer=FALSE )
#           if( mI==dim(SD_mean_Z_ctm)[[3]] & TmbData$n_c>1 ) mtext(side=4, text=category_names[cI], outer=FALSE, line=0.5)
#         }}
#       mtext( side=1:2, text=c("Year","Location"), outer=TRUE, line=c(0,0) )
#       dev.off()
#       
#       # Write to file
#       COG_Table = NULL
#       for( cI in 1:TmbData$n_c ){
#         for( mI in 1:dim(SD_mean_Z_ctm)[[3]]){
#           Tmp = cbind("m"=mI, "Year"=year_labels, "COG_hat"=SD_mean_Z_ctm[cI,,mI,'Estimate'], "SE"=SD_mean_Z_ctm[cI,,mI,'Std. Error'])
#           if( TmbData$n_c>1 ) Tmp = cbind( "Category"=category_names[cI], Tmp)
#           COG_Table = rbind(COG_Table, Tmp)
#         }}
#       
#       # Plot area
#       #KernelArea_Table = cbind("Year"=year_labels, "KernelArea"=SD_log_area_Z_tmm[,2,1,1], "SE"=SD_log_area_Z_tmm[,2,1,2])
#       #png( file=FileName_Area, width=4, height=4, res=200, units="in")
#       #  par( mfrow=c(1,1), mar=c(3,3,2,0), mgp=c(1.75,0.25,0), tck=-0.02, oma=c(0,0,0,0))
#       #  plot_lines( x=year_labels, y=SD_log_area_Z_tmm[,2,1,1], ybounds=SD_log_area_Z_tmm[,2,1,1]%o%rep(1,2)+SD_log_area_Z_tmm[,2,1,2]%o%c(-1,1), fn=plot, bounds_type="shading", col_bounds=rgb(1,0,0,0.2), col="red", lwd=2, xlab="Year", ylab="ln(km^2)", type="l", main="Kernel approximation to area occupied")
#       #dev.off()
#       
#       # Return stuff
#       Return = c(Return, list("SD_mean_Z_ctm"=SD_mean_Z_ctm, "COG_Table"=COG_Table))
#     }
#     
#     #
#     # Only run if necessary outputs are available
#     if( !any(c("effective_area_tl","effective_area_ctl","effective_area_cyl") %in% names(Report)) ){
#       message( "To plot effective area occupied, please re-run with Options['Calculate_effective_area']=1" )
#     }else{
#       message( "Plotting effective area occupied..." )
#       
#       # Extract estimates
#       SD = TMB::summary.sdreport(Sdreport)
#       SD_effective_area_ctl = SD_log_effective_area_ctl = array( NA, dim=c(unlist(TmbData[c('n_c','n_t','n_l')]),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) )
#       # Effective area
#       if( use_biascorr==TRUE && "unbiased"%in%names(Sdreport) ){
#         SD_effective_area_ctl[] = SD[which(rownames(SD)==EffectiveName),c('Est. (bias.correct)','Std. Error')]
#       }
#       if( !any(is.na(SD_effective_area_ctl)) ){
#         message("Using bias-corrected estimates for effective area occupied (natural scale)...")
#       }else{
#         message("Not using bias-corrected estimates for effective area occupied (natural scale)...")
#         SD_effective_area_ctl[] = SD[which(rownames(SD)==EffectiveName),c('Estimate','Std. Error')]
#       }
#       # Log-Effective area
#       if( use_biascorr==TRUE && "unbiased"%in%names(Sdreport) ){
#         SD_log_effective_area_ctl[] = SD[which(rownames(SD)==paste0("log_",EffectiveName)),c('Est. (bias.correct)','Std. Error')]
#       }
#       if( !any(is.na(SD_log_effective_area_ctl)) ){
#         message("Using bias-corrected estimates for effective area occupied (log scale)...")
#       }else{
#         message("Not using bias-corrected estimates for effective area occupied (log scale)...")
#         SD_log_effective_area_ctl[] = SD[which(rownames(SD)==paste0("log_",EffectiveName)),c('Estimate','Std. Error')]
#       }
#       
#       # Plot area
#       # plot_index( Index_ctl = exp(abind::adrop(SD_log_effective_area_ctl[,,,'Estimate',drop=FALSE], drop = 4)),
#       #             sd_Index_ctl = abind::adrop(SD_log_effective_area_ctl[,,,'Std. Error',drop=FALSE], drop = 4),
#       #             year_labels = year_labels,
#       #             years_to_plot = years_to_plot,
#       #             strata_names = strata_names,
#       #             category_names = category_names,
#       #             DirName = "",
#       #             PlotName = FileName_EffArea,
#       #             scale = "log",
#       #             interval_width = interval_width,
#       #             xlab = "Year",
#       #             ylab = "Effective area occupied [km^2]",
#       #             Yrange = c(NA,NA),
#       #             width = ceiling(TmbData$n_c/ceiling(sqrt(TmbData$n_c)))*4,
#       #             #plot_args = list(log="y"),
#       #             height = ceiling(sqrt(TmbData$n_c))*4 )
#       
#       plot_index_SN( #Index_ctl = (abind::adrop(SD_effective_area_ctl[,,,'Estimate',drop=FALSE], drop = 4))/total_river_length*100,
#         #sd_Index_ctl = (abind::adrop(SD_effective_area_ctl[,,,'Std. Error',drop=FALSE], drop = 4)/total_river_length)*100,
#         Index_ctl=array((SD_effective_area_ctl[,,,'Estimate']/total_river_length)*100,dim(SD_effective_area_ctl)[1:3]),
#         sd_Index_ctl=array((SD_effective_area_ctl[,,,'Std. Error']/total_river_length)*100,dim(SD_effective_area_ctl)[1:3]),
#         year_labels = as.character(year_labels),
#         years_to_plot = years_to_plot,
#         strata_names = strata_names,
#         category_names = category_names,
#         DirName = "",
#         PlotName = FileName_EffArea,
#         #scale = "log",
#         scale = "uniform",
#         interval_width = interval_width,
#         xlab = "Year",
#         ylab = "% River length occupied",
#         Yrange = c(NA,NA),
#         width = ceiling(TmbData$n_c/ceiling(sqrt(TmbData$n_c)))*4,
#         #plot_args = list(log="y"),
#         height = ceiling(sqrt(TmbData$n_c))*4 )
#       
#       
#       
#       #png( file=FileName_EffArea, width=ceiling(TmbData$n_c/ceiling(sqrt(TmbData$n_c)))*2.5, height=ceiling(sqrt(TmbData$n_c))*2.5, res=200, units="in")
#       #  par( mfrow=c(1,1), mar=c(2,2,1,0), mgp=c(1.75,0.25,0), tck=-0.02, oma=c(1,1,1,0), mfrow=c(ceiling(sqrt(TmbData$n_c)),ceiling(TmbData$n_c/ceiling(sqrt(TmbData$n_c)))))
#       #  for( cI in 1:TmbData$n_c ){
#       #    Ybounds = SD_log_effective_area_ctl[cI,,1,1]%o%rep(interval_width,2) + SD_log_effective_area_ctl[cI,,1,2]%o%c(-interval_width,interval_width)
#       #    plot_lines( x=year_labels, y=SD_log_effective_area_ctl[cI,,1,1], ybounds=Ybounds, ylim=range(Ybounds), fn=plot, bounds_type="shading", col_bounds=rgb(1,0,0,0.2), col="red", lwd=2, xlab="", ylab="", type="l", main=category_names[cI])
#       #  }
#       #  mtext( side=1:3, text=c("Year","ln(km^2)","Effective area occupied"), outer=TRUE, line=c(0,0,0) )
#       #dev.off()
#       
#       # Write to file
#       EffectiveArea_Table = NULL
#       for( cI in 1:TmbData$n_c ){
#         Tmp = cbind("Year"=year_labels, "EffectiveArea"=SD_log_effective_area_ctl[cI,,1,1], "SE"=SD_log_effective_area_ctl[cI,,1,2])
#         if( TmbData$n_c>1 ) Tmp = cbind( "Category"=category_names[cI], Tmp)
#         EffectiveArea_Table = rbind(EffectiveArea_Table, Tmp)
#       }
#       
#       # Return stuff
#       Return = c(Return, list("SD_effective_area_ctl"=SD_effective_area_ctl, "SD_log_effective_area_ctl"=SD_log_effective_area_ctl, "EffectiveArea_Table"=EffectiveArea_Table))
#     }
#     
#     # Return list of stuff
#     return( invisible(Return) )
#   }



# plot_index_SN <-
#   function( Index_ctl,
#             sd_Index_ctl = array(0,dim(Index_ctl)),
#             year_labels = NULL,
#             years_to_plot = NULL,
#             strata_names = NULL,
#             category_names = NULL,
#             scale = "uniform",
#             plot_legend = NULL,
#             DirName = paste0(getwd(),"/"),
#             PlotName = "Index.png",
#             interval_width = 1,
#             width = NULL,
#             height = NULL,
#             xlab = "Year",
#             ylab = "Index",
#             bounds_type = "whiskers",
#             col = NULL,
#             col_bounds = NULL,
#             Yrange = c(0,NA),
#             type = "b",
#             plot_lines_args = list(),
#             plot_args = list(),
#             SampleSize_ctz = NULL,
#             Y2range = c(0,NA),
#             y2lab = "",
#             ... ){
#     
#     # Change inputs
#     if( length(dim(Index_ctl))==length(dim(sd_Index_ctl)) ){
#       if( length(dim(Index_ctl))==2 ){
#         Index_ctl = Index_ctl %o% rep(1,1)
#         sd_Index_ctl = sd_Index_ctl %o% rep(1,1)
#       }
#     }else{
#       stop("Mismatch in dimensions for `Index_ctl` and `sd_Index_ctl` in `plot_index`")
#     }
#     n_categories = dim(Index_ctl)[1]
#     n_years = dim(Index_ctl)[2]
#     n_strata = dim(Index_ctl)[3]
#     mfrow = c( ceiling(sqrt(n_categories)), ceiling(n_categories/ceiling(sqrt(n_categories))) )
#     if( !is.null(SampleSize_ctz) ){
#       if( !all( dim(SampleSize_ctz)[1:2] == dim(Index_ctl)[1:2] ) ){
#         stop("Check input `SampleSize_ctz`")
#       }
#     }
#     
#     if(any(is.na(as.numeric(year_labels)))){
#       x_Years = 1:length(year_labels)
#     }else{
#       x_Years = as.numeric(year_labels)
#     }
#     #if( all(is.numeric(Year_Set)) ){
#     #  year_names = Year_Set
#     #}else{
#     #  year_names = Year_Set
#     #  Year_Set = 1:length(Year_Set)
#     #}
#     
#     Pretty = function(vec){
#       Return = pretty(vec)
#       Return = Return[which(Return %in% vec)]
#       return(Return)
#     }
#     
#     # Defaults
#     if( is.null(col)) col = rainbow(n_strata)
#     if( is.null(col_bounds)) col_bounds = rainbow(n_strata)
#     if( is.null(width)) width = mfrow[2] * 3
#     if( is.null(height)) height = mfrow[1] * 3
#     
#     # Fill in missing
#     if( is.null(year_labels) ) year_labels = 1:n_years
#     if( is.null(years_to_plot) ) years_to_plot = 1:n_years
#     if( is.null(strata_names) ) strata_names = 1:n_strata
#     if( is.null(category_names) ) category_names = 1:n_categories
#     if( is.null(plot_legend)) plot_legend = ifelse(n_strata>1, TRUE, FALSE)
#     
#     # Plot
#     Par = combine_lists( default=list(mar=c(2,2,1,0),mgp=c(2,0.5,0),tck=-0.02,yaxs="i",oma=c(2,2,0,0),mfrow=mfrow), input=list(...) )
#     if(!is.na(PlotName)){
#       png( file=paste0(DirName,PlotName), width=width, height=height, res=200, units="in")  # paste0(DirName,ifelse(DirName=="","","/"),PlotName)
#       on.exit( dev.off() )
#     }
#     par( Par )
#     for( z1 in 1:n_categories ){
#       # Calculate y-axis limits
#       if(scale=="uniform") Ylim = range(Index_ctl[z1,years_to_plot,]%o%c(1,1) + sd_Index_ctl[z1,years_to_plot,]%o%c(-interval_width,interval_width)*1.05, na.rm=TRUE)
#       if(scale=="log") Ylim = range(Index_ctl[z1,years_to_plot,]%o%c(1,1) * exp(sd_Index_ctl[z1,years_to_plot,]%o%c(-interval_width,interval_width)*1.05), na.rm=TRUE)
#       Ylim = ifelse( is.na(Yrange), Ylim, Yrange )
#       Xlim = range(x_Years[years_to_plot]) + c(-1,1) * diff(range(x_Years[years_to_plot]))/20
#       # Plot stuff
#       plot_inputs = combine_lists( default=list(1, type="n", xlim=Xlim, ylim=Ylim, xlab="", ylab="", main=ifelse(n_categories>1,category_names[z1],""), xaxt="n"),
#                                    input=plot_args )
#       do.call( what=plot, args=plot_inputs )
#       for(z3 in 1:n_strata){
#         if(scale=="uniform") ybounds = Index_ctl[z1,years_to_plot,z3]%o%c(1,1) + sd_Index_ctl[z1,years_to_plot,z3]%o%c(-interval_width,interval_width)
#         if(scale=="log") ybounds = Index_ctl[z1,years_to_plot,z3]%o%c(1,1) * exp(sd_Index_ctl[z1,years_to_plot,z3]%o%c(-interval_width,interval_width))
#         if( n_strata==1 ) x_offset = 0
#         if( n_strata>=2 ) x_offset = seq(-0.1, 0.1, length=n_strata)[z3]
#         plot_lines_defaults = list( y=Index_ctl[z1,years_to_plot,z3], x=x_Years[years_to_plot]+x_offset, ybounds=ybounds, type=type, col=col[z3], col_bounds=col_bounds[z3],
#                                     ylim=Ylim, bounds_type=bounds_type )
#         plot_lines_inputs = combine_lists( default=plot_lines_defaults, input=plot_lines_args )
#         do.call( what=plot_lines, args=plot_lines_inputs )
#       }
#       # Plot lines for sample size
#       if( !is.null(SampleSize_ctz) ){
#         Y2lim = c(1, 1.2) * range(SampleSize_ctz[z1,years_to_plot,], na.rm=TRUE)
#         Y2lim = ifelse( is.na(Y2range), Y2lim, Y2range )
#         Labels = pretty(Y2lim)
#         At = Labels / diff(range(Y2lim,na.rm=TRUE)) * diff(Ylim) + Ylim[1]
#         axis( side=4, at=At, labels=Labels )
#         for( z3 in 1:dim(SampleSize_ctz)[3] ){
#           Y = SampleSize_ctz[z1,years_to_plot,z3] / diff(range(Y2lim,na.rm=TRUE)) * diff(Ylim) + Ylim[1]
#           lines( x=x_Years, y=Y, col=col[z3], lwd=3, lty="dotted" )
#           #points( x=year_labels, y=Y, col=col[z3], cex=1.5 )
#         }
#       }
#       if(plot_legend==TRUE){
#         legend( "top", bty="n", fill=rainbow(n_strata), legend=as.character(strata_names), ncol=2 )
#       }
#       axis( 1, at=Pretty(x_Years[years_to_plot]), labels=year_labels[match(Pretty(x_Years[years_to_plot]),x_Years)] )
#     }
#     mtext( side=c(1,2,4), text=c(xlab,ylab,y2lab), outer=TRUE, line=c(0,0) )
#     
#     return(invisible(plot_lines_inputs))
#   }





