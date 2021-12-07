#######     Elipse ENM  for Thalpomena  by Marlon Cobos     ################
############################################################################

setwd("E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap")

citation("tidyverse")

# Installing and loading packages
if(!require(devtools)){
  install.packages("devtools")
}
if(!require(ellipse4nm)){
  devtools::install_github("marlonecobos/ellipsenm")
}
library(ellipsenm)

help(ellipsenm)


#####################################################################
#####################################################################

          ALGERIANA

#####################################################################
#####################################################################

# reading data
occurrences <- read.csv("E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\Thalpi1.csv")
colnames(occurrences)

occurrences1 <- occurrences[occurrences$Species == 1,]
occurrences2 <- occurrences[occurrences$Species == 2,]
occurrences3 <- occurrences[occurrences$Species == 3,]
occurrences4 <- occurrences[occurrences$Species == 4,]



# raster layers of environmental data (this ones are masked to the accessible area)
# users must prepare their layers accordingly if using other data
vars <- raster::stack(list.files(path = "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\Variables_crop\\", pattern = ".asc", full.names = TRUE))

alg<-raster::stack(list.files(path = "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\Variables_algeriana\\", pattern = ".asc", full.names = TRUE))
# preparing training and testing data
data_split1 <- split_data(occurrences1, method = "random", longitude = "Longitud", 
                         latitude = "Latitud", train_proportion = 0.75,
                         save = TRUE, name = "algeriana_occ")

# sets of variables (example)
sets <- list(set_1 = c("bio_1", "bio_4", "bio_10", "bio_14"),
             set_2 = c("bio_1", "bio_14", "bio_10")) # change as needed

variable_sets <- prepare_sets(vars, sets)

# methods to create ellipsoids
methods <- c("covmat", "mve1")

# model calibration process
calib <- ellipsoid_calibration(data_split1, species = "Species", longitude = "Longitud", 
                               latitude = "Latitud", variables = variable_sets,
                               methods = methods, level = 99, selection_criteria = "S_OR_P",
                               error = 5, iterations = 500, percentage = 50,
                               output_directory = "calibration_results")

class(calib)
# check your directory, folder "calibration_results"

############ ellipsoid model  #####################


ell_model_algeriana <- ellipsoid_model(data = occurrences1, species = "Species",
                              longitude = "Longitud", latitude = "Latitud",
                              raster_layers = alg, method = "covmat", level = 99,
                              replicates = 5, prediction = "suitability",
                              return_numeric = TRUE, format = "GTiff",
                              overwrite = FALSE, output_directory = "ellipsenm_model_algeriana")

class(ell_model1)
# check your directory, folder "ellipsenm_model1"
##########################################################################
#####         ALGERIANA                       ########################
########################################################

data_split1 <- split_data(occurrences1, method = "random", longitude = "Longitud", 
                          latitude = "Latitud", train_proportion = 0.75,
                          save = TRUE, name = "algeriana_occ")

sets <- list(set_1 = c("bio_11", "bio_14", "bio_10", "bio_2"),
             set_2 = c("bio_5", "bio_3", "bio_10"),
             set_3 = c("bio_11", "bio_14", "bio_10"))

variable_sets <- prepare_sets(vars, sets)

methods <- c("covmat", "mve1")

calib <- ellipsoid_calibration(data_split1, species = "Species", longitude = "Longitud", 
                              latitude = "Latitud", variables = variable_sets,
                              methods = methods, level = 95, selection_criteria = "S_OR_P",
                              error = 5, iterations = 500, percentage = 75, overwrite = TRUE,
                              output_directory = "calibration_results_algeriana")

class(calib)

alg<-raster::stack(list.files(path = "D:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\Variables_algeriana\\", pattern = ".asc", full.names = TRUE))

ell_model_algeriana <- ellipsoid_model(data = occurrences1, species = "Species",
                                       longitude = "Longitud", latitude = "Latitud",
                                       raster_layers = alg, method = "covmat", level = 99,
                                       replicates = 100, prediction = "suitability",
                                       return_numeric = TRUE, format = "GTiff",
                                       overwrite = TRUE, output_directory = "ellipsenm_model_algeriana")


##########################################################################
#####         AZUREIPENNIS                        ########################
########################################################
data_split2 <- split_data(occurrences2, method = "random", longitude = "Longitud", 
                          latitude = "Latitud", train_proportion = 0.75,
                          save = TRUE, name = "azureipennis_occ")

sets <- list(set_1 = c("bio_13", "bio_14", "bio_5", "bio_6"),
             set_2 = c("bio_1", "bio_2", "bio_7"),
             set_3 = c("bio_13", "bio_16", "bio_10", "bio_5"))

variable_sets <- prepare_sets(vars, sets)

methods <- c("covmat", "mve1")

calib <- ellipsoid_calibration(data_split2, species = "Species", longitude = "Longitud", 
                               latitude = "Latitud", variables = variable_sets,
                               methods = methods, level = 95, selection_criteria = "S_OR_P",
                               error = 5, iterations = 500, percentage = 75, overwrite = TRUE,
                               output_directory = "calibration_results_azureipennis")

class(calib)

azu<-raster::stack(list.files(path = "D:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\Variables_azureipennis\\", pattern = ".asc", full.names = TRUE))

ell_model_azureipennis <- ellipsoid_model(data = occurrences2, species = "Species",
                                       longitude = "Longitud", latitude = "Latitud",
                                       raster_layers = azu, method = "covmat", level = 99,
                                       replicates = 100, prediction = "suitability",
                                       return_numeric = TRUE, format = "GTiff",
                                       overwrite = TRUE, output_directory = "ellipsenm_model_azureipennis")

##########################################################################
#####         COERULESCENS                       ########################
########################################################

data_split3 <- split_data(occurrences3, method = "random", longitude = "Longitud", 
                          latitude = "Latitud", train_proportion = 0.75,
                          save = TRUE, name = "coerulescens_occ")

sets <- list(set_1 = c("bio_3", "bio_6", "bio_12", "bio_14"),
             set_2 = c("bio_15", "bio_3", "bio_17", "bio_10"),
             set_3 = c("bio_3", "bio_6", "bio_12"))

variable_sets <- prepare_sets(vars, sets)

methods <- c("covmat", "mve1")

calib <- ellipsoid_calibration(data_split2, species = "Species", longitude = "Longitud", 
                               latitude = "Latitud", variables = variable_sets,
                               methods = methods, level = 95, selection_criteria = "S_OR_P",
                               error = 5, iterations = 500, percentage = 75, overwrite = TRUE,
                               output_directory = "calibration_results_coerulescens")

class(calib)
coe<-raster::stack(list.files(path = "D:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\Variables_coerulescens\\", pattern = ".asc", full.names = TRUE))

ell_model_coerulescens <- ellipsoid_model(data = occurrences3, species = "Species",
                                       longitude = "Longitud", latitude = "Latitud",
                                       raster_layers = coe, method = "mve1", level = 99,
                                       replicates = 100, prediction = "suitability",
                                       return_numeric = TRUE, format = "GTiff",
                                       overwrite = TRUE, output_directory = "ellipsenm_model_coerulescens")


##########################################################################
#####         VIRIDIPENNIS                       ########################
########################################################

data_split4 <- split_data(occurrences4, method = "random", longitude = "Longitud", 
                          latitude = "Latitud", train_proportion = 0.75,
                          save = TRUE, name = "viridipennis_occ")

sets <- list(set_1 = c("bio_5", "bio_15", "bio_17", "bio_7"),
             set_2 = c("bio_15", "bio_5", "bio_7", "bio_14"),
             set_3 = c("bio_2", "bio_11", "bio_15", "bio_14"),
             set_4 = c("bio_2", "bio_11", "bio_14", "bio_7"))

variable_sets <- prepare_sets(vars, sets)

methods <- c("covmat", "mve1")

calib <- ellipsoid_calibration(data_split4, species = "Species", longitude = "Longitud", 
                               latitude = "Latitud", variables = variable_sets,
                               methods = methods, level = 95, selection_criteria = "S_OR_P",
                               error = 5, iterations = 500, percentage = 75, overwrite = TRUE,
                               output_directory = "calibration_results_viridipennis")

class(calib)

vir<-raster::stack(list.files(path = "D:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\Variables_viridipennis\\", pattern = ".asc", full.names = TRUE))

ell_model_viridipennis <- ellipsoid_model(data = occurrences4, species = "Species",
                                          longitude = "Longitud", latitude = "Latitud",
                                          raster_layers = vir, method = "mve1", level = 99,
                                          replicates = 100, prediction = "suitability",
                                          return_numeric = TRUE, format = "GTiff",
                                          overwrite = TRUE, output_directory = "ellipsenm_model_viridipennis")







#################################################################
################# overlap ################################
###############################################################
# to be compareble, same layers for ll predictions were used
#variable correlation was doe based on the whole dataset in maxent 


# preparing data
vext <- raster::extent(alg)
ext1 <- raster::extent(vext[1], (mean(vext[1:2]) + 0.2), vext[3:4])
ext2 <- raster::extent((mean(vext[1:2]) + 0.2), vext[2], vext[3:4])

# croping variables and splitting occurrences
vars1 <- raster::stack(raster::crop(alg, ext1))
vars2 <- raster::stack(raster::crop(alg, ext2))

occurrences1 <- occurrences[occurrences$Species == 1,]
occurrences2 <- occurrences[occurrences$Species == 2,]
occurrences3 <- occurrences[occurrences$Species == 3,]
occurrences4 <- occurrences[occurrences$Species == 4,]

# preparing overlap objects to perform analyses
niche1 <- overlap_object(occurrences1, species =  "Species", longitude = "Longitud", 
                         latitude = "Latitud", method = "covmat", level = 95, 
                         variables = vars1)

niche2 <- overlap_object(occurrences2, species =  "Species", longitude = "Longitud", 
                         latitude = "Latitud", method = "covmat", level = 95, 
                         variables = vars1)

niche3 <- overlap_object(occurrences3, species =  "Species", longitude = "Longitud", 
                         latitude = "Latitud", method = "covmat", level = 95, 
                         variables = vars1)
niche4 <- overlap_object(occurrences4, species =  "Species", longitude = "Longitud", 
                         latitude = "Latitud", method = "covmat", level = 95, 
                         variables = vars1)
# niche overlap analysis
overlap <- ellipsoid_overlap(niche1, niche3, niche4)

# niche overlap analysis with test of significance
overlap_st <- ellipsoid_overlap(niche1, niche2, overlap_type = "all",
                                significance_test = TRUE, replicates = 100)

write.csv(overlap_st, file = "overlap_st.csv", row.names = FALSE)

plot_overlap(overlap, background = TRUE,  proportion = 1, background_type = "back_union")
help(ellipsoid_overlap)





install.packages(ecospat)




##############################################################################
##############################################################################
##############################################################################

# Prep for Nicheoverlap in EllipsENM

if(!require(devtools)){
  install.packages("devtools")
}
if(!require(kuenm)){
  devtools::install_github("https://github.com/marlonecobos/kuenm")
}

library(kuenm)

kuenm_rpca

setwd("E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\PCA\\")
vars <- raster::stack(list.files(path = "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\Variables_crop\\", pattern = ".asc", full.names = TRUE))


sets <- list(set_1 = c("bio_1", "bio_2", "bio_3", "bio_4", "bio_5", "bio_6", "bio_7", "bio_10", "bio_11", "bio_12", "bio_13", "bio_14", "bio_15", "bio_16", "bio_17"),
             set_2 = c("bio_1", "bio_2", "bio_3", "bio_4", "bio_5", "bio_6", "bio_7", "bio_10", "bio_11"),
             set_3 = c("bio_12", "bio_13", "bio_14", "bio_15", "bio_16", "bio_17"))


###full set of variables

set_1 <- raster::stack(list.files(path = "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\PCA\\set_1\\", pattern = ".asc", full.names = TRUE))

npcs <- 3

rpca_set_1 <- kuenm_rpca(variables = set_1, var.scale = TRUE, write.result = TRUE, n.pcs = npcs)



###temperature variables only
set_2 <- raster::stack(list.files(path = "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\PCA\\set_2\\", pattern = ".asc", full.names = TRUE))

npcs <- 3

rpca_set_2 <- kuenm_rpca(variables = set_2, var.scale = TRUE, write.result = TRUE, n.pcs = npcs)



###precipitation variables only
set_3 <- raster::stack(list.files(path = "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\PCA\\set_3\\", pattern = ".asc", full.names = TRUE))

npcs <- 3

rpca_set_3 <- kuenm_rpca(variables = set_3, var.scale = TRUE, write.result = TRUE, n.pcs = npcs)

###crop to PCAs to M area 

library(raster)
setwd("E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\PCA\\PCA_results_set_3\\Initial") # this is the directory that has the .bil files
envs <- stack(sapply(list.files(pattern='tif$'), raster)) # this makes a list of all the .bil files, then calls raster() on them, then stacks them together
envs # when you look at your new stack, you'll see all the rasters and some stats on them
envs[[1]] # this will give you just the first raster
writeRaster(envs, 'E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\PCA\\PCA_results_set_3\\Initial\\pc.asc', bylayer=TRUE) # this writes all the rasters in the stack as 'bio_*' where * is the order number in the sta
crs(envs) 


###algeriana crop to M area
setwd("E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\algeriana")

spvec <- dir()
exclude <- c("Variables_set_1", "G_Variables", "Variables_set_2","Variables_set_3")
spvector <- spvec[!spvec %in% exclude]

scenarios <- c("current")

var_list <- list(stack(list.files("Variables_set_3", pattern = ".asc", full.names = TRUE)))


var_names <- list(list.files("Variables_set_3", pattern = ".asc", full.names = FALSE))


sets <- c("Set_3")
library('rgeos')
for (i in 1:length(spvector)) {
  # reading species data
  sp <- read.csv(paste(spvector[i], "/", paste(spvector[i], ".csv", sep = ""), sep = ""))
  head(sp)
  sp_nodup <- unique(sp)
  
  # create buffer
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  occ_sp <- sp::SpatialPointsDataFrame(coords = sp_nodup[, 2:3], data = sp_nodup,
                                       proj4string = WGS84)
  
  centroid <- rgeos::gCentroid(occ_sp, byid = FALSE)
  
  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))
  
  occ_pr <- sp::spTransform(occ_sp, AEQD)
  
  buff_area <- rgeos::gBuffer(occ_pr, width = 15000) #25km buffer
  buff_area <- raster::disaggregate(buff_area)
  buff_areap <- sp::spTransform(buff_area, WGS84)
  plot(buff_areap)
  
  
  # folder for Ms
  dir.create(paste(spvector[i], "M_variables", sep = "/"))
  
  for (j in 1:length(var_list)) {
    set <- var_list[[j]]
    
    # folder for sets
    infolder <- paste(spvector[i], "M_variables", sets[j], sep = "/")
    dir.create(infolder)
    
    # mask variables
    masked <- mask(crop(set, buff_areap), buff_areap)
    
    # write variables
    for (k in 1:length(unstack(masked))) {
      writeRaster(masked[[k]], filename = paste(infolder, var_names[[j]][k], sep = "/"),
                  format = "ascii", overwrite=TRUE)
    }
    cat("   Set", j, "of", length(var_list), "finished\n")
  }
  
  cat("Species", i, "of", length(spvector), "finished\n")
}


###coerulescens crop to M area
setwd("E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\coerulescens")

spvec <- dir()
exclude <- c("Variables_set_1", "G_Variables", "Variables_set_2","Variables_set_3")
spvector <- spvec[!spvec %in% exclude]

scenarios <- c("current")

var_list <- list(stack(list.files("Variables_set_1", pattern = ".asc", full.names = TRUE)))


var_names <- list(list.files("Variables_set_1", pattern = ".asc", full.names = FALSE))


sets <- c("Set_1")
#library('rgeos')
for (i in 1:length(spvector)) {
  # reading species data
  sp <- read.csv(paste(spvector[i], "/", paste(spvector[i], ".csv", sep = ""), sep = ""))
  head(sp)
  sp_nodup <- unique(sp)
  
  # create buffer
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  occ_sp <- sp::SpatialPointsDataFrame(coords = sp_nodup[, 2:3], data = sp_nodup,
                                       proj4string = WGS84)
  
  centroid <- rgeos::gCentroid(occ_sp, byid = FALSE)
  
  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))
  
  occ_pr <- sp::spTransform(occ_sp, AEQD)
  
  buff_area <- rgeos::gBuffer(occ_pr, width = 15000) #25km buffer
  buff_area <- raster::disaggregate(buff_area)
  buff_areap <- sp::spTransform(buff_area, WGS84)
  plot(buff_areap)
  
  
  # folder for Ms
  dir.create(paste(spvector[i], "M_variables", sep = "/"))
  
  for (j in 1:length(var_list)) {
    set <- var_list[[j]]
    
    # folder for sets
    infolder <- paste(spvector[i], "M_variables", sets[j], sep = "/")
    dir.create(infolder)
    
    # mask variables
    masked <- mask(crop(set, buff_areap), buff_areap)
    
    # write variables
    for (k in 1:length(unstack(masked))) {
      writeRaster(masked[[k]], filename = paste(infolder, var_names[[j]][k], sep = "/"),
                  format = "ascii", overwrite=TRUE)
    }
    cat("   Set", j, "of", length(var_list), "finished\n")
  }
  
  cat("Species", i, "of", length(spvector), "finished\n")
}




###azureipennis crop to M area
setwd("E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\azureipennis")

spvec <- dir()
exclude <- c("Variables_set_1", "G_Variables", "Variables_set_2","Variables_set_3")
spvector <- spvec[!spvec %in% exclude]

scenarios <- c("current")

var_list <- list(stack(list.files("Variables_set_3", pattern = ".asc", full.names = TRUE)))


var_names <- list(list.files("Variables_set_3", pattern = ".asc", full.names = FALSE))


sets <- c("Set_3")
#library('rgeos')
for (i in 1:length(spvector)) {
  # reading species data
  sp <- read.csv(paste(spvector[i], "/", paste(spvector[i], ".csv", sep = ""), sep = ""))
  head(sp)
  sp_nodup <- unique(sp)
  
  # create buffer
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  occ_sp <- sp::SpatialPointsDataFrame(coords = sp_nodup[, 2:3], data = sp_nodup,
                                       proj4string = WGS84)
  
  centroid <- rgeos::gCentroid(occ_sp, byid = FALSE)
  
  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))
  
  occ_pr <- sp::spTransform(occ_sp, AEQD)
  
  buff_area <- rgeos::gBuffer(occ_pr, width = 15000) #25km buffer
  buff_area <- raster::disaggregate(buff_area)
  buff_areap <- sp::spTransform(buff_area, WGS84)
  plot(buff_areap)
  
  
  # folder for Ms
  dir.create(paste(spvector[i], "M_variables", sep = "/"))
  
  for (j in 1:length(var_list)) {
    set <- var_list[[j]]
    
    # folder for sets
    infolder <- paste(spvector[i], "M_variables", sets[j], sep = "/")
    dir.create(infolder)
    
    # mask variables
    masked <- mask(crop(set, buff_areap), buff_areap)
    
    # write variables
    for (k in 1:length(unstack(masked))) {
      writeRaster(masked[[k]], filename = paste(infolder, var_names[[j]][k], sep = "/"),
                  format = "ascii", overwrite=TRUE)
    }
    cat("   Set", j, "of", length(var_list), "finished\n")
  }
  
  cat("Species", i, "of", length(spvector), "finished\n")
}



###viridipennis crop to M area
setwd("E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\viridipennis\\")

spvec <- dir()
exclude <- c("Variables_set_1", "Variables_set_2","Variables_set_3")
spvector <- spvec[!spvec %in% exclude]

scenarios <- c("current")

var_list <- list(stack(list.files("Variables_set_1", pattern = ".asc", full.names = TRUE)))


var_names <- list(list.files("Variables_set_1", pattern = ".asc", full.names = FALSE))


sets <- c("Set_1")
#library('rgeos')
for (i in 1:length(spvector)) {
  # reading species data
  sp <- read.csv(paste(spvector[i], "/", paste(spvector[i], ".csv", sep = ""), sep = ""))
  head(sp)
  sp_nodup <- unique(sp)
  
  # create buffer
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  occ_sp <- sp::SpatialPointsDataFrame(coords = sp_nodup[, 2:3], data = sp_nodup,
                                       proj4string = WGS84)
  
  centroid <- rgeos::gCentroid(occ_sp, byid = FALSE)
  
  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))
  
  occ_pr <- sp::spTransform(occ_sp, AEQD)
  
  buff_area <- rgeos::gBuffer(occ_pr, width = 15000) #25km buffer
  buff_area <- raster::disaggregate(buff_area)
  buff_areap <- sp::spTransform(buff_area, WGS84)
  plot(buff_areap)
  
  
  # folder for Ms
  dir.create(paste(spvector[i], "M_variables", sep = "/"))
  
  for (j in 1:length(var_list)) {
    set <- var_list[[j]]
    
    # folder for sets
    infolder <- paste(spvector[i], "M_variables", sets[j], sep = "/")
    dir.create(infolder)
    
    # mask variables
    masked <- mask(crop(set, buff_areap), buff_areap)
    
    # write variables
    for (k in 1:length(unstack(masked))) {
      writeRaster(masked[[k]], filename = paste(infolder, var_names[[j]][k], sep = "/"),
                  format = "ascii", overwrite=TRUE)
    }
    cat("   Set", j, "of", length(var_list), "finished\n")
  }
  
  cat("Species", i, "of", length(spvector), "finished\n")
}

###why buffer of 15km

################################################################################
################################################################################
################################################################################

  
  #Create stacks for each species you just cropped
  # Calibration areas
  
  path_algeriana_set_1 <- "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\algeriana\\algeriana\\M_variables\\Set_1"
vars_list <-list.files(path = path_algeriana_set_1, pattern = ".asc", full.names = TRUE) #variables 
vars_algeriana_set_1 <- stack(vars_list) #create a stack
crs(vars_algeriana_set_1) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

path_algeriana_set_2 <- "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\algeriana\\algeriana\\M_variables\\Set_2"
vars_list2 <-list.files(path = path_algeriana_set_2, pattern = ".asc", full.names = TRUE) #variables 
vars_algeriana_set_2 <- stack(vars_list2) #create a stack
crs(vars_algeriana_set_2) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

path_algeriana_set_3 <- "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\algeriana\\algeriana\\M_variables\\Set_3"
vars_list3 <-list.files(path = path_algeriana_set_3, pattern = ".asc", full.names = TRUE) #variables 
vars_algeriana_set_3 <- stack(vars_list3) #create a stack
crs(vars_algeriana_set_3) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '





path_azureipennis_set_1 <- "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\azureipennis\\azureipennis\\M_variables\\Set_1"
vars_list4<-list.files(path = path_azureipennis_set_1, pattern = ".asc", full.names = TRUE) #variables 
vars_azureipennis_set_1 <- stack(vars_list4) #create a stack
crs(vars_azureipennis_set_1) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

path_azureipennis_set_2 <- "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\azureipennis\\azureipennis\\M_variables\\Set_2"
vars_list5 <-list.files(path = path_azureipennis_set_2, pattern = ".asc", full.names = TRUE) #variables 
vars_azureipennis_set_2 <- stack(vars_list5) #create a stack
crs(vars_azureipennis_set_2) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

path_azureipennis_set_3 <- "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\azureipennis\\azureipennis\\M_variables\\Set_3"
vars_list6 <-list.files(path = path_azureipennis_set_3, pattern = ".asc", full.names = TRUE) #variables 
vars_azureipennis_set_3 <- stack(vars_list6) #create a stack
crs(vars_azureipennis_set_3) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '





path_coerulescens_set_1 <- "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\coerulescens\\coerulescens\\M_variables\\Set_1"
vars_list7 <-list.files(path = path_coerulescens_set_1, pattern = ".asc", full.names = TRUE) #variables 
vars_coerulescens_set_1 <- stack(vars_list7) #create a stack
crs(vars_coerulescens_set_1) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

path_coerulescens_set_2 <- "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\coerulescens\\coerulescens\\M_variables\\Set_2"
vars_list8 <-list.files(path = path_coerulescens_set_2, pattern = ".asc", full.names = TRUE) #variables 
vars_coerulescens_set_2 <- stack(vars_list8) #create a stack
crs(vars_coerulescens_set_2) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

path_coerulescens_set_3 <- "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\coerulescens\\coerulescens\\M_variables\\Set_3"
vars_list9 <-list.files(path = path_coerulescens_set_3, pattern = ".asc", full.names = TRUE) #variables 
vars_coerulescens_set_3 <- stack(vars_list9) #create a stack
crs(vars_coerulescens_set_3) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '





path_viridipennis_set_1 <- "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\viridipennis\\viridipennis\\M_variables\\Set_1"
vars_list10 <-list.files(path = path_viridipennis_set_1, pattern = ".asc", full.names = TRUE) #variables 
vars_viridipennis_set_1 <- stack(vars_list10) #create a stack
crs(vars_viridipennis_set_1) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

path_viridipennis_set_2 <- "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\viridipennis\\viridipennis\\M_variables\\Set_2"
vars_list11 <-list.files(path = path_viridipennis_set_2, pattern = ".asc", full.names = TRUE) #variables 
vars_viridipennis_set_2 <- stack(vars_list11) #create a stack
crs(vars_viridipennis_set_2) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

path_viridipennis_set_3 <- "E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\viridipennis\\viridipennis\\M_variables\\Set_3"
vars_list12 <-list.files(path = path_viridipennis_set_3, pattern = ".asc", full.names = TRUE) #variables 
vars_viridipennis_set_3 <- stack(vars_list12) #create a stack
crs(vars_viridipennis_set_3) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '





# preparing overlap objects to perform analyses----------

occurrences <- read.csv("E:\\offene_Manuskripte\\Revision_Thalpomena\\nicheoverlap\\Thalpi1.csv")
colnames(occurrences)

sp1 <- occurrences[occurrences$Species == 1,]
sp2 <- occurrences[occurrences$Species == 2,]
sp3 <- occurrences[occurrences$Species == 3,]
sp4 <- occurrences[occurrences$Species == 4,]



E_algeriana_set_1 <- overlap_object(sp1, species =  "Species", longitude = "Longitud", 
                      latitude = "Latitud", method = "covmat", level = 95, 
                      variables = vars_algeriana_set_1)

E_algeriana_set_2 <- overlap_object(sp1, species =  "Species", longitude = "Longitud", 
                      latitude = "Latitud", method = "covmat", level = 95, 
                      variables = vars_algeriana_set_2)

E_algeriana_set_3 <- overlap_object(sp1, species =  "Species", longitude = "Longitud", 
                      latitude = "Latitud", method = "covmat", level = 95, 
                      variables = vars_algeriana_set_2)




E_azureipennis_set_1 <- overlap_object(sp2, species =  "Species", longitude = "Longitud", 
                      latitude = "Latitud", method = "covmat", level = 95, 
                      variables = vars_azureipennis_set_1)

E_azureipennis_set_2 <- overlap_object(sp2, species =  "Species", longitude = "Longitud", 
                                       latitude = "Latitud", method = "covmat", level = 95, 
                                       variables = vars_azureipennis_set_2)

E_azureipennis_set_3 <- overlap_object(sp2, species =  "Species", longitude = "Longitud", 
                                       latitude = "Latitud", method = "covmat", level = 95, 
                                       variables = vars_azureipennis_set_3)




E_coerulescens_set_1 <- overlap_object(sp3, species =  "Species", longitude = "Longitud", 
                                       latitude = "Latitud", method = "covmat", level = 95, 
                                       variables = vars_coerulescens_set_1)

E_coerulescens_set_2 <- overlap_object(sp3, species =  "Species", longitude = "Longitud", 
                                       latitude = "Latitud", method = "covmat", level = 95, 
                                       variables = vars_coerulescens_set_2)

E_coerulescens_set_3 <- overlap_object(sp3, species =  "Species", longitude = "Longitud", 
                                       latitude = "Latitud", method = "covmat", level = 95, 
                                       variables = vars_coerulescens_set_3)




E_viridipennis_set_1 <- overlap_object(sp4, species =  "Species", longitude = "Longitud", 
                                       latitude = "Latitud", method = "covmat", level = 95, 
                                       variables = vars_viridipennis_set_1)

E_viridipennis_set_2 <- overlap_object(sp4, species =  "Species", longitude = "Longitud", 
                                       latitude = "Latitud", method = "covmat", level = 95, 
                                       variables = vars_viridipennis_set_2)


E_viridipennis_set_3 <- overlap_object(sp4, species =  "Species", longitude = "Longitud", 
                                       latitude = "Latitud", method = "covmat", level = 95, 
                                       variables = vars_viridipennis_set_3)



# niche overlap analysis===========
#Types of overlap  all = performs all types of overlap analyses allowed.
#full = measures overlap of the complete volume of the ellipsoidal niches.
#back_union = measures overlap of ellipsoidal niches considering only the 
#union of the environmental conditions relevant for the two species (backgrounds).
#Null hypothesis = niches are overlapped and if the observed values are as extreme
#or more extreme than the lower limit of the values found for the random-ellipsoids, the null hypothesis is rejected. 

#p value > 0.05 = overlapped
#p value <0.05 = non-overlapped
help(eoverlap_object)
overlap_12_set_1 <- ellipsoid_overlap(E_algeriana_set_1, E_azureipennis_set_1, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                confidence_limit = 0.05)
overlap_13_set_1 <- ellipsoid_overlap(E_algeriana_set_1, E_coerulescens_set_1, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                confidence_limit = 0.05)
overlap_14_set_1 <- ellipsoid_overlap(E_algeriana_set_1, E_viridipennis_set_1, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                confidence_limit = 0.05)

overlap_12_set_2 <- ellipsoid_overlap(E_algeriana_set_2, E_azureipennis_set_2, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)
overlap_13_set_2 <- ellipsoid_overlap(E_algeriana_set_2, E_coerulescens_set_2, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)
overlap_14_set_2 <- ellipsoid_overlap(E_algeriana_set_2, E_viridipennis_set_2, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)


overlap_12_set_3 <- ellipsoid_overlap(E_algeriana_set_3, E_azureipennis_set_3, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)
overlap_13_set_3 <- ellipsoid_overlap(E_algeriana_set_3, E_coerulescens_set_3, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)
overlap_14_set_3 <- ellipsoid_overlap(E_algeriana_set_3, E_viridipennis_set_3, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)

###-----------------------------------------------------------------------------

overlap_23_set_1 <- ellipsoid_overlap(E_azureipennis_set_1, E_coerulescens_set_1, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)
overlap_24_set_1 <- ellipsoid_overlap(E_azureipennis_set_1, E_viridipennis_set_1, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)

overlap_23_set_2 <- ellipsoid_overlap(E_azureipennis_set_2, E_coerulescens_set_2, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)
overlap_24_set_2 <- ellipsoid_overlap(E_azureipennis_set_2, E_viridipennis_set_2, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)


overlap_23_set_3 <- ellipsoid_overlap(E_azureipennis_set_3, E_coerulescens_set_3, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)
overlap_24_set_3 <- ellipsoid_overlap(E_azureipennis_set_3, E_viridipennis_set_3, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)

###-----------------------------------------------------------------------------

overlap_34_set_1 <- ellipsoid_overlap(E_coerulescens_set_1, E_viridipennis_set_1, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)

overlap_34_set_2 <- ellipsoid_overlap(E_coerulescens_set_2, E_viridipennis_set_2, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)


overlap_34_set_3 <- ellipsoid_overlap(E_coerulescens_set_3, E_viridipennis_set_3, n_points = 1000000, significance_test = TRUE, replicates = 100,
                                      confidence_limit = 0.05)


#Overap_M - Niche_1_vs_2------------
hist(overlap_34_set_3@significance_results$union_random$Niche_1_vs_2$overlap, 
     main = "Overlap T. coerulescens & T. viridipennis Set3", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 100))
abline(v = quantile(overlap_34_set_3@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "red", lwd = 2, lty = 2)
abline(v = overlap_34_set_3@union_overlap$overlap[1], col = "lightblue", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("lightblue", "red"), lty = c(1, 2), lwd = 2)
dev.off()



#Save images------------
# plotting ellispodis and background for full overlap
plot_overlap(overlap_34_set_3, background = TRUE, proportion = 0.6, background_type = "full")

# plotting ellispodis and background for overlap based on accessible environments
plot_overlap(overlap_34_set_3, background = TRUE,  proportion = 1, background_type = "back_union")

library(rgl)
rgl.snapshot( "overlap_34_set_3", fmt = "png", top = TRUE )
browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), width=500), sep=""))
rgl.viewpoint(theta = 0, phi = 15, fov = 60, zoom = 0, interactive = TRUE )

