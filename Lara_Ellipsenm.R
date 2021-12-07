#Overlap analysis--=========

#occurrence data 
sp1 <- read.csv('sp1.csv')
sp2 <- read.csv('sp2.csv')
sp3 <- read.csv('sp3.csv')
sp4 <- read.csv('sp4.csv')


# Areas by buffering records (200 km buffer)
sp_M1 <- buffer_area(sp1, longitude = "long", latitude = "lat", 
                       buffer_distance = 25)
sp_M2 <- buffer_area(sp2, longitude = "long", latitude = "lat", 
                     buffer_distance = 25)
sp_M3 <- buffer_area(sp3, longitude = "long", latitude = "lat", 
                     buffer_distance = 25)
sp_M4 <- buffer_area(sp4, longitude = "long", latitude = "lat", 
                     buffer_distance = 25)

#Crop variables----------
#Mask variables you created before of the whole projection area (PCS of temp, prec and all ). 
#Mask them on the xtent of calibration areas of both species you want to compare 

require(raster)
path <- ".path to variabl;es"
variables_list <-list.files(path = path, pattern = ".tif", full.names = TRUE) #variables 
variables <- stack(variables_list) #create a stack

require(rgdal)
var_mask <- mask(crop(variables, sp_M1), sp_M1)

## names for layers
rnames <- paste0("C:\\Users\\mari1\\Dropbox (Senckenberg)\\Vivian\\Bio_cut\\Bio_temp\\", names(variables), ".asc") # users select the format

## saving layers in new folder
sav <- lapply(1:nlayers(var_mask), function(x) {
  writeRaster(var_mask[[x]], filename = rnames[x], format = "ascii") # change format accordingly
})


#Create stacks for each species you just cropped
# Calibration areas

path <- "PATH  WHERE THE CROPPED VARIABLES SET1 - sp1/"
vars_list <-list.files(path = path, pattern = ".asc", full.names = TRUE) #variables 
vars1 <- stack(vars_list) #create a stack
crs(vars1) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

path2 <- "PATH  WHERE THE CROPPED VARIABLES SET1 - sp2/"
vars_list2 <-list.files(path = path2, pattern = ".asc", full.names = TRUE) #variables 
vars2 <- stack(vars_list2) #create a stack
crs(vars2) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

path3 <- "PATH  WHERE THE CROPPED VARIABLES SET1 - sp2/"
vars_list3 <-list.files(path = path2, pattern = ".asc", full.names = TRUE) #variables 
vars3 <- stack(vars_list2) #create a stack
crs(vars3) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

path2 <- "PATH  WHERE THE CROPPED VARIABLES SET1 - sp2/"
vars_list2 <-list.files(path = path2, pattern = ".asc", full.names = TRUE) #variables 
vars2 <- stack(vars_list2) #create a stack
crs(vars2) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '



# preparing overlap objects to perform analyses----------
E_1 <- overlap_object(sp1, species =  "species", longitude = "long", 
                    latitude = "lat", method = "covmat", level = 95, 
                    variables = vars1)

E_2 <- overlap_object(sp2, species =  "species", longitude = "long", 
                    latitude = "lat", method = "covmat", level = 95, 
                    variables = vars2)

E_3 <- overlap_object(sp3, species =  "species", longitude = "long", 
                      latitude = "lat", method = "covmat", level = 95, 
                      variables = vars2)

E_4 <- overlap_object(sp4, species =  "species", longitude = "long", 
                      latitude = "lat", method = "covmat", level = 95, 
                      variables = vars2)

# niche overlap analysis===========
#Types of overlap  all = performs all types of overlap analyses allowed.
#full = measures overlap of the complete volume of the ellipsoidal niches.
#back_union = meausures overlap of ellipsoidal niches considering only the 
#union of the environmental conditions relevant for the two species (backgrounds).
#Null hypothesis = niches are overlaped and if the observed values are as extreme
#or more extreme than the lower limit of the values found for the random-ellipsoids, the null hypothesis is rejected. 

#p value > 0.05 = overlaped
#p value <0.05 = non-overlaped

overlap_12 <- ellipsoid_overlap(E_1, E_2, n_points = 1000000, significance_test = TRUE, replicates = 1000,
                                  confidence_limit = 0.05)
overlap_13 <- ellipsoid_overlap(E_1, E_3, n_points = 1000000, significance_test = TRUE, replicates = 1000,
                                  confidence_limit = 0.05)
overlap_14 <- ellipsoid_overlap(E_1, E_4, n_points = 1000000, significance_test = TRUE, replicates = 1000,
                                 confidence_limit = 0.05)


#Overap_M - Niche_1_vs_2------------
hist(overlap_12@significance_results$union_random$Niche_1_vs_2$overlap, 
     main = "Overlap ", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 300))
abline(v = quantile(overlap_12@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "red", lwd = 2, lty = 2)
abline(v = overlap_12@union_overlap$overlap[1], col = "lightblue", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("lightblue", "red"), lty = c(1, 2), lwd = 2)
dev.off()



#Save images------------
# plotting ellispodis and background for full overlap
plot_overlap(overlap_temp, background = TRUE, proportion = 0.6, background_type = "full")

# plotting ellispodis and background for overlap based on accessible environments
plot_overlap(overlap_temp, background = TRUE,  proportion = 1, background_type = "back_union")

library(rgl)
rgl.snapshot( "SP1_2", fmt = "png", top = TRUE )
browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), width=500), sep=""))
rgl.viewpoint( theta = 0, phi = 15, fov = 60, zoom = 0, interactive = TRUE )
