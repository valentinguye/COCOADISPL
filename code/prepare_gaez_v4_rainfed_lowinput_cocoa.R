
##### 0. PACKAGES, WD, OBJECTS #####


### WORKING DIRECTORY SHOULD BE CORRECT IF THIS SCRIPT IS RUN WITHIN R_project_for_individual_runs
### OR CALLED FROM LUCFP PROJECT master.do FILE.
### IN ANY CASE IT SHOULD BE (~/LUCFP/data_processing) 


### PACKAGES ###
library(tidyverse)
library(aws.s3)
library(sf)
library(raster)
library(rnaturalearth)
library(readxl)
library(xlsx)
library(stringr)
library(DescTools)
library(here)
aws.signature::use_credentials()
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")


# Download for these crops: maize, cassava, plantain, yam, cocoyam, rice, sorghum and millet

# Renaming layers with the following name mapping matrix
mapmat_data <- c("coc", "Cocoa")

# 3 of these are not in the data downloaded (but that does matter): Millet, Maizesilage, and Pasture
mapmat <- matrix(data = mapmat_data, 
                 nrow = length(mapmat_data)/2,
                 ncol = 2, 
                 byrow = TRUE)

colnames(mapmat) <- c("abrev", "Names")


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#### PREPARE SUITABILITY INDICES ####

dir.create(here("temp_data", "GAEZ_v4", "AES_index_value", "Rain-fed", "Low-input", "Cocoa"), recursive = TRUE) 

datadir <- here("input_data", "GAEZ_v4", "AES_index_value", "Rain-fed", "Low-input", "Cocoa")
targetdir <- here("temp_data", "GAEZ_v4", "AES_index_value", "Rain-fed", "Low-input", "Cocoa")
tmpdir <- here("temp_data", "tmp")

if (!dir.exists(tmpdir)) dir.create(tmpdir, recursive = TRUE)
if (dir.exists(targetdir)) {
  file.remove(list.files(path = targetdir,
                         pattern = ".tif", full.names = TRUE))
} else dir.create(targetdir, recursive = TRUE)



files <- list.files(path = datadir, pattern = ".tif")
crops <- unlist(strsplit(files, split = ".tif"))
for(crop in crops){
  crops[grepl(crop, crops)] <- mapmat[grepl(crop, paste0("sxLr_", mapmat[,"abrev"])),"Names"]
}

ne_gha <- ne_countries(country = "Ghana") %>% st_as_sf()
ext <- extent(st_bbox(ne_gha))

## Import most crops
for (j in 1:length(files)) {
  #if (any(crops[j] == cropsToAggregate)) next
  print(files[j])
  # unzip(zipfile = here(datadir, files[j]), exdir = tmpdir)
  dt <- raster(here(datadir, files[j]))
  
  # crop to GHANA AOI
  dt_trop <- crop(dt, ext)
  
  # A few points are -0.09 with no apparent reason. 
  dt_trop[dt_trop<0] <- NA 
  
  names(dt_trop) <- crops[j]
  writeRaster(dt_trop,
              filename = here(targetdir, paste0(crops[j], ".tif")),
              overwrite = TRUE)
}
rm(dt, dt_trop)

## Create a brick for convenience. (actually not necessary, but need to read the layer anyway)
rasterlist_gaez <- list.files(path = targetdir,
                              pattern = "",
                              full.names = TRUE) %>% as.list()
gaez_cocoa <- brick(rasterlist_gaez)

# project to PAS CRS. 

gaez_cocoa <- projectRaster(gaez_cocoa, crs = 32630)
plot(gaez_cocoa)
pas <- read_sf("input_data/WDPA_PROTECTED_AREAS_GHA_AUG21.gpkg")
plot(st_geometry(pas), add = T)

writeRaster(gaez_cocoa, 
            here("temp_data", "GAEZ_v4", "AES_index_value", "Rain-fed", "Low-input", "Cocoa", "ghana_cocoa.tif"), 
            overwrite = TRUE)


s3write_using(
  x = gaez_cocoa,
  object = "ghana/cocoa/displacement_econometrics/temp_data/GAEZ_v4/AES_index_value/Rain-fed/Low-input/Cocoa/ghana_cocoa.tif",
  FUN = writeRaster,
  bucket = "trase-storage",
  opts = c("check_region" = T)
)

rm(rasterlist_gaez, gaez_cocoa)

rm(ext, mapmat, mapmat_data)