
# Workstation set-up ------------------------------------------------------
library(tidyverse)
library(aws.s3)
library(sf)
library(dismo)
library(units)
library(readxl)
library(xlsx)
library(stringr)
library(DescTools)
library(rnaturalearth)
aws.signature::use_credentials()
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

ne_gha <- ne_countries(country = "Ghana", scale = 10) %>% st_as_sf()


# RMSC
pas <- s3read_using(
  object = "ghana/spatial/BOUNDARIES/PROTECTED_AREAS/RMSC/out/RMSC_PROTECTED_AREAS_GHA.gpkg",
  FUN = read_sf,
  bucket = "trase-storage",
  opts = c("check_region" = T))

outpark_9km <- st_read("temp_data/rmsc_outpark_9km") 
inpark_9km <- st_read("temp_data/rmsc_inpark_9km") 

inout_9km <- rbind(outpark_9km, inpark_9km)

# GAEZ
cocoa_aesi <- raster(here::here("temp_data", "GAEZ_v4", "AES_index_value", "Rain-fed", "Low-input", "Cocoa", "Cocoa.tif"))

inout_9km <- st_transform(inout_9km, crs = crs(cocoa_aesi))
ne_gha <- st_transform(ne_gha, crs = crs(cocoa_aesi))
plot(cocoa_aesi)
plot(st_geometry(inout_9km), add = T)
plot(st_geometry(ne_gha), add = T)
