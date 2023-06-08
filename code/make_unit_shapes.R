##====== Script summary ===========================================
# action: Build polygons of study units around Ghanaian protected areas (PAs)
# author: Valentin Guye
# date: "08/06/2023"
# output: 
##=================================================================

# Workstation set-up ------------------------------------------------------
library(tidyverse)
library(aws.s3)
library(sf)
library(readxl)
library(xlsx)
library(stringr)
library(DescTools)
aws.signature::use_credentials()
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

# RMSC
pas <- s3read_using(
  object = "ghana/spatial/BOUNDARIES/PROTECTED_AREAS/RMSC/out/RMSC_PROTECTED_AREAS_GHA.gpkg",
  FUN = read_sf,
  bucket = "trase-storage",
  opts = c("check_region" = T))

# WDPA
pas <- s3read_using(
  object = "ghana/spatial/BOUNDARIES/PROTECTED_AREAS/WDPA/OUT/WDPA_PROTECTED_AREAS_GHA_AUG21.gpkg",
  FUN = read_sf,
  bucket = "trase-storage",
  opts = c("check_region" = T))


# districts <- s3read_using(
#   object = "ghana/spatial/BOUNDARIES/DISTRICTS_REGIONS/OUT/GHA_DISTRICTS.geojson",
#   bucket = "trase-storage",
#   FUN = read_sf,
#   #sheet = "Cacao", 
#   #skip = 3,
#   opts = c("check_region" = T)
# )



