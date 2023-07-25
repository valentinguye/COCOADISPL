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
# library(dismo)
library(units)
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

pas <- read_sf("input_data/WDPA_PROTECTED_AREAS_GHA_AUG21.gpkg")
pas <- st_simplify(pas, dTolerance = 10)

pas_union <- st_union(st_geometry(pas))

# districts <- s3read_using(
#   object = "ghana/spatial/BOUNDARIES/DISTRICTS_REGIONS/OUT/GHA_DISTRICTS.geojson",
#   bucket = "trase-storage",
#   FUN = read_sf,
#   #sheet = "Cacao", 
#   #skip = 3,
#   opts = c("check_region" = T)
# )

#the average parc size is 12933.13 [hectare] (but this is with big parks in the north)
set_units(st_area(pas), "hectare") %>% mean()
set_units(st_area(pas), "hectare") %>% median()
# assume that in the tropical region, the mean is around 10 kha and the median around 5 kha. 
# If we consider 100-ha in-park polygons, this means splitting the average park in 100 bits and the median park in 50 bits. 

#---------------Previous rationale to program size of things with the DISC method -----------------------

# We would like the final discs to fit in 1x1km squares (100 ha), approx. 
# The radius of such disc is half the square side: 500 m. 
# This is pi*500^2 = 785398.2 m2 = 78.54 ha. 
# And we want our in-park units to be twice smaller than the discs, since discs tend to have one half in and one half outside the park. 
# Hence, in-park units should be ~40 ha large. 

# If would like the final discs to fit in 9x9km squares (8100 ha), approx. 
# The radius of such disc is half the square side: 4500 m. 
# This is pi*4500^2 = 63617251 m2 = 6361.73 ha. 
# And we want our in-park units to be twice smaller than the discs, since discs tend to have one half in and one half outside the park. 
# Hence, in-park units should be ~3181 ha large. 

# To be given in HECTARES
# INPARK_UNIT_SIZE <- 40 %>% set_units("hectare")

# --------------Current rationale to program size of things with the VORONOI method -----------------------
# In this case, the out-park area is the same as the in-parc one, there is no disc. 
# intuitively: imagine two squares of the same area, one in the park one outside, and they share one of their sides, on the park bord.  
# In this case, we define the in-park area directly as the final resolutions, either 1x1km or 9x9km. 
INPARK_UNIT_SIZE <- 100 %>% set_units("hectare")
# and from there, we set the buffer depth in relation to the in-park unit size. 
# The average in-parc unit, assumed squared, has a border of square-root of its area. 
# The voronoi that has the same area, needs to be trimmed at this border size as well (assuming it's a square too)
# THIS IS IN **METER**
OUT_BUFFER_SIZE <- sqrt(set_units(INPARK_UNIT_SIZE, NULL) * 10000)

# NOTE: this method does not work for 9x9km res, it's too big. 
# (and so probably even the 100ha for 1x1km res. will be too big for some small parks)
# --> for 9x9km, will need to find another solution, like downscaling GAEZ and then accommodate by spatial clustering s.e. at GAEZ or more res. 
# --> for 1x1km, we can remove smallest parks from study. 

sf_poly <- pas[4,]
out_buffer_size <- OUT_BUFFER_SIZE

make_units <- function(sf_poly, out_buffer_size){
  
  sf_poly <- sf_poly %>% dplyr::select(contains("WDPAID"))
  
  # make a buffer around the park 
  out_buffer <- st_buffer(sf_poly, out_buffer_size) %>% select(-1)
  
  # determine n_areas from sf_poly area. 
  n_areas <- (set_units(st_area(sf_poly), "hectare") / INPARK_UNIT_SIZE) %>% set_units(NULL) %>% round()
  # create random points
  set.seed(4321)
  points_rnd <- st_sample(sf_poly, size = 10000)
  #k-means clustering
  points <- do.call(rbind, st_geometry(points_rnd)) %>%
    as_tibble() %>% setNames(c("lon","lat"))
  k_means <- kmeans(points, centers = n_areas)
  # create voronoi polygons
  voronoi_polys <- dismo::voronoi(k_means$centers, ext = out_buffer)#out_buffer
  # clip to sf_poly
  voronoi_sf <- st_as_sf(voronoi_polys)
  st_crs(voronoi_sf) <- st_crs(sf_poly)
  
  # IN-PARK polygons
  inpark_sf <- 
    voronoi_sf %>% 
    # trim area outside the park
    st_intersection(sf_poly)
  
  inpark_sf <- 
    inpark_sf %>% 
    rename(INPARK_UNIT_ID = id) %>% 
    mutate(AREA_M2 = st_area(geometry), 
           UNIT_ID = paste0(WDPAID, "-", INPARK_UNIT_ID),
           IN_OR_OUT = "INPARK")
  
  # OUT-PARK Ppolygons
  outpark_sf <- 
    voronoi_sf %>% 
    # remove inner park (this drops features that are fully in the park) - so the ids
    st_difference(sf_poly) %>% 
    # keep only area within outter buffer
    st_intersection(out_buffer) %>% 
    # remove area that falls within other parks 
    st_difference(pas_union)
  
  outpark_sf <- 
    outpark_sf %>% 
    rename(INPARK_UNIT_ID = id) %>% 
    mutate(AREA_M2 = st_area(geometry), 
           UNIT_ID = paste0(WDPAID, "-", INPARK_UNIT_ID),
           IN_OR_OUT = "OUTPARK")
    
  
  mean(set_units(outpark_sf$AREA_M2, "hectare"))
  
  # Keep only in-park polygons that have a matching treatment polygon outside
  inpark_sf <- 
    inpark_sf %>% 
    filter(INPARK_UNIT_ID %in% outpark_sf$INPARK_UNIT_ID)
  
  plot(st_geometry(sf_poly))
  plot(st_geometry(inpark_sf), add = T)
  plot(outpark_sf[,"INPARK_UNIT_ID"], add = T)
   
  # # Check that in and out polygons have the same ID
  # plot(st_geometry(sf_poly))
  # plot(st_geometry(inpark_sf[1,"INPARK_UNIT_ID"]), add = T)
  # plot(outpark_sf[outpark_sf$INPARK_UNIT_ID==inpark_sf[1,]$INPARK_UNIT_ID, "INPARK_UNIT_ID"], add = T)
  
  # stack in- and out-park polygons
  
  in_out_sf <- rbind(inpark_sf, outpark_sf)
  
  
  # DISC METHOD
  # # Now extract lines at the intersection of these equal area shapes and the park boundaries
  # # Define the parc as a MULTILINESTRING, rather than a MULTIPOLYGON
  # park_bnd  <- st_boundary(sf_poly) %>% st_geometry()# keep only the geometry, to not repeat the id
  # park_bnd == st_cast(sf_poly, to = "MULTILINESTRING") %>% st_geometry() # (yields exactly the same)
  # 
  # # this has less or as many features as equal_areas, because it drops the equal_areas features that are entirely within the park.   
  # edges <- st_intersection(equal_areas, park_bnd)
  # # some are only points, remove them
  # # st_geometry_type(edges$geometry)
  # # edges <- edges %>% filter(!grepl("POINT", st_geometry_type(geometry)))
  # 
  # # edges2 <- edges %>% filter(st_geometry_type(geometry) == "MULTILINESTRING") %>% st_line_merge()
  # 
  # # equal_area_55 <-  equal_areas %>% filter(INPARK_UNIT_ID==19)
  # # edge_55 <- edges %>% filter(INPARK_UNIT_ID==19)
  # # plot(st_geometry(equal_area_55))
  # # plot(st_geometry(edge_55), add = T, col = "red")
  # 
  #   # define buffer radius such that the area of the disc is two times the equal area 
  # radius = sqrt(2*mean(set_units(equal_areas$area, NULL))/pi)
  # 
  # disc_centro <- 
  #   edges %>% 
  #   st_centroid() 
  # 
  # tmt_disc <- 
  #   edges %>% 
  #   st_centroid() %>% 
  #   st_buffer(radius) %>% 
  #   # remove the part inside the park to define the treatment zone
  #   st_difference(sf_poly)
  # 
  # # # Since this leaves significant shares of PA borders uncovered by treatment discs, add a buffer area
  # # # Another method is to make a buffer around the edges
  # # tmt_buf <- 
  # #   edges %>% 
  # #   st_buffer(radius) %>% 
  # #   # remove the part inside the park to define the treatment zone
  # #   st_difference(sf_poly)
  
  return(in_out_sf)
}

collect_list <- list()
for(PA in unique(pas$WDPAID)[1:10]){
  collect_list[[as.character(PA)]] <- make_units(sf_poly = pas[pas$WDPAID == PA,], 
                                                 out_buffer_size = OUT_BUFFER_SIZE)
}

output_sf <- bind_rows(collect_list)  

length(unique(output_sf$UNIT_ID))*2 == nrow(output_sf)

plot(output_sf[,"IN_OR_OUT"], add = T)












# PLOTTING #############
plot(st_geometry(sf_poly))
plot(st_geometry(equal_areas), add = T)

plot(st_geometry(sf_poly))
plot(st_geometry(edges), add = T, col = "blue")
plot(st_geometry(edges_disc), add = T, col = "blue")

plot(st_geometry(sf_poly))
plot(st_geometry(disc_centro), add = T, col = "red")
plot(st_geometry(equal_areas), add = T)
plot(tmt_disc[,"INPARK_UNIT_ID"], add = T)



ggplot(data = equal_areas) + 
  geom_sf() +
  geom_sf_text(
    aes(label = INPARK_UNIT_ID), data = equal_areas,
  )


