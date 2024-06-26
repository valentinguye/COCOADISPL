


library(tidyverse)
library(aws.s3)
library(sf)
library(dismo)
library(units)
library(readxl)
library(xlsx)
library(stringr)
library(DescTools)
aws.signature::use_credentials()
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")


# This has villages that were actually not surveyed
sur_vil_dist <-
  s3read_using(
    object = "ghana/cocoa/displacement_econometrics/input_data/surveys_thomas/Village list with distance_Ghana.csv", 
    bucket = "trase-storage",
    FUN = read.csv,
    opts = c("check_region" = TRUE)
  )

# Updated - this has the correct list of villages. 
sur_vil <-
  s3read_using(
    object = "ghana/cocoa/displacement_econometrics/input_data/surveys_thomas/Updated list of villages_Ghana survey.csv", 
    bucket = "trase-storage",
    FUN = read.csv,
    opts = c("check_region" = TRUE)
  )

# All villages
#... 
all_vil <-
  s3read_using(
    object = "ghana/cocoa/displacement_econometrics/input_data/surveys_thomas/Deforestation analysis villages_Ghana.csv", 
    bucket = "trase-storage",
    FUN = read.csv,
    opts = c("check_region" = TRUE)
  ) %>% 
  mutate(SURVEYED = if_else(surveyed...1.Yes..0.No. == 1, TRUE, FALSE))

# RMSC
pas <- s3read_using(
  object = "ghana/spatial/BOUNDARIES/PROTECTED_AREAS/RMSC/out/RMSC_PROTECTED_AREAS_GHA.gpkg",
  FUN = read_sf,
  bucket = "trase-storage",
  opts = c("check_region" = T))

# WDPA
# pas <- s3read_using(
#   object = "ghana/spatial/BOUNDARIES/PROTECTED_AREAS/WDPA/OUT/WDPA_PROTECTED_AREAS_GHA_AUG21.gpkg",
#   FUN = read_sf,
#   bucket = "trase-storage",
#   opts = c("check_region" = T))

# pas <- read_sf("input_data/WDPA_PROTECTED_AREAS_GHA_AUG21.gpkg")
pas <- st_simplify(pas, dTolerance = 10)

pas_union <- st_union(st_geometry(pas))
plot(pas_union)

# MAKE PARK ID - would need to be adjusted for WDPA PARKS throughout script. 
if(length(unique(pas$NAME)) == nrow(pas)){
  pas$RMSCID <- seq(1, nrow(pas))
}

#the average parc size is 12933.13 [hectare] (but this is with big parks in the north)
set_units(st_area(pas), "hectare") %>% mean()
set_units(st_area(pas), "hectare") %>% median()
# assume that in the tropical region, the mean is around 10 kha and the median around 5 kha. 
# If we consider 100-ha in-park polygons, this means splitting the average park in 100 bits and the median park in 50 bits. 


# Spatialize villages 
all_vil <- 
  all_vil %>% 
  st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = 4326) %>%  
  st_transform(crs = st_crs(pas))

ggplot(st_crop(pas, st_bbox(st_buffer(all_vil, 10000)))) + 
  geom_sf() +  
  geom_sf_text(aes(label = RMSCID), size = 3) + # , geometry = geom
  geom_sf(data = all_vil) + 
  theme_minimal() 

# restrict the AOI to a square including any park area within 10km from any village for now
# (2km is the buffer depth Thomas chose to sample villages)
parks_poly <- st_union(st_crop(pas, st_bbox(st_buffer(all_vil, 10000)))) %>% st_sf()
# DON'T remove villages in parks for now. 
# all_vil <- all_vil %>% filter(lengths(st_within(all_vil, st_union(pas))) == 0)
vill_points <- all_vil

ggplot(parks_poly) + 
  geom_sf() + 
  geom_sf(data = vill_points) + 
  theme_minimal() 

make_units <- function(parks_poly, vill_points){
  
# Here, we do not set an in-park unit size (that would match the resolution of Schroth or gaez)
# rather, the size of the polygons in-parks are determined by the number of villages around. 
# Problem: we currently don't observe all villages. 
# So we limit the polygon of every village in the opposite direction of the park, with a buffer. 
# So we make a unionized buffer around villages of size M km
# The size of the buffer is determined arbitrarily to 6km 

# (Recall the average distance of households to their farms is ~2.7 km 
# sur_vil_dist$mean_distance %>% mean(na.rm = T)
  buf_vil <-
    vill_points %>%
    st_buffer(9000) %>%
    st_union() %>% 
    st_as_sf()
  
  # make a buffer around the park, use the buffer depth Thomas chose to sample villages: 2km
  # OUT_BUFFER_SIZE <- 2000
  # out_buffer <- st_buffer(parks_poly, OUT_BUFFER_SIZE) %>% dplyr::select(-1) # (remove the park ID from this object, it's redundant)
  
  # create voronoi polygons
  voronoi_polys <- dismo::voronoi(st_coordinates(vill_points), ext = parks_poly)#buf_vil out_buffer
  voronoi_sf <- st_as_sf(voronoi_polys)
  st_crs(voronoi_sf) <- st_crs(parks_poly)
  # voronoi_sf$intd_village <- vill_points$ID

  # need to associate back the voronois to their village ID, because it is not clear what the id var created by dismo::voronoi is
  sgbp <- st_contains(voronoi_sf, vill_points)
  if(nrow(vill_points) != nrow(voronoi_sf) | 
     unique(lengths(sgbp)) != 1){stop("id process needs to be reconsidered")}
  
  voronoi_sf$ROW_INDEX <- unlist(sgbp)
  vill_points$ROW_INDEX <- rownames(vill_points) %>% as.numeric()
  voronoi_sf <- 
    voronoi_sf %>% 
    left_join(st_drop_geometry(vill_points), by = "ROW_INDEX")
  
  # trim voronoi polygons by an arbitrary buffer around parks 
  voronoi_sf <- 
    voronoi_sf %>% 
    st_intersection(buf_vil)
  
  ggplot() + 
    theme_minimal() +
    geom_sf(data = parks_poly) +
    geom_sf(data = vill_points, aes(col = SURVEYED)) +
    geom_sf(data = voronoi_sf, alpha = 0) +
    scale_color_discrete(guide = guide_legend(title = "Surveyed"))
  
  # Keep only surveyed villages, we don't need the others anymore 
  sur_voronoi_sf <- 
    voronoi_sf %>% 
    filter(SURVEYED)
  
  # ON EN EST LA, GERER LA PRODUCTION D'ID
  
  # IN-PARK polygons
  inpark_sf <- 
    voronoi_sf %>% 
    # trim area outside the park - this drops polygons that are fully outside the parks
    st_intersection(parks_poly)
  
  inpark_sf <- 
    inpark_sf %>% 
    # rename(INPARK_ID = id) %>% 
    mutate(AREA_HA = set_units(st_area(geometry), "hectares"), 
           # UNIT_ID = paste0(RMSCID, "-", INPARK_ID),
           IN_OUT = "INPARK")
  
  
  # OUT-PARK Polygons
  outpark_sf <- 
    voronoi_sf %>% 
    # remove inner park (this drops features that are fully in the park) - so the ids
    st_difference(parks_poly) %>% 
    # keep only area within outter buffer
    st_intersection(out_buffer)
    # # remove area that falls within other parks (not necessary if parks_poly is already the union)
    # st_difference(pas_union)
  
  outpark_sf <- 
    outpark_sf %>% 
    # rename(INPARK_ID = id) %>% 
    mutate(AREA_HA = set_units(st_area(geometry), "hectares"), 
           # UNIT_ID = paste0(RMSCID, "-", INPARK_ID),
           IN_OUT = "OUTPARK")
  
  
  mean(set_units(outpark_sf$AREA_HA, "hectare"))
  
  # Keep only in-park polygons that have a matching treatment polygon outside
  # inpark_sf <- 
  #   inpark_sf %>% 
  #   filter(INPARK_ID %in% outpark_sf$INPARK_ID)
  
  ggplot() + 
    theme_minimal() +
    geom_sf(data = parks_poly) +
    geom_sf(data = vill_points, aes(col = SURVEYED)) +
    geom_sf(data = voronoi_sf, alpha = 0) +
    scale_color_discrete(guide = guide_legend(title = "Surveyed")) + 
    geom_sf(data = inpark_sf, fill = "green", alpha = 0.5) + 
    geom_sf(data = outpark_sf, fill = "grey", alpha = 0.5) 
  
  
  # ggplot(parks_poly) + 
  #   geom_sf(fill = "white") +
  #   theme_minimal()
  # 
  # ggplot(inpark_sf) + 
  #   geom_sf(fill = "white") + 
  #   theme_minimal()
  # 
  # ggplot(outpark_sf) + 
  #   geom_sf(aes(fill = UNIT_ID)) +
  #   geom_sf(data = st_geometry(parks_poly), aes(fill = NA)) +
  #   geom_sf(data = st_geometry(inpark_sf), aes(), fill = "white") + 
  #   theme_minimal()
  
  plot(st_geometry(parks_poly))
  plot(st_geometry(inpark_sf), add = T)
  plot(outpark_sf[,"INPARK_ID"], add = T) 
  
  # # Check that in and out polygons have the same ID
  # plot(st_geometry(parks_poly))
  # plot(st_geometry(inpark_sf[1,"INPARK_ID"]), add = T)
  # plot(outpark_sf[outpark_sf$INPARK_ID==inpark_sf[1,]$INPARK_ID, "INPARK_ID"], add = T)
  
  # stack in- and out-park polygons
  
  in_out_sf <- rbind(inpark_sf, outpark_sf)
  

  
  # DISC METHOD
  # # Now extract lines at the intersection of these equal area shapes and the park boundaries
  # # Define the parc as a MULTILINESTRING, rather than a MULTIPOLYGON
  # park_bnd  <- st_boundary(parks_poly) %>% st_geometry()# keep only the geometry, to not repeat the id
  # park_bnd == st_cast(parks_poly, to = "MULTILINESTRING") %>% st_geometry() # (yields exactly the same)
  # 
  # # this has less or as many features as equal_areas, because it drops the equal_areas features that are entirely within the park.   
  # edges <- st_intersection(equal_areas, park_bnd)
  # # some are only points, remove them
  # # st_geometry_type(edges$geometry)
  # # edges <- edges %>% filter(!grepl("POINT", st_geometry_type(geometry)))
  # 
  # # edges2 <- edges %>% filter(st_geometry_type(geometry) == "MULTILINESTRING") %>% st_line_merge()
  # 
  # # equal_area_55 <-  equal_areas %>% filter(INPARK_ID==19)
  # # edge_55 <- edges %>% filter(INPARK_ID==19)
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
  #   st_difference(parks_poly)
  # 
  # # # Since this leaves significant shares of PA borders uncovered by treatment discs, add a buffer area
  # # # Another method is to make a buffer around the edges
  # # tmt_buf <- 
  # #   edges %>% 
  # #   st_buffer(radius) %>% 
  # #   # remove the part inside the park to define the treatment zone
  # #   st_difference(parks_poly)
  
  return(in_out_sf)
}
rm(parks_poly, inpark_unit_size)