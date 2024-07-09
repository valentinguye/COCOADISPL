


library(sf)
library(tidyverse)
library(aws.s3)
library(dismo)
library(units)
library(readxl)
library(xlsx)
library(stringr)
library(DescTools)
aws.signature::use_credentials()
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")


# This has villages that were actually not surveyed, but it has the distance variable, so read it for informative purpose. 
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
# unlike Deforestation analysis villages_Ghana.csv, this has consistent IDs with village IDs in Updated list of villages_Ghana survey.csv, 
# so they can be joined
all_vil <-
  s3read_using(
    object = "ghana/cocoa/displacement_econometrics/input_data/surveys_thomas/Deforestation analysis villages_Ghana_ID_fixed.csv", 
    bucket = "trase-storage",
    FUN = read.csv,
    opts = c("check_region" = TRUE)
  ) %>% 
  mutate(SURVEYED = if_else(surveyed...1.Yes..0.No. == 1, TRUE, FALSE)) %>% 
  dplyr::select(-`surveyed...1.Yes..0.No.`)

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
vill_points <- 
  all_vil %>% 
  st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = 4326) %>%  
  st_transform(crs = st_crs(pas))

ggplot(st_crop(pas, st_bbox(st_buffer(vill_points, 10000)))) + 
  geom_sf() +  
  geom_sf_text(aes(label = RMSCID), size = 3) + # , geometry = geom
  geom_sf(data = vill_points) + 
  theme_minimal() 

# restrict the AOI to a square including any park area within 10km from any village for now
# (2km is the buffer depth Thomas chose to sample villages)
parks_polyS <- st_crop(pas, st_bbox(st_buffer(vill_points, 10000))) %>% st_sf()
parks_poly <- st_union(parks_polyS) %>% st_sf()
# DON'T remove villages in parks for now. 
# vill_points <- vill_points %>% filter(lengths(st_within(vill_points, st_union(pas))) == 0)

ggplot(parks_poly) + 
  geom_sf() + 
  geom_sf(data = vill_points) + 
  theme_minimal() 


# Here, we do not set an in-park unit size (that would match the resolution of Schroth or gaez)
# rather, the size of the polygons in-parks are determined by the number of villages around. 
# Problem: we currently don't observe all villages. 
# So we limit the polygon of every village in the opposite direction of the park, with a buffer. 
# So we make a unionized buffer around villages of size M km

# The size of the buffer is determined arbitrarily to 9km, because this fills the inner-parks space right. 
buffer_km <- 9
# This is a parameter for sensitivity checks. 
# The trade-off is between including as many of every village's farms as possible, 
# while not giving spurious space to villages that have more unobserved village neighbors and which voronois
# are not thus not constrained while they should. 
# (typically on parks' spikes in the outerbound of the region)

# The average distance of households to their farms, averaged again across villages is ~2.7 km
# But we would need the distribution across HH directly. 
sur_vil_dist$mean_distance %>% summary()
  
ggplot(sur_vil_dist, aes(x=mean_distance)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
ggplot(sur_vil_dist, aes(x=median_distance)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
  
  buf_vil <-
    vill_points %>%
    st_buffer(buffer_km*1000) %>%
    st_union() %>% 
    st_as_sf()
  
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
  

  # IN-PARK polygons
  inpark_sf <- 
    sur_voronoi_sf %>% 
    # trim area outside the park - this drops 6 villages which polygons are fully outside the parks
    st_intersection(parks_polyS) # use the shape file with one polygon per park, so that 
    # the intersection returns separate rows for different parks. 
  
  inpark_sf <- 
    inpark_sf %>% 
    mutate(POLYGON_AREA_HA = set_units(st_area(geometry), "hectares"), 
           RMSC_VILLAGE_ID = paste0(RMSCID, "-", Village_ID),
           IN_OUT = "INPARK") 
  
  if(length(unique(inpark_sf$RMSC_VILLAGE_ID)) != nrow(inpark_sf)){stop("ID issue")}
  
  # OUT-PARK Polygons
  outpark_sf <- 
    sur_voronoi_sf %>% 
    # remove inner park (this drops features that are fully in the park) - so the ids
    st_difference(parks_poly) 
    # # remove area that falls within other parks (not necessary if parks_poly is already the union)
    # st_difference(pas_union)
  
  outpark_sf <- 
    outpark_sf %>% 
    mutate(POLYGON_AREA_HA = set_units(st_area(geometry), "hectares"), 
           RMSC_VILLAGE_ID = paste0("OUTPARK-", Village_ID),
           IN_OUT = "OUTPARK")
  
  
  mean(set_units(outpark_sf$POLYGON_AREA_HA, "hectare"))
  mean(set_units(inpark_sf$POLYGON_AREA_HA, "hectare"))
  
  # Keep only in-park polygons that have a matching treatment polygon outside
  # inpark_sf <-
  #   inpark_sf %>%
  #   filter(Village_ID %in% outpark_sf$Village_ID)
  # No, because for now we want to keep those villages (only 1 actually) which polyon is only within a park. 
  
  ggplot() + 
    theme_minimal() +
    geom_sf(data = parks_poly) +
    geom_sf(data = vill_points, aes(col = SURVEYED)) +
    geom_sf(data = sur_voronoi_sf, alpha = 0) +
    scale_color_discrete(guide = guide_legend(title = "Surveyed")) + 
    geom_sf(data = inpark_sf, fill = "green", alpha = 0.5) + 
    geom_sf(data = outpark_sf, fill = "lightblue", alpha = 0.5) 
  
  
  # # Check that in and out polygons have the same ID
  plot(st_geometry(parks_poly))
  plot(st_geometry(inpark_sf[1:10,"Village_ID"]), add = T)
  plot(outpark_sf[outpark_sf$Village_ID %in% inpark_sf[1:10,]$Village_ID, "Village_ID"], add = T)
  
  # stack in- and out-park polygons
  
  in_out_sf <- 
    inpark_sf %>% 
    dplyr::select(names(outpark_sf)) %>% 
    rbind(outpark_sf) %>% 
    arrange(Village_ID) 
  

### EXPORT -----------
  
  inpark_sf <- in_out_sf %>% filter(IN_OUT == "INPARK")
  outpark_sf <- in_out_sf %>% filter(IN_OUT == "OUTPARK")
  
  # Export as geojson
  dir.create("temp_data/park_village_voronois/")
  st_write(inpark_sf, paste0("temp_data/park_village_voronois/rmsc_inpark_village_voronois_", buffer_km,"km.shp"), append = FALSE)
  
  dir.create("C:/Users/guye/OneDrive - UCL/shared_repository_UCLouvain_Cambridge/park_village_voronois/")
  st_write(inpark_sf, 
           paste0("C:/Users/guye/OneDrive - UCL/shared_repository_UCLouvain_Cambridge/park_village_voronois/rmsc_inpark_village_voronois_", 
                  buffer_km,"km.shp"), 
           append = FALSE)
  
  # s3write_using(
  #   x = inpark_sf,
  #   object = paste0("ghana/cocoa/displacement_econometrics/rmsc_inpark_village_voronois_", buffer_km,"km"),
  #   FUN = st_write,
  #   driver = "GeoJSON",
  #   bucket = "trase-storage",
  #   opts = c("check_region" = T)
  # )
  
  
  # Export as geojson
  st_write(outpark_sf, paste0("temp_data/park_village_voronois/rmsc_outpark_village_voronois_", buffer_km,"km.shp"), append = FALSE)
  
  st_write(outpark_sf, 
           paste0("C:/Users/guye/OneDrive - UCL/shared_repository_UCLouvain_Cambridge/park_village_voronois/rmsc_outpark_village_voronois_", 
                  buffer_km,"km.shp"), 
           append = FALSE)
  # s3write_using(
  #   x = outpark_sf,
  #   object = paste0("ghana/cocoa/displacement_econometrics/rmsc_outpark_village_voronois_", buffer_km,"km.shp"),
  #   FUN = st_write,
  #   driver = "GeoJSON",
  #   bucket = "trase-storage",
  #   opts = c("check_region" = T)
  # )
  
