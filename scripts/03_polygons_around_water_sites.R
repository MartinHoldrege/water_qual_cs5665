# Martin Holdrege

# CS 5665 final project

# scripted started 10/20/19

# For making polygons around water measurement sites, to be loaded into
# google earth engine. 


# dependencies ------------------------------------------------------------

library(dataRetrieval)
library(tidyverse)
library(rgeos)
library(sp)

# loading data ------------------------------------------------------------

NWISqw1 <- readRDS("data/NWIS/NWISqw_raw_lwr48_v1.rds")


# site locations ----------------------------------------------------------

sites_dat1 <- NWISqw1 %>% # sites data was collected for
  group_by(site_no) %>% 
  summarize(n = n())

site_info <- readNWISsite(siteNumber = sites_dat1$site_no)
site_info$dec_coord_datum_cd %>% unique()

sites_dat1 <- sites_dat1 %>% 
  left_join(site_info, by = "site_no")


# polgyons around sites ---------------------------------------------------

# set the projection
crs_nad83 <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") 
crs_albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")


# create spatial points df
sites_spdf1 <- SpatialPointsDataFrame(sites_dat1[, c("dec_long_va", "dec_lat_va")], data = sites_dat1,
                       proj4string = crs_nad83)


sites_spdf2 <- spTransform(sites_spdf1, crs_albers)  # reproject!!

# 500 m circle around plots
sites_buf1 <- gBuffer(sites_spdf2, byid = TRUE, width = 500, quadsegs = 2)  # units in meters
sites_buf2 <- gBuffer(sites_spdf2, byid = TRUE, width = 3000, quadsegs = 2) # bigger buffer
object.size(sites_buf1)
object.size(sites_buf2)
# library(spdplyr)


# saving shapefiles (for use in GEE) --------------------------------------

rgdal::writeOGR(obj = sites_buf1, dsn = "data",  layer="sites_buf1", driver="ESRI Shapefile")
rgdal::writeOGR(obj = sites_buf2, dsn = "data",  layer="sites_buf2", driver="ESRI Shapefile")
saveRDS(sites_buf1$site_no, "data/site_no_buf1.rds") # seperately saving site no



