# Martin Holdrege

# CS 5665 final project

# scripted started 10/20/19

# Processing NWIS dissolved oxygen data


# dependencies ------------------------------------------------------------

library(tidyverse)
library(maps)

# loading data ------------------------------------------------------------

NWISqw1 <- readRDS("data/NWIS/NWISqw_raw_lwr48_v1.rds")
allsites <- readRDS("data/NWIS/NWIS_sites_lwr48_v1.rds")

# site locations ----------------------------------------------------------

names(NWISqw1)

sites_dat1 <- NWISqw1 %>% # sites data was collected for
  group_by(site_no) %>% 
  summarize(n = n()) %>% 
  left_join(allsites, by = "site_no")

dim(sites_dat1)
hist(sites_dat1$n)

png("figures/maps/sites_map_v1.png")
maps::map("usa")
title(main="Dissolved Oxygen Stream Measurement Sites")
points(x=sites_dat1$dec_long_va, 
       y=sites_dat1$dec_lat_va,
       col = rgb(0, 0, 1, 0.1),
       pch = 20,
       cex = .7)
dev.off()


