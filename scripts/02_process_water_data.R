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

# png("figures/maps/sites_map_v1.png")
maps::map("usa")
title(main="Dissolved Oxygen Stream Measurement Sites")
points(x=sites_dat1$dec_long_va, 
       y=sites_dat1$dec_lat_va,
       col = rgb(0, 0, 1, 0.1),
       pch = 20,
       cex = .7)
dev.off()


# clean data  --------------------------------------------------------------

# V code means contaminated
hist(NWISqw1$result_va[NWISqw1$remark_cd != "V" | is.na(NWISqw1$remark_cd)])
hist(NWISqw1$result_va)


NWISqw2 <- as_tibble(NWISqw1) %>% 
  filter(remark_cd != "V" | is.na(remark_cd), # not contaminated samples,
         medium_cd == "WS")# surface water)
hist(NWISqw2$result_va)
         
dim(NWISqw2)
dim(NWISqw1)

# sample type codes
NWISqw2$samp_type_cd %>% table()

num_unique <- lapply(NWISqw1, function(x) length(unique(x)))
cols_low_unique <- names(keep(num_unique, function(x) x < 40))

lapply(NWISqw2[, cols_low_unique], function(x) sort(unique(x)))

sum(NWISqw2$result_va >20, na.rm = T) # values above 20 aren't really possible

NWISqw3 <- NWISqw2 %>% 
  filter(samp_type_cd %in% c(7, 9, "H"), # normal samples (not spikes etc)
         result_va < 20)

hist(NWISqw3$result_va)


# site summaries ----------------------------------------------------------

site_sum1 <- NWISqw3 %>% 
  group_by(site_no) %>% 
  summarise(n = sum(!is.na(result_va)),
            result_va_m = mean(result_va, na.rm = TRUE),
            result_va_sd = sd(result_va, na.rm = TRUE),
            result_va_med = median(result_va, na.rm = TRUE))

site_sum1 %>% 
  select(n:result_va_med) %>% 
  lapply(hist)

# save data ---------------------------------------------------------------


saveRDS(site_sum1, "data/NWIS/NWISqw_site-summary_lwr48_v1.rds")
