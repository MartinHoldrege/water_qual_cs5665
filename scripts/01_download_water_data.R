# Martin Holdrege

# CS 5665 final project

# scripted started 9/4/19

# For downloading water quality data from the water quality portal (WQP)

# pulling DO data from streams only. 


# dependencies ------------------------------------------------------------

library(tidyverse)
library(dataRetrieval)

source("scripts/water_qual_functions.R")

# finding sites -----------------------------------------------------------

# codes:
do_mgl <- "00300" 

O2Cds_all <- parameterCdFile %>% 
  filter(str_detect(parameter_nm, "[Oo]xygen"), 
         str_detect(parameter_nm, "[Dd]issolved"))

O2Cds_mgl <- O2Cds_all %>% 
  filter(str_detect(parameter_units, "mg/l"), 
         parameter_group_nm == "Inorganics, Major, Non-metals")

sites2 <- safely_whatNWISsites(
  stateCd = "UT",
  parameterCd=c(do_mgl),
  #hasDataTypeCD = "dv",
  startDt= "2018-01-01",
  endDt = "2018-12-31"
)$result
dim(sites2)
readNWISsite(sites2$site_no) %>% head() %>% View()


dat %>% 
  filter(parm_cd == "00300", site_tp_cd == "ST") %>% 
  .$site_no %>% 
  unique() %>% 
  length()

lower48 <- state.abb[!state.abb %in% c("AK", "HI")]

sites_l <- map(lower48, function(x){
  message(x)
  out <- safely_whatNWISsites(stateCd = x,
                parameterCd = O2Cds_mgl$parameter_cd,
                startDt= "2000-01-01",
                endDt = "2019-10-20"
                )
  out$result
})



names(sites_l) <- lower48

ut <- whatWQPsites(stateCd = "UT",
             parameterCd = O2Cds_mgl$parameter_cd)

sites2 <- bind_rows(sites_l) %>% 
  filter(str_detect(site_tp_cd, "^ST(?!-TS).*$")) # not matching tidal stream

sites2$site_tp_cd %>% unique

dim(sites2)

what_data1 <- whatNWISdata(siteNumber = sites2$site_no[1:2])
# what_data1 %>% View()

saveRDS(sites2, "data/NWIS/NWIS_sites_lwr48_v1.rds")

# download data -----------------------------------------------------------


NWISqw1 <- readNWISqw(siteNumbers = sites2$site_no, # 8 minutes to run
                   parameterCd = O2Cds_mgl$parameter_cd,
                   startDate = "2000-01-01",
                   endDate = "2019-10-20")

dim(NWISqw1)

saveRDS(NWISqw1, "data/NWIS/NWISqw_raw_lwr48_v1.rds")


