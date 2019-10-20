# Martin Holdrege

# scripted started 9/4/19

# For downloading water quality data from the water quality portal (WQP)


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
                parameterCd = O2Cds_mgl$parameter_cd #,
                # startDt= "2018-01-01",
                # endDt = "2018-12-31"
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
what_data1 <- whatNWISdata(siteNumber = sites2$site_no[1])
what_data1 %>% View()
dat %>% 
  filter(parm_cd == "00300", site_tp_cd == "ST") %>% 
  .$site_no %>% 
  unique() %>% 
  length()
dim(sites2)

dim(sites1)
names(sites2)
names(sites1)


# download data -----------------------------------------------------------

# failed:
# dat1 <- readNWISdata(stateCd = "UT", parameterCd = O2Cds_mgl$parameter_cd,
#                      service = "qw",
#                      startDate = "2018-01-01", 
#                      seriesCatalogOutput = TRUE)

NWISqw1 <- readNWISqw(siteNumbers = sites2$site_no, 
                   parameterCd = O2Cds_mgl$parameter_cd,
                   startDate = "2009-01-01")
dim(dat2)
#NWISdata1 <- map(lower48, )
dim(dat1)
names(dat1)
dat1 %>% 
  filter(site_tp_cd == "ST") %>% 
  .$site_no %>% 
  unique() %>% 
  length()

# download WQP data  --------------------------------------------------------

ut_wqp <- readWQPdata(statecode="UT",characteristicName= "Oxygen")
str(ut_wqp)
dim(ut_wqp)
whatWQPsites(statecode = "UT")


siteInfo <- attr(ut_wqp, "siteInfo")
wqp_summary1 <- ut_wqp %>% 
  filter(str_detect(ResultMeasure.MeasureUnitCode, "mg/l")) %>% 
  group_by(MonitoringLocationIdentifier) %>%  
  summarise(count=n(),
            start=min(ActivityStartDateTime, na.rm = TRUE),
            end=max(ActivityStartDateTime, na.rm = TRUE),
            max = max(ResultMeasureValue, na.rm = TRUE),
            mean = mean(ResultMeasureValue, na.rm = TRUE))%>% 
  left_join(siteInfo, by = "MonitoringLocationIdentifier") %>% 
  ungroup() %>% 
  filter(str_detect(MonitoringLocationTypeName, "[Ss]tream"))
dim(wqp_summary1)
summary(wqp_summary1)
