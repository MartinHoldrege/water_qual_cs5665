# Martin Holdrege

# CS 5665 final project

# scripted started 11/26/19

# Processing data extracted from google earth engine 
# (based on polygons around the water measurement sites)

# this is land cover classification (% of each pixel type)


# dependencies ------------------------------------------------------------

library(tidyverse)


# loading data ------------------------------------------------------------

# land cover
lc1 <- read_csv("data/sitesBuff1LandCoverData.csv")

head(lc1)

# site locations
site_no <- readRDS("data/site_no_buf1.rds")



# parsing data ------------------------------------------------------------

# the histogram column needs to be parsed. it includes all the data in a string


# extracting possible cover types

cover_types <- lc1$histogram %>% 
  str_extract_all("\\d+(?==)") %>% 
  unlist() %>% 
  unique() %>% 
  sort()

lc2 <- lc1 %>% 
  rename(index = `system:index`) %>% 
  select(index, histogram)

# adding empty cover columns
for (type in cover_types) {
  col <- paste0("cover", type)
  lc2[[col]] <- NA_real_
}

lc2 <- lc2 %>% 
  mutate(vectors = map(histogram, extract_cover_value_pairs))

# rows where didn't parse
lc2 %>% 
  filter(map_lgl(vectors, is.null))

# populating the cover columns [very slow]
lc3 <- lc2
for (i in 1:nrow(lc3)) {
  vec <- lc3$vectors[[i]]
  if(is.null(vec)) {
    next
  }
  for (j in seq_along(vec)) {
    col_name <- names(vec[j])
    lc3[i, ][[col_name]] <- vec[j]
  }

}

lc3$RowSum <- rowSums(select(lc3, matches("^cover\\d+$")), na.rm = TRUE)
lc3$site_no <- site_no

lc3

lc4 <- lc3 %>% 
  select(site_no, RowSum, matches("^cover\\d+$"))


# saving data -------------------------------------------------------------

saveRDS(lc4, "data/land_cover_by_site_v1.rds")
