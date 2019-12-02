# Martin Holdrege


# script started 11/29/19

# CS 5665 final project
# model dissolved o2 as a function of land cover. 



# libraries ---------------------------------------------------------------

library(tidyverse)


# load water data data -----------------------------------------------------

water1 <- readRDS("data/NWIS/NWISqw_site-summary_lwr48_v1.rds")


# load cover data ---------------------------------------------------------
# data from GEE

cover1 <- readRDS("data/land_cover_by_site_v1.rds") # cover within 0.5 km of sites
cover_3km <- readRDS("data/land_cover_by_site_buf2_v1.rds") # within 3 km of sites


sum(cover1$RowSum < 600) # problematic sites (should all have about same total cover)

# for cover NA is actually 0 for that class

cov_regex <- "^cover\\d+"

cover1b <- cover1 %>% 
  inner_join(cover_3km, by = "site_no", suffix = c("", "_3km"))

cover2 <- cover1b %>% 
  filter(RowSum > 600) %>% 
  mutate_at(.vars = vars(matches(cov_regex)),
            .funs = function(x) ifelse(is.na(x), 0, x)
  ) 

cover3 <- cover2 #%>% 
  # mutate_at(.vars = vars(matches("^cover\\d+$")),
  #           .funs = function(x) x/cover2$RowSum) %>%  # turning into proportion
  # mutate_at(.vars = vars(matches("^cover\\d+_3km$")),
  #           .funs = function(x) x/cover2$RowSum_3km)


# load slope data ---------------------------------------------------------


# mean slope by polgyon (from gee)
slope1 <- read_csv("data/buf1_mean_slope_by_site.csv")
slope1_3km <- read_csv("data/buf2_mean_slope_by_site.csv")

slope1b <- slope1 %>% 
  select(site_no, mean) %>% 
  rename(slope = mean)

slope1b_3km <- slope1_3km %>% 
  select(site_no, mean) %>% 
  rename(slope_3km = mean)

slope2 <- inner_join(slope1b_3km, slope1b, by = "site_no")

plot(slope ~ slope_3km, data = slope2)

# combine data ------------------------------------------------------------

all1 <- inner_join(water1, cover3, by = "site_no") %>% 
  inner_join(slope2, by = "site_no") 
names(all1)
dim(all1)


# exploratory figures -----------------------------------------------------
# just mean response and cover variables
cover_vars <- names(cover3) %>% 
  .[str_detect(. ,cov_regex)] %>% 
  .[!is.na(.)]
pred_vars <- c("slope", "slope_3km", cover_vars)
all1_m_cover <- select(all1, result_va_m, pred_vars) %>% 
  drop_na()

# par(mfrow = c(4, 4))
# for (var in pred_vars) {
#   print(var)
#   print(
#     plot(as.formula(paste("result_va_m ~", var)), data = all1)
#   )
# }

cor(all1_m_cover)
cor(all1$result_va_m, all1$slope)
cor(all1$result_va_m, all1$slope_3km)




# train/test data ---------------------------------------------------------

all2 <- all1_m_cover
all2_scaled <- all1_m_cover %>% 
  mutate_at(vars(pred_vars),
            function(x) as.numeric(scale(x))) 


n <- nrow(all2)
vec <- 1:n
train_rows <- sample(vec, size = 0.7*n)
test_rows <- vec[!vec %in% train_rows]

all2_train <- all2[train_rows, ]

all2_test <- all2[test_rows, ]

# correlations with response

pred_cors <- tibble(pred_var = pred_vars,
                    cor = NA_real_)
for (i in seq_along(pred_vars)) {
  pred_cors$cor[i] <- cor(all2$result_va_m, all2[[pred_vars[[i]]]])

}
pred_cors %>% 
  arrange(desc(cor)) %>% 
  print(n = 50)

cover_classes_3km <- names(all2)[str_detect(names(all2), "^cover\\d+_3km")] %>% 
  str_extract("\\d+")

par(mfrow = c(4, 4))
for (class in cover_classes_3km) {
  cov_buf1 <- paste0("cover", class)
  cov_buf2 <- paste0("cover", class, "_3km")
  val <- cor(all2[[cov_buf1]], all2[[cov_buf2]])
  print(
    plot(all2[[cov_buf1]], all2[[cov_buf2]],
       main = class)
  )
  print(round(val, 3))
}
  


# OLS regression ----------------------------------------------------------

# issue with model fitting
lm1 <- lm(result_va_m ~ ., data = all2_train)
summary(lm1)



# lasso -------------------------------------------------------------------




# cart --------------------------------------------------------------------




# random forest -----------------------------------------------------------







