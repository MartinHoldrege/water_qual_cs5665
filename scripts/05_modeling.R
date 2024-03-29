# Martin Holdrege


# script started 11/29/19

# CS 5665 final project
# model dissolved o2 as a function of land cover. 



# packages ---------------------------------------------------------------

library(tidyverse)
library(glmnet) # for lasso
library(rpart) # tree
library(randomForest)

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
  inner_join(cover_3km, by = "site_no", suffix = c("_0.5km", "_3km"))

cover2 <- cover1b %>% 
  filter(RowSum_0.5km > 600) %>% 
  mutate_at(.vars = vars(matches(cov_regex)),
            .funs = function(x) ifelse(is.na(x), 0, x)
  ) 

cover3 <- cover2 %>%
  mutate_at(.vars = vars(matches("^cover\\d+_0.5km$")),
            .funs = function(x) x/cover2$RowSum_0.5km) %>%  # turning into proportion
  mutate_at(.vars = vars(matches("^cover\\d+_3km$")),
            .funs = function(x) x/cover2$RowSum_3km)


# load slope data ---------------------------------------------------------


# mean slope by polgyon (from gee)
slope1 <- read_csv("data/buf1_mean_slope_by_site.csv")
slope1_3km <- read_csv("data/buf2_mean_slope_by_site.csv")

slope1b <- slope1 %>% 
  select(site_no, mean) %>% 
  rename(slope_0.5km = mean)

slope1b_3km <- slope1_3km %>% 
  select(site_no, mean) %>% 
  rename(slope_3km = mean)

slope2 <- inner_join(slope1b_3km, slope1b, by = "site_no")

plot(slope_0.5km ~ slope_3km, data = slope2)

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
pred_vars <- c("slope_0.5km", "slope_3km", cover_vars)
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
with(all1_m_cover, cor(result_va_m, slope_0.5km))
with(all1_m_cover, cor(result_va_m, slope_3km))




# train/test data ---------------------------------------------------------

all2 <- all1_m_cover
all2_scaled <- all1_m_cover %>% 
  mutate_at(vars(pred_vars),
            function(x) as.numeric(scale(x))) 


n <- nrow(all2)
vec <- 1:n
set.seed(1234)
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
  cov_buf1 <- paste0("cover", class, "_0.5km")
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

lm_pred <- predict(lm1, all2_test)

null_mod <- lm(result_va_m ~ 1, data = all2_train)
null_pred <- predict(null_mod, all2_test)

preds <- tibble(null_pred, lm_pred) # df of prediction

# lasso -------------------------------------------------------------------

# caclulating best lambda
lambda_seq <- 10^seq(2, -4, by = -.1)
set.seed(75)
cv_output <- cv.glmnet(x = as.matrix(all2_train[, -1]), 
                       y = as.matrix(all2_train[, 1]),
                       alpha = 1, 
                       lambda = lambda_seq)

# lambda that gives best cross validated error. 
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified

lasso_best <- glmnet(x = as.matrix(all2_train[, -1]), 
                     y = as.matrix(all2_train[, 1]),
                     lambda = best_lam)
preds$lasso_pred <- predict(lasso_best, s = best_lam, 
                      newx = as.matrix(all2_test[, -1]))

# cart --------------------------------------------------------------------

tree1.full <- rpart(result_va_m ~ ., data = all2_train, method = "anova", 
                    control = rpart.control(cp=0.0,minsplit=2)) # full grown tree. 
#plot(VETH.full)

# according to 1 SE rule tree of size 4 (or maybe 3) is optimal. 
par(mfrow = c(1, 1))
plotcp(tree1.full, 
       xlim = c(0, 15)) 

tree.pruned <- rpart(result_va_m ~ ., data = all2_train, method = "anova", 
                    control = rpart.control(cp=0.0056))

wide <- 6
png("figures/pruned_tree1.png",
    width = 4, height = 3, units = "in", res = 600)
par(oma = c(0,0,0,0), mar = c(2,2,2,2), xpd = NA)
plot(tree.pruned)
text(tree.pruned, use.n = TRUE)
dev.off()
preds$tree_pred <- predict(tree.pruned, all2_test)

# random forest -----------------------------------------------------------

rf1 <- randomForest(result_va_m ~ ., data = all2_train, 
                         importance = TRUE, keep.forest = TRUE) # fit model, 
# importance = true so can evaluate variable importance

png("figures/ranforest_varImplot1.png",
    width = 8, height = 8, units = "in", res = 600)
par(mfrow = c(1, 1))
varImpPlot(rf1, scale = FALSE,
           main = "") 
dev.off()

png("figures/ranforest_partialPlot.png",
    width = 8, height = 4, units = "in", res = 600)

par(mfrow = c(1, 2))
partialPlot(rf1, pred.data =  as.data.frame(all2_train), slope_3km,
            main = "",
            ylab = "Predicted dissolved oxygen (mg/L)",
            xlab = "Mean slope within 3 km (degrees)")

partialPlot(rf1, pred.data =  as.data.frame(all2_train), cover41_3km,
            main = "",
            ylab = "Predicted dissolved oxygen (mg/L)",
            xlab = "Proportion of deciduous forest within 3 km")
dev.off()

preds$forest_pred <- predict(rf1, all2_test)

# calculating MSE ---------------------------------------------------------

preds$y <- all2_test$result_va_m

pred_names <- names(preds)[str_detect(names(preds), "_pred$")]

mse <- tibble(model = pred_names,
              mse = NA_real_)

for (col in pred_names) {
  sse <- sum((preds$y-preds[[col]])^2)
  mse[mse$model == col, ]$mse <- sse/length(preds$y)
}
mse
