# Things to check
# > Effects of date (day-of-year) and time?
# > Different slopes by habitat type?

rm(list = ls())

### Load packages ----
library(tidyverse)
library(brms)
library(partykit) # for regression trees
library(detect)
library(magrittr)

### Functions ----
FuncComb <- function(dat) {
  dat %<>%
    dplyr::mutate(yr_base = yr - min(yr, na.rm = TRUE),
                  weather_wind_comb = case_when(
                    weather_wind %in% c("0_calm", "1_smoke_drifts", "2_light_breeze") ~ "calm",
                    weather_wind %in% c( "3_constant_breeze", "4_branches_move", "5_trees_sway") ~ "windy"
                  ),
                  weather_noise_comb = case_when(
                    weather_noise == "0_low" ~ "low",
                    weather_noise %in% c("1_moderate", "2_high") ~ "high"
                  ),
                  hab_type_comb = case_when(
                    hab_type == "forest" ~ "forest",
                    hab_type != "forest" ~ "not_forest"
                  )
    )
}

FuncLOOTable_BRM <- function(mod_list, AICwts_vec, stackwts_vec) {
# Function to generate a LOO summary table
#
# Args:
#   mod_list:  Named list of fitted regression models (with 'add_ic' already applied. Add prior to function so can check and reloo as necessary
#   AICwts_vec:  Pass in a named vector of the weights, as estimated by 'model_weights' with weights = "loo" (AIC weights computed based on LOO-IC)
#   stackwts_vec:  Pass in a named vector of the weights, as estimated by 'model_weights' with weights = "loo2" (stacked weights computed)
#
# Returns:
#   IC summary table with weights
#
temp_list <- lapply(mod_list, function(x) as.numeric(c(x$criteria$loo$estimates[c("looic"),], x$criteria$loo$estimates[c("p_loo"),])))
temp_df <- data.frame(do.call("rbind", temp_list))
temp_summary <- cbind(names(mod_list), temp_df) %>%
mutate_if(is.factor, as.character)
rownames(temp_summary) <- NULL
colnames(temp_summary) = c("model", "looic", "se(looic)", "p-loo", "se(p-loo)")
AICwts_df <- enframe(AICwts_vec)
stackwts_df <- enframe(stackwts_vec)
loo_summary <- temp_summary %>%
full_join(AICwts_df, by = c("model" = "name")) %>%
dplyr::rename(AICweight = value) %>%
full_join(stackwts_df, by = c("model" = "name")) %>%
dplyr::rename(stackingweight = value) %>%
arrange(looic) %>%
mutate_if(is.numeric, round, 2)
return(loo_summary)
}

### Read in formatted data ----
df_locs <- readRDS(here::here("Data_out", "df_locs.RDS")) # site locations
df_survey_conditions <- readRDS(here::here("Data_out", "df_survey_conditions.RDS")) # event covariate data
df_full_obs <- readRDS(here::here("Data_out", "df_full_obs.RDS"))
df_finaldat <- readRDS(here::here("Data_out", "df_finaldat.RDS"))

### Subset the observation summary data ----
tt<-df_full_obs %>% dplyr::filter(species_code == "TUTI") %>%
  dplyr::left_join(df_locs[c("location_name", "physiognomy", "hab_type")], by = "location_name") %>%
  dplyr::left_join(df_survey_conditions[c("location_name", "event_date", "julian_prop", "start_time_interval", "weather_wind", "weather_wind_num", "weather_temperature", "weather_temperature_cs", "weather_sky", "weather_sky_revised_num", "weather_noise", "weather_noise_num")], by = c("location_name", "event_date"))

### Classification trees for exploring covariates ----
tt_comb <- FuncComb(tt) 
tt_tree <- partykit::ctree(sum_indiv ~ as.factor(unit_code) + as.factor(hab_type_comb) + weather_wind_num + weather_sky_revised_num + weather_temperature_cs + weather_noise_num + julian_prop, data = tt_comb)

plot(tt_tree)
# table(predict(tt_tree), tt$sum_indiv)
# unit_code (JELA has most detections) is the main predictor. Habitat type (combined) is next predictor. When habitat type was NOT combined, it did not come out as a predictor. When hab_type_comb is not a predictor, weather_noise is a predictor. Lowest mean detections is for noisy non-JELA surveys in non-forest habitats. From EDA plots, though, marsh (a non-forest habitat) habitats had detections of TUTI as high as forest plots 
# No important predictors if each unit_code evaluated separately.
# NOTE: Table of unit code vs noise shows that JELA (which had highest densities) also had lowest noise surveys, so they are somewhat confounded possibly
# table(tt$unit_code, tt$weather_noise)
# 
ggplot(tt, aes(x = as.factor(yr), y = sum_indiv)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "crossbar",  width = 0.75, fatten = 3, colour = "red") +
  stat_summary(fun = "median", geom = "crossbar",  width = 0.75, fatten = 3, colour = "red", linetype = "dashed") +
  geom_jitter(height = 0.2, width = 0.4, alpha = 0.25) +
  labs(title = "TUTI detections, by weather_noise and unit_code", y = "Detections", subtitle = "Each (jittered) point represents a point count survey.\nRed solid line shows average detections/survey; dashed line shows median.") +
  theme_bw(base_size = 12) +
  facet_wrap(vars(unit_code))

### BRMS ANALYSIS ----
# Model data
# Combine some category levels into meaningful groups, for TUTI. I did also rerun tree classifcation with the combined covariates but still only unit_code was important.
mod_dat <- tt_comb %>%
  dplyr::select(sum_indiv, yr_base, unit_code, location_name, hab_type, hab_type_comb, weather_wind_num, weather_wind_comb, weather_noise_num, weather_noise_comb, julian_prop)

# First, count the number of NA's in different columns b/c that will affect what records are dropped in different models. summary() doesn't count NA's in character columns, so use this instead:
colSums(is.na(mod_dat))

# Dataset with complete cases only
mod_dat_complete <- mod_dat[complete.cases(mod_dat),]
colSums(is.na(mod_dat_complete))

## BRMS models
# Simplest model to try
m0_brm <- brm(sum_indiv ~ yr_base*unit_code + (1|location_name),
              data = mod_dat_complete,
              family = poisson,
              iter = 4000, warmup = 1000, chains = 4, cores = 4,
              file = here::here("Model_fits", "TUTI_m0")
              )
m0_brm <- add_criterion(m0_brm, criterion = c("loo", "waic"), force_save = TRUE)

# ... habitat type
m1_brm <- update(m0_brm, formula. = ~ . + hab_type_comb, newdata = mod_dat_complete, file = here::here("Model_fits", "TUTI_m1"))
m1_brm <- add_criterion(m1_brm, criterion = c("loo", "waic"), force_save = TRUE)

# ... habitat type with PRIORS
m1_priors_brm <- brm(sum_indiv ~ yr_base*unit_code + hab_type_comb + (1|location_name),
                     data = mod_dat_complete,
                     family = poisson,
                     prior = c(prior(normal(0, 100), class = Intercept), 
                               prior(normal(0, 1), class = b)), 
                     iter = 4000, warmup = 1000, chains = 4, cores = 4,
                     file = here::here("Model_fits", "TUTI_m1_priors_brm")
)
m1_priors_brm <- add_criterion(m1_priors_brm, criterion = c("loo", "waic"), force_save = TRUE)


# ...noise
m2_brm <- update(m0_brm, formula. = ~ . + weather_noise_num, newdata = mod_dat_complete, file = here::here("Model_fits", "TUTI_m2"))
m2_brm <- add_criterion(m2_brm, criterion = c("loo", "waic"), force_save = TRUE)

# ...wind
m3_brm <- update(m0_brm, formula. = ~ . + weather_wind_comb, newdata = mod_dat_complete, file = here::here("Model_fits", "TUTI_m3"))
m3_brm <- add_criterion(m3_brm, criterion = c("loo", "waic"), force_save = TRUE) # no divergence probs

# ...wind and noise
m4_brm <- update(m0_brm, formula. = ~ . + weather_wind_comb + weather_noise_num, newdata = mod_dat_complete, file = here::here("Model_fits", "TUTI_m4"))
m4_brm <- add_criterion(m4_brm, criterion = c("loo", "waic"), force_save = TRUE)

# ... habitat type and noise
m5_brm <- update(m0_brm, formula. = ~ . + hab_type_comb + weather_noise_num, newdata = mod_dat_complete, file = here::here("Model_fits", "TUTI_m5"))
m5_brm <- add_criterion(m5_brm, criterion = c("loo", "waic"), force_save = TRUE)

# ... habitat type and wind
m6_brm <- update(m0_brm, formula. = ~ . + hab_type_comb + weather_wind_comb, newdata = mod_dat_complete, file = here::here("Model_fits", "TUTI_m6"))
m6_brm <- add_criterion(m6_brm, criterion = c("loo", "waic"), force_save = TRUE)

# ... habitat type, wind and noise
m7_brm <- update(m0_brm, formula. = ~ . + hab_type_comb + weather_wind_comb + weather_noise_num, newdata = mod_dat_complete,
file = here::here("Model_fits", "TUTI_m7"))
m7_brm <- add_criterion(m7_brm, criterion = c("loo", "waic"), force_save = TRUE)

# ... julian date. Trying this one b/c best model includes habitat and second best includes habitat and noise.
m8_brm <- update(m0_brm, formula. = ~ . + julian_prop, newdata = mod_dat_complete, file = here::here("Model_fits", "TUTI_m8"))
m8_brm <- add_criterion(m8_brm, criterion = c("loo", "waic"), force_save = TRUE)

## Evaluate BRMS models
mod = m6_brm 
summary(mod) # slightly shorter summary of model results

# Coefficient plot
mcmc_plot(mod) # quick coefficient plot


# Model convergence check
plot(mod) # plots showing posterior distribution of parameters, and trace plots

# Posterior predictive checks
pp_check(mod, type = "dens_overlay")
pp_check(mod, type = "stat_2d")
pp_check(mod, type = "loo_pit_qq")
pp_check(mod, type = "rootogram")
pp_check(mod, type = "bars_grouped", group = "unit_code")
pp_check(mod, type = "bars_grouped", group = "yr_base")
pp_check(mod, type = "violin_grouped", group = "unit_code")

## Plot conditional effects
# plot(conditional_effects(mod, effects = "unit_code"), points=TRUE, point_args = list(width = 0.3, height = 0.1, alpha = 0.4))

# Save conditional effects plots as list, for use with ggplot functions
p <- plot(conditional_effects(mod), points=TRUE, point_args = list(width = 0.3, height = 0.1, alpha = 0.4), plot = FALSE)
p$unit_code + theme_bw(base_size = 14)
p$`yr_base:unit_code` + theme_bw(base_size = 14) + facet_wrap(vars(unit_code))

## Look for high leverage points
plot(loo(mod), label_points = TRUE)

## Compare BRMS models
# To get a table with model weights (assumes you've added the IC to the model, as shown above)...
mod_list <- list(m0_brm = m0_brm, m1_priors_brm = m1_priors_brm, m3_brm = m3_brm, m6_brm = m6_brm)
AICwts_vec = model_weights(m0_brm, m1_priors_brm, m3_brm, m6_brm,  weights = "loo") # assumes you've used 'add_ic' to append LOO-IC to each model
stackwts_vec = model_weights(m0_brm, m1_priors_brm, m3_brm, m6_brm, weights = "stacking")

# mod_list <- list(m0_brm = m0_brm, m1_brm = m1_brm, m1_priors_brm = m1_priors_brm, m2_brm = m2_brm, m3_brm = m3_brm, m4_brm = m4_brm, m5_brm = m5_brm, m6_brm = m6_brm, m7_brm = m7_brm, m8_brm = m8_brm)
# AICwts_vec = model_weights(m0_brm, m1_brm, m1_priors_brm, m2_brm, m3_brm, m4_brm, m5_brm, m6_brm, m7_brm, m8_brm,  weights = "loo") # assumes you've used 'add_ic' to append LOO-IC to each model
# stackwts_vec = model_weights(m0_brm, m1_brm, m1_priors_brm, m2_brm, m3_brm, m4_brm, m5_brm, m6_brm, m7_brm, m8_brm, weights = "stacking")

FuncLOOTable_BRM(mod_list = mod_list, AICwts_vec = AICwts_vec, stackwts_vec = stackwts_vec)

## BRMS UPSHOT
# Best-supported model included hab_type_comb (forest vs. non-forest), next supported also added noise but it's far less supported. This is consistent with classification tree results
# <<<<<<<<<<<<<< PICK UP FROM HERE--BRMS PERFORMS BETTER WITH THE PRIORS, BUT NEED TO CENTER/SCALE EVERYTHING AND SELECT WEAKLY INFORMATIVE PRIORS. INCLUDE TEMPERATURE AS A POLYNOMIAL.

# # Examine data on habitat maps ----
# # THese have physiognomy: GUIS
# # SAAN doesn't even have a veg polygon shapefile
# (map_layers <- st_layers(here::here("Data_in", "guisgeodata.gdb")))
# map_veg <- st_read(here::here("Data_in", "guisgeodata.gdb"), layer = "guis_vegpoly")
# map_veg <- st_transform(map_veg, crs = 4326)
# 
# pal<-colorFactor(palette ="YlGnBu", domain = map_veg$Physiognomy)
# 
# leaflet() %>% 
#   addTiles() %>%
#   addPolygons(data = map_veg, color = "black", weight = 1.25, fillColor = ~pal(Physiognomy), fillOpacity = 0.5)

### SOLYMOS METHOD ----
tt_raw <- df_finaldat %>% dplyr::filter(species_code == "TUTI") %>%
  dplyr::left_join(df_locs[c("location_name", "physiognomy", "hab_type")], by = "location_name") %>%
  dplyr::left_join(df_survey_conditions[c("location_name", "event_date", "weather_wind", "weather_temperature_cs", "weather_sky_revised_num", "weather_noise_num", "weather_noise", "julian_prop", "start_time_interval")], by = c("location_name", "event_date"))
tt_raw_comb <- FuncComb(tt_raw)

# These are the distance classes
unique(tt_raw[c("distance_bin", "distance_bin_id")])

## Testing assumptions
# >>>>> REALLY, TIME INTERVAL SHOULD BE RELATIVE TO SUNRISE <<<<
# >>>>> TEST EFFECT OF JULIAN DAY <<<<<

# ASSUMPTION: Half-normal distribution for distance

# Plot of actual distribution
ggplot(tt_raw, aes(x= distance_bin_id, weight = count)) + geom_bar() # lower detections in closest distance bin BUT this may be appropriate, considering that the closest bins have smaller area. See QPAD book.

# Does it differ by habitat type? YES, KEEP 
ggplot(tt_raw, aes(x= distance_bin_id, weight = count)) + geom_bar() + facet_grid(cols = vars(hab_type)) # the more open the habitat, the less likely birds detected at closer distance classes (but sample sizes are small)

# Does it differ by time interval? YES, KEEP
ggplot(tt_raw, aes(x= distance_bin_id, weight = count)) + geom_bar() + facet_grid(cols = vars(time_bin)) # it seems that the later time periods, observers more likely to record the birds from farther distance classes (perhaps the first birds they detect/record tend to be the ones closer in, then as those are "removed" they detect the ones farther out?)


# ASSUMPTION: Exponential distribution for time since first detection

# Plot of actual distribution YES, KEEP
ggplot(tt_raw, aes(x= time_bin_id, weight = count)) + geom_bar()
# it's really not exponential, perhaps b/c as time goes on it's hard to keep track of which birds have already been counted and also some birds may come into the survey area (immigration, a violation of closure assumption)

# Does it differ by habitat type? Not enough non-forest data
ggplot(tt_raw, aes(x= time_bin_id, weight = count)) + geom_bar() + facet_grid(cols = vars(hab_type)) 

# Does it differ by noise?
ggplot(tt_raw, aes(x= time_bin_id, weight = count)) + geom_bar() + facet_grid(cols = vars(weather_noise)) 

