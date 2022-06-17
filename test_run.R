# Things to check
# > Effects of date (day-of-year) and time?
# > Different slopes by habitat type?

### Load packages ----
library(sf)
library(tidyverse)
library(brms)

### Functions ----
FuncDetectTable <- function(dat, colname, event_cov = TRUE) {
  dat %>% 
    dplyr::group_by(get(colname)) %>% 
    dplyr::summarize(
      num_locs = ifelse(event_cov, NA, length(unique(location_name))),
      num_surveys = n(),
      avg_detect = sum(sum_indiv)/num_surveys)
}

FuncDetectPlot <- function(spec_code, dat, colname) {
  ggplot(dat, aes_string(x = colname, y = "sum_indiv")) +
    geom_violin(scale = "count", fill = "lightblue", alpha = 0.5) +
    stat_summary(fun = "mean", geom = "crossbar",  width = 0.75, fatten = 3, colour = "red") +
    stat_summary(fun = "median", geom = "crossbar",  width = 0.75, fatten = 3, colour = "red", linetype = "dashed") +
    geom_jitter(height = 0.2, width = 0.4, alpha = 0.25) +
    labs(title = paste0(spec_code, " detections, by ", colname), y = "Detections", subtitle = "Each (jittered) point represents a point count survey.\nRed solid line shows average detections/survey; dashed line shows median.") +
    theme_bw(base_size = 12)
}

FuncDetectTimePlot <- function(spec_code, dat, colname) {
  ggplot(dat, aes_string(x = "yr_visit", y = "sum_indiv")) +
    geom_violin(scale = "count", aes(fill = factor(yr)), alpha = 0.35) +
    stat_summary(fun = "mean", geom = "crossbar",  width = 0.75, fatten = 2, colour = "red") +
    stat_summary(fun = "median", geom = "crossbar",  width = 0.75, fatten = 2, colour = "red", linetype = "dashed") +
    geom_jitter(height = 0.2, width = 0.4, alpha = 0.25) +
    ggthemes::scale_fill_colorblind(name = "Year") +
    labs(title = paste0(spec_code, " detections, by ", colname), y = "Detections", subtitle = "Each (jittered) point represents a point count survey.\nRed solid line shows average detections/survey; dashed line shows median.") +
    theme_bw(base_size = 12) +
    facet_wrap(vars(get(colname)))
}

FuncDetectCorrPlot <- function(spec_code, dat, colname) {
  ggplot(dat, aes_string(x = colname, y = "sum_indiv")) +
    geom_count() +
    scale_size_area() +
    geom_smooth() +
    labs(title = paste0(spec_code, " detections, by ", colname), y = "Detections", subtitle = "Point size is scaled to number of point count surveys.") +
    theme_bw(base_size = 12)
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

### Subset the data ----
tt<-df_full_obs %>% dplyr::filter(species_code == "TUTI") %>%
  dplyr::left_join(df_locs[c("location_name", "physiognomy", "hab_type")], by = "location_name") %>%
  dplyr::left_join(df_survey_conditions[c("location_name", "event_date", "weather_wind", "weather_temperature", "weather_sky", "weather_noise")], by = c("location_name", "event_date"))

### Summary tables and figures ----

# Distribution of habitats across park units
table(tt$unit_code, tt$physiognomy) # distribution of physiog across parks
table(tt$unit_code, tt$hab_type) # distribution of hab_type across parks. BITH and GUIS-MS only have forest. GUIS-FL has the highest % of open.

# Detection counts vs park unit
FuncDetectTable(tt, "unit_code", event_cov = FALSE)
FuncDetectPlot(spec_code = "TUTI", tt, colname = "unit_code")
FuncDetectTimePlot(spec_code = "TUTI", tt, colname = "unit_code")

# Examine detection counts vs point location or event covariates
FuncDetectTable(tt, "physiognomy", event_cov = FALSE)
FuncDetectPlot(spec_code = "TUTI", tt, colname = "physiognomy")

FuncDetectTable(tt, "hab_type", event_cov = FALSE)
FuncDetectPlot(spec_code = "TUTI", tt, colname = "hab_type")
FuncDetectTimePlot(spec_code = "TUTI", tt, colname = "hab_type")

FuncDetectTable(tt, "weather_wind")
FuncDetectPlot(spec_code = "TUTI", tt, colname = "weather_wind")
FuncDetectTable(tt, "weather_wind")

FuncDetectTable(tt, "weather_sky")
FuncDetectPlot(spec_code = "TUTI", tt, colname = "weather_sky")

FuncDetectTable(tt, "weather_noise")

### BRMS ANALYSIS ----
# Model data
# Combine some levels for TUTI
mod_dat <- tt %>%
  dplyr::mutate(yr_base = yr - min(yr, na.rm = TRUE),
weather_wind_comb = case_when(
weather_wind %in% c("0_calm", "1_smoke_drifts") ~ "calm",
weather_wind %in% c("2_light_breeze", "3_constant_breeze", "4_branches_move", "5_trees_sway") ~ "windy"
),
weather_noise_comb = case_when(
weather_noise == "0_low" ~ "low",
weather_noise %in% c("1_moderate", "2_high") ~ "high"
),
hab_type_comb = case_when(
hab_type == "forest" ~ "forest",
hab_type != "forest" ~ "not_forest"
)
) %>%
dplyr::select(sum_indiv, yr_base, unit_code, location_name, hab_type_comb, weather_wind_comb, weather_noise_comb)

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
m1_brm <- update(m0_brm, formula. = ~ . + hab_type_comb)
m1_brm <- add_criterion(m1_brm, criterion = c("loo", "waic"), force_save = TRUE)

# ...noise
m2_brm <- update(m0_brm, formula. = ~ . + weather_noise_comb)
m2_brm <- add_criterion(m2_brm, criterion = c("loo", "waic"), force_save = TRUE)

# ...wind
m3_brm <- update(m0_brm, formula. = ~ . + weather_wind_comb)
m3_brm <- add_criterion(m3_brm, criterion = c("loo", "waic"), force_save = TRUE)

# ...wind and noise
m4_brm <- update(m0_brm, formula. = ~ . + weather_wind_comb + weather_noise_comb)
m4_brm <- add_criterion(m4_brm, criterion = c("loo", "waic"), force_save = TRUE)

# ... habitat type and noise
m5_brm <- update(m0_brm, formula. = ~ . + hab_type_comb + weather_noise_comb)
m5_brm <- add_criterion(m5_brm, criterion = c("loo", "waic"), force_save = TRUE)

# ... habitat type and wind
m6_brm <- update(m0_brm, formula. = ~ . + hab_type_comb + weather_wind_comb)
m6_brm <- add_criterion(m6_brm, criterion = c("loo", "waic"), force_save = TRUE)

# ... habitat type, wind and noise
m7_brm <- update(m0_brm, formula. = ~ . + hab_type_comb + weather_wind_comb + weather_noise_comb)
m7_brm <- add_criterion(m7_brm, criterion = c("loo", "waic"), force_save = TRUE)


## Evaluate BRMS models
mod = m4_brm 
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
pp_check(mod, type = "bars")
pp_check(mod, type = "bars_grouped", group = "unit_code")
pp_check(mod, type = "bars_grouped", group = "yr_base")
pp_check(mod, type = "violin_grouped", group = "unit_code")

## Plot conditional effects
plot(conditional_effects(mod, effects = "unit_code"), points=TRUE, point_args = list(width = 0.3, height = 0.1, alpha = 0.4))

# Save conditional effects plots as list, for use with ggplot functions
p <- plot(conditional_effects(mod), points=TRUE, point_args = list(width = 0.3, height = 0.1, alpha = 0.4), plot = FALSE)
p$unit_code + theme_bw(base_size = 14)
p$`yr_base:unit_code`
p$`yr_base:unit_code` + facet+wrap(vars(unit_code))
p$`yr_base:unit_code` + facet_wrap(vars(unit_code))

## Look for high leverage points
plot(loo(mod), label_points = TRUE)

## Compare BRMS models
# To get a table with model weights (assumes you've added the IC to the model, as shown above)...
mod_list <- list(m0_brm = m0_brm, m1_brm = m1_brm, m2_brm = m2_brm, m3_brm = m3_brm)
AICwts_vec = model_weights(m0_brm, m1_brm, m2_brm, m3_brm, weights = "loo") # assumes you've used 'add_ic' to append LOO-IC to each model
stackwts_vec = model_weights(m0_brm, m1_brm, m2_brm, m3_brm, weights = "stacking")
FuncLOOTable_BRM(mod_list = mod_list, AICwts_vec = AICwts_vec, stackwts_vec = stackwts_vec)

## BRMS UPSHOT
# Had problems w/model when I used the original covariates
# interesting! once I included unit_code, the noise effect was not significant. table of unit code vs noise shows that JELA (which had highest densities) also had lowest noise surveys. So should plot out noise effect by unit code separately.
# BUT...NOTE THE RANDOM EFFECT DISTRIBUTION HIGHLY NON-NORMAL, PROBABLY NEED TO FIX SOMETHING WITH THAT...
table(tt$unit_code, tt$weather_noise)

ggplot(tt, aes_string(x = "weather_noise", y = "sum_indiv", fill = "unit_code")) +
  geom_violin(scale = "count", alpha = 0.5) +
  stat_summary(fun = "mean", geom = "crossbar",  width = 0.75, fatten = 3, colour = "red") +
  stat_summary(fun = "median", geom = "crossbar",  width = 0.75, fatten = 3, colour = "red", linetype = "dashed") +
  geom_jitter(height = 0.2, width = 0.4, alpha = 0.25) +
  labs(title = "TUTI detections, by weather_noise and unit_code", y = "Detections", subtitle = "Each (jittered) point represents a point count survey.\nRed solid line shows average detections/survey; dashed line shows median.") +
  theme_bw(base_size = 12) + 
  facet_wrap(vars(unit_code))





# # Examine data on habitat maps
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
