# CORRECTED VERSION (FILTERS)
# CORRECTED VERSION (ADDING 2022 + )
# CORRECTED VERSION OF LOG-ASSETS (ATQ)
# OPTIMIZED VERSION IN CODE

rm(list=ls())

# -------------------------------
# 1. Libraries
# -------------------------------

install.packages("fixest")
install.packages("car")
install.packages("plm")
install.packages("dplyr")
install.packages("modelsummary")
library(readr)
library(dplyr)
library(fixest)
library(plm)
library(car)
library(modelsummary) 

# -------------------------------
# 2. Function for all industries
# -------------------------------

run_panel_allfirms_models <- function(filepath, year_range, label_prefix = "") {
  panel_all <- read_csv(filepath) %>%
    mutate(across(where(is.character), ~ replace(., tolower(.) %in% c("na", "nan", "none", ""), NA))) %>%
    mutate(across(c("PRisk", "volatility_std", "log_assets", "SIC"), as.numeric),
           year = as.numeric(substr(quarter, 1, 4)),
           sic2 = as.character(floor(SIC / 100)),
           gvkey = as.character(gvkey),
           PRisk_std = (PRisk - mean(PRisk, na.rm = TRUE)) / sd(PRisk, na.rm = TRUE)) %>%
    filter(year >= year_range[1] & year <= year_range[2], !is.na(volatility_std))
  
  m1 <- feols(volatility_std ~ PRisk_std, data = panel_all, cluster = "gvkey")
  m2 <- feols(volatility_std ~ PRisk_std + log_assets, data = panel_all, cluster = "gvkey")
  m3 <- feols(volatility_std ~ PRisk_std + log_assets | quarter, data = panel_all, cluster = "gvkey")
  m4 <- feols(volatility_std ~ PRisk_std + log_assets | quarter + sic2, data = panel_all, cluster = "gvkey")
  m5 <- feols(volatility_std ~ PRisk_std + log_assets | quarter + gvkey, data = panel_all, cluster = "gvkey")
  
  setNames(list(m1, m2, m3, m4, m5),
           paste0(label_prefix, " - (", 1:5, ")"))
}

# -------------------------------
# 3. Function for Automotive sector
# -------------------------------

run_panel_auto_models <- function(filepath, year_range, label_prefix = "") {
  panel_all_auto <- read_csv(filepath) %>%
    mutate(across(where(is.character), ~ replace(., tolower(.) %in% c("na", "nan", "none", ""), NA))) %>%
    mutate(across(c("PRisk", "volatility_std", "log_assets", "SIC"), as.numeric),
           year = as.numeric(substr(quarter, 1, 4)),
           gvkey = as.character(gvkey),
           PRisk_std = (PRisk - mean(PRisk, na.rm = TRUE)) / sd(PRisk, na.rm = TRUE)) %>%
    filter(year >= year_range[1] & year <= year_range[2],
           SIC >= 3711 & SIC <= 3715,
           !is.na(volatility_std))
  
  m1 <- feols(volatility_std ~ PRisk_std, data = panel_all_auto, cluster = "gvkey")
  m2 <- feols(volatility_std ~ PRisk_std + log_assets, data = panel_all_auto, cluster = "gvkey")
  m3 <- feols(volatility_std ~ PRisk_std + log_assets | quarter, data = panel_all_auto, cluster = "gvkey")
  m5 <- feols(volatility_std ~ PRisk_std + log_assets | quarter + gvkey, data = panel_all_auto, cluster = "gvkey")
  
  setNames(list(m1, m2, m3, m5),
           paste0(label_prefix, " - (", c(1, 2, 3, 5), ")"))
}

# -------------------------------
# 4. Running 4 Models
# -------------------------------

ruta_panel <- "/Users/IshaTellez/Library/Mobile Documents/com~apple~CloudDocs/Durham/3. Easter Term/Dissertation/Statistical - PRisk/1. Hassan Variable Rep (ALL firms)/1. Volatility/Statistical Calculation/panel_volatility_with_assets_2002_2025.csv"

# All industries
models_all <- c(
  run_panel_allfirms_models(ruta_panel, c(2002, 2016), "All Firms 2002–16"),
  run_panel_allfirms_models(ruta_panel, c(2017, 2025), "All Firms 2017–25")
)

# Automotive industry
models_auto <- c(
  run_panel_auto_models(ruta_panel, c(2002, 2016), "Auto 2002–16"),
  run_panel_auto_models(ruta_panel, c(2017, 2025), "Auto 2017–25")
)

# -------------------------------
# 5. Summary tables
# -------------------------------

modelsummary(
  models_all,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  statistic = "std.error",
  coef_map = c(
    "PRisk_std" = "Political Risk (std.)",
    "log_assets" = "Log of Assets",
    "(Intercept)" = "Constant"
  ),
  title = "Panel B – Volatility ~ PRisk (All Firms)"
)

modelsummary(
  models_auto,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  statistic = "std.error",
  coef_map = c(
    "PRisk_std" = "Political Risk (std.)",
    "log_assets" = "Log of Assets",
    "(Intercept)" = "Constant"
  ),
  title = "Panel B – Volatility ~ PRisk (Automotive Only)"
)

# -------------------------------
# 6. Checks 
# -------------------------------

panel_test <- read_csv(ruta_panel) %>%
  mutate(across(where(is.character), ~ replace(., tolower(.) %in% c("na", "nan", "none", ""), NA))) %>%
  mutate(across(c("PRisk", "volatility_std", "log_assets", "SIC"), as.numeric),
         year = as.numeric(substr(quarter, 1, 4)),
         gvkey = as.character(gvkey),
         PRisk_std = (PRisk - mean(PRisk, na.rm = TRUE)) / sd(PRisk, na.rm = TRUE))

Checksv <- list(
  "All Firms 2002–2016" = panel_test %>% filter(year >= 2002 & year <= 2016, !is.na(volatility_std)),
  "All Firms 2017–2025" = panel_test %>% filter(year >= 2017 & year <= 2025, !is.na(volatility_std)),
  "Auto 2002–2016" = panel_test %>% filter(year >= 2002 & year <= 2016, SIC >= 3711 & SIC <= 3715, !is.na(volatility_std)),
  "Auto 2017–2025" = panel_test %>% filter(year >= 2017 & year <= 2025, SIC >= 3711 & SIC <= 3715, !is.na(volatility_std))
)

# Loop for checking 
for (label in names(Checksv)) {
  df <- Checksv[[label]]
  cat("\n", label, "\n")
  
  # 1. VIF
  cat("\n[1] VIF:\n")
  print(vif(lm(volatility_std ~ PRisk_std + log_assets, data = df)))
  
  # 2. Within/Between
  cat("\n[2] Variance Within/Between PRisk_std:\n")
  pdata <- pdata.frame(df, index = c("gvkey", "quarter"))
  cat("Within =", round(mean(tapply(pdata$PRisk_std, pdata$gvkey, var, na.rm = TRUE), na.rm = TRUE), 5), "\n")
  cat("Between =", round(var(tapply(pdata$PRisk_std, pdata$gvkey, mean, na.rm = TRUE), na.rm = TRUE), 5), "\n")
  
  # 3. Hausman
  cat("\n[3] Hausman test:\n")
  print(phtest(
    plm(volatility_std ~ PRisk_std + log_assets, data = pdata, model = "within"),
    plm(volatility_std ~ PRisk_std + log_assets, data = pdata, model = "random")
  ))
}
