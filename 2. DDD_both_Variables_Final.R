rm(list=ls())

# -------------------------------
# 1. Library
# -------------------------------
install.packages("gt")
install.packages("ggtext")

library(pretrends)
library(gt)
library(readr)
library(dplyr)
library(fixest)
library(stringi)
library(modelsummary)
library(ggplot2)
library(zoo)
library(ggtext)

route <- "/Users/IshaTellez/Library/Mobile Documents/com~apple~CloudDocs/Durham/3. Easter Term/Dissertation/Statistical - PRisk/1. Hassan Variable Rep (ALL firms)/2. Implied Volatlity/Statistical Analysis/panel_with_lobbying_impvol.csv"
route_vol <- "/Users/IshaTellez/Library/Mobile Documents/com~apple~CloudDocs/Durham/3. Easter Term/Dissertation/Statistical - PRisk/1. Hassan Variable Rep (ALL firms)/1. Volatility/Statistical Calculation/panel_with_lobbying.csv"

main_panel <- read_csv(route, locale = locale(encoding = "UTF-8"))
#head(main_panel)

# Strings cleaning
safe_clean <- function(x){
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x <- stringi::stri_trans_tolower(x)
  x <- ifelse(x %in% c("na","nan","none",""), NA, x)
  return(x)
}
main_panel <- main_panel %>%
  mutate(across(where(is.character), safe_clean))

#----- Panel Vol

main_panel_vol <- read_csv(route_vol, locale = locale(encoding = "UTF-8"))

# -------------------------------
# 1. DD Models : IMPLIED VOLATILITY
# -------------------------------
panel_dd <- main_panel %>%
  filter(hqcountrycode == "us") %>%
  mutate(
    post_usmca = ifelse(quarter >= "2018Q3", 1, 0),
    auto_firm = ifelse(SIC %in% c(3711, 3713, 3714, 3715), 1, 0),
#    us_based = ifelse(hqcountrycode == "US", 1, 0)
  ) %>%
  filter(!is.na(impvol_std), !is.na(log_assets), !is.na(log1p_lobby))

# DD(Auto vs Non Auto)
model_did <- feols(impvol_std ~ auto_firm * post_usmca + log_assets | gvkey + quarter, cluster = "gvkey", data = panel_dd)
#panel_dd %>%
#  count(auto_firm, us_based, post_usmca)

summary(model_did)

# Parallel Trends Assumption DD Models
# Creating K variable for pre-tends events
add_k <- function(df) {
  df %>%
    mutate(
      quarter_fixed = toupper(quarter),
      quarter_date = as.Date(as.yearqtr(quarter_fixed, format = "%YQ%q")),
      k = as.integer((as.yearqtr(quarter_date) - as.yearqtr("2018 Q3")) * 4)
    ) %>%
    filter(!is.na(k), k >= -8, k <= 8)
}
panel_k <- add_k(panel_dd)

did_pretrend <- feols(
  impvol_std ~ i(k, auto_firm, ref = -1) + log_assets |
    gvkey + quarter, cluster = "gvkey", data = panel_k
)

#
coef_data <- as.data.frame(did_pretrend$coeftable)
coef_data$k <- as.numeric(gsub(".*::\\s*(-?\\d+).*", "\\1", rownames(coef_data)))
coef_data$ci_low <- coef_data$Estimate - 1.96 * coef_data$`Std. Error`
coef_data$ci_high <- coef_data$Estimate + 1.96 * coef_data$`Std. Error`
coef_data <- subset(coef_data, !is.na(k) & k >= -8 & k <= 8)

ggplot(coef_data, aes(x = k, y = Estimate)) +
  geom_hline(yintercept = 0,) +
  geom_vline(xintercept = -1, linetype = "dotted", alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                width = 0.1, alpha = 0.6) +
  geom_point(size = 1, fill = "white", stroke = 1) +
  labs(
    title = "Pre-trends (Event Study) for DD Model - Implied Volatility",
    x = "Quarter (k)",
    y = "Coefficient Estimate"
  ) +
  scale_x_continuous(breaks = -8:8) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(family = "Times New Roman", 
                              size = 14, hjust = 0.5),
    axis.title = element_text(family = "Times New Roman", size = 12),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )
  
# Placebo Test DD Implied Volatility
panel_placebo <- panel_dd %>%
  filter(quarter < "2018Q1") %>%
  mutate(placebo_treat = ifelse(quarter >= "2010Q1", 1, 0))

placebo_did <- feols(
  impvol_std ~ auto_firm * placebo_treat + log_assets |
    gvkey + quarter,
  cluster = "gvkey",
  data = panel_placebo
)
summary(placebo_did)

# -------------------------------
# 2. DD Models : REALIZED VOLATILITY
# -------------------------------

panel_dd_vol <- main_panel_vol %>%
  filter(tolower(hqcountrycode) == "us") %>%
  mutate(
    post_usmca = ifelse(quarter >= "2018Q3", 1, 0),
    auto_firm = ifelse(SIC %in% c(3711, 3713, 3714, 3715), 1, 0)
  ) %>%
  filter(!is.na(volatility_std), !is.na(log_assets), !is.na(log1p_lobby))

# DD(Auto vs Non Auto)
model_did_vol <- feols(
  volatility_std ~ auto_firm * post_usmca + log_assets | gvkey + quarter,
  cluster = "gvkey",
  data = panel_dd_vol
)
summary(model_did_vol)

# Parallel Trends Assumption DD Models
add_k <- function(df) {
  df %>%
    mutate(
      quarter_fixed = toupper(quarter),
      quarter_date = as.Date(as.yearqtr(quarter_fixed, format = "%YQ%q")),
      k = as.integer((as.yearqtr(quarter_date) - as.yearqtr("2018 Q3")) * 4)
    ) %>%
    filter(!is.na(k), k >= -8, k <= 8)
}
panel_k_vol <- add_k(panel_dd_vol)

did_pretrend_vol <- feols(
  volatility_std ~ i(k, auto_firm, ref = -1) + log_assets |
    gvkey + quarter, cluster = "gvkey", data = panel_k_vol
)

# Coef table
coef_data_vol <- as.data.frame(did_pretrend_vol$coeftable)
coef_data_vol$k <- as.numeric(gsub(".*::\\s*(-?\\d+).*", "\\1", rownames(coef_data_vol)))
coef_data_vol$ci_low <- coef_data_vol$Estimate - 1.96 * coef_data_vol$`Std. Error`
coef_data_vol$ci_high <- coef_data_vol$Estimate + 1.96 * coef_data_vol$`Std. Error`
coef_data_vol <- subset(coef_data_vol, !is.na(k) & k >= -8 & k <= 8)

# Plot
ggplot(coef_data_vol, aes(x = k, y = Estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -1, linetype = "dotted", alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                width = 0.1, alpha = 0.6) +
  geom_point(size = 1, fill = "white", stroke = 1) +
  labs(
    title = "Pre-trends (Event Study) for DD Model – Realized Volatility",
    x = "Quarter (k)",
    y = "Coefficient Estimate"
  ) +
  scale_x_continuous(breaks = -8:8) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(family = "Times New Roman", size = 14, hjust = 0.5),
    axis.title = element_text(family = "Times New Roman", size = 12),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )

# Placebo Test DD Realized Volatility
panel_placebo_vol <- panel_dd_vol %>%
  filter(quarter < "2018Q1") %>%
  mutate(placebo_treat = ifelse(quarter >= "2010Q1", 1, 0))

placebo_did_vol <- feols(
  volatility_std ~ auto_firm * placebo_treat + log_assets |
    gvkey + quarter,
  cluster = "gvkey",
  data = panel_placebo_vol
)

summary(placebo_did_vol)


## Summary both models
main_model_list <- list(
  "DD baseline (ImpVol)"   = model_did,
  "Placebo test (ImpVol)"  = placebo_did,
  "DD baseline (RealVol)"  = model_did_vol,
  "Placebo test (RealVol)" = placebo_did_vol
)

modelsummary(
  main_model_list,
  statistic = "p.value",
  stars = TRUE,
  vcov = ~gvkey,
  gof_map = c("nobs", "r.squared"),
  output = "gt"
) %>%
  gt::tab_spanner(
    label = "Implied Volatility",
    columns = c("DD baseline (ImpVol)", "Placebo test (ImpVol)")
  ) %>%
  gt::tab_spanner(
    label = "Realized Volatility",
    columns = c("DD baseline (RealVol)", "Placebo test (RealVol)")
  ) %>%
  gt::opt_table_font(
    font = list(
      gt::google_font("Times New Roman"),
      gt::default_fonts()
    )
  )

# END OF TRANSCRIPT FOR
# DID MODELS
# BOTH REALIZED & IMPLIED VOLATILITY
###########################################################

# ----------------------------------------
# 3. DDD Models: IMPLIED VOLATILITY
# ----------------------------------------
# AUTO:USMCA:HIGH_PRISK

panel_us <- main_panel %>%
  filter(hqcountrycode == "us") %>%
  mutate(
    post_usmca = ifelse(quarter >= "2018q3", 1, 0),
    auto_firm = ifelse(SIC %in% c(3711, 3713, 3714, 3715), 1, 0)
  ) %>%
  filter(!is.na(impvol_std), !is.na(log_assets), !is.na(log1p_lobby), !is.na(PRisk))

# calculate PRisk percentiles by firm in pre-USMCA
prisk_groups <- panel_us %>%
  filter(quarter < "2018q3") %>%
  group_by(gvkey) %>%
  summarise(mean_PRisk = mean(PRisk, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    prisk_group = case_when(
      mean_PRisk >= quantile(mean_PRisk, 0.75, na.rm = TRUE) ~ "high",
      mean_PRisk <= quantile(mean_PRisk, 0.25, na.rm = TRUE) ~ "low",
      TRUE ~ "mid"
    )
  )

panel_us <- panel_us %>%
  left_join(prisk_groups %>% select(gvkey, prisk_group), by = "gvkey") %>%
  filter(prisk_group %in% c("high","low")) %>%   # Top 25%
  mutate(high_PRisk = ifelse(prisk_group == "high", 1, 0))

# DDD Model Dummy
model_ddd_HighPRisk <- feols( #PRisk As Dummy
  impvol_std ~ auto_firm * post_usmca * high_PRisk + log_assets |
    gvkey + quarter,
  cluster = "gvkey",
  data = panel_us
)
summary(model_ddd_HighPRisk)

# DDD Model Continuous
model_ddd_PRiskCont <- feols(
  impvol_std ~ auto_firm * post_usmca * PRisk + log_assets |
    gvkey + quarter,
  cluster = "gvkey",
  data = panel_us
)
summary(model_ddd_PRiskCont)


# Event Study
add_k <- function(df) {
  df %>%
    mutate(
      quarter_fixed = toupper(quarter),
      quarter_date = as.Date(as.yearqtr(quarter_fixed, format = "%YQ%q")),
      k = as.integer((as.yearqtr(quarter_date) - as.yearqtr("2018 Q3")) * 4)
    ) %>%
    filter(!is.na(k), k >= -8, k <= 8)
}

panel_k <- add_k(panel_us)

# Dummy PRisk
did_pretrend_highlow <- feols(
  impvol_std ~ i(k, auto_firm*high_PRisk, ref = -1) + log_assets |
    gvkey + quarter, cluster = "gvkey", data = panel_k
)

# Continuous PRisk
did_pretrend_prisk <- feols(
  impvol_std ~ i(k, auto_firm*PRisk, ref = -1) + log_assets |
    gvkey + quarter, cluster = "gvkey", data = panel_k
)

iplot(did_pretrend_highlow,
      main = "Pre-trends: DDD Auto:HighPRisk Implied Volatility",
      xlab = "Quarters from USMCA (2018Q3)")

iplot(did_pretrend_prisk,
      main = "Pre-trends: DDD Auto:PRisk Implied Volatility",
      xlab = "Quarters from USMCA (2018Q3)")

### PLACEBOS
panel_placebo <- panel_us %>%
  filter(quarter < "2018q1") %>%
  mutate(placebo_treat = ifelse(quarter >= "2010q1", 1, 0))

# Dummy
placebo_highlow <- feols(
  impvol_std ~ auto_firm * placebo_treat * high_PRisk + log_assets |
    gvkey + quarter, cluster = "gvkey", data = panel_placebo
)

# Continuous
placebo_prisk <- feols(
  impvol_std ~ auto_firm * placebo_treat * PRisk + log_assets |
    gvkey + quarter, cluster = "gvkey", data = panel_placebo
)

summary(placebo_highlow)
summary(placebo_prisk)

# ----------------------------------------
# 4. DDD Models: REALIZED VOLATILITY
# ----------------------------------------
panel_us_vol <- main_panel_vol %>%
  filter(tolower(hqcountrycode) == "us") %>%
  mutate(
    post_usmca = ifelse(quarter >= "2018q3", 1, 0),
    auto_firm = ifelse(SIC %in% c(3711, 3713, 3714, 3715), 1, 0)
  ) %>%
  filter(!is.na(volatility_std), !is.na(log_assets), !is.na(log1p_lobby), !is.na(PRisk))

# high - low panel
prisk_groups_vol <- panel_us_vol %>%
  filter(quarter < "2018q3") %>%
  group_by(gvkey) %>%
  summarise(mean_PRisk = mean(PRisk, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    prisk_group = case_when(
      mean_PRisk >= quantile(mean_PRisk, 0.75, na.rm = TRUE) ~ "high",
      mean_PRisk <= quantile(mean_PRisk, 0.25, na.rm = TRUE) ~ "low",
      TRUE ~ "mid"
    )
  )

panel_us_vol <- panel_us_vol %>%
  left_join(prisk_groups_vol %>% select(gvkey, prisk_group), by = "gvkey") %>%
  filter(prisk_group %in% c("high","low")) %>%
  mutate(high_PRisk = ifelse(prisk_group == "high", 1, 0))

# Dummy Realvol
model_ddd_HighPRisk_vol <- feols(
  volatility_std ~ auto_firm * post_usmca * high_PRisk + log_assets |
    gvkey + quarter,
  cluster = "gvkey",
  data = panel_us_vol
)

# Continuous Realvol
model_ddd_PRiskCont_vol <- feols(
  volatility_std ~ auto_firm * post_usmca * PRisk + log_assets |
    gvkey + quarter,
  cluster = "gvkey",
  data = panel_us_vol
)

# Event Study
add_k <- function(df) {
  df %>%
    mutate(
      quarter_fixed = toupper(quarter),
      quarter_date = as.Date(as.yearqtr(quarter_fixed, format = "%YQ%q")),
      k = as.integer((as.yearqtr(quarter_date) - as.yearqtr("2018 Q3")) * 4)
    ) %>%
    filter(!is.na(k), k >= -8, k <= 8)
}

panel_k_vol <- add_k(panel_us_vol)

# Dummy Realvol Pre-trends
did_pretrend_highlow_vol <- feols(
  volatility_std ~ i(k, auto_firm*high_PRisk, ref = -1) + log_assets |
    gvkey + quarter,
  cluster = "gvkey",
  data = panel_k_vol
)

# Continuous Realvol Pre-trends
did_pretrend_prisk_vol <- feols(
  volatility_std ~ i(k, auto_firm*PRisk, ref = -1) + log_assets |
    gvkey + quarter,
  cluster = "gvkey",
  data = panel_k_vol
)

# Graphs
iplot(did_pretrend_highlow_vol,
      main = "Pre-trends: DDD Auto:HighPRisk Realized Volatility",
      xlab = "Quarters from USMCA (2018Q3)")

iplot(did_pretrend_prisk_vol,
      main = "Pre-trends: DDD Auto:PRisk Realized Volatility",
      xlab = "Quarters from USMCA (2018Q3)")

# Placebos
panel_placebo_vol <- panel_us_vol %>%
  filter(quarter < "2018q1") %>%
  mutate(placebo_treat = ifelse(quarter >= "2010q1", 1, 0))

#Placebo dummy realvol
placebo_highlow_vol <- feols(
  volatility_std ~ auto_firm * placebo_treat * high_PRisk + log_assets |
    gvkey + quarter,
  cluster = "gvkey",
  data = panel_placebo_vol
)

# Placebo continuos realvol
placebo_prisk_vol <- feols(
  volatility_std ~ auto_firm * placebo_treat * PRisk + log_assets |
    gvkey + quarter,
  cluster = "gvkey",
  data = panel_placebo_vol
)

summary(model_ddd_HighPRisk_vol)
summary(model_ddd_PRiskCont_vol)
summary(placebo_highlow_vol)
summary(placebo_prisk_vol)

# MODEL SUMMARY FOR BOTH VARIABLES

# Dummy

dummy_model_list <- list(
  "DDD: High vs Low PRisk (ImpVol)"   = model_ddd_HighPRisk,
  "Placebo: High vs Low (ImpVol)"     = placebo_highlow,
  "DDD: High vs Low PRisk (RealVol)"  = model_ddd_HighPRisk_vol,
  "Placebo: High vs Low (RealVol)"    = placebo_highlow_vol
)

modelsummary(
  dummy_model_list,
  statistic = "p.value",
  stars = TRUE,
  vcov = ~gvkey,
  gof_map = c("nobs", "r.squared"),
  output = "gt"
) %>%
  gt::tab_spanner(
    label = "Implied Volatility",
    columns = c("DDD: High vs Low PRisk (ImpVol)", "Placebo: High vs Low (ImpVol)")
  ) %>%
  gt::tab_spanner(
    label = "Realized Volatility",
    columns = c("DDD: High vs Low PRisk (RealVol)", "Placebo: High vs Low (RealVol)")
  ) %>%
  gt::opt_table_font(
    font = list(
      gt::google_font("Times New Roman"),
      gt::default_fonts()
    )
  )


# Continuous

continuous_model_list <- list(
  "DDD: PRisk continuous (ImpVol)"   = model_ddd_PRiskCont,
  "Placebo: PRisk continuous (ImpVol)" = placebo_prisk,
  "DDD: PRisk continuous (RealVol)"  = model_ddd_PRiskCont_vol,
  "Placebo: PRisk continuous (RealVol)" = placebo_prisk_vol
)

modelsummary(
  continuous_model_list,
  statistic = "p.value",
  stars = TRUE,
  vcov = ~gvkey,
  gof_map = c("nobs", "r.squared"),
  output = "gt"
) %>%
  gt::tab_spanner(
    label = "Implied Volatility",
    columns = c("DDD: PRisk continuous (ImpVol)", "Placebo: PRisk continuous (ImpVol)")
  ) %>%
  gt::tab_spanner(
    label = "Realized Volatility",
    columns = c("DDD: PRisk continuous (RealVol)", "Placebo: PRisk continuous (RealVol)")
  ) %>%
  gt::opt_table_font(
    font = list(
      gt::google_font("Times New Roman"),
      gt::default_fonts()
    )
  )


# END OF TRANSCRIPT FOR
# DDD MODELS (3x3)
# BOTH REALIZED & IMPLIED VOLATILITY
###########################################################

# ----------------------------------------
# OLS MODELS Imvol and Realvol
# ----------------------------------------

ols_naive_impvol <- feols(
  impvol_std ~ auto_firm + log_assets | gvkey + quarter,
  cluster = "gvkey",
  data = panel_dd
)

# OLS with High PRisk y FE
ols_highprisk_impvol <- feols(
  impvol_std ~ auto_firm + high_PRisk + log_assets | gvkey + quarter,
  cluster = "gvkey",
  data = panel_us
)

summary(ols_highprisk_impvol)

ols_naive_realvol <- feols(
  volatility_std ~ auto_firm + log_assets | gvkey + quarter,
  cluster = "gvkey",
  data = panel_dd_vol
)

ols_highprisk_realvol <- feols(
  volatility_std ~ auto_firm + high_PRisk + log_assets | gvkey + quarter,
  cluster = "gvkey",
  data = panel_us_vol
)

ols_models <- list(
  "OLS Naive (ImpVol)"       = ols_naive_impvol,
  "OLS High PRisk (ImpVol)"  = ols_highprisk_impvol,
  "OLS Naive (RealVol)"      = ols_naive_realvol,
  "OLS High PRisk (RealVol)" = ols_highprisk_realvol
)

modelsummary(
  ols_models,
  statistic = "p.value",
  stars = TRUE,
  vcov = ~gvkey,
  gof_map = c("nobs", "r.squared", "within.r.squared"),
  output = "gt"
) %>%
  gt::tab_spanner(
    label = "Implied Volatility",
    columns = c("OLS Naive (ImpVol)", "OLS High PRisk (ImpVol)")
  ) %>%
  gt::tab_spanner(
    label = "Realized Volatility",
    columns = c("OLS Naive (RealVol)", "OLS High PRisk (RealVol)")
  ) %>%
  gt::opt_table_font(
    font = list(
      gt::google_font("Times New Roman"),
      gt::default_fonts()
    )
  )

