#------------------------------------------------------------------------------
# main.R 
# 
# Entry point into program
#------------------------------------------------------------------------------

library(tidyverse)
source('./src/calc.R')

# Read depreciation parameters
depreciation_params = read_csv('./resources/depreciation.csv')

# Set baseline parameter values
baseline_param_values = tibble(
  y             = 0,
  r             = 0.0821 - 0.0224, 
  pi            = 0.0224, 
  delta         = depreciation_params %>% filter(asset_type == 'All') %>% pull(delta), 
  z             = depreciation_params %>% filter(asset_type == 'All') %>% pull(z),  
  b             = 0.958,
  m             = 0.5, 
  phi           = 0.6, 
  share_taxable = 0.42, 
  share_death   = 0.4316, 
  n             = 9.1096 * (1 - 0.4316 - 0.0286) / (1 - 0.4316) + 0.345 * 0.0286 / (1 - 0.4316), 
  tau_corp      = 0.21, 
  tau_bb        = 0.01,
  tau_div       = 0.2041, 
  tau_kg        = 0.2041, 
  tau_i         = 0.2816
)

# Baseline overall METR on equity-financed investment
baseline_param_values %>% 
  calc_metr('avg') %>% 
  pivot_longer(everything()) %>% 
  print(n = 30)

# Effect of buyback tax on equity-finance METR
baseline_param_values %>%
  calc_bb_tax_effect() %>% 
  pivot_longer(cols = everything()) %>% 
  print(n = 20)

# Buyback-dividend differential
tibble(tau_bb = seq(0, 0.04, 0.01)) %>% 
  calc_sensitivity(type = 'bb_diff') %>% 
  pivot_longer(cols = -tau_bb) %>% 
  pivot_wider(names_from = tau_bb) %>% 
  tail(3)

# Debt-equity differential  
tibble(tau_bb = c(0, 0.01)) %>% 
  calc_sensitivity(type = 'debt_diff') %>% 
  pivot_longer(cols = -tau_bb) %>% 
  pivot_wider(names_from = tau_bb) %>% 
  tail(3)

# Effect of buyback tax on different asset's METRs
depreciation_params %>%
  select(-asset_type) %>% 
  calc_sensitivity('avg') %>% 
  mutate(asset_type = depreciation_params$asset_type, .before = everything()) %>% 
  arrange(-bb_tax_effect) %>%
  mutate(bb_tax_effect_relative = bb_tax_effect / metr_without) %>% 
  pivot_longer(cols = -asset_type) %>% 
  pivot_wider(names_from = asset_type) %>% 
  tail(4)





