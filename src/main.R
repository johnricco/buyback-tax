#------------------------------------------------------------------------------
# main.R 
# 
# Entry point into program
#------------------------------------------------------------------------------

library(tidyverse)
source('./src/calc.R')

# Set baseline parameter values which don't vary with TCJA law
baseline_params = tibble(
  r             = 0.0821 - 0.0224, 
  pi            = 0.0224,
  m             = 0.5, 
  phi           = 0.6, 
  share_taxable = 0.42, 
  share_death   = 0.4316, 
  n             = 9.1096 * (1 - 0.4316 - 0.0286) / (1 - 0.4316) + 0.345 * 0.0286 / (1 - 0.4316), 
  tau_bb        = 0.01,
  tau_div       = 0.2041, 
  tau_kg        = 0.2041
)

# Read scenario-specific parameters
scenario_params = read_csv('./resources/scenario_values.csv')

# Create table of parameters
params = scenario_params %>% 
  expand_grid(baseline_params)


#--------------
# Calculations
#--------------

# Overall METR 
params %>% 
  calc_metr('avg') %>% 
  group_by(
    law, legal_form, financing
  ) %>% 
  summarise(
    metr = weighted.mean(metr, share)
  )


# Effect of buyback tax
params %>%
  filter(legal_form == 'corp', financing == 'equity') %>% 
  calc_bb_tax_effect() %>% 
  group_by(law) %>% 
  summarise(
    across(
      .cols = c('metr_with', 'metr_without', 'bb_tax_effect'), 
      .fns  = ~ weighted.mean(., share) 
    ) 
  )


# Buyback-dividend differential
bb_differential = tibble(tau_bb = seq(0, 0.04, 0.001)) %>% 
  calc_sensitivity(type = 'bb_diff') %>% 
  filter(law == 'current_law', legal_form == 'corp', financing == 'equity') %>% 
  group_by(tau_bb) %>% 
  summarise(
    across(
      .cols = c('metr_bb', 'metr_div', 'bb_differential'), 
      .fns  = ~ weighted.mean(., share) 
    ), 
    .groups = 'drop'
  )

bb_differential %>%
  filter(tau_bb %in% c(0, 0.01)) %>% 
  mutate(percent_change = 1 - bb_differential / lag(bb_differential))

# Buyback-dividend differential across taxable status
bb_differential_taxable_status = expand_grid(tau_bb = seq(0, 0.1, 0.001), share_taxable = c(0, 1)) %>% 
  calc_sensitivity(type = 'bb_diff') %>% 
  filter(law == 'current_law', legal_form == 'corp', financing == 'equity') %>% 
  group_by(taxable = share_taxable == 1, tau_bb) %>% 
  summarise(
    across(
      .cols = c('metr_bb', 'metr_div', 'bb_differential'), 
      .fns  = ~ weighted.mean(., share) 
    ), 
    .groups = 'drop'
  ) 

bb_differential_taxable_status %>%
  filter(tau_bb %in% c(0, 0.01))



# Debt-equity differential  
calc_sensitivity(NULL, 'avg') %>% 
  filter(legal_form == 'corp', financing != 'avg') %>%
  group_by(
    law, financing
  ) %>% 
  summarise(
    across(
      .cols = c('metr_without', 'metr_with'), 
      .fns  = ~ weighted.mean(., share) 
    ), 
    .groups = 'drop'
  ) %>% 
  pivot_longer(cols = starts_with('metr')) %>% 
  pivot_wider(names_from = financing) %>% 
  mutate(debt_advantage = equity - debt) %>% 
  group_by(law) %>%
  mutate(
    bb_tax_effect_pp  = debt_advantage - lag(debt_advantage),
    bb_tax_effect_pct = debt_advantage / lag(debt_advantage) - 1
  )


# Legal form differential
calc_sensitivity(NULL, 'avg') %>% 
  filter(financing == 'avg') %>%
  group_by(
    law, legal_form
  ) %>% 
  summarise(
    across(
      .cols = c('metr_without', 'metr_with'), 
      .fns  = ~ weighted.mean(., share) 
    ), 
    .groups = 'drop'
  ) %>% 
  pivot_longer(cols = starts_with('metr')) %>% 
  pivot_wider(names_from = legal_form) %>% 
  mutate(pt_advantage = corp - pt) %>% 
  group_by(law) %>%
  mutate(
    bb_tax_effect_pp  = pt_advantage - lag(pt_advantage),
    bb_tax_effect_pct = pt_advantage / lag(pt_advantage) - 1
  )
  

# Effect of buyback tax on different assets' METR
calc_sensitivity(NULL, 'avg') %>% 
  filter(law == 'current_law', legal_form == 'corp', financing != 'debt') %>%
  select(financing, asset_type, metr_with, metr_without, bb_tax_effect) %>% 
  mutate(bb_tax_effect_pct = bb_tax_effect / metr_with) %>% 
  group_by(financing) %>% 
  arrange(-bb_tax_effect, .by_group = T)
  





