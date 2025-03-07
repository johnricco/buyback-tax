#------------------------------------------------------------------------------
# main.R 
# 
# Entry point into program
#------------------------------------------------------------------------------

library(tidyverse)
library(Hmisc)
source('./src/calc.R')

# Set baseline parameter values which don't vary with TCJA law
baseline_params = expand_grid(
  r             = 0.0821 - 0.0224, 
  pi            = 0.0224,
  m             = 0.5, 
  phi           = c(0, 0.6, 1),
  share_taxable = c(0, 0.5842, 1), 
  share_death   = 0.4316, 
  n             = 9.1096 * (1 - 0.4316 - 0.0286) / (1 - 0.4316) + 0.345 * 0.0286 / (1 - 0.4316), 
  tau_bb        = seq(0, 0.1, 0.005),
  tau_div       = 0.2041, 
  tau_kg        = 0.2041
)

# Read scenario-specific parameters
scenario_params = read_csv('./resources/scenario_values.csv')

# Create overall table of parameters
params = scenario_params %>% 
  expand_grid(baseline_params)


#-------------------------
# Firm-level calculations
#-------------------------

# Calculate all possible before-tax rates of return, including overall asset-weighted average
params_with_rho = params %>% 
  calc_rho() %>% 
  bind_rows(
    (.) %>% 
      group_by(law, legal_form, financing, tau_bb, phi, share_taxable) %>% 
      summarise(
        asset_type = 'All', 
        across(
          .cols = -c(asset_type, asset_share), 
          .fns  = ~ weighted.mean(., asset_share)
        ), 
        asset_share = 1,
        .groups = 'drop'
      )
  ) %>% 
  arrange(law, legal_form, desc(financing), asset_type) 


#-------------------
# METR calculations
#-------------------

# Overall METR 
params_with_rho %>% 
  filter(tau_bb == 0.01, phi == 0.6) %>%
  calc_metr('avg') %>% 
  filter(asset_type == 'All') %>% 
  select(law, legal_form, financing, wedge, metr)


# Effect of buyback tax
params_with_rho %>%
  calc_metr('avg') %>% 
  filter(
    phi == 0.6, 
    share_taxable == 0.5842,
    tau_bb %in% c(0, 0.01),
    legal_form == 'corp', 
    financing  == 'equity',
    asset_type == 'All'
  ) %>% 
  select(law, name = tau_bb, value = metr) %>% 
  pivot_wider() %>% 
  mutate(delta = `0.01` - `0`)


# Buyback-dividend differential
params_with_rho %>% 
  filter(
    phi == 0 | phi == 1,
    law        == 'current_law', 
    legal_form == 'corp', 
    financing  == 'equity',
    asset_type == 'All'
  ) %>% 
  calc_metr(type = 'bb_diff') %>% 
  mutate(name = if_else(phi == 0, 'div', 'bb')) %>% 
  select(tau_bb, share_taxable, name, value = metr) %>% 
  pivot_wider() %>% 
  mutate(delta = bb - div) %>% 
  print(n = 65)


# Debt-equity differential  
params_with_rho %>% 
  filter(
    phi           == 0.6, 
    share_taxable == 0.5842, 
    tau_bb      %in% c(0, 0.01), 
    legal_form    == 'corp', 
    financing     != 'avg', 
    asset_type    == 'All'
  ) %>% 
  calc_metr('avg') %>% 
  select(law, name = financing, tau_bb, value = metr) %>% 
  pivot_wider() %>% 
  mutate(differential = debt - equity)
  

# Legal form differential
params_with_rho %>% 
  filter(
    phi           == 0.6, 
    share_taxable == 0.5842, 
    tau_bb      %in% c(0, 0.01), 
    financing     != 'debt', 
    asset_type    == 'All'
  ) %>% 
  calc_metr('avg') %>% 
  select(law, name = legal_form, financing, tau_bb, value = metr) %>% 
  pivot_wider() %>% 
  mutate(differential = corp - pt)
  

# Effect of buyback tax on different assets' METR
by_asset = params_with_rho %>% 
  filter(
    phi           == 1, 
    share_taxable == 0.5842, 
    tau_bb      %in% c(0, 0.01),  
    legal_form    == 'corp',
    financing     == 'equity', 
    asset_type    != 'All'
  ) %>% 
  calc_metr('bb_diff') %>% 
  select(law, financing, asset_type, name = tau_bb, value = metr) %>% 
  pivot_wider() %>% 
  mutate(bb_tax_effect = `0.01` - `0`) %>% 
  group_by(law) %>% 
  arrange(-bb_tax_effect, .by_group = T) %>% 
  ungroup()

by_asset %>% 
  left_join(
    params %>% 
      distinct(asset_type, asset_share), 
    by = 'asset_type'
  ) %>% 
  group_by(law) %>% 
  summarise(
    stdev_without = weighted.var(`0`,    asset_share) ^ 0.5,
    stdev_with    = weighted.var(`0.01`, asset_share) ^ 0.5,
  )

