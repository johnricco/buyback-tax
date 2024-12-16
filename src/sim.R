#------------------------------------------------------------------------------
# sim.R
# 
# TODO
#------------------------------------------------------------------------------



baseline_param_values = tibble(
  r             = 0.05, 
  pi            = 0.02, 
  delta         = 0.04, 
  z             = 0.85 ,  
  m             = 0.5, 
  phi           = 0.6, 
  share_taxable = 0.4, 
  share_death   = 0.4, 
  n             = 8, 
  tau_corp      = 0.21, 
  tau_bb        = 0.01,
  tau_div       = 0.238, 
  tau_kg        = 0.238
)



calc_sensitivity = function(new_values) {
  
  #----------------------------------------------------------------------------
  # Calculates tax rate metrics across a user-supplied range of non-baseline 
  # values for one or more parameters, all else equal.
  # 
  # Parameters:
  # - new_values (df) : tibble of new values for one or more parameters
  #   
  # Returns: tibble of tax rate metrics across all combinations of specified 
  #          parameter values (df).
  #----------------------------------------------------------------------------
  
  baseline_param_values %>% 
    select(-all_of(colnames(new_values))) %>% 
    expand_grid(new_values) %>% 
    calc_bb_differential() %>%
    calc_bb_tax_effect() %>% 
    return()
}



get_sensitivity_chart = function(x_values, x_var, y_var, x_lab, title, subtitle) {
  
  #----------------------------------------------------------------------------
  # TODO 
  # 
  # Parameters:
  # - new_values (df) : TODO
  #   
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  x_values %>% 
    expand_grid(share_taxable = c(0, 1)) %>% 
    calc_sensitivity() %>% 
    mutate(share_taxable = if_else(share_taxable == 0, 'Nontaxable', 'Taxable')) %>%
    ggplot(aes(x = !!x_var, y = !!y_var, colour = as.factor(share_taxable))) +
    geom_line() +
    geom_hline(yintercept = 0) +
    theme_bw() + 
    labs(x = x_lab, y = element_blank(), colour = element_blank()) + 
    scale_x_continuous(labels = scales::percent_format()) + 
    scale_y_continuous(labels = scales::percent_format()) + 
    ggtitle(title, subtitle = subtitle)
}


#-------------------------
# Tax differential charts
#-------------------------

# Economic factors 

# r
get_sensitivity_chart(
  x_values = tibble(r = seq(0.01, 0.1, 0.001)), 
  x_var    = quo(r), 
  y_var    = quo(bb_differential), 
  x_lab    = 'Real rate of return', 
  title    = 'Effect of the real rate of return', 
  subtitle = 'Tax differential between buybacks and dividends'
)

# pi
get_sensitivity_chart(
  x_values = tibble(pi = seq(0, 0.1, 0.001)), 
  x_var    = quo(pi), 
  y_var    = quo(bb_differential), 
  x_lab    = 'Inflation rate', 
  title    = 'Effect of inflation', 
  subtitle = 'Tax differential between buybacks and dividends'
)

# delta
get_sensitivity_chart(
  x_values = tibble(delta = seq(0, 1, 0.01)), 
  x_var    = quo(delta), 
  y_var    = quo(bb_differential), 
  x_lab    = 'Depreciation rate', 
  title    = 'Effect of depreciation rate', 
  subtitle = 'Tax differential between buybacks and dividends'
)

# BEHAVIORAL FACTORS

# m
get_sensitivity_chart(
  x_values = tibble(m = seq(0, 1, 0.01)), 
  x_var    = quo(m), 
  y_var    = quo(bb_differential), 
  x_lab    = 'Share of equity financed through retained earnings', 
  title    = 'Effect of financing source', 
  subtitle = 'Tax differential between buybacks and dividends'
)

# n
get_sensitivity_chart(
  x_values = tibble(n = seq(0.01, 30, 0.01)), 
  x_var    = quo(n), 
  y_var    = quo(bb_differential), 
  x_lab    = 'Holding period', 
  title    = 'Effect of holding period', 
  subtitle = 'Tax differential between buybacks and dividends'
)

# share_death
get_sensitivity_chart(
  x_values = tibble(share_death = seq(0, 1, 0.01)), 
  x_var    = quo(share_death), 
  y_var    = quo(bb_differential), 
  x_lab    = 'Share of gains held until dealth', 
  title    = 'Effect of step-up', 
  subtitle = 'Tax differential between buybacks and dividends'
)

# TAX LAW FACTORS

# z
get_sensitivity_chart(
  x_values = tibble(z = seq(0, 1, 0.01)), 
  x_var    = quo(z), 
  y_var    = quo(bb_differential), 
  x_lab    = 'PV of depreciation deductions', 
  title    = 'Effect of PV of depreciation deductions', 
  subtitle = 'Tax differential between buybacks and dividends'
)

# tau_corp
get_sensitivity_chart(
  x_values = tibble(tau_corp = seq(0, 0.35, 0.001)), 
  x_var    = quo(tau_corp), 
  y_var    = quo(bb_differential), 
  x_lab    = 'Corporate tax rate', 
  title    = 'Effect of corporate tax rate', 
  subtitle = 'Tax differential between buybacks and dividends'
)

# tau_bb
get_sensitivity_chart(
  x_values = tibble(tau_bb = seq(0, 0.06, 0.001)), 
  x_var    = quo(tau_bb), 
  y_var    = quo(bb_differential), 
  x_lab    = 'Buyback excise tax rate', 
  title    = 'Effect of buyback excise tax rate', 
  subtitle = 'Tax differential between buybacks and dividends'
)


# tau_div/tau_kg
get_sensitivity_chart(
  x_values = tibble(tau_kg = seq(0.238, 0.434, 0.001), tau_div = seq(0.238, 0.434, 0.001)), 
  x_var    = quo(tau_kg), 
  y_var    = quo(bb_differential), 
  x_lab    = 'Capital gains/dividend rate', 
  title    = 'Effect of capital gains/dividend rate', 
  subtitle = 'Tax differential between buybacks and dividends'
)



#----------------------------------
# Buyback excise tax effect charts
#----------------------------------

# Economic factors 

# r
get_sensitivity_chart(
  x_values = tibble(r = seq(0.01, 0.1, 0.001)), 
  x_var    = quo(r), 
  y_var    = quo(bb_tax_effect), 
  x_lab    = 'Real rate of return', 
  title    = 'Effect of the real rate of return', 
  subtitle = 'Marginal impact of buyback excise tax rate on METR'
)

# pi
get_sensitivity_chart(
  x_values = tibble(pi = seq(0, 0.1, 0.001)), 
  x_var    = quo(pi), 
  y_var    = quo(bb_tax_effect), 
  x_lab    = 'Inflation rate', 
  title    = 'Effect of inflation', 
  subtitle = 'Marginal impact of buyback excise tax rate on METR'
)

# delta
get_sensitivity_chart(
  x_values = tibble(delta = seq(0, 1, 0.01)), 
  x_var    = quo(delta), 
  y_var    = quo(bb_tax_effect), 
  x_lab    = 'Depreciation rate', 
  title    = 'Effect of depreciation rate', 
  subtitle = 'Marginal impact of buyback excise tax rate on METR'
)

# BEHAVIORAL FACTORS

# m
get_sensitivity_chart(
  x_values = tibble(m = seq(0, 1, 0.01)), 
  x_var    = quo(m), 
  y_var    = quo(bb_tax_effect), 
  x_lab    = 'Share of equity financed through retained earnings', 
  title    = 'Effect of financing source', 
  subtitle = 'Marginal impact of buyback excise tax rate on METR'
)

# n
get_sensitivity_chart(
  x_values = tibble(n = seq(0.01, 30, 0.01)), 
  x_var    = quo(n), 
  y_var    = quo(bb_tax_effect), 
  x_lab    = 'Holding period', 
  title    = 'Effect of holding period', 
  subtitle = 'Marginal impact of buyback excise tax rate on METR'
)

# share_death
get_sensitivity_chart(
  x_values = tibble(share_death = seq(0, 1, 0.01)), 
  x_var    = quo(share_death), 
  y_var    = quo(bb_tax_effect), 
  x_lab    = 'Share of gains held until dealth', 
  title    = 'Effect of step-up', 
  subtitle = 'Marginal impact of buyback excise tax rate on METR'
)

# TAX LAW FACTORS

# z
get_sensitivity_chart(
  x_values = tibble(z = seq(0, 1, 0.01)), 
  x_var    = quo(z), 
  y_var    = quo(bb_tax_effect), 
  x_lab    = 'PV of depreciation deductions', 
  title    = 'Effect of PV of depreciation deductions', 
  subtitle = 'Marginal impact of buyback excise tax rate on METR'
)

# tau_corp
get_sensitivity_chart(
  x_values = tibble(tau_corp = seq(0, 0.35, 0.001)), 
  x_var    = quo(tau_corp), 
  y_var    = quo(bb_tax_effect), 
  x_lab    = 'Corporate tax rate', 
  title    = 'Effect of corporate tax rate', 
  subtitle = 'Marginal impact of buyback excise tax rate on METR'
)

# tau_bb
get_sensitivity_chart(
  x_values = tibble(tau_bb = seq(0, 0.06, 0.001)), 
  x_var    = quo(tau_bb), 
  y_var    = quo(bb_tax_effect), 
  x_lab    = 'Buyback excise tax rate', 
  title    = 'Effect of buyback excise tax rate', 
  subtitle = 'Marginal impact of buyback excise tax rate on METR'
)


# tau_div/tau_kg
get_sensitivity_chart(
  x_values = tibble(tau_kg = seq(0.238, 0.434, 0.001), tau_div = seq(0.238, 0.434, 0.001)), 
  x_var    = quo(tau_kg), 
  y_var    = quo(bb_tax_effect), 
  x_lab    = 'Capital gains/dividend rate', 
  title    = 'Effect of capital gains/dividend rate', 
  subtitle = 'Marginal impact of buyback excise tax rate on METR'
)



