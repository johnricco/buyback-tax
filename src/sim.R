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
  baseline_param_values %>% 
    select(-all_of(colnames(new_values))) %>% 
    expand_grid(new_values) %>% 
    calc_bb_differential() %>%
    calc_bb_tax_effect() %>% 
    return()
}

expand_grid(
  share_taxable = c(0, 1),
  m = seq(0, 1, 0.01)
) %>% 
  calc_sensitivity() %>% 
  ggplot(aes(x = m, y = bb_differential, colour = as.factor(share_taxable))) +
  geom_line() +
  geom_hline(yintercept = 0) + 
  theme_bw()


