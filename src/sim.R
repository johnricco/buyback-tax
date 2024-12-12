#------------------------------------------------------------------------------
# sim.R
# 
# TODO
#------------------------------------------------------------------------------


expand_grid(
    r             = 0.06, 
    pi            = 0.02, 
    delta         = 0.04, 
    z             = 0.85,  
    m             = 0.5, 
    phi           = c(0, 1), 
    share_taxable = 0.4, 
    share_death   = 0.4, 
    n             = 8, 
    tau_corp      = 0.21, 
    tau_bb        = seq(0, 0.06, 0.01),
    tau_div       = 0.2, 
    tau_kg        = 0.2
  ) %>% 
  calc_metr() %>%
  ggplot(aes(x = tau_bb, y = metr, colour = as.factor(phi))) + 
  geom_line() +
  geom_point()
