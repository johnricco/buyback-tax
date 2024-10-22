#------------------------------------------------------------------------------
# sim.R
# 
# TODO
#------------------------------------------------------------------------------


run_sensitivity = function(r, pi, delta, share_retained, share_taxable, 
                           share_death, n, tau_corp, tau_bb, tau_div, tau_kg) {
  
  #----------------------------------------------------------------------------
  # TODO
  # 
  # Parameters: (all can be vectors)
  # - r              (dbl) : real rate of return on corporate equity 
  # - pi             (dbl) : expected inflation rate
  # - delta          (dbl) : economic depreciation rate
  # - share_retained (dbl) : share of earnings retained for future investment
  # - share_taxable  (dbl) : share of corporate equity subject to domestic tax
  # - share_death    (dbl) : share of gains held until death
  # - n              (dbl) : holding period for capital gains in years
  # - tau_corp       (dbl) : corporate tax rate
  # - tau_bb         (dbl) : buyback excise tax rate
  # - tau_div        (dbl) : tax rate on dividends
  # - tau_kg         (dbl) : tax rate on capital gains
  # 
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  # Generate combos of parameters
  expand_grid(r, pi, delta, share_retained, share_taxable, share_death, n, tau_corp, tau_bb, tau_div, tau_kg) %>% 
    
    # Calculate METRs and tax differential
    rowwise() %>% 
    mutate(
      metr_div  = calc_metr('div', r, pi, delta, share_retained, share_taxable, share_death, n, tau_corp, tau_bb, tau_div, tau_kg)['metr'], 
      metr_bb   = calc_metr('bb',  r, pi, delta, share_retained, share_taxable, share_death, n, tau_corp, tau_bb, tau_div, tau_kg)['metr'], 
      metr_diff = metr_div - metr_bb
    ) %>% 
    ungroup() %>% 
    return()
}


# test
run_sensitivity(0.06, 0.02, 0, 0.8, 0.4, 0.43, c(10, 30), 0, seq(0, 0.05, 0.001), 0.21, 0.21) %>% 
  pivot_longer(cols = c(metr_div, metr_bb)) %>% 
  ggplot(aes(x = tau_bb, y = value, colour = name)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~n)
