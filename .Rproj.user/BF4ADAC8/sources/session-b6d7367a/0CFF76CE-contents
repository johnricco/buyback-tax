#------------------------------------------------------------------------------
# sim.R
# 
# TODO
#------------------------------------------------------------------------------


run_sensitivity = function(r, pi, delta, z, share_retained, share_taxable, 
                           share_death, n, tau_corp, tau_bb, tau_div, tau_kg) {
  
  #----------------------------------------------------------------------------
  # TODO
  # 
  # Parameters: (all can be vectors)
  # - r              (dbl) : real rate of return on corporate equity 
  # - pi             (dbl) : expected inflation rate
  # - delta          (dbl) : economic depreciation rate
  # - z              (dbl) : present value of depreciation deductions
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
  expand_grid(r, pi, delta, z, share_retained, share_taxable, share_death, n, tau_corp, tau_bb, tau_div, tau_kg) %>% 
    
    # Calculate METRs and tax differential
    rowwise() %>% 
    mutate(
      metr_div  = calc_metr('div', r, pi, delta, z, share_retained, share_taxable, share_death, n, tau_corp, tau_bb, tau_div, tau_kg)['metr'], 
      metr_bb   = calc_metr('bb',  r, pi, delta, z, share_retained, share_taxable, share_death, n, tau_corp, tau_bb, tau_div, tau_kg)['metr'], 
      metr_diff = metr_div - metr_bb
    ) %>% 
    ungroup() %>% 
    return()
}

