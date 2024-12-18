#------------------------------------------------------------------------------
# calc.R
# 
# Contains tax rate calculation functions
#------------------------------------------------------------------------------



calc_metr = function(df) {
  
  #----------------------------------------------------------------------------
  # Calculates marginal effective tax rate on the marginal corporate 
  # investment.
  # 
  # Parameters:
  # - df               (df)  : tibble with the following variables: 
  #   - r              (dbl) : real rate of return on corporate equity 
  #   - pi             (dbl) : expected inflation rate
  #   - delta          (dbl) : economic depreciation rate
  #   - z              (dbl) : present value of depreciation deductions
  #   - m              (dbl) : share of investment financed by retained earnings
  #   - phi            (dbl) : share of distributions structured as buybacks 
  #   - share_taxable  (dbl) : share of equity subject to domestic tax
  #   - share_death    (dbl) : share of gains held until death
  #   - n              (dbl) : holding period for capital gains in years
  #   - tau_corp       (dbl) : corporate tax rate
  #   - tau_bb         (dbl) : buyback excise tax rate
  #   - tau_div        (dbl) : tax rate on dividends
  #   - tau_kg         (dbl) : tax rate on capital gains
  # 
  # Returns: tibble with METR and intermediate calculations (df).
  #----------------------------------------------------------------------------
  
  df %>% 
    mutate(
      
      #------
      # Firm
      #------
      
      # Calculate gross-up term for corporate tax 
      cit_gross_up = (1 - tau_corp * z) / (1 - tau_corp),
      
      # Calculate gross-up term for buyback tax 
      bb_gross_up = (1 - phi) + (phi * (1 - m)) + ((phi * m) / (1 - tau_bb)),
      
      # Calculate required before-tax rate of return
      rho = ((r + delta) * cit_gross_up * bb_gross_up) - delta,
      
      #-------
      # Saver
      #-------
      
      # Calculate after-tax return on capital gains attributable to retained earnings
      s_kg = log(1 + (exp((r + pi) * n) - 1) * (1 - ((1 - share_death) * share_taxable * tau_kg))) / n - pi,
      
      # Calculate after-tax return on buybacks
      s_bb_immediate     = (r + pi) * (1 - (share_taxable * tau_kg)) - pi,
      share_bb_immediate = (exp(r + pi) - 1) / exp(r + pi),
      s_bb = (share_bb_immediate * s_bb_immediate) + ((1 - share_bb_immediate) * s_kg),
      
      # Calculate after-tax return on dividends
      s_div = (r + pi) * (1 - share_taxable * tau_div) - pi,
      
      # Calculate weighted average after-tax return
      s = (
        
        # Financed by new issuance, paid out as dividends
        ((1 - m) * (1 - phi) * s_div) +
        
        # Financed by new issuance, paid out as buybacks
        ((1 - m) * phi * s_bb) + 
        
        # Financed by retained earnings, paid out as dividends
        (m * (1 - phi) * s_kg) + 
        
        # Financed by retained earnings, paid out as buybacks
        (m * phi * s_kg)
      
      ),

      #---------
      # Overall
      #---------
      
      # Calculate marginal effective tax rate
      metr = 1 - (s / rho)
    
    ) %>% 
    return()
}



calc_bb_differential = function(df) {
  
  #----------------------------------------------------------------------------
  # Calculates tax differential between buybacks and dividends conditional, 
  # all else equal.
  #
  # Parameters:
  # - df (df) : tibble with variables described in calc_metr()
  #
  # Returns: tibble with tax differential between buybacks and dividends (df).
  #----------------------------------------------------------------------------
  
  # Calculate METRs under both buyback (phi = 1) and dividend (phi = 0) cases
  bb = df %>% 
    mutate(phi = 1) %>% 
    calc_metr() 
  div = df %>% 
    mutate(phi = 0) %>% 
    calc_metr()
  
  # Calculate differential and return
  df %>% 
    mutate(
      phi = NA, 
      bb_differential = bb$metr - div$metr
    ) %>% 
    return()
}


calc_bb_tax_effect = function(df) {
  
  #----------------------------------------------------------------------------
  # Calculates effect of the buyback excise tax on the buyback-dividend tax 
  # differential at the margin. In other words, the partial derivative of the 
  # differential with respect to the buyback excise rate.
  # 
  # Parameters:
  # - df (df) : tibble with variables described in calc_metr()
  #   
  # Returns: tibble with column for the marginal buyback tax effect (df).
  #----------------------------------------------------------------------------
  
  # Calculate actual differential
  actual = df %>%
    calc_bb_differential()
  
  # Calculate differential after a small increment
  marginal = df %>% 
    mutate(tau_bb = tau_bb + .001) %>% 
    calc_bb_differential()
  
  # Calculate effect as scaled difference
  df %>% 
    mutate(bb_tax_effect = (marginal$bb_differential - actual$bb_differential) * 10) %>% 
    return()
}

