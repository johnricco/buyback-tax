#------------------------------------------------------------------------------
# calc.R
# 
# Contains tax rate calculation functions
#------------------------------------------------------------------------------



calc_metr = function(df, type) {
  
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
  # - type (str) : 'avg' if calculating an overall METR where phi is set at or
  #                near its baseline value; 'diff' if calculating a tax rate
  #                differential exercise in which phi takes a value of 0 or 1
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
        ((1 - m) * phi * case_when(
          type == 'diff' ~ s_bb,
          type == 'avg'  ~ s_kg,
          T              ~ NA
        )) + 
        
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
    calc_metr(type = 'diff') 
  div = df %>% 
    mutate(phi = 0) %>% 
    calc_metr(type = 'diff')
  
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
  # Calculates effect of the 1% buyback excise tax on the METR for a given set
  # of all-else-equal parameters.
  # 
  # Parameters:
  # - df (df) : tibble with variables described in calc_metr()
  #   
  # Returns: tibble with column for the buyback tax effect (df).
  #----------------------------------------------------------------------------
  
  # Calculate METRs under 0% and 1% excise taxes
  zero_percent = calc_metr(df %>% mutate(tau_bb = 0),    'avg')$metr
  one_percent  = calc_metr(df %>% mutate(tau_bb = 0.01), 'avg')$metr
  
  # Calculate difference, add to data, and return
  df %>% 
    mutate(bb_tax_effect = one_percent - zero_percent) %>% 
    return()
}

