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
  # - df (df)  : tibble with the following variables: 
  #   - legal_form    (str) : 'corp' (C corporation) or 'pt' (pass-through)
  #   - financing     (str) : 'equity' or 'debt' or 'avg'
  #   - r             (dbl) : real rate of return
  #   - pi            (dbl) : expected inflation rate
  #   - discount_rate (dlb) : r + pi, adjusted for tax treatment of debt financing 
  #   - delta         (dbl) : economic depreciation rate
  #   - z             (dbl) : present value of depreciation deductions
  #   - m             (dbl) : share of investment financed by retained earnings
  #   - phi           (dbl) : share of distributions structured as buybacks 
  #   - share_taxable (dbl) : share of C-corp equity subject to tax
  #   - share_death   (dbl) : share of gains held until death
  #   - n             (dbl) : holding period for capital gains in years
  #   - tau_entity    (dbl) : corporate tax rate for C-corp financing; average effective marginal rate on pass-through income otherwise
  #   - tau_bb        (dbl) : buyback excise tax rate
  #   - tau_div       (dbl) : tax rate on dividends
  #   - tau_kg        (dbl) : tax rate on capital gains
  #   - tau_i         (dbl) : tax rate on interest income
  # - type (str) : 'avg' if calculating an overall METR where phi is set at or
  #                near its baseline value; 'bb_diff' if calculating a tax rate
  #                differential exercise in which phi takes a value of 0 or 1
  # 
  # Returns: tibble with METR and intermediate calculations (df).
  #----------------------------------------------------------------------------
  
  df %>% 
    mutate(
      
      # Set variable for debt share of financing 
      y = case_when(
        financing == 'equity' ~ 0, 
        financing == 'debt'   ~ 1, 
        financing == 'avg'    ~ 0.3
      ),
      
      #------
      # Firm
      #------
      
      # Calculate gross-up term for "entity"-level income tax 
      income_tax_gross_up = (1 - tau_entity * z) / (1 - tau_entity),
      
      # Calculate gross-up term for buyback tax (certain equity-financed C-corp investment only)
      bb_gross_up = (1 - phi) + 
                    (phi * (1 - m)) + 
                    (phi * m) / (1 - (tau_bb * (1 - y) * (legal_form == 'corp'))),
      
      # Calculate required real before-tax rate of return
      rho = (discount_rate - pi + delta) * income_tax_gross_up * bb_gross_up - delta,
      
      #---------------------------
      # Saver: C-corporate equity
      #---------------------------
      
      # Calculate after-tax return on capital gains attributable to retained earnings
      s_e_kg = log(1 + (exp((r + pi) * n) - 1) * (1 - ((1 - share_death) * share_taxable * tau_kg))) / n - pi,
      
      # Calculate after-tax return on buybacks
      s_e_bb_immediate = (r + pi) * (1 - (share_taxable * tau_kg)) - pi,
      share_bb_immediate = (exp(r + pi) - 1) / exp(r + pi),
      s_e_bb = share_bb_immediate * s_e_bb_immediate + (1 - share_bb_immediate) * s_e_kg,
      
      # Calculate after-tax return on dividends
      s_e_div = (r + pi) * (1 - share_taxable * tau_div) - pi,
      
      # Calculate weighted average after-tax return on (C-corp) equity
      s_e = (
        
        # Financed by new issuance, paid out as dividends
        ((1 - m) * (1 - phi) * s_e_div) +
        
        # Financed by new issuance, paid out as buybacks
        ((1 - m) * phi * case_when(
          type == 'bb_diff' ~ s_e_bb,
          type == 'avg'     ~ s_e_kg,
          T                 ~ NA
        )) + 
        
        # Financed by retained earnings, paid out as dividends
        (m * (1 - phi) * s_e_kg) + 
        
        # Financed by retained earnings, paid out as buybacks
        (m * phi * s_e_kg)
      
      ),
      
      #-------------------------
      # Saver: other situations
      #-------------------------
      
      # Calculate after-tax return on debt
      s_d = (r + pi) * (1 - tau_i * share_taxable) - pi,
      
      # Calculate average after-tax return
      s = case_when(
        legal_form == 'corp' ~ (1 - y) * s_e + y * s_d,
        legal_form == 'pt'   ~ (1 - y) * r   + y * s_d, 
      ),

      #------
      # METR
      #------
      
      # Calculate tax wedge
      wedge = rho - s,
      
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
    calc_metr(type = 'bb_diff') 
  div = df %>% 
    mutate(phi = 0) %>% 
    calc_metr(type = 'bb_diff')
  
  # Calculate differential and return
  df %>% 
    mutate(
      phi      = NA, 
      metr_bb  = bb$metr, 
      metr_div = div$metr,
      bb_differential = metr_bb - metr_div
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
  without = calc_metr(df %>% mutate(tau_bb = 0),    'avg')$metr
  with    = calc_metr(df %>% mutate(tau_bb = 0.01), 'avg')$metr
  
  # Calculate difference, add to data, and return
  df %>% 
    mutate(
      metr_without  = without, 
      metr_with     = with, 
      bb_tax_effect = metr_with - metr_without) %>% 
    return()
}



calc_sensitivity = function(new_values, type) {
  
  #----------------------------------------------------------------------------
  # Calculates tax rate metrics across a user-supplied range of non-baseline 
  # values for one or more parameters, all else equal.
  # 
  # Parameters:
  # - new_values (df) : tibble of new values for one or more parameters
  # - type      (str) : 'avg' or 'bb_diff'
  #   
  # Returns: tibble of tax rate metrics across all combinations of specified 
  #          parameter values (df).
  #----------------------------------------------------------------------------
  
  # Prepare input data
  input_data = params
  if (!is.null(new_values)) {
    input_data = params %>% 
      select(-all_of(colnames(new_values))) %>% 
      expand_grid(new_values)
  }
  
  # Buyback-dividend differential  
  if (type == 'bb_diff') {
    input_data %>% 
      calc_bb_differential() %>% 
      return()
    
  # Effect of the buyback excise tax on the overall METR
  } else if (type == 'avg') {
    input_data %>% 
      calc_bb_tax_effect() %>% 
      return()
    
    # Invalid input  
  } else {
    return(NA)
  }
}
