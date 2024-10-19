#------------------------------------------------------------------------------
# main.R 
# 
# Entry point into program
#------------------------------------------------------------------------------

library(tidyverse)

#------------
# Parameters
#------------

# Corporate returns and behavior
real_return    = 0.05
inflation      = 0.02
depreciation   = 0.04553728
share_retained = 0.5

# Characteristics of investors
share_taxable = 0.5
years_held    = 8
share_death   = 0.46


#-------------
# Calculation
#-------------




calc_metr = function(payout, r, pi, delta, share_retained, share_taxable, 
                     share_death, n, tau_corp, tau_bb, tau_div, tau_kg) {
  
  #----------------------------------------------------------------------------
  #  TODO
  # 
  # Parameters:
  # TODO
  # 
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  #------
  # Firm
  #------
  
  # Calculate effect of corporate taxes on required rate of return
  tax_gross_up = calc_tax_gross_up(payout, tau_corp, tau_bb, share_retained)
  
  # Calculate required before-tax rate of return
  rho = r * tax_gross_up

  
  #----------
  # Investor
  #----------
  
  # Calculate after-tax return on payout (dividend)
  s_div = calc_s_div(r, share_taxable, tau_div)
  
  # Calculate after-tax return on retained earnings
  s_retained = calc_s_retained(r, pi, share_taxable, share_death, tau_kg, n)
  
  # Average after-tax returns
  s = s_div * (1 - share_retained) + s_retained * share_retained

  
  #---------
  # Overall
  #---------
  
  # Calculate marginal effective tax rate
  metr = (rho - s) / rho 
  
  # Return METR and key intermediate calculations 
  return(
    c(
      'rho'        = rho, 
      's_div'      = s_div,
      's_retained' = s_retained,
      'metr'       = metr
    )
  )
}




calc_tax_gross_up = function(payout, tau_corp, tau_bb, share_retained) {
  
  #----------------------------------------------------------------------------
  # Calculates gross-up factor reflecting the effect of tax policy on the 
  # required before-tax rate of return. 
  # 
  # Parameters:
  # - payout         (str) : payout type ('div' or 'bb')   
  # - tau_corp       (dbl) : corporate tax rate
  # - tau_bb         (dbl) : buyback excise tax rate
  # - share_retained (dbl) : share of earnings retained for future investment
  # 
  # Returns: gross-up factor (dbl)
  #----------------------------------------------------------------------------
 
  # Calculate gross-up factor
  t = 1 / (1 - tau_corp)
  
  # Adjust for buyback tax
  t = t * if_else(payout == 'bb', 1 + share_retained * (1 / (1 - tau_bb) - 1), 1) 
  
  return(t)
}




calc_s_div = function(r, share_taxable, tau_div) {
  
  #----------------------------------------------------------------------------
  # Calculates after-tax return on dividends ("s", per CBO's CapTax notation, 
  # for dividends specifically).
  # 
  # Parameters:
  # - r             (dbl) : real rate of return on corporate equity 
  # - share_taxable (dbl) : share of corporate equity subject to domestic tax
  # - tau_div       (dbl) : tax rate on dividends
  # 
  # Returns: after-tax return on dividends (dbl)
  #----------------------------------------------------------------------------
  
  return(r * (1 - share_taxable * tau_div))
}



calc_s_retained = function(r, pi, share_taxable, share_death, tau_kg, n) {
  
  #----------------------------------------------------------------------------
  # Calculates after-tax return on retained earnings ("s", per CBO's CapTax
  # notation, for capital gains attributable to non-payment to shareholders).
  # 
  # Parameters:
  # - r             (dbl) : real rate of return on corporate equity 
  # - pi            (dbl) : expected inflation rate
  # - share_taxable (dbl) : share of corporate equity subject to domestic tax
  # - share_death   (dbl) : share of gains held until death
  # - tau_kg        (dbl) : tax rate on capital gains
  # - n             (dbl) : holding period in years
  # 
  # Returns: after-tax return on retained earnings (dbl)
  #----------------------------------------------------------------------------
  
  # Calculate nominal rate of return
  nominal_r = (1 + r) * (1 + pi) - 1
  
  # Calculate sales price at end of holding period (relative to basis of $1)
  price = exp(nominal_r * n)
  
  # Calculate effective tax rate, accounting for step up and capital gains
  etr = share_taxable * (1 - share_death) * tau_kg
  
  # Calculate tax liability
  tax = (price - 1) * etr
  
  # Calculate nominal annualized real after-tax rate of return  
  nominal_s_retained = log(price - tax) / n
  
  # Deflate and return
  return((1 + nominal_s_retained) / (1 + pi) - 1)
}


