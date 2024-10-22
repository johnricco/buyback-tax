#------------------------------------------------------------------------------
# calc.R
# 
# Contains tax rate calculation functions
#------------------------------------------------------------------------------



calc_metr = function(payout, r, pi, delta, share_retained, share_taxable, 
                     share_death, n, tau_corp, tau_bb, tau_div, tau_kg) {
  
  #----------------------------------------------------------------------------
  # Calculates marginal effective tax rate on corporate investment assuming 
  # specified type of payout (dividends or buyback).
  # 
  # Parameters:
  # - payout         (str) : payout type ('div' or 'bb')   
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
  # Returns: METR and intermediate calculations (dbl[])
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
  
  # Calculate after-tax return on retained earnings
  s_retained = calc_s_retained(r, pi, share_taxable, share_death, tau_kg, n)
  
  # Calculate after-tax return on payout
  if (payout == 'div') {
    s_payout = calc_s_div(r, share_taxable, tau_div)
  } else {
    s_payout = s_retained
  }
  
  # Average after-tax returns
  s = (s_payout * (1 - share_retained)) + (s_retained * share_retained)
  
  
  #---------
  # Overall
  #---------
  
  # Calculate marginal effective tax rate
  metr = 1 - (s / rho) 
  
  # Return METR and key intermediate calculations 
  return(
    c(
      'rho'        = rho, 
      's_payout'   = s_payout,
      's_retained' = s_retained,
      's'          = s,
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
  
  # Calculate sales price at end of holding period (relative to basis of $1)
  price = exp((r + pi) * n)
  
  # Calculate effective tax rate, accounting for step up and capital gains
  etr = share_taxable * (1 - share_death) * tau_kg
  
  # Calculate tax liability
  tax = (price - 1) * etr
  
  # Calculate real annualized real after-tax rate of return  
  return((log(price - tax) / n) - pi)
}


