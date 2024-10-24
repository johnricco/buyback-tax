#------------------------------------------------------------------------------
# calc.R
# 
# Contains tax rate calculation functions
#------------------------------------------------------------------------------



calc_metr = function(payout, r, pi, delta, z, share_retained, share_taxable, 
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
  # Returns: METR and intermediate calculations (dbl[])
  #----------------------------------------------------------------------------
  
  #------
  # Firm
  #------
  
  # Calculate required before-tax rate of return
  rho = ((r + delta) * (1 - tau_corp * z)) / (1 - tau_corp) - delta
  
  # Add buyback tax 
  bb_effect = 0
  if (payout == 'bb') {
    bb_effect = r * (1 / (1 - (1 - share_retained) * tau_bb) - 1)
  }
  rho = rho + bb_effect
  
  
  #----------
  # Investor
  #----------
  
  # Calculate after-tax return on retained earnings
  s_retained = calc_s_kg(r, pi, share_taxable, share_death, tau_kg, n)
  
  # Calculate after-tax return on payout: dividend
  if (payout == 'div') {
    s_payout = calc_s_div(r, share_taxable, tau_div)
  
  # Calculate after-tax return on payout: dividend
  } else {
    share_sell = (exp(r + pi) - 1) / exp(r + pi)
    s_sell     = calc_s_kg(r, 0, share_taxable, share_death, tau_kg, 1)
    s_hold     = calc_s_kg(r, 0, share_taxable, share_death, tau_kg, n)
    s_payout   = (s_sell * share_sell) + (s_hold * (1 - share_sell))
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



calc_s_kg = function(r, pi, share_taxable, share_death, tau_kg, n) {
  
  #----------------------------------------------------------------------------
  # Calculates after-tax return on capital gains ("s", per CBO's CapTax
  # notation, for capital gains attributable to either non-payment of
  # profits or sales induced by buybacks).
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


