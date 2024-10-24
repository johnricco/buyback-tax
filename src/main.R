#------------------------------------------------------------------------------
# main.R 
# 
# Entry point into program
#------------------------------------------------------------------------------

library(tidyverse)



source('./src/calc.R')
source('./src/sim.R')








# test
run_sensitivity(0.06, 0.02, 0.04, 0.85, 0.8, 0.4, 0.43, 8, 0, seq(0, 0.05, 0.0005), 0.21, 0.21) %>% 
  group_by(n) %>% 
  filter(metr_diff <= 0) %>% 
  slice(1) %>% 
  ungroup() %>% 
  ggplot(aes(x = n, y = tau_bb)) +
  geom_line() + 
  geom_point()

  
    
  
  # pivot_longer(cols = c(metr_div, metr_bb)) %>% 
  # ggplot(aes(x = tau_bb, y = value, colour = name)) + 
  # geom_point() + 
  # geom_line() + 
  # facet_wrap(~n)
