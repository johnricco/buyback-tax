#------------------------------------------------------------------------------
# main.R 
# 
# Entry point into program
#------------------------------------------------------------------------------

library(tidyverse)



source('./src/calc.R')
source('./src/sim.R')
















# expand_grid(
#   share_buybacks = seq(0, 1, 0.01), 
#   share_retained = seq(0, 1, 0.01)
# ) %>% 
#   mutate(
#     share_dividends = 1 - share_buybacks, 
#     share_new_issuance = 1 - share_retained,
#     buyback_factor = ((share_dividends + ((share_buybacks * share_new_issuance) + (share_buybacks * share_retained) / (1 - 0.01)) - 1) / 0.01)
#   ) %>% 
#   ggplot(aes(x = share_retained, y = share_dividends, fill = buyback_factor)) + 
#   geom_tile() + 
#   theme_classic() + 
#   scale_fill_gradient(low = 'white', high = '#e34b4b') + 
#   labs(y = 'Use of profits: share dividends (vs buybacks)', 
#        x = 'Source of funds: share retained earnings (vs new issuance)', 
#        fill = 'Effect of buyback tax') + 
#   geom_hline(yintercept = 0.5) + 
#   geom_vline(xintercept = 0.5) + 
#   annotate('text', x = 0.25, y = 0.25, label = 'Source: new issuance \n Use: buybacks') + 
#   annotate('text', x = 0.25, y = 0.75, label = 'Source: new issuance \n Use: dividends') + 
#   annotate('text', x = 0.75, y = 0.25, label = 'Source: retained earnings \n Use: buybacks') + 
#   annotate('text', x = 0.75, y = 0.75, label = 'Source: retained earnings \n Use: dividends') + 
#   scale_x_continuous(labels = scales::percent_format()) + 
#   scale_y_continuous(labels = scales::percent_format())
# 

