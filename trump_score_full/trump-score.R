library(rvest)
library(tidyverse)

h <- read_html('https://projects.fivethirtyeight.com/congress-trump-score/house/')

trump_score <- h %>% 
    html_nodes('table.member-list') %>% 
    map(html_table, fill = TRUE) %>% 
    set_names(c('Senate', 'House')) %>%
    map(set_names, 
        c('member', 'member_short', 'party', 'district', 'trump_score', 
          'trump_margin', 'predicted_score', 'trump_plus_minus')) %>% 
    map(mutate_all, na_if, '—') %>% 
    map_df(mutate_at, vars(trump_score:trump_plus_minus), parse_number, 
           .id = 'chamber')
           