
library(dplyr)

wisc4_lookup_tab = read.csv('data/wisc4GAI_lookup.csv')
wisc5_lookup_tab = read.csv('data/wisc5GAI_lookup.csv')

wisc = read.csv('data/KKI_WechslerIntelligenceScaleforChildren.csv')

wisc = as_tibble(wisc)

wisc = wisc %>% 
  filter(!is.na(WISC_VERSION))

wisc4_vars = names(wisc)[5:10]
wisc5_vars = names(wisc)[11:15]

wisc$scaled_sum = rep(0, dim(wisc)[1])

for (i in 1:dim(wisc)[1]) {
  if (wisc[i,]$WISC_VERSION == 4) {
    wisc[i,]$scaled_sum = sum(wisc[i, wisc4_vars])
  } else {
    wisc[i,]$scaled_sum = sum(wisc[i, wisc5_vars])
  }
}

wisc = wisc %>%
  select(-c(wisc4_vars, wisc5_vars)) %>%
  filter(!is.na(scaled_sum))



sum_to_gai = function(version, sum) {
  if (version == 4) {
    return (wisc4_lookup_tab$WISC4.GAI[which(wisc4_lookup_tab$WISC4.ScaledSum == sum)])
  } else {
    return (wisc5_lookup_tab$WISC5.GAI[which(wisc5_lookup_tab$WISC5.ScaledSum == sum)])
  }
}


wisc = wisc %>%
  rowwise() %>%
  mutate(GAI = sum_to_gai(WISC_VERSION, scaled_sum))


