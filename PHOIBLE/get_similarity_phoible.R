library(tidyverse)
library(vegan)
library(readr)
library(dplyr)
# this script allows calculation of phonemic similarity between any two languages


#url_ = "https://github.com/phoible/dev/blob/master/data/phoible.csv?raw=true"
#col_types = cols(InventoryID='i', Marginal='l', .default='c')
#phoible = read_csv(url(url_), col_types=col_types)

phoible = read_csv("Clean_data/language_lvl_data/phoible_data_long.csv")

# one langoid can have multiple inventory description; solution: use the one with the most features

# group by inventory and count size, remove duplicates with smallest size and extract inventoryID to keep
inventoryIds = phoible %>% 
  group_by(ISO6393, InventoryID, .groups = "drop") %>% 
  summarise(size = n()) %>% 
  slice_max(size) %>% pull(InventoryID) 

# filter to only keep the inventory with largest sizes
phoible = phoible %>% filter(InventoryID %in% inventoryIds)
phoible %>% group_by(ISO6393) %>% count() %>% nrow() # 2095 unique languages


get_phon_similarity_matrix = function(langs){
  # takes a vector of ISO codes and returns a jaccard similarity matrix for all languages present in PHOIBLE
  # in phoible
  langs_in_phoible = phoible %>%
    filter(ISO6393 %in% langs) %>%
    distinct(ISO6393) %>%
    pull(ISO6393)
  
  binary_matrix = phoible %>% 
    filter(ISO6393 %in% langs_in_phoible) %>% 
    select(ISO6393, Phoneme) %>% 
    distinct() %>%
    mutate(presence = 1) %>%
    pivot_wider(names_from = Phoneme, values_from = presence, values_fill = list(presence = 0)) %>%
    select(-ISO6393) %>%
    as.matrix()
  
  distance_matrix = vegdist(binary_matrix, method = "jaccard") %>%
    as.matrix()
  sim_matrix = 1 - distance_matrix
  
  colnames(sim_matrix) = langs_in_phoible
  rownames(sim_matrix) = langs_in_phoible
  
  return(sim_matrix)
}

