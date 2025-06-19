library(tidyverse)

# this script access PHOIBLE Online, subsets to only one Inventory per language and write the file


url_ = "https://github.com/phoible/dev/blob/master/data/phoible.csv?raw=true"
col_types = cols(InventoryID='i', Marginal='l', .default='c')
phoible = read_csv(url(url_), col_types=col_types)


# group by inventory and count size, remove duplicates with smallest size and extract inventoryID to keep
inventoryIds = phoible %>% 
  group_by(ISO6393, InventoryID, .groups = "drop") %>% 
  summarise(size = n()) %>% 
  slice_max(size) %>% pull(InventoryID) 

# filter to only keep the inventory with largest sizes
View(phoible)

phoible = phoible %>% filter(InventoryID %in% inventoryIds)

write_csv(phoible, "Clean_data/language_lvl_data/phoible_data_long.csv")
