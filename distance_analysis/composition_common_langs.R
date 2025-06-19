library(tidyverse)
library(xtable)

common_db = read_csv("Clean_data/language_lvl_data/langs_in_all_db.csv")

common_db %>%
  group_by(macroarea) %>%
  summarize(n = n()) %>%
  mutate(percent = n / sum(n))
