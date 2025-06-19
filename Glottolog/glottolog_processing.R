library(tidyverse)
library(lingtypology)


### this script downloads glottolog and processes it

df_glotto = lingtypology::glottolog
df_glotto = as_tibble(df_glotto)

#write raw
write_csv(df_glotto, "Raw_data/Glottolog.csv")
df_glotto = read_csv("Raw_data/Glottolog.csv")
# select columns and subset to language lvl
df_glotto = df_glotto %>% 
  filter(level == "language") %>%
  select(glottocode, language, iso, area, affiliation, latitude, longitude)  %>%
  rename("ISO6393" = iso, "macroarea" = area, "family" = affiliation)

#add isolate as affiliation
df_glotto = df_glotto %>%
  mutate(family = case_when(
    family == "" ~ "isolate",
    is.na(family) ~ "isolate",
    TRUE ~ family
  ))


#write clean
write_csv(df_glotto, "Clean_data/language_lvl_data/Glottolog_processed.csv")
