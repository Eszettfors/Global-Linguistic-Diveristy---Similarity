library(tidyverse)
library(readxl)
library(stringr)


#This scripts processes the ethnologue dump and extracts language families according to ethnologue


df = read_excel("Raw_data/ethnologue_dump.xlsx")

df = df %>%
  select(Name, iso639P3code, Classification) %>%
  rename("language" = Name, "ISO6393" = iso639P3code, "family" = Classification)


df = df %>% 
  rowwise() %>%
  mutate(family = str_split(family, ",")[[1]][1])


write_csv(df, "Clean_data/language_lvl_data/language_families_ethnologue.csv")
