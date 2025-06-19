library(tidyverse)
library(vegan)
library(readr)
library(dplyr)
# this script access PHOIBLE Online, takes the speaker data, and returns an rds-file
# denoting how many languages are covered and a distance matrix with jaccard distance between those languages present


phoible = read_csv("Clean_data/language_lvl_data/phoible_data_long.csv")
df_countries = read_csv("Clean_data/country_lvl_data/country_lang_pop.csv")

# fix namibia
df_countries = df_countries %>% mutate(country_code = case_when(
  country == "Namibia" ~ "NA",
  TRUE ~ country_code
))

head(df_countries)

# one langoid can have multiple inventory description; solution: use the one with the most features

# group by inventory and count size, remove duplicates with smallest size and extract inventoryID to keep
inventoryIds = phoible %>% 
  group_by(ISO6393, InventoryID, .groups = "drop") %>% 
  summarise(size = n()) %>% 
  slice_max(size) %>% pull(InventoryID) 

# filter to only keep the inventory with largest sizes
phoible = phoible %>% filter(InventoryID %in% inventoryIds)
phoible %>% group_by(ISO6393) %>% count() %>% nrow() # 2095 unique languages

get_n_langs = function(df, countryID){
  # takes a df and country ID and returns the number of langs in the df
  langs = df %>% filter(country_code == countryID) %>% pull(ISO6393)
  return(length(langs))
}

get_n_langs_in_phoible = function(df, countryID){
  # takes a df and country ID and returns the number of langs present in PHOIBLE
  langs = df %>% filter(country_code == countryID) %>% pull(ISO6393)
  n_langs_in_phoible = phoible %>% filter(ISO6393 %in% langs) %>% select(ISO6393) %>% unique() %>% nrow()
  return(n_langs_in_phoible)
}

get_lang_coverage = function(df, countryID){
  # takes a df and country ID and returns percentage of languages present in PHOIBLE
  langs = get_n_langs(df, countryID)
  n_langs_in_phoible = get_n_langs_in_phoible(df, countryID)
  return(n_langs_in_phoible/langs * 100)
}


get_n_speakers = function(df, countryID){
  #takes a df and countryID and returns the number of speakers in that country of any language
  n_speaker = df %>% 
    filter(country_code == countryID) %>% 
    summarise(speakers = sum(population))
  return(as.numeric(n_speaker))
}

get_n_speakers_in_phoible = function(df, countryID){
  # takes a data frame and a country and returns the number of speakers of languages present in PHOIBLE
  speakers_in_phoible = df %>%
    filter(country_code == countryID) %>%
    filter(ISO6393 %in% phoible$ISO6393) %>% 
      summarize(speakers = sum(population)) %>%
      pull(speakers)
  return(as.numeric(speakers_in_phoible))
}
  
get_speaker_coverage_phoible = function(df, countryID){
  # takes a dataframe and a country and returns percentage of speakers in the df present in PHOIBLE
  n_speakers = get_n_speakers(df, countryID)
  n_speakers_phoible = get_n_speakers_in_phoible(df, countryID)
  return(n_speakers_phoible/n_speakers * 100)
}
 
get_similarity_matrix = function(df, countryID){
  # takes a df and country ID and returns a jaccard disimilarity matrix for all languages present in PHOIBLE
  # in that country
  langs = df %>% 
    filter(country_code == countryID) %>%
    pull(ISO6393)
  
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

# subset to one row for each country
sim_coverage = df_countries %>% 
  distinct(country_code, country)

# add calculations to the subsetted df
sim_coverage = sim_coverage %>%
  rowwise() %>%
  mutate(n_langs = get_n_langs(df_countries, country_code),
         n_langs_in_phoible = get_n_langs_in_phoible(df_countries, country_code),
         phon_lang_coverage = get_lang_coverage(df_countries, country_code),
         n_speakers = get_n_speakers(df_countries, country_code),
         n_speakers_in_phoible = get_n_speakers_in_phoible(df_countries, country_code),
         phon_speaker_coverage = get_speaker_coverage_phoible(df_countries, country_code),
         phon_similarity = list(get_similarity_matrix(df_countries, country_code)))

head(sim_coverage)

write_rds(sim_coverage, "Clean_data/distances/phoible_coverage_similarity_population.rds")

