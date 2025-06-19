library(lingtypology)
library(tidyverse)
library(vegan)

# this script accesses the Ethno/joshua data and calculates a phylogenetic similarity matrix
# between all languages in every country


df_country = read_csv("Clean_data/country_lvl_data/country_lang_pop.csv")
df_glotto = lingtypology::glottolog
df_glotto = as_tibble(df_glotto)


df_country %>% distinct(ISO6393)
head(df_country)

#namibia
df_country = df_country %>%
  mutate(country_code = case_when(
    country == "Namibia" ~ "NA",
    TRUE ~ country_code
  ))

# select languages in ethnologue data
df_glotto = df_glotto %>%
  filter(iso %in% df_country$ISO6393)

df_glotto = df_glotto %>% select(iso, language, affiliation)
head(df_glotto)
# affiliation contains all branches -> turn each possible branch into a vector -> 

# isolates are assigned different "isolate-families"
df_glotto = df_glotto %>%
  mutate(affiliation = case_when(
    affiliation == "" ~ paste("isolate", iso, sep = "_"),
    TRUE ~ affiliation
  ))

# bookkeepings are removed since they can't be assigned any family
df_glotto = df_glotto %>% filter(affiliation != "Bookkeeping")


get_similarity_matrix = function(df, country_ID){
  # takes a data frame with country language pairs and a country code, returns a similarity matrix with 1 - jaccarddistance
  # for all languages in that country
  
  langs = df %>% filter(country_code == country_ID) %>% pull(ISO6393)
  
  #langs in glotto
  langs_in_glotto = df_glotto %>% filter(iso %in% langs) %>% pull(iso)
  
  # calculate distances
  dist_matrix = df_glotto %>%
    filter(iso %in% langs_in_glotto) %>%
    separate_rows(affiliation, sep = ",") %>%
    pivot_wider(names_from = affiliation, values_from = affiliation, values_fill = NA) %>%
    select(-c(iso, language)) %>%
    mutate(across(everything(), ~ case_when(
      !is.na(.) ~ 1,
      TRUE ~ 0
    ))) %>%
    as.matrix() %>%
    vegdist(method = "jaccard") %>% as.matrix()
  
  # convert to similarity
  sim_matrix = 1-dist_matrix
  
  # add ISO to names
  rownames(sim_matrix) = langs_in_glotto
  colnames(sim_matrix) = langs_in_glotto
  return(sim_matrix)
}


get_similarity_matrix(df_country, "SE") %>% View()

# subset to one row for each country
sim_coverage = df_country %>% 
  distinct(country_code)

# add calculations to the subsetted df
sim_coverage = sim_coverage %>%
  rowwise() %>%
  mutate(phylogenetic_similarity = list(get_similarity_matrix(df_country, country_code)))


# write data
write_rds(sim_coverage, "Clean_data/distances/glottolog_similarity.rds")





