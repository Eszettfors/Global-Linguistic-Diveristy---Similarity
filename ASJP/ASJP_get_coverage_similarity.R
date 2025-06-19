library(tidyverse)
library(stringdist)
# this script reads the ethnologue data with country-language pairs and for each country calculates the coverage of languages and speaker in ASJP. For each
# country, a matrix with mean normalized levenshtein similarity per language is given

# read data asjp
df_asjp = read_csv("Clean_data/language_lvl_data/asjp_wide.csv")

# filter to 28
df_asjp = df_asjp %>% filter(words > 27 )

# read country_lang data ---------------
df_country = read_csv("Clean_data/country_lvl_data/country_lang_pop.csv")

df_country = df_country %>% mutate(country_code = case_when(
  country == "Namibia" ~ "NA",
  TRUE ~ country_code
))

head(df_country)
# define functions -----
get_n_langs = function(df, countryID){
  # takes a df and country ID and returns the number of langs in the df
  langs = df %>%
    filter(country_code == countryID) %>% pull(ISO6393)
  return(length(langs))
}


get_n_langs_in_asjp = function(df, countryID){
  # takes a df and country ID and returns the number of langs present in asjp
  langs = df %>% filter(country_code == countryID) %>% pull(ISO6393)
  n_langs_in_asjp = df_asjp %>%
    filter(ISO6393 %in% langs) %>% 
    select(ISO6393) %>% 
    unique() %>%
    nrow()
  return(n_langs_in_asjp)
}



get_lang_coverage = function(df, countryID){
  # takes a df and country ID and returns percentage of languages present in asjp
  langs = get_n_langs(df, countryID)
  n_langs_in_asjp = get_n_langs_in_asjp(df, countryID)
  return(n_langs_in_asjp/langs * 100)
}


get_n_speakers = function(df, countryID){
  #takes a df and countryID and returns the number of speakers in that country of any language
  n_speaker = df %>% 
    filter(country_code == countryID) %>% 
    summarise(speakers = sum(population))
  return(as.numeric(n_speaker))
}

get_n_speakers_in_asjp = function(df, countryID){
  # takes a data frame and a country and returns the number of speakers of languages present in asjp
  speakers_in_asjp = df %>%
    filter(country_code == countryID) %>%
    filter(ISO6393 %in% df_asjp$ISO6393) %>% 
    summarize(speakers = sum(population)) %>%
    pull(speakers)
  return(as.numeric(speakers_in_asjp))
}

get_speaker_coverage_asjp = function(df, countryID){
  # takes a dataframe and a country and returns percentage of speakers in the df present in asjp
  n_speakers = get_n_speakers(df, countryID)
  n_speakers_asjp = get_n_speakers_in_asjp(df, countryID)
  return(n_speakers_asjp/n_speakers * 100)
}

get_concept_vector = function(ISO){
  # this function takes a language ISO code and returns a vector with concept values from ASJP
  if (!ISO %in% df_asjp$ISO6393){
    stop(error)
  }
  vec = df_asjp %>%
    filter(ISO6393 == ISO) %>%
    select(!c("lang_id", "ISO6393", "words", "completeness", "language", "macroarea", "family")) %>%
    t() %>%
    as.vector()
  return(vec)
}


get_mean_ldn_sim = function(v1, v2){
  # this function takes two language vectors with concepts and calculates the mean normalized levenshtein distance 
  # between them
  
  # get vector with normalized levenshtein distances
  ldn = stringdist(v1, v2, method = "lv") / pmax(nchar(v1), nchar(v2))
  
  # average ldn
  mean_ldn = mean(ldn, na.rm = TRUE)
  
  return(1 - mean_ldn)
}


get_ldn_sim_matrix = function(iso_vec){
  # function takes a vector of iso codes and returns a matrix with pairwise similarity
  
  
  sim_m = matrix(NA, ncol = length(iso_vec), nrow = length(iso_vec), dimnames = list(iso_vec, iso_vec))
  
  # populate upper triangl
  i = 1
  for (lang1 in iso_vec){
    for (lang2 in iso_vec[i:length(iso_vec)]){
      concept_vec_1 = get_concept_vector(lang1)
      concept_vec_2 = get_concept_vector(lang2)
      
      sim = get_mean_ldn_sim(concept_vec_1, concept_vec_2)
      
      sim_m[lang1, lang2] = sim
      sim_m[lang2, lang1] = sim

    }
    i = i + 1
  }
  return(sim_m)
}

get_ldn_sim_matrix_country = function(df, countryID){
  # takes a data frame with country language pairs and a country ID. 
  # returns a matrix with the similiarity between each language
  
  # retrieve ISO code in country
  langs = df %>%
    filter(country_code == countryID) %>%
    pull(ISO6393)
  
  # retrive ISO codes present in grambank
  langs_in_asjp = df_asjp %>%
    filter(ISO6393 %in% langs) %>%
    pull(ISO6393)


  # get matrix
  sim_m = get_ldn_sim_matrix(langs_in_asjp)
  
  return(sim_m)
}


# handle special signs ~ ? juxtaposition -> remove + previous letter
# * = nasalization -> simply remove
#and https://www.researchgate.net/publication/43336388_Sound_Symbolism_in_Basic_Vocabulary
param_cols = df_asjp %>% select(!c(lang_id, ISO6393, words, completeness, language, macroarea, family)) %>% colnames()
param_cols
df_asjp = df_asjp %>%
  mutate(across(param_cols, ~ gsub("\\*", "", .x))) %>%
  mutate(across(param_cols, ~ gsub("[a-zA-Z]~", "", .x)))

sim_coverage = df_country %>% 
  distinct(country_code, country)

# add calculations to the subsetted df
sim_coverage = sim_coverage %>%
  rowwise() %>%
  mutate(n_langs = get_n_langs(df_country, country_code),
         n_langs_in_asjp = get_n_langs_in_asjp(df_country, country_code),
         asjp_lang_coverage = get_lang_coverage(df_country, country_code),
         n_speakers = get_n_speakers(df_country, country_code),
         n_speakers_in_asjp = get_n_speakers_in_asjp(df_country, country_code),
         asjp_speaker_coverage = get_speaker_coverage_asjp(df_country, country_code),
         ldn_sim_matrix = list(get_ldn_sim_matrix_country(df_country, country_code)))

View(sim_coverage)

write_rds(sim_coverage, "Clean_data/distances/asjp_coverage_similarity_population.rds")
