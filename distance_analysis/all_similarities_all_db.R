library(tidyverse)
library(vegan)
library(stringdist)

# this script subsets all database data to their smallest common denominator and 
# compares them all pairwise creating similarity matrices

##### read data ---------
df_phoible = read_csv("Clean_data/language_lvl_data/phoible_data_long.csv")
df_grambank = read_csv("Clean_data/language_lvl_data/grambank_data_wide.csv")
df_asjp = read_csv("Clean_data/language_lvl_data/asjp_wide.csv")
df_glotto = read_csv("Clean_data/language_lvl_data/Glottolog_processed.csv")

#apply thresholds
df_grambank = df_grambank %>%
  filter(Completeness > 25)

df_asjp = df_asjp %>%
  filter(words > 27)

#### Phoible ---------

# group by inventory and count size, remove duplicates with smallest size and extract inventoryID to keep
inventoryIds = df_phoible %>% 
  group_by(ISO6393, InventoryID, .groups = "drop") %>% 
  summarise(size = n()) %>% 
  slice_max(size) %>% pull(InventoryID) 

# filter to only keep the inventory with largest sizes
df_phoible = df_phoible %>% filter(InventoryID %in% inventoryIds)

get_phon_similarity_matrix = function(langs){
  # takes a vector of ISO codes and returns a jaccard similarity matrix for all languages present in PHOIBLE
  # in phoible
  langs_in_phoible = df_phoible %>%
    filter(ISO6393 %in% langs) %>%
    distinct(ISO6393) %>%
    pull(ISO6393)
  
  binary_matrix = df_phoible %>% 
    filter(ISO6393 %in% langs_in_phoible) %>% 
    dplyr::select(ISO6393, Phoneme) %>% 
    distinct() %>%
    mutate(presence = 1) %>%
    pivot_wider(names_from = Phoneme, values_from = presence, values_fill = list(presence = 0)) %>%
    dplyr::select(-ISO6393) %>%
    as.matrix()
  
  distance_matrix = vegdist(binary_matrix, method = "jaccard") %>%
    as.matrix()
  sim_matrix = 1 - distance_matrix
  
  colnames(sim_matrix) = langs_in_phoible
  rownames(sim_matrix) = langs_in_phoible
  
  return(sim_matrix)
}
europe = read_csv("Clean_data/language_lvl_data/european_languages_in_all_db.csv")
nrow(europe) # 36 languages

europe_iso = europe %>% pull(ISO6393)
europe_names = europe %>% pull(language)
europe_family = europe %>% pull(family)

#named vector
iso_to_name = setNames(europe_names, europe_iso)
iso_to_family = setNames(europe_family, europe_iso)

### get similarity matrices
phon_sim = get_phon_similarity_matrix(europe_iso)
##### grambank --------

get_feature_vector = function(ISO) {
  # takes an ISO code and returns a vector with feature values from grambank
  vec = df_grambank %>% 
    filter(ISO6393 == ISO) %>%
    dplyr::select(!c(ISO6393, Language_name, Glottocode, Macroarea, Family_name)) %>%
    t() %>%
    as.vector()
  
  return(vec)
}

get_feature_overlap = function(l1, l2){
  # this function takes two language vectors of size n with categorical values.
  # it subsets the vectors to features for which both vectors are defined and returns the fraction
  # of overlapping values and the number of common features as a measure of reliability
  # if the number of overlapping features are smaller than 49 -> returns NA
  
  # create df of languages with features as columns
  df = t(data.frame(l1, l2))
  
  # subset to features without any missing values = both languages are defined
  df = df[, colSums(is.na(df)) == 0]
  
  #the number of features for which both languages are defined
  def_length = ncol(df)
  
  # check if the length is null or smaller than 49 -> return NA
  if (is.null(def_length) || def_length < 49) {
    return(NA)
  }
  else{
    # count instances of the language vectors overlapping. TRUE = same, FALSE = different
    tab = table(df[1,] == df[2,])
    
    # frac overlap = TRUE / (TRUE + FALSE)
    overlap = as.numeric(tab["TRUE"] / sum(tab))
    
    return(overlap)
  }
}

get_syn_similarity_matrix = function(langs){
  # takes a vector of iso codes and outputs a matrix with feature overlap between them
  
  
  # retrive ISO codes present in grambank
  langs_in_gram = df_grambank %>%
    filter(ISO6393 %in% langs) %>%
    pull(ISO6393)
  
  # create an empty matrix of with every language as a row and column entry to hold similarity values
  n = length(langs_in_gram)
  sim_m = matrix(NA,
                 ncol = n,
                 nrow = n,
                 dimnames = list(langs_in_gram, langs_in_gram))
  
  # copy the empty matrix to hold the number of features for which any two languages are defined
  def_m = sim_m
  
  # loop through upper triangle and populate both upper and lower triangle
  i = 1
  for (lang1 in langs_in_gram){
    for (lang2 in langs_in_gram[i:length(langs_in_gram)]){
      feature_vec_1 = get_feature_vector(lang1)
      feature_vec_2 = get_feature_vector(lang2)
      
      distance = get_feature_overlap(feature_vec_1, feature_vec_2)
      
      sim_m[lang1, lang2] = distance[1]
      sim_m[lang2, lang1] = distance[1]
  
    }
    i = i + 1
  }
  return(sim_m)
}


##### asjp ---------

get_concept_vector = function(ISO){
  # this function takes a language ISO code and returns a vector with concept values from ASJP
  
  vec = df_asjp %>%
    filter(ISO6393 == ISO) %>%
    dplyr::select(!c("lang_id", "ISO6393", "words", "completeness", "language", "macroarea", "family")) %>%
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

get_lex_similarity_matrix = function(iso_vec){
  # function takes a vector of iso codes and returns a matrix with pairwise similarity
  
  # subset to langs features in asjp
  iso_vec = df_asjp %>% 
    filter(ISO6393 %in% iso_vec) %>% 
    pull(ISO6393)
  
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

#### glottolog -----------

# define each isolate as its own family
df_glotto = df_glotto %>%
  mutate(family = case_when(
    family == "isolate" ~ paste("isolate", glottocode, sep = "_"),
    family == "" ~ paste("isolate", glottocode, sep = "_"),
    is.na(family) ~ paste("isolate", glottocode, sep = "_"),
    TRUE ~ family
  ))

# bookkeepings are removed since they can't be assigned any family
df_glotto = df_glotto %>% filter(family != "Bookkeeping")

get_phyl_similarity_matrix = function(langs){
  # takes a vector of ISO codes and returns a similarity matrix with 1 - jaccarddistance
  
  #langs in glotto
  langs_in_glotto = df_glotto %>% filter(ISO6393 %in% langs) %>% pull(ISO6393)
  
  # calculate distances
  dist_matrix = df_glotto %>%
    dplyr::select(ISO6393, family) %>%
    filter(ISO6393 %in% langs_in_glotto) %>%
    separate_rows(family, sep = ",") %>%
    pivot_wider(names_from = family, values_from = family, values_fill = NA) %>%
    dplyr::select(-c(ISO6393)) %>%
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

##### testing

test = c("swe", "dan", "fra", "eng", "fns")
get_phon_similarity_matrix(test)
get_syn_similarity_matrix(test)
get_lex_similarity_matrix(test)
get_phyl_similarity_matrix(test)

##### get smallest common denominator for correlation analysis

scd = df_glotto %>%
  filter(ISO6393 %in% df_grambank$ISO6393) %>%
  filter(ISO6393 %in% df_asjp$ISO6393) %>% 
  filter(ISO6393 %in% df_phoible$ISO6393)

write_csv(scd, "Clean_data/language_lvl_data/langs_in_all_db.csv")

scd_iso = scd %>%
  pull(ISO6393)

# uncomment to create matrices
#phon_mat = get_phon_similarity_matrix(scd_iso)
#syn_mat = get_syn_similarity_matrix(scd_iso)
#lex_mat = get_lex_similarity_matrix(scd_iso)
#phyl_mat = get_phyl_similarity_matrix(scd_iso)

#write_rds(phon_mat, "clean_data/distances/phon_mat_all_common.rds")
#write_rds(syn_mat, "clean_data/distances/syn_mat_all_common.rds")
#write_rds(lex_mat, "clean_data/distances/lex_mat_all_common.rds")
#write_rds(phyl_mat, "clean_data/distances/phyl_mat_all_common.rds")
#

