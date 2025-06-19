library(lingtypology)
library(tidyverse)
library(vegan)

# allows the calculation of phylogenetic similarity by any number of languages in glottolog
# based on jaccard similarity


df_glotto = lingtypology::glottolog
df_glotto = as_tibble(df_glotto)

# select languages in ethnologue data

df_glotto

head(df_glotto)
# affiliation contains all branches -> turn each possible branch into a vector -> 

# isolates are assigned different "isolate-families"
df_glotto = df_glotto %>%
  mutate(affiliation = case_when(
    affiliation == "" ~ paste("isolate", iso, sep = "_"),
    TRUE ~ affiliation
  ))
  

get_phyl_similarity_matrix = function(langs){
    # takes a vector of ISO codes and returns a similarity matrix with 1 - jaccarddistance
    
    #langs in glotto
    langs_in_glotto = df_glotto %>% filter(iso %in% langs) %>% pull(iso)
  
    # calculate distances
    dist_matrix = df_glotto %>%
    select(iso, affiliation) %>%
    filter(iso %in% langs_in_glotto) %>%
    separate_rows(affiliation, sep = ",") %>%
    pivot_wider(names_from = affiliation, values_from = affiliation, values_fill = NA) %>%
    select(-c(iso)) %>%
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



