library(tidyverse)

# this script reads all calculated coverages and distance measures on a country level
# and outputs a combined data object with all coverages and distances
# a csv is also outputted with mean pairwise similarity for each country

# read data
phoib = readRDS("Clean_data/distances/phoible_coverage_similarity_population.rds")
gram = readRDS("Clean_data/distances/grambank_coverage_similarity_population.rds")
asjp = readRDS("Clean_data/distances/asjp_coverage_similarity_population.rds")
glotto = readRDS("Clean_data/distances/glottolog_similarity.rds")

# check that all countries are present
nrow(phoib)
nrow(gram)
nrow(asjp)
nrow(glotto)

# check that the baselevel was correctly calculated
table(phoib$n_langs == gram$n_langs)
table(gram$n_langs == asjp$n_langs)

# select relevant columns
phoib = phoib %>%
  select(country_code, country, n_langs, n_speakers, n_langs_in_phoible, n_speakers_in_phoible, phon_similarity) %>%
  rename("langs" = n_langs,
         "speakers" = n_speakers,
         "langs_phoible" = n_langs_in_phoible,
         "speakers_phoible" = n_speakers_in_phoible,
         "sim_matrix_phoible" = phon_similarity)

gram = gram %>%
  select(country_code, n_langs_in_grambank, n_speakers_in_grambank, feat_sim_matrix) %>%
  rename("langs_grambank" = n_langs_in_grambank,
         "speakers_grambank" = n_speakers_in_grambank,
         "sim_matrix_grambank" = feat_sim_matrix)

asjp = asjp %>%
  select(country_code, n_langs_in_asjp, n_speakers_in_asjp, ldn_sim_matrix) %>%
  rename("langs_asjp" = n_langs_in_asjp,
         "speakers_asjp" = n_speakers_in_asjp,
         "sim_matrix_asjp" = ldn_sim_matrix)

glotto = glotto %>%
  rename("sim_matrix_glottolog" = phylogenetic_similarity)

# join dataframe

dbs = phoib %>%
  left_join(gram, by = "country_code") %>%
  left_join(asjp, by = "country_code") %>%
  left_join(glotto, by = "country_code")

# write
write_rds(dbs, "Clean_data/distances/all_coverage_sim_m.rds")

#### calculate mean similarity per country---------

get_mean_sim = function(sim_m){
  # takes a similarity matrix and returns the average similarity of the upper matrix
  
  # extracts values in upper triangle
  vals = sim_m[upper.tri(sim_m)]
  
  #remove potential NAs
  vals = vals[!is.na(vals)]

  # get mean  
  mean_sim = mean(vals)

  
  return(mean_sim)
}

# countries with no coverage
dbs_no_coverage =   dbs %>%
  filter((langs_phoible & langs_grambank & langs_asjp) == 0)

# filter countries without any coverage
dbs = dbs %>%
  filter((langs_phoible & langs_grambank & langs_asjp) != 0)

# get mean pairwise similarity
dbs = dbs %>% 
  mutate(mean_phoible_similarity = get_mean_sim(sim_matrix_phoible),
         mean_grambank_similarity = get_mean_sim(sim_matrix_grambank),
         mean_asjp_similarity = get_mean_sim(sim_matrix_asjp),
         mean_glottolog_similarity = get_mean_sim(sim_matrix_glottolog))

dbs_csv = dbs %>%
  select(!c(sim_matrix_asjp,
            sim_matrix_phoible,
            sim_matrix_grambank,
            sim_matrix_glottolog))

write_csv(dbs_csv, "Clean_data/country_lvl_data/countries_mean_similarities.csv")



