library(tidyverse)
library(DescTools)
library(rnaturalearth)
library(rnaturalearthdata)
library(boot)
library(patchwork)
library(ggrepel)
library(corrplot)
library(xtable)
library(GGally)

# in this script, the linguistic diversity of the world is assessed based on richness, relative abundance
# and similarity measures

theme_set(theme_bw())

# read data
df_phon = readRDS("~/Projekt/Master_arbeit_2_new_data/Clean_data/distances/phoible_coverage_similarity_population.rds")
df_syn =  readRDS("~/Projekt/Master_arbeit_2_new_data/Clean_data/distances/grambank_coverage_similarity_population.rds")
df_lex = readRDS("~/Projekt/Master_arbeit_2_new_data/Clean_data/distances/asjp_coverage_similarity_population.rds")
df_phyl = readRDS("~/Projekt/Master_arbeit_2_new_data/Clean_data/distances/glottolog_similarity.rds")
df_country = read_csv("Clean_data/country_lvl_data/countries_mean_similarities.csv")
df_pop = read_csv("Clean_data/country_lvl_data/country_lang_pop.csv")
df_asjp = read_csv("Clean_data/language_lvl_data/asjp_wide.csv")

df_pop %>%
  summarize(countries = n_distinct(country_code))

df_country %>% 
  summarize(countries = n_distinct(country_code))
# extract similarity matrices

# namibia
df_country = df_country %>% 
  mutate(country_code = case_when(country == "Namibia" ~ "NA",
                                  TRUE ~ country_code))
df_pop = df_pop %>%
  mutate(country_code = case_when(country == "Namibia" ~ "NA",
                                  TRUE ~ country_code))



get_inverse_simpson = function(pop_vec){
  # takes a vector with population numbers and calculates the inverse simpson for that vector
  # 1/(sum(pi * pi)
  prop = pop_vec / sum(pop_vec)
  simps = sum(prop*prop)
  return(1/simps)
  
}

get_simpson_diversity = function(prop, sim_m){
  # takes a vector with proportions and a matrix with similarities 
  # and calcualte the leinster cobbold diversity
  #D = 1/sum(pi * Zij * pj)
  
  # matrix multiply the similarities with the proportions
  # -> expected similarity of each language weighted by proportion
  expected = sim_m %*% prop
  
  # multiply expected similarities with their propotion, sum and take the inverse
  D = 1/sum(prop * expected)
  return(D)
}


get_lex_diversity_q2 = function(c){
  # takes a country code and returns the proportions of languages based on present
  # in asjp and the lexical similarity weighted diversity with q = 2
  lex_m = df_lex %>% 
    filter(country_code == c) %>%
    pull(ldn_sim_matrix)
  lex_m = lex_m[[1]]

  # subset to langs in matrix and order
  props = df_pop %>% filter(country_code == c) %>%
    filter(ISO6393 %in% colnames(lex_m)) %>%
    mutate(prop = population /sum(population)) %>%
    select(ISO6393, prop) %>%
    mutate(order = match(ISO6393, colnames(lex_m))) %>%
    arrange(order) %>% pull(prop)
  
  # get diversity
  return(get_simpson_diversity(props, lex_m))
}


get_syn_diversity_q2 = function(c){
  # takes a country code and returns the proportions of languages based on present
  # in grambank and the syntacitcal similarity weighted diversity
  
  syn_m = df_syn %>% 
    filter(country_code == c) %>%
    pull(feat_sim_matrix)
  syn_m = syn_m[[1]]
  
  # assume complete Similarity in case of NA -> does not contribute to diversity
  syn_m[is.na(syn_m)] = 1
  
  # subset to langs in matrix and order
  props = df_pop %>% filter(country_code == c) %>%
    filter(ISO6393 %in% colnames(syn_m)) %>%
    mutate(prop = population /sum(population)) %>%
    select(ISO6393, prop) %>%
    mutate(order = match(ISO6393, colnames(syn_m))) %>%
    arrange(order) %>% pull(prop)
  
  # get diversity
  return(get_simpson_diversity(props, syn_m))
}

get_phon_diversity_q2 = function(c){
  # takes a country code and returns the proportions of languages based on present
  # in phoible and the phonemic similarity weighted diversity
  phon_m = df_phon %>% 
    filter(country_code == c) %>%
    pull(phon_similarity)
  phon_m = phon_m[[1]]
  
  # subset to langs in matrix and order
  props = df_pop %>% filter(country_code == c) %>%
    filter(ISO6393 %in% colnames(phon_m)) %>%
    mutate(prop = population /sum(population)) %>%
    select(ISO6393, prop) %>%
    mutate(order = match(ISO6393, colnames(phon_m))) %>%
    arrange(order) %>% pull(prop)
  
  # get diversity
  return(get_simpson_diversity(props, phon_m))
}

get_phyl_diversity_q2 = function(c){
  # takes a country code and returns the proportions of languages based on present
  # in glottolog and the phylogenetic similarity weighted diversity
  
  phyl_m = df_phyl %>% 
    filter(country_code == c) %>%
    pull(phylogenetic_similarity)
  phyl_m = phyl_m[[1]]
  
  # assume complete Similarity in case of NA
  phyl_m[is.na(phyl_m)] = 1
  
  # subset to langs in matrix and order
  props = df_pop %>% filter(country_code == c) %>%
    filter(ISO6393 %in% colnames(phyl_m)) %>%
    mutate(prop = population /sum(population)) %>%
    select(ISO6393, prop) %>%
    mutate(order = match(ISO6393, colnames(phyl_m))) %>%
    arrange(order) %>% pull(prop)
  
  # get diversity
  return(get_simpson_diversity(props, phyl_m))
}

get_shannon_entropy = function(pop_vec){
  # calculates the shannon entropy given a vector with populations
  prop_vec = pop_vec / sum(pop_vec)
  
  # calculate entropy
  shannon = -1 * sum(prop_vec*log(prop_vec))
  return(shannon)
}

get_exp_shannon = function(pop_vec){
  # wrapper function to turn shannon entropy into a hill number
  exp_shannon = exp(get_shannon_entropy(pop_vec))
  return(exp_shannon)
}

get_shannon_diversity = function(prop, sim_m){
  # calculates diversity for q = 1 ergo shannon given a vector with proportions
  
  # for each proportion, get the expected similarity to all other proportions
  expected = log(sim_m %*% prop)
  
  # for each proportion, multiply by expected similarity to all other proportions
  # and derive entropy
  E = -1 * sum(prop * expected)
  
  # exponentiate entropy to get diversity
  D = exp(E)
  
  return(D)
}

get_diversity_q = function(prop, sim_m, q){
  # a general function to implement diversity for any q
  
  # to avoid division with zero, implement shannon diversity as a special case
  if (q == 1){
    return(get_shannon_diversity(prop, sim_m))
  }
  
  # get expected similarity to all other prop for each proportion
  expected = sim_m %*% prop
  
  # raise the expected similarity to the power of q-1
  expected_order = expected^(q-1)
  
  # multiply the expected similarity with each proportion and take the reciprocal
  D = (sum(prop * expected_order))^(1/(1-q))
  
  return(D)
}

get_naive_div_q = function(c, q) {
  # function takes a country code and calculates the naive diversity of order q
  
  # subset to langs in matrix and order
  props = df_pop %>% filter(country_code == c) %>%
    mutate(prop = population /sum(population)) %>% pull(prop)
  
  # create a similarity matrix with the length of props
  I = diag(length(props))
  
  # get diversity
  D = get_diversity_q(props, I, q)
  return(D)
}
get_naive_div_q(c = "RS", q = 0)

get_lex_div_q = function(c, q) {
  # function takes a country code and calculates the lexical diversity of order q
  lex_m = df_lex %>% 
    filter(country_code == c) %>%
    pull(ldn_sim_matrix)
  lex_m = lex_m[[1]]
  
  # subset to langs in matrix and order
  props = df_pop %>% filter(country_code == c) %>%
    filter(ISO6393 %in% colnames(lex_m)) %>%
    mutate(prop = population /sum(population)) %>%
    select(ISO6393, prop) %>%
    mutate(order = match(ISO6393, colnames(lex_m))) %>%
    arrange(order) %>% pull(prop)
  
  # get diversity
  D = get_diversity_q(props, lex_m, q)
  return(D)
}

get_lex_div_q(c = "RS", q = 0)


# calcualtes the similarity weighted diversity for all types; obs -
# missing population numbers makes syn and phon senseless
df_sim_div = df_country %>%
  rowwise() %>%
  mutate(lex_diversity_q2 = get_lex_diversity_q2(country_code),
         syn_diversity_q2 = get_syn_diversity_q2(country_code),
         phon_diversity_q2 = get_phon_diversity_q2(country_code),
         phyl_diversity_q2 = get_phyl_diversity_q2(country_code))

write_csv(df_sim_div, "Clean_data/country_lvl_data/all_functional_diversities_q2.csv")

#### calculate_diversity measures with q = 0,1,2 based on lexical similarity

df_country = df_pop %>%
  select(country_code, country, ISO6393, population) %>%
  group_by(country_code) %>%
  summarize(richness = n_distinct(ISO6393),
            exponent_shannon = get_exp_shannon(population),
            inverse_simpson = get_inverse_simpson(population)) %>%
  left_join(df_country, by = "country_code") %>%
  rowwise() %>%
  mutate(
    lex_div_q_0 = get_lex_div_q(country_code, q = 0),
    lex_div_q_1 = get_lex_div_q(country_code, q = 1),
    lex_div_q_2 = get_lex_div_q(country_code, q = 2)
  ) 

# two countries for which calculation doesnt work with q = 2
df_country = df_country %>% mutate(
  lex_div_q_2 = case_when(country_code %in% c("JE", "PN") ~ 1,
                          TRUE ~ lex_div_q_2)
)

#### write data
write_csv(df_country, "Clean_data/country_lvl_data/country_diversity.csv")

#### richness. -----
# add geometry
df_map = rnaturalearth::ne_countries(scale = "large", type = "countries")
df_map = df_map %>%
  select(iso_a2_eh, geometry, continent)

df_map = df_map %>%
  mutate(iso_a2_eh = case_when(is.na(iso_a2_eh) ~ "NA",
                                  TRUE ~ iso_a2_eh)) %>%
  group_by(iso_a2_eh) %>%
  summarize(iso_a2_eh = first(iso_a2_eh),
         geometry = first(geometry),
         continent = first(continent))
head(df_map)

df_country = df_country %>% 
  left_join(df_map, join_by("country_code" == "iso_a2_eh")) %>%
  ungroup()

df_country = df_country %>%
  mutate(country = case_when(country_code == "TV" ~ "Tuvalu",
                   country_code == "TK" ~ "Tokelau",
                   country_code == "PN" ~ "Pitcairn Islands",
                   country_code == "ST" ~ "Sao Tome and Principe",
                   country_code == "JE" ~ "Jersey",
                   country_code == "NU" ~ "Niue",
                   TRUE ~ country))

# plot map
ggplot(data = df_country) + 
  geom_sf(aes(geometry = geometry, fill = langs), color = "black") + 
  scale_fill_viridis_c() + 
  labs(title = "Richness",
       fill = "Richness") +
  theme(legend.position = "bottom",
        axis.ticks = element_blank(),
        axis.text = element_blank())


# top 5 countries according to richness
df_country %>%
  select(country, richness) %>%
  slice_max(richness, n = 10)


#### mean diversity -----

violin_sims = df_country %>%
  pivot_longer(cols = starts_with("mean"), values_to = "values") %>%
  mutate(name = case_when(name == "mean_phoible_similarity" ~ "Mean Phonemic Similarity",
                          name == "mean_grambank_similarity" ~ "Mean Morphosyntactic Similarity",
                          name == "mean_glottolog_similarity" ~ "Mean Phylogenetic Similarity",
                          TRUE ~ "Mean Lexical Similarity")) %>%
  ggplot(aes(x = values, y = name, fill = name), color = "black") + 
  geom_violin() + 
  geom_boxplot(width = 0.1, fill = "white") + 
  labs(x = "Similarity",
       y = NULL,
       fill = "Measure") + 
  theme(legend.position = "none")
plot(violin_sims)
ggsave("Diversity_analysis/plots/violin_sims.png", violin_sims, width = 8, height = 4, dpi = 300)

### add color for NA

# mean phonemic similarity
Desc(df_country$mean_phoible_similarity)
map_mean_phon = ggplot(data = df_country) + 
  geom_sf(aes(geometry = geometry, fill = mean_phoible_similarity), color = "black") + 
  scale_fill_viridis_c(na.value = NA) + 
  labs(fill ="Similarity",
       title = "a) Mean Phonemic Similarity") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())
plot(map_mean_phon)

# mean syntactic
Desc(df_country$mean_grambank_similarity)
map_mean_syn = ggplot(data = df_country) + 
  geom_sf(aes(geometry = geometry, fill = mean_grambank_similarity), color = "black") + 
  scale_fill_viridis_c() + 
  labs(fill = "Similarity",
       title = "b) Mean Morphosyntactic Similarity") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())
plot(map_mean_syn)

# mean lexical
Desc(df_country$mean_asjp_similarity)
map_mean_lexicon = ggplot(data = df_country) + 
  geom_sf(aes(geometry = geometry, fill = mean_asjp_similarity), color = "black") + 
  scale_fill_viridis_c() + 
  labs(fill = "Similarity",
       title = "c) Mean Lexical Similarity") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()
        )
plot(map_mean_lexicon)

# mean phylogenetic
Desc(df_country$mean_glottolog_similarity)
map_mean_phyl = ggplot(data = df_country) + 
  geom_sf(aes(geometry = geometry, fill = mean_glottolog_similarity), color = "black") + 
  scale_fill_viridis_c() + 
  labs(fill = "Similarity",
       title = "d) Mean Phylogenetic Similarity") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()
  )
plot(map_mean_phyl)

maps = map_mean_phon / map_mean_syn / map_mean_lexicon / map_mean_phyl
plot(maps)
ggsave("Diversity_analysis/plots/maps_mean_sim.png", maps, width = 8, height = 10, dpi = 300)


#### correlation --------

# spearman function for bootstrapping
srho = function(data, indices) {
  d = data[indices, ]
  rho = cor(d[,1], d[,2], method = "spearman")
  
  return(rho)
}

# bootstraping funciton
boot_srho = function(data, x, y){
  boot_data = as.data.frame(data[,c(x, y)])
  boot_res = boot(boot_data, srho, R = 1000)
  ci = boot.ci(boot_res, type = "perc")  # or type = "bca"
  return(c(srho(boot_data), ci$percent[4:5]))
}


# select relevant columns and remove NA
corrs = df_country %>%
  select(mean_asjp_similarity,
         mean_grambank_similarity,
         mean_phoible_similarity,
         mean_glottolog_similarity) %>% na.omit()


set.seed(123)

# lex vs sym
ggplot(data = corrs, aes(y = mean_asjp_similarity, mean_grambank_similarity)) + 
  geom_point() + geom_smooth(method = "lm")

boot_srho(corrs, "mean_asjp_similarity", "mean_grambank_similarity")

# lex vs phon
ggplot(data = corrs, aes(y = mean_asjp_similarity, mean_phoible_similarity)) + 
  geom_point() + geom_smooth(method = "lm")

boot_srho(corrs, "mean_asjp_similarity", "mean_phoible_similarity")


# lex vs phyl
ggplot(data = df_country, aes(y = mean_asjp_similarity, mean_glottolog_similarity)) + 
  geom_point() + geom_smooth(method = "lm")

boot_srho(corrs, "mean_asjp_similarity", "mean_glottolog_similarity")

# syn vs phon
ggplot(data = df_country, aes(y = mean_grambank_similarity, mean_phoible_similarity)) + 
   geom_point() + geom_smooth(method = "lm")


boot_srho(corrs, "mean_grambank_similarity", "mean_phoible_similarity")

# syn vs phyl
ggplot(data = df_country, aes(y = mean_grambank_similarity, mean_glottolog_similarity)) + 
  geom_point() + geom_smooth(method = "lm")

boot_srho(corrs, "mean_grambank_similarity", "mean_glottolog_similarity")

# phon vs phyl
ggplot(data = df_country, aes(y = mean_glottolog_similarity, mean_phoible_similarity)) + 
  geom_point() + geom_smooth(method = "lm")

boot_srho(corrs, "mean_phoible_similarity", "mean_glottolog_similarity")

#### diversity -----

Desc(df_country$richness)
Desc(df_country$exponent_shannon)
Desc(df_country$inverse_simpson)
Desc(df_country$lex_div_q_0)
Desc(df_country$lex_div_q_1)
Desc(df_country$lex_div_q_2)

# maps
richness = ggplot(data = df_country) + 
  geom_sf(aes(geometry = geometry, fill = richness), color = "black") + 
  scale_fill_viridis_c() + 
  labs(fill = "Languages",
       title = "a) Naive Diversity, q = 0") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.key.size = unit(0.5, "cm"))

#top 10 richness
df_country %>%
  slice_max(richness, n =  10) %>%
  select(country_code, country, richness) %>% xtable()


exp_shan = ggplot(data = df_country) + 
  geom_sf(aes(geometry = geometry, fill = exponent_shannon), color = "black") + 
  scale_fill_viridis_c() + 
  labs(fill = "Languages",
       title = "b) Naive Diversity, q = 1") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.key.size = unit(0.5, "cm"))

# top 10 exp shannon
df_country %>%
  slice_max(exponent_shannon, n = 10) %>%
  select(country_code, country, exponent_shannon)

inv_simp = ggplot(data = df_country) + 
  geom_sf(aes(geometry = geometry, fill = inverse_simpson), color = "black") + 
  scale_fill_viridis_c() + 
  labs(fill = "Languages",
       title = "c) Naive Diversity, q = 2") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.key.size = unit(0.5, "cm"))

# top 10 inv simson
df_country %>%
  slice_max(inverse_simpson, n = 10) %>%
  select(country_code, country, inverse_simpson)

div_q_0 = ggplot(data = df_country) + 
  geom_sf(aes(geometry = geometry, fill = lex_div_q_0), color = "black") + 
  scale_fill_viridis_c() + 
  labs(fill = "Languages",
       title = "a) Diversity, q = 0") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

# top 10 div q = 0
df_country %>%
  slice_max(lex_div_q_0, n = 10) %>%
  select(country_code, country, lex_div_q_0)


div_q_1 = ggplot(data = df_country) + 
  geom_sf(aes(geometry = geometry, fill = lex_div_q_1), color = "black") + 
  scale_fill_viridis_c() + 
  labs(fill = "Languages",
       title = "b) Diversity, q = 1") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

# top 10 div q = 1
df_country %>%
  slice_max(lex_div_q_1, n = 10) %>%
  select(country, lex_div_q_1)


div_q_2 = ggplot(data = df_country) + 
  geom_sf(aes(geometry = geometry, fill = lex_div_q_2), color = "black") + 
  scale_fill_viridis_c() + 
  labs(fill = "Languages",
       title = "c) Diversity, q = 2") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

# top 10 div q = 2
df_country %>%
  slice_max(lex_div_q_2, n = 10) %>%
  select(country_code, country, lex_div_q_2, continent)

naive_plots = richness / exp_shan / inv_simp
ggsave("Diversity_analysis/plots/naive_plots.png", naive_plots, width = 8, height = 10, dpi = 300)


non_naive_plots = div_q_0 / div_q_1 / div_q_2
plot(non_naive_plots)
ggsave("Diversity_analysis/plots/non_naive_plots.png", non_naive_plots, width = 8, height = 10, dpi = 300)

# summarize per continent
df_country %>%
  filter(continent == "Seven seas (open ocean)")

df_country = df_country %>%
  mutate(continent = case_when(
    country_code == "CC" ~ "Asia",
    country_code == "CX" ~ "Oceania",
    country_code == "GF" ~ "South America",
    country_code == "GP" ~ "North America",
    country_code == "MQ" ~ "North America",
    country_code == "RE" ~ "Africa",
    country_code == "SJ" ~ "Europe",
    country_code == "YT" ~ "Africa",
    country_code == "IO" ~ "Asia",
    country_code == "MU" ~ "Africa",
    country_code == "MV" ~ "Asia",
    country_code == "SC" ~ "Africa",
    country_code == "SH" ~ "Africa",
    country_code == "TK" ~ "Oceania",
    TRUE ~ continent))



df_country %>%
  group_by(continent) %>%
  summarize(mean_richness = mean(richness),
            mean_exponent_shannon = mean(exponent_shannon),
            mean_inverse_simpson = mean(inverse_simpson),
            mean_lex_div_q_0 = mean(lex_div_q_0),
            mean_lex_div_q_1 = mean(lex_div_q_1),
            mean_lex_div_q_2 = mean(lex_div_q_2)) %>% View()


#### impact of adding similarity? --------------------

### subset to langs in asjp to control for lack of coverage
df_asjp = df_asjp %>%
  filter(words > 27)
df_pop_asjp = df_pop %>% 
  filter(ISO6393 %in% df_asjp$ISO6393)

# add measures
df_diversity = df_pop_asjp %>%
  group_by(country_code) %>% 
  summarize(richness = n_distinct(ISO6393),
            exponent_shannon = get_exp_shannon(population),
            inverse_simpson = get_inverse_simpson(population)) %>%
  filter(richness != 1) %>%
  left_join(df_country %>% select(country_code, country, continent, geometry), by = "country_code") %>%
  rowwise() %>%
  mutate(
    lex_div_q_0 = get_lex_div_q(country_code, q = 0),
    lex_div_q_1 = get_lex_div_q(country_code, q = 1),
    lex_div_q_2 = get_lex_div_q(country_code, q = 2)
  ) 

# check and fix NA
colSums(is.na(df_diversity))
df_diversity %>%
  filter(is.na(country))

df_diversity = df_diversity %>%
  mutate(country = case_when(country_code == "ST" ~ "São Tomé and Príncipe",
                             country_code == "TV" ~ "Tuvalu",
                             TRUE ~ country))




# impact of considering evenness
res = boot_srho(df_diversity, "richness", "inverse_simpson") # 0.43
boot_srho(df_diversity, "exponent_shannon", "inverse_simpson") # 0.97

# impact of considering similarity
boot_srho(df_diversity, "inverse_simpson", "lex_div_q_2") # 0.96 - small impact, few rank shifts

boot_srho(df_diversity, "richness", "exponent_shannon") # 0.57
boot_srho(df_diversity, "exponent_shannon", "lex_div_q_1") # 0.94

# impact of considering similarity and evennes
boot_srho(df_diversity, "richness", "lex_div_q_2") # 0.42

# rank difference measures
df_diversity$rank_inverse_simpson = rank(df_diversity$inverse_simpson)
df_diversity$rank_lex_div_q_2 = rank(df_diversity$lex_div_q_2)

df_diversity = df_diversity %>%
  mutate(rank_diff = rank_lex_div_q_2 - rank_inverse_simpson)

# top 10 rank gainers
top_10_gain = df_diversity %>%
  ungroup() %>%
  slice_max(n = 10, rank_diff, with_ties = FALSE) %>%
  select(country_code,
         country,
         continent,
         richness,
         inverse_simpson,
         lex_div_q_2,
         rank_diff)
print(top_10_gain)

# top 10 rank losers
top_10_loss = df_diversity %>%
  ungroup() %>%
  slice_min(n = 10, rank_diff, with_ties = FALSE) %>%
  select(country_code,
         country,
         continent,
         richness,
         inverse_simpson,
         lex_div_q_2,
         rank_diff)
View(top_10_loss)


# top 20 rank changes 
top_20_change = df_diversity %>%
  ungroup() %>%
  mutate(abs_rank_diff = abs(rank_diff)) %>%
  slice_max(n = 20, abs_rank_diff, with_ties = FALSE) %>%
  select(country_code,
         country,
         richness,
         inverse_simpson,
         lex_div_q_2,
         rank_diff,
         abs_rank_diff)
print(top_20_change)

df_diversity = df_diversity %>%
  mutate(label = if_else(country_code %in% top_20_change$country_code,
                         country,
                         NA_character_))

div_diff = ggplot(df_diversity,
       aes(x = rank_inverse_simpson, y = rank_lex_div_q_2)) + 
  geom_point(aes(fill = continent, color = continent)) + 
  geom_abline() + 
  geom_text_repel(aes(label = label),
                  size = 3,
                  segment.color = "Black",
                  segment.size = 0.4,
                  segment.alpha = 1) + 
  labs(x = "Rank Naive Diversity, q = 2",
       y = "Rank Non Naive Diversity, q = 2") + 
  theme(
    legend.position = c(0.85, 0.2))

ggsave("Diversity_analysis/plots/naive_vs_non_naive.png", div_diff, width = 8, height = 6, dpi = 300)


df_diversity %>%
  group_by(continent) %>%
  summarize(mean_richness = mean(richness),
            mean_inverse_simpson = mean(inverse_simpson),
            mean_lex_div_q_2 = mean(lex_div_q_2)) %>%
  mutate(relative_loss = (mean_inverse_simpson-mean_lex_div_q_2)/mean_inverse_simpson)

##### specific countries

df_pop_asjp %>% 
  filter(country == "Zambia") %>%
  View()



##### calculate the diversity profile for different countries and compare


get_diversity_profile = function(c, non_naive = TRUE, range = 4) {
  # this function takes a country or a vector of countries calculates its diversity profile, if not naive, it uses lexical similarity
  
  if (is.vector(c) && length(c) == 1){ # check if string
    #vecs to gold values
    div_values = c()
    qs = c()
    # loop through qs
    for (q in seq(0,range, 0.05)){
      # if naive, run applicable fucntion
      if (non_naive == TRUE){
        div_values = c(div_values, get_lex_div_q(c, q = q))
      } else {
        div_values = c(div_values, get_naive_div_q(c, q = q))
      }
      # save qs
      qs = c(qs, q)
    }
    # create dataframe for plotting
    div_prof = data.frame(qs, div_values)
    
    # plot
    p = div_prof %>%
      ggplot(aes(x = qs, y = div_values)) + 
      geom_line() + 
      labs(x = "q",
           y = "Diversity")
    
    plot(p)
    return(p)
  } else if(is.vector(c)){ # check if vector
      df_div_prof = data.frame()
      for (country_code in c){ 
        # get diveristy values for each country
        div_values = c()
        qs = c()
        for (q in seq(0,range, 0.05)){
          if (non_naive == TRUE) { # apply different function depending on if naive or not
            div_values = c(div_values, get_lex_div_q(country_code, q = q))
          } else {
            div_values = c(div_values, get_naive_div_q(country_code, q = q))
          }
          qs = c(qs, q)
        }
        # vector filled with country code
        name_vec = rep(country_code, length(div_values))
        # create a dataframe
        df_div_prof = rbind(df_div_prof, data.frame(name_vec, qs, div_values))
      }
      
      # plot the data
     p = df_div_prof %>%
       ggplot(aes(y = div_values, x = qs, fill = name_vec)) + 
       geom_line(aes(color = name_vec, linetype = name_vec)) + 
       labs(x = "q",
            y = "Diversity",
            linetype = "Country",
            color = "Country")
     plot(p)
     return(p)
  }
}

 df_diversity %>%
  ungroup() %>%
  slice_max(lex_div_q_1, n = 20)

div_prof_non_naive = get_diversity_profile(c("PG", "VU", "US"), non_naive = FALSE)
ggsave("Diversity_analysis/plots/div_profile_naive.png", width = 8, height = 6, dpi = 300)

div_prof_non_naive = get_diversity_profile(c("PG", "VU", "CM"), non_naive = TRUE, range = 10)
ggsave("Diversity_analysis/plots/divprofile_non_naiv.png", width = 8, height = 6, dpi = 300)

div_prof_non_naive = get_diversity_profile(top_10, non_naive = TRUE, range = 10)

### corrplot


# naive diversity


ggpairs(na.omit(df_country) %>%
          select(continent, richness, exponent_shannon, inverse_simpson) %>%
          ungroup() %>%
          mutate(richness = rank(richness), exponent_shannon = rank(exponent_shannon), inverse_simpson = rank(inverse_simpson)) %>%
          rename("Naive Diversity, q = 0" = richness, "Naive Diversity, q = 1" = exponent_shannon, "Naive Diversity, q = 2" = inverse_simpson),
        upper = list(continuous = wrap("cor", stars = FALSE)),
        ggplot2::aes(color = continent))



# non naive diversity
cm = df_diversity %>%
  select(richness, exponent_shannon, inverse_simpson, lex_div_q_0, lex_div_q_1, lex_div_q_2) %>%
  cor(., method = "spearman")

colnames(cm) = c("Naive Diversity, q = 0", "Naive Diversity, q = 1", "Naive Diversity, q = 2", "Diversity, q = 0", "Diversity, q = 1", "Diversity, q = 2")
rownames(cm) = c("Naive Diversity, q = 0", "Naive Diversity, q = 1", "Naive Diversity, q = 2", "Diversity, q = 0", "Diversity, q = 1", "Diversity, q = 2")
corrplot.mixed(cm, lower = "number", upper = "circle", tl.col = "black", tl.pos = "l", tl.srt = 45)


ggpairs(df_diversity %>%
          select(continent, richness, exponent_shannon, inverse_simpson, lex_div_q_0, lex_div_q_1, lex_div_q_2) %>%
          ungroup() %>%
          mutate(richness = rank(richness), exponent_shannon = rank(exponent_shannon), inverse_simpson = rank(inverse_simpson),
                 lex_div_q_0 = rank(lex_div_q_0), lex_div_q_1 = rank(lex_div_q_1), lex_div_q_2 = rank(lex_div_q_2)) %>%
          rename("Naive Diversity, q = 0" = richness, "Naive Diversity, q = 1" = exponent_shannon, "Naive Diversity, q = 2" = inverse_simpson,
                 "Non-naive Diversity, q = 0" = lex_div_q_0, "Non-naive Diversity, q = 1" = lex_div_q_1, "Non-naive Diversity, q = 2" = lex_div_q_2),
        upper = list(continuous = wrap("cor", stars = FALSE)),
        ggplot2::aes(color = continent))

rank(df_diversity$richness)

### table for appendix

df_country %>%
  mutate(lang_coverage_phoible = round(langs_phoible/langs * 100, 2),
         speaker_coverage_phoible = round(speakers_phoible/speakers * 100, 2),
         lang_coverage_grambank = round(langs_grambank/langs * 100, 2),
         speaker_coverage_grambank = round(speakers_grambank/speakers * 100, 2),
         lang_coverage_asjp = round(langs_asjp/langs * 100, 2),
         speaker_coverage_asjp = round(speakers_asjp/speakers * 100, 2)) %>%
  select(country_code, country,
         lang_coverage_phoible,
         speaker_coverage_phoible,
         lang_coverage_grambank,
         speaker_coverage_grambank,
         lang_coverage_asjp,
         speaker_coverage_asjp) %>%
  xtable()


df_country %>% 
  select(country, mean_phoible_similarity, mean_grambank_similarity, mean_asjp_similarity, mean_glottolog_similarity) %>%
  xtable()


df_country %>%
  select(country, richness, exponent_shannon, inverse_simpson, lex_div_q_0, lex_div_q_1, lex_div_q_2) %>%
  xtable()
