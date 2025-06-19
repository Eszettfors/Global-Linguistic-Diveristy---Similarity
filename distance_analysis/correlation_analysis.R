library(tidyverse)
library(corrplot)
library(DescTools)
library(patchwork)

#### this script takes the subset of all languages existing in all databases
# and their similarity matrices and correlates them

# read data
phyl_mat = readRDS("~/Projekt/Master_arbeit_2_new_data/Clean_data/distances/phyl_mat_all_common.rds")
lex_mat = readRDS("~/Projekt/Master_arbeit_2_new_data/Clean_data/distances/lex_mat_all_common.rds")
phon_mat = readRDS("~/Projekt/Master_arbeit_2_new_data/Clean_data/distances/phon_mat_all_common.rds")
syn_mat = readRDS("~/Projekt/Master_arbeit_2_new_data/Clean_data/distances/syn_mat_all_common.rds")
df_glotto = read_csv("Clean_data/language_lvl_data/Glottolog_processed.csv")


ncol(phyl_mat) # 1012
ncol(phyl_mat) # 1012
# 1012 languages -> 1024144 datapoints in each matrix

theme_set(theme_bw())

## align order of matrices
align_matrix = function(m){
  m = m[order(rownames(m)),]
  m = m[, order(colnames(m))]
  return(m)
}

# align matrices alphabetically
phyl_mat = align_matrix(phyl_mat)
lex_mat = align_matrix(lex_mat)
syn_mat = align_matrix(syn_mat)
phon_mat = align_matrix(phon_mat)


#### pivot the matrix long
phyl_long = as.data.frame(phyl_mat) %>%
  rownames_to_column(var = "lang1") %>%
  pivot_longer(-lang1, names_to = "lang2",
               values_to = "phyl_sim")

# add lang family and macroarea
phyl_long = df_glotto %>%
  select(ISO6393, family, macroarea) %>%
  right_join(phyl_long, join_by("ISO6393" == "lang1")) %>%
  rename(lang_1_family = "family", lang_1_macroarea = "macroarea", lang1 = "ISO6393")

phyl_long = df_glotto %>%
  select(ISO6393, family, macroarea) %>%
  right_join(phyl_long, join_by("ISO6393" == "lang2")) %>%
  rename(lang_2_family = "family", lang_2_macroarea = "macroarea", lang2 = "ISO6393")

# remove doublet parirs and create a unique identifier
phyl_long = phyl_long %>%
  filter(match(lang1, rownames(phyl_mat)) < match(lang2, colnames(phyl_mat))) %>%
  unite("lang_pair", lang1, lang2, sep = "_", remove = FALSE)

#lex
lex_long = as.data.frame(lex_mat) %>%
  rownames_to_column(var = "lang1") %>%
  pivot_longer(-lang1, names_to = "lang2",
               values_to = "lex_sim")

lex_long = lex_long %>%
  filter(match(lang1, rownames(lex_mat)) < match(lang2, colnames(lex_mat))) %>%
  unite("lang_pair", lang1, lang2, sep = "_", remove = TRUE)

# syn
syn_long = as.data.frame(syn_mat) %>%
  rownames_to_column(var = "lang1") %>%
  pivot_longer(-lang1, names_to = "lang2",
               values_to = "syn_sim")

syn_long = syn_long %>%
  filter(match(lang1, rownames(syn_mat)) < match(lang2, colnames(syn_mat))) %>%
  unite("lang_pair", lang1, lang2, sep = "_", remove = TRUE)

# phon
phon_long = as.data.frame(phon_mat) %>%
  rownames_to_column(var = "lang1") %>%
  pivot_longer(-lang1, names_to = "lang2",
               values_to = "phon_sim")

phon_long = phon_long %>%
  filter(match(lang1, rownames(phon_mat)) < match(lang2, colnames(phon_mat))) %>%
  unite("lang_pair", lang1, lang2, sep = "_", remove = TRUE)


### join data frames
df_sim = phyl_long %>%
  left_join(lex_long, by = "lang_pair") %>%
  left_join(syn_long, by = "lang_pair") %>%
  left_join(phon_long, by = "lang_pair")

colSums(is.na(df_sim)) # 16549 NA in morphosyntactic similarities


# filter NA from syn_sim
df_sim = df_sim %>% 
  filter(!is.na(syn_sim))
nrow(df_sim)

# get descriptive statistic
Desc(df_sim$phyl_sim)
Desc(df_sim$lex_sim)
Desc(df_sim$syn_sim)
Desc(df_sim$phon_sim)

# get procent of lang paris with more than 50% syn similarity
ecdf_syn = ecdf(df_sim$syn_sim)
ecdf_syn(0.5)

### qqplot for syntactical similarity
qq_syn = df_sim %>%
  ggplot(aes(sample = syn_sim)) + 
  geom_qq() + 
  stat_qq_line() + 
  labs(x = "Theoretical Quantiles",
       y = "Sample Quantiles",
       title = "Normal Q-Q Plot of Morphosyntactical Similarity")

# save 
ggsave("distance_analysis/plots/qq_syn.png", qq_syn, width = 8, height = 4)

# rename for plotting purposes
df_sim = df_sim %>%
  rename(Phylogenetic = "phyl_sim",
         Lexical = "lex_sim",
         Morphosyntactic = "syn_sim",
         Phonemic = "phon_sim")

# pivotlong
df_sim_long = df_sim %>% pivot_longer(cols = c("Phylogenetic",
                                               "Lexical",
                                               "Morphosyntactic",
                                               "Phonemic"), values_to = "values") %>%
  rename("Similarity" = name)


# plot violins
ggplot(data = df_sim_long, aes(y = values, fill = Similarity,  x = Similarity)) + 
  geom_violin(trim = TRUE, alpha = 0.7, color = "black") +
  geom_boxplot(width = 0.04, fill = "white") + 
  coord_flip() + 
  labs(x = NULL,
       y = "Similarity") + 
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14))

# plot histograms
histograms = ggplot(data = df_sim_long, aes(x = values, fill = Similarity)) + 
  geom_histogram(alpha = 0.5, position = "identity", color = "black") + 
  geom_density() + 
  labs(x = "Similarity",
       y = "Frequency") + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.85, 0.8))
plot(histograms)

# save histogram plot
ggsave("distance_analysis/plots/histogram_measures.png", histograms, width = 8, height = 4)

####  lex vs phyl?
lex_phyl = ggplot(df_sim, aes(y = Lexical,
                   x = Phylogenetic)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "a) Lexical and Phylogenetic Similarity,\n rho = 0.210, p < 0.001")
print(lex_phyl)
cor.test(df_sim$Lexical, df_sim$Phylogenetic, method = "spearman")

# lex vs syn
lex_morph = ggplot(df_sim, aes(y = Lexical,
                   x = Morphosyntactic)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "b) Lexical and Morphosyntactic Similarity,\n rho = 0.06, p < 0.001")
plot(lex_morph)
cor.test(df_sim$Lexical, df_sim$Morphosyntactic, method = "spearman")


# lex vs phon
lex_phon = ggplot(df_sim, aes(y = Lexical,
                   x = Phonemic)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "c) Lexical and Phonemic inventory Similarity,\n rho = 0.175, p < 0.001")
plot(lex_phon)
cor.test(df_sim$Lexical, df_sim$Phonemic, method = "spearman")
kendall_cor(df_sim$Lexical, df_sim$Phonemic)

# syn vs phyl
syn_phyl = ggplot(df_sim, aes(y = Morphosyntactic,
                   x = Phylogenetic)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "d) Morphosyntactic and Phylogenetic Similarity,\n rho = 0.22, p < 0.001")
plot(syn_phyl)
cor.test(df_sim$Morphosyntactic, df_sim$Phylogenetic, method = "spearman")

# syn vs phon
syn_phon = ggplot(df_sim, aes(y = Morphosyntactic,
                   x = Phonemic)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "e) Morphosyntactic and Phonemic inventory Similarity,\n rho = 0.10, p < 0.001")
plot(syn_phon)
cor.test(df_sim$Morphosyntactic, df_sim$Phonemic, method = "spearman")


# phon vs phyl
phon_phyl = ggplot(df_sim, aes(y = Phonemic,
                   x = Phylogenetic)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "f) Phylogenetic and Phonemic inventory Similarity,\n rho = 0.18, p < 0.001")
plot(phon_phyl)
cor.test(df_sim$Phylogenetic, df_sim$Phonemic, method = "spearman")

#plot all
(lex_phyl + lex_morph) / (lex_phon + syn_phyl) / (syn_phon + phon_phyl)



write_csv(df_sim, "Clean_data/distances/language_pairs_and_all_similarities.csv")


View(df_sim)

######

df_sim %>% filter(Phylogenetic > 0.75) %>%
  View()

df_sim %>% filter(lang2 == "eng") %>% View()
