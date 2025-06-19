library(tidyverse)
source("distance_analysis/all_similarities_all_db.R")
library(cluster)
library(dendextend)
library(patchwork)
library(MASS)
library(ggrepel)
library(plotly)
library(patchwork)
library(geosphere)
# this script takes a small number of european languages, calculates similarity matrices between them and perfoms a cluster analysis based on 
# hierarchical clustering

# read data
europe = read_csv("Clean_data/language_lvl_data/european_languages_in_all_db.csv")
nrow(europe) # 36 languages
europe = europe %>%
  mutate(family = case_when(language == "lithuanian" ~ "Slavic",
                            TRUE ~ family))
 
europe_iso = europe %>% pull(ISO6393)
europe_names = europe %>% pull(language)
europe_family = europe %>% pull(family)


#named vector
iso_to_name = setNames(europe_names, europe_iso)
iso_to_family = setNames(europe_family, europe_iso)

### get similarity matrices
phon_sim = get_phon_similarity_matrix(europe_iso)
rownames(phon_sim) = iso_to_name[rownames(phon_sim)]
colnames(phon_sim) = iso_to_name[colnames(phon_sim)]

syn_sim = get_syn_similarity_matrix(europe_iso)
rownames(syn_sim) = iso_to_name[rownames(syn_sim)]
colnames(syn_sim) = iso_to_name[colnames(syn_sim)]

lex_sim = get_lex_similarity_matrix(europe_iso)
rownames(lex_sim) = iso_to_name[rownames(lex_sim)]
colnames(lex_sim) = iso_to_name[colnames(lex_sim)]

phyl_sim = get_phyl_similarity_matrix(europe_iso)
rownames(phyl_sim) = iso_to_name[rownames(phyl_sim)]
colnames(phyl_sim) = iso_to_name[colnames(phyl_sim)]


## align matrices

align_matrix = function(m){
  m = m[order(rownames(m)),]
  m = m[, order(colnames(m))]
  return(m)
}

syn_sim = align_matrix(syn_sim)
lex_sim = align_matrix(lex_sim)
phyl_sim = align_matrix(phyl_sim)
phon_sim = align_matrix(phon_sim)

set.seed(123)

par(mfrow = c(1,1))
par(mar = c(5, 5, 4, 2))
# phylogenetic clustering

phyl_dist = as.dist(1-phyl_sim)
phyl_clust = hclust(phyl_dist, method = "average")
plot(phyl_clust, cex = 0.8, hang = -1, main = "Phylogenetic Clustering", xlab = "", sub = "")
phyl_dend = as.dendrogram(phyl_clust)



# lexical clustering
lex_dist = as.dist(1-lex_sim)
lex_clust = hclust(lex_dist, method = "average")
plot(lex_clust, cex = 0.8, hang = -1, main = "Lexical Clustering", xlab = "", sub = "")
lex_dend = as.dendrogram(lex_clust)


# syntactic clustering
syn_dist = as.dist(1-syn_sim)
syn_clust = hclust(syn_dist, method = "average")
plot(syn_clust, cex = 0.8, hang = -1, main = "Morphosyntactic Clustering", xlab = "", sub = "")
syn_dend = as.dendrogram(syn_clust)


# phonetic clustering
phon_dist = as.dist(1-phon_sim)
phon_clust = hclust(phon_dist, method = "average")
plot(phon_clust, cex = 0.8, hang = -1, main = "Phonemic Clustering", xlab = "", sub = "")
phon_dend = as.dendrogram(phon_clust)




# phyl vs lex
dend_list_phyl_lex = dendlist(phyl_dend, lex_dend)
dend_list_aligned_phyl_lex = untangle(dend_list_phyl_lex, method = "step2side")
entanglement(dend_list_aligned_phyl_lex)
tanglegram(dend_list_aligned_phyl_lex,
           sort = FALSE,
           highlight_distinct_edges = FALSE,
           common_subtrees_color_lines = TRUE,
           common_subtrees_color_branches = TRUE, 
           main = paste("Entanglement =", round(entanglement(dend_list_aligned_phyl_lex), 2)),
           main_left = "Phylogenetic Cluster",
           main_right = "Lexical Cluster")

# phyl vs syn
dend_list_phyl_syn = dendlist(phyl_dend, syn_dend)
dend_list_aligned_phyl_syn = untangle(dend_list_phyl_syn, method = "step2side")
entanglement(dend_list_aligned_phyl_syn)

tanglegram(dend_list_aligned_phyl_syn,
           sort = FALSE,
           highlight_distinct_edges = FALSE,
           common_subtrees_color_lines = TRUE,
           common_subtrees_color_branches = TRUE, 
           main = paste("Entanglement =", round(entanglement(dend_list_aligned_phyl_syn), 2)),
           main_left = "Phylogenetic Cluster",
           main_right = "Syntactical Cluster")

# phyl vs phon
dend_list_phyl_phon = dendlist(phyl_dend, phon_dend)
dend_list_aligned_phyl_phon= untangle(dend_list_phyl_phon, method = "step2side")
entanglement(dend_list_aligned_phyl_phon)
tanglegram(dend_list_aligned_phyl_phon,
           sort = FALSE,
           highlight_distinct_edges = FALSE,
           common_subtrees_color_lines = TRUE,
           common_subtrees_color_branches = TRUE, 
           main = paste("Entanglement =", round(entanglement(dend_list_aligned_phyl_phon), 2)),
           main_left = "Phylogenetic Cluster",
           main_right = "Phonemic cluster")


# lex vs syn 
dend_list_lex_syn = dendlist(lex_dend, syn_dend)
dend_list_aligned_lex_syn= untangle(dend_list_lex_syn, method = "step2side")
entanglement(dend_list_aligned_lex_syn)
tanglegram(dend_list_aligned_lex_syn,
           sort = FALSE,
           highlight_distinct_edges = FALSE,
           common_subtrees_color_lines = TRUE,
           common_subtrees_color_branches = TRUE, 
           main = paste("Entanglement =", round(entanglement(dend_list_aligned_lex_syn), 2)),
           main_left = "Lexical Cluster",
           main_right = "Syntactic cluster")

# lex vs phon
dend_list_lex_phon = dendlist(lex_dend, phon_dend)
dend_list_aligned_lex_phon = untangle(dend_list_lex_phon, method = "step2side")
entanglement(dend_list_aligned_lex_phon)
tanglegram(dend_list_aligned_lex_phon,
           sort = FALSE,
           highlight_distinct_edges = FALSE,
           common_subtrees_color_lines = TRUE,
           common_subtrees_color_branches = TRUE, 
           main = paste("Entanglement =", round(entanglement(dend_list_aligned_lex_phon), 2)),
           main_left = "Lexical Cluster",
           main_right = "phonemic cluster")

#syn vs phon 
dend_list_syn_phon = dendlist(syn_dend, phon_dend)
dend_list_aligned_syn_phon = untangle(dend_list_syn_phon, method = "step2side")
entanglement(dend_list_aligned_syn_phon)
tanglegram(dend_list_aligned_syn_phon,
           sort = FALSE,
           highlight_distinct_edges = FALSE,
           common_subtrees_color_lines = TRUE,
           common_subtrees_color_branches = TRUE, 
           main = paste("Entanglement =", round(entanglement(dend_list_aligned_syn_phon), 2)),
           main_left = "Syntactic Cluster",
           main_right = "Phonemic cluster"
           )


# cophenetic correlation

# cophenetic distance is the height where two languages join for the first time. The correlation takes these distances and correlates them
cor.test(as.vector(cophenetic(phyl_clust)), as.vector(cophenetic(lex_clust)))

cor.test(as.vector(cophenetic(phyl_clust)), as.vector(cophenetic(syn_clust)))


cor.test(as.vector(cophenetic(phyl_clust)), as.vector(cophenetic(phon_clust)))

cor.test(as.vector(cophenetic(lex_clust)), as.vector(cophenetic(syn_clust)))

cor.test(as.vector(cophenetic(lex_clust)), as.vector(cophenetic(phon_clust)))

cor.test(as.vector(cophenetic(syn_clust)), as.vector(cophenetic(phon_clust)))


##### MDS

## 2d plots

get_elbow = function(d){
stress_values = c()
for (k in 1:10) {
  mds_result = isoMDS(d, k = k)
  stress_values = c(stress_values, mds_result$stress)
}
# Plot stress vs dimensions
plot(1:10, stress_values, type = "b",
     xlab = "Number of Dimensions (k)",
     ylab = "Stress",
     main = "Elbow Method for isoMDS")
}

#phyl

get_elbow(phyl_dist) # optimum at k = 4
mds_phyl = isoMDS(phyl_dist, k = 4)
df_phyl = as.data.frame(mds_phyl$points)
colnames(df_phyl) = c("dimension_1", "dimension_2", "dimension_3", "dimension_4")
stress_phyl = mds_phyl$stress
print(stress_phyl) # excellent fit


df_phyl$language = rownames(df_phyl)
df_phyl = df_phyl %>% 
  left_join(europe, by = "language")

phyl_2d = ggplot(df_phyl,aes(y = dimension_2, x = dimension_1, label = language)) + 
         geom_point(aes(color = family, size = 1)) + 
  geom_text_repel(vjust = -0.5, size = 3,
                  max.overlaps = 50) +
  labs(x = "First Dimension",
       y = "Second Dimension",
       title = paste("a) Phylogenetic MDS, k = 4, stress = ", round(stress_phyl, 2)/100)) + 
  theme(legend.position = "None")


# lex
get_elbow(lex_dist)
mds_lex = isoMDS(lex_dist, k = 4)
df_lex = as.data.frame(mds_lex$points)
colnames(df_lex) = c("dimension_1", "dimension_2", "dimension_3")
stress_lex = mds_lex$stress
print(stress_lex) #ok fit

df_lex$language = rownames(df_lex)
df_lex = df_lex %>% 
  left_join(europe, by = "language")

lex_2d = ggplot(df_lex,aes(y = dimension_2, x = dimension_1, label = language)) + 
  geom_point(aes(color = family, size = 1)) + 
  geom_text_repel(vjust = -0.5, size = 3) + 
  labs(x = "First Dimension",
       y = "Second Dimension",
       title = paste("Lexical MDS, k = 4, stress = ", round(stress_lex, 2)/100)) + 
  theme(legend.position = "None") + 
  geom_abline(intercept = -0.13,
              slope = -0.5,
              linetype = "dotted",
              color = "red", size = 1.5)
print(lex_2d)

# syn
get_elbow(syn_dist)
mds_syn = isoMDS(syn_dist, k = 4)
df_syn = as.data.frame(mds_syn$points)
colnames(df_syn) = c("dimension_1", "dimension_2", "dimension_3")
stress_syn = mds_syn$stress

df_syn$language = rownames(df_syn)
df_syn = df_syn %>% 
  left_join(europe, by = "language")

syn_2d = ggplot(df_syn, aes(y = dimension_2, x = dimension_1, label = language)) + 
  geom_point(aes(color = family, size = 3)) + 
  labs(x = "First Dimension",
        y = "Second Dimension",
        title = paste("Syntactical MDS, k = 4, stress = ", round(stress_syn, 2)/100)) + 
  geom_text_repel(vjust = -0.5, size = 3) + 
  theme(legend.position = "right") + 
  geom_vline(xintercept = -0.06,
              linetype = "dotted",
              color = "red", size = 1.5)
print(syn_2d)


# phon
get_elbow(phon_dist)
mds_phon = isoMDS(phon_dist, k = 4)
df_phon = as.data.frame(mds_phon$points)
colnames(df_phon) = c("dimension_1", "dimension_2", "dimension_3")
stress_phon = mds_phon$stress

df_phon$language = rownames(df_phon)
df_phon = df_phon %>% 
  left_join(europe, by = "language")

phon_2d = ggplot(df_phon, aes(y = dimension_2, x = dimension_1, label = language)) + 
  geom_point(aes(color = family, size = 1)) + 
  labs(x = "First Dimension",
       y = "Second Dimension",
       title = paste("Phonemic MDS, k = 4, stress = ", round(stress_phon, 2)/100)) + 
  geom_text_repel(vjust = -0.5, size = 3) + 
  theme(legend.position = "right")
# combined plot of first 2 dimensions

## 3d plots
#phyl
plot_ly(df_phyl, x = ~dimension_1, y = ~dimension_2, z = ~dimension_3,
        type = 'scatter3d', mode = 'markers+text',
        color = ~ family, colors = "Set1",
        text = ~language, textposition = 'top center',
        marker = list(size = 4)) %>%
  layout(title = paste("Phylogenetic MDS, k = 4, stress = ", round(stress_phyl, 2)),
         scene = list(
           xaxis = list(title = 'Dimension 1'),
           yaxis = list(title = 'Dimension 2'),
           zaxis = list(title = 'Dimension 3')))


plot_ly(df_lex, x = ~dimension_1, y = ~dimension_2, z = ~dimension_3,
        type = 'scatter3d', mode = 'markers+text',
        color = ~ family, colors = "Set1",
        text = ~language, textposition = 'top center',
        marker = list(size = 4)) %>%
  layout(title = paste("Lexical MDS, k = 4, stress = ", round(stress_lex, 2)),
         scene = list(
           xaxis = list(title = 'Dimension 1'),
           yaxis = list(title = 'Dimension 2'),
           zaxis = list(title = 'Dimension 3')))


plot_ly(df_syn, x = ~dimension_1, y = ~dimension_2, z = ~dimension_3,
        type = 'scatter3d', mode = 'markers+text',
        color = ~ family, colors = "Set1",
        text = ~language, textposition = 'top center',
        marker = list(size = 4)) %>%
  layout(title = paste("Morphosyntactic MDS, k = 4, stress = ", round(stress_syn, 2)),
         scene = list(
           xaxis = list(title = 'Dimension 1'),
           yaxis = list(title = 'Dimension 2'),
           zaxis = list(title = 'Dimension 3')))

plot_ly(df_phon, x = ~dimension_1, y = ~dimension_2, z = ~dimension_3,
        type = 'scatter3d', mode = 'markers+text',
        color = ~ family, colors = "Set1",
        text = ~language, textposition = 'top center',
        marker = list(size = 4)) %>%
  layout(title = paste("Phonemic inventory MDS, k = 4, stress = ", round(stress_phon, 2)),
         scene = list(
           xaxis = list(title = 'Dimension 1'),
           yaxis = list(title = 'Dimension 2'),
           zaxis = list(title = 'Dimension 3')))


# correlation between distance and similarity?

# does any one of the measures tend to capture geographicproximity better than the other?

# get coordinates
df_glotto = read_csv("Clean_data/language_lvl_data/Glottolog_processed.csv")
head(df_glotto)

#generate distance matrix

### get coordinates
coords = df_glotto %>%
  filter(ISO6393 %in% europe_iso) %>%
  select(ISO6393, latitude, longitude)

### get distance matrix

# create empty matric
geo_dist_m = matrix(nrow = length(europe_iso), ncol = length(europe_iso), dimnames = list(europe_iso, europe_iso))

for (lang1 in europe_iso){
  lang1_long = coords %>%
    filter(ISO6393 == lang1) %>%
    pull(longitude)
  lang1_lat = coords %>%
    filter(ISO6393 == lang1) %>%
    pull(latitude)
  for (lang2 in europe_iso){
    lang2_long = coords %>%
      filter(ISO6393 == lang2) %>%
      pull(longitude)
    lang2_lat = coords %>%
      filter(ISO6393 == lang2) %>%
      pull(latitude)
    
    distance = distHaversine(c(lang1_long, lang1_lat), c(lang2_long, lang2_lat))
    geo_dist_m[lang1, lang2] = distance
    
  }
}

# change to language names
rownames(geo_dist_m) = iso_to_name[rownames(geo_dist_m)]
colnames(geo_dist_m) = iso_to_name[colnames(geo_dist_m)]


# sanity check
geo_hclust = hclust(as.dist(geo_dist_m), method = "average")
plot(geo_hclust)


#### align matrices for correlation
align_matrix = function(m){
  # aligns matrices according to their alphabetical order
  m = m[order(rownames(m)),]
  m = m[, order(colnames(m))]
  return(m)
}

# align according and get upper_triangle
lex_align =  align_matrix(lex_sim)
syn_align = align_matrix(syn_sim)
phyl_align = align_matrix(phyl_sim)
phon_align = align_matrix(phon_sim)
geo_align = align_matrix(geo_dist_m)


lex_vec = lex_align[upper.tri(lex_align)]
syn_vec = syn_align[upper.tri(syn_align)]
phyl_vec = phyl_align[upper.tri(phyl_align)]
phon_vec = phon_align[upper.tri(phon_align)]
geo_vec = geo_align[upper.tri(geo_align)]

df_cor = data.frame(lex_vec, syn_vec, phyl_vec, phon_vec, geo_vec)


cor(df_cor, method = "spearman")

