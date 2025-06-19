library(tidyverse)
library(xtable)
library(patchwork)
library(treemap)
library(treemapify)

# this script analysis the ASJP data based on the swadesh-40 list

df_asjp = read_csv("Clean_data/language_lvl_data/asjp_wide.csv")
df_ethno = read_csv("Clean_data/language_lvl_data/lang_pop.csv")

theme_set(theme_bw())

#completeness####
params = df_asjp %>% ungroup() %>%
  select(!c(lang_id, ISO6393, completeness, words, language, macroarea, family))

params = params %>%
  mutate(across(everything(), ~ case_when(
    !is.na(.) ~ 1,
    TRUE ~ 0
  )))

col_vals = params %>% 
  summarize(across(everything(), sum))

params = col_vals %>% 
  pivot_longer(cols = everything()) %>% data.frame()

params = params %>%
  mutate(completeness = value/nrow(df_asjp)*100)

par(mfrow = c(2,1))
# ecdfs
ecdf_func_lang = ecdf(df_asjp$completeness)
ecdf_func_word = ecdf(df_asjp$words)
ecdf_func_word(28)

plot(ecdf_func_lang, col = "black", lwd = 2, verticals = TRUE, do.points = FALSE,
     xlab = "Completeness (%)", ylab = "Cumulative Probability",
     main = "a) ECDF Plot of Completeness per language")
abline(v = 70, h = ecdf_func_lang(70), lty = 2)


ecdf_func_params = ecdf(params$completeness)

plot(ecdf_func_params, col = "black", lwd = 2, verticals = TRUE, do.points = FALSE,
     xlab = "Completeness (%)", ylab = "Cumulative Probability",
     main = "b) ECDF Plot of Completeness per parameter")


##### lang/fam/speaker coverage

nrow(df_asjp) # 5592
nrow(df_ethno) # 6757

# langs in ethnologue as well
df_asjp = df_asjp %>%
  filter(ISO6393 %in% df_ethno$ISO6393) %>% ungroup()

df_asjp = df_asjp %>% 
  select(!c(macroarea, family, language)) %>% 
  inner_join(df_ethno, join_by(ISO6393 == ISO6393)) %>%
  relocate(lang_id, ISO6393, language, macroarea, family, words, completeness, population, population_category) %>%
  select(!lang_id)

n_langs = nrow(df_asjp)
print(n_langs) #5063

lang_coverage = n_langs / nrow(df_ethno) * 100
print(lang_coverage) # 74.93

# language types  
df_asjp %>%
  filter(family %in% c("Isolate",
                       "Artificial Language",
                       "Bookkeeping",
                       "Mixed Language",
                       "Pidgin",
                       "Sign Language",
                       "Speech Register",
                       "Unattested",
                       "Unclassifiable")) %>%
  count(family) # 104 Isolates, 2 artificials, 5 bookkeeping, 1 pidgin, 3 speech registers

### family coverage ------
n_families = df_asjp %>%
  filter(!family %in% c("Isolate",
                        "Sign Language",
                        "Artificial Language",
                        "Bookkeeping",
                        "Pdigin",
                        "Speech Register"
  )) %>%
  count(family) %>% nrow()
print(n_families) # 214

n_families_ethno = df_ethno %>%
  filter(!family %in% c("Isolate",
                        "Artificial Language",
                        "Bookkeeping",
                        "Mixed Language",
                        "Pidgin",
                        "Sign Language",
                        "Speech Register",
                        "Unattested",
                        "Unclassifiable")) %>%
  distinct(family) %>%
  nrow()

family_coverage = n_families / n_families_ethno * 100
print(family_coverage) #100

### speakers -------
n_speakers = sum(df_asjp$population)
print(n_speakers) # 7362391331

n_speakers_ethno = sum(df_ethno$population)
speakers_coverage = n_speakers / n_speakers_ethno * 100
print(speakers_coverage) # 96.44135

# completeness
df_asjp %>%
  summarize(avg_comp = mean(completeness)) #87.0

### table summary #####
tab_langs = df_asjp %>%
  group_by(macroarea) %>%
  summarize(n_lang = n(),
            sum_pop = sum(population),
            completeness = mean(completeness))

tab_fam = df_asjp %>% 
  group_by(macroarea) %>%
  distinct(family) %>%
  filter(!family %in% c("Isolate",
                        "Sign Language",
                        "Artificial Language",
                        "Bookkeeping",
                        "Pdigin",
                        "Speech Register")) %>%
  summarize(n_families = n())

tab_summary = tab_fam %>% left_join(
  tab_langs,
  join_by("macroarea" == "macroarea")
) %>% 
  relocate(macroarea, n_lang)
tab_summary

xtable(tab_summary)


###### remove langs with fewer than 28 words defined ######
df_asjp = df_asjp %>% filter(words > 27)

#### #n_langs
n_langs = df_asjp %>% 
  summarize(n_langs = n()) %>% pull(n_langs) # 4567

n_langs/nrow(df_ethno)

# n family
df_asjp %>%
  filter(!family %in% c("Isolate",
                         "Sign Language",
                         "Artificial Language",
                         "Bookkeeping",
                         "Pdigin",
                         "Speech Register")) %>% 
  distinct(family) %>%
  nrow() # 214

df_asjp %>%
  filter(family %in% c("Isolate",
                       "Sign Language",
                       "Artificial Language",
                       "Bookkeeping",
                       "Pdigin",
                       "Speech Register")) %>% 
  count(family) # 99 Isolates

# n speakers
n_speakers = df_asjp %>%
  summarize(speakers = sum(population)) %>% pull(speakers)

n_speakers / sum(df_ethno$population)

# completeness
df_asjp %>%
  summarize(avg_comp = mean(completeness))# 90.9


#### macroarea plots -----
# count langs per Macroarea
macro_asjp = df_asjp %>%
  group_by(macroarea) %>%
  summarize(n_asjp = n()) %>%
  mutate(percent_asjp = n_asjp/sum(n_asjp) * 100)

macro_ethno = df_ethno %>%
  group_by(macroarea) %>%
  summarize(n_ethno = n()) %>%
  mutate(percent_ethno = n_ethno/sum(n_ethno) * 100)

# merge
macro = macro_asjp %>% left_join(macro_ethno, by = "macroarea")

# plot
prop_comp_macro = macro %>%
  select(macroarea, percent_asjp, percent_ethno) %>%
  pivot_longer(cols = c(percent_asjp, percent_ethno),
               values_to = "percent",
               names_to = "data") %>%
  mutate(data = case_when(
    data == "percent_asjp" ~ "ASJP",
    data == "percent_ethno" ~ "Baseline"
  )) %>%
  mutate(data = factor(data,
                       levels = (c("Baseline", "ASJP"))))%>%
  ggplot(aes(y = percent,
             x = macroarea,
             fill = data)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  geom_text(aes(label = paste(round(percent), "%", sep = "")),
            position = position_dodge(width = 0.9),
            vjust = -0.2) + 
  ylim(0,36) + 
  labs(x = "Macroarea",
       y = "Percent (%)")
prop_comp_macro

# calculate percent absolute difference
diff_macro = macro %>%
  mutate(abs_percent_diff = percent_asjp - percent_ethno) %>%
  ggplot(aes(y = abs_percent_diff,
             x = macroarea,
             fill = macroarea)) + 
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none") + 
  geom_text(aes(label = paste(round(abs_percent_diff, 2), "%", sep = "")),
            vjust = -0.2) +
  ylim(-6, 3.2) +
  labs(y = "Absolute Percent Difference",
       x = "Macroarea")

macro_asjp = prop_comp_macro / diff_macro
macro_asjp
ggsave("ASJP/plots/macro_asjp.png", macro_asjp, width = 10, height = 6, dpi = 300)
#### speaker plots --------

category_asjp = df_asjp %>%
  group_by(population_category) %>%
  summarize(n_asjp = n()) %>%
  mutate(percent_asjp = n_asjp / sum(n_asjp) *100)

category_ethno = df_ethno %>%
  group_by(population_category) %>%
  summarize(n_ethno = n()) %>%
  mutate(percent_ethno = n_ethno / sum(n_ethno) * 100)

# merge
pop = category_asjp %>% left_join(category_ethno, by = "population_category")
pop$population_category = factor(pop$population_category, levels = c("Low", "Lower-Mid", "Upper-Mid", "High"))

# plot
prop_comp_speaker = pop %>%
  select(population_category, percent_asjp, percent_ethno) %>%
  pivot_longer(cols = c(percent_asjp, percent_ethno),
               values_to = "percent",
               names_to = "data") %>%
  mutate(data = case_when(
    data == "percent_asjp" ~ "ASJP",
    data == "percent_ethno" ~ "Baseline"
  )) %>%
  mutate(data = factor(data,
                       levels = (c("Baseline", "ASJP")))) %>%
  ggplot(aes(y = percent,
             x = population_category,
             fill = data)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  geom_text(aes(label = paste(round(percent), "%", sep = "")),
            position = position_dodge(width = 0.9),
            vjust = -0.1) + 
  ylim(0,31) + 
  labs(x = "Speakers Category",
       y = "Percent (%)")

diff_pop = pop %>%
  mutate(abs_percent_diff = percent_asjp - percent_ethno) %>%
  ggplot(aes(y = abs_percent_diff,
             x = population_category,
             fill = population_category)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = paste(round(abs_percent_diff, 2), "%", sep = "")),
            vjust = -0.2) +
  ylim(-2, 3.5) + 
  labs(y = "Absolute Percent Difference",
       x = "Speakers Category") +
  theme(legend.position = "none")

asjp_pop = prop_comp_speaker / diff_pop
ggsave("ASJP/plots/asjp_pop.png", plot = asjp_pop, width = 10, height = 6, dpi = 300)

# speakers not present in ASJP
not_in_ASJP = df_ethno %>% 
  filter(!ISO6393 %in% df_asjp$ISO6393) %>%
  slice_max(population, n = 50)

tree_asjp = ggplot(data = not_in_ASJP,
       aes(area = population, fill = family, label = paste(language, "\n", population))) + 
  geom_treemap(colour = "black") + 
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) + 
  labs(fill = "Language Family")

ggsave("ASJP/plots/asjp_tree.png", plot = tree_asjp, width = 10, height = 6, dpi = 300)

#### country coverage

df_country = readRDS("Clean_data/distances/asjp_coverage_similarity_population.rds")

df_country = df_country %>% mutate(country_code = case_when(country == "Namibia"~ "NA",
                                                            TRUE ~ country_code))

# coverage
Desc(df_country$asjp_lang_coverage)
hist = df_country %>%
  ggplot(aes(x = asjp_lang_coverage)) + 
  geom_histogram(binwidth = 10, color = "black", fill = "lightblue") + 
  labs(y = "Number of Countries",
       x = "Coverage in ASJP (%)") + 
  theme(axis.title.x = element_blank())

box = df_country %>%
  ggplot(aes(x = asjp_lang_coverage)) + 
  geom_boxplot(fill = "lightblue") + 
  labs(y = "",
       x = "Coverage in ASJP (%)") + 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())


hist/box + plot_layout(heights = c(3, 1))


nrow(df_country)
head(df_country)

## Violin plot

head(df_country)
coverage_long = df_country %>%
  pivot_longer(cols = c(asjp_lang_coverage, asjp_speaker_coverage), values_to = "coverage")


violin = ggplot(data = coverage_long, aes(y = coverage,fill = name,  x = name)) + 
  geom_violin(trim = TRUE, alpha = 0.7, color = "black") +
  geom_boxplot(width = 0.07, fill = "white") + 
  coord_flip() + 
  labs(x = NULL,
       y = "Coverage (%)") + 
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14),  # Increases horizontal axis text size
        axis.text.y = element_text(size = 14)) + 
  scale_x_discrete(labels = c("Languages", "Speakers"))

Desc(df_country$asjp_lang_coverage)
Desc(df_country$asjp_speaker_coverage)

df_country %>% filter(asjp_speaker_coverage == 0)
violin
ggsave("ASJP/plots/violin.png", plot = violin, width = 10, height = 4, dpi = 300)

#map

world_map = ne_countries(scale = "large", returnclass = "sf")

df_country %>%
  filter(!country_code %in% world_map$iso_a2_eh) # some territories can't be plotted


# join geo data with coverage data
df_map = df_country %>% left_join(world_map %>%
                                    select(iso_a2_eh,name, continent, geometry),
                                  by = join_by("country_code" == "iso_a2_eh"))

# plot worldmap
richness = ggplot(data = df_map) +
  geom_sf(aes(geometry = geometry, fill = asjp_lang_coverage)) +
  scale_fill_viridis_c() + 
  labs(fill = "Coverage (%)",
       title = "a) Languages Covered") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())


speakers = ggplot(data = df_map) +
  geom_sf(aes(geometry = geometry, fill = asjp_speaker_coverage)) +
  scale_fill_viridis_c() + 
  labs(fill = "Coverage (%)",
       title = "b) Speakers Covered") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

maps = richness / speakers
maps
ggsave("asjp/plots/maps.png", maps, width = 10, height = 10, dpi = 300)


View(df_country)
