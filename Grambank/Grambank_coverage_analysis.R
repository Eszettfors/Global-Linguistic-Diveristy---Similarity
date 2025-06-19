library(tidyverse)
library(lingtypology)
library(xtable)
library(DescTools)
library(patchwork)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(treemapify)
library(maps)

#### this script analyses the composition of the grambank data based on the ethnologue data
# for which L1 population data exists.

# read data
df_langs = read_csv("Clean_data/language_lvl_data/lang_pop.csv")

df_grambank = read_csv("Clean_data/language_lvl_data/grambank_data_wide.csv")

theme_set(theme_bw())

# number of langs
df_langs %>% nrow() # 6757 langs

df_grambank %>% nrow() # 2467 langs

# langs in grambank and ethno?

df_grambank %>% 
  filter(ISO6393 %in% df_langs$ISO6393) %>%
  nrow() # 2149 langs

# langs in grambank but not in ethnologue?
df_grambank  %>%
  filter(!ISO6393 %in% df_langs$ISO6393) %>% 
  head()

### subset to langs in ethnologue and grambank
df_grambank = df_grambank %>% 
  filter(ISO6393 %in% df_langs$ISO6393)

# add population, population category and status
df_grambank = df_grambank %>%
  select(!Family_name) %>% 
  left_join(
  df_langs %>% select(ISO6393,
                      population,
                      population_category,
                      family),
  join_by("ISO6393" == "ISO6393" )
)

df_grambank = df_grambank %>%
  rename("language" = Language_name, "macroarea" = Macroarea) %>%
  select(!Glottocode) %>%
  relocate(ISO6393, language, family, population, population_category)

head(df_grambank)

### get data completeness
# defined data completeness as "Completeness may be computed by dividing the available items or records by the expected total number [46], resulting in a percentage if multiplied by 100."
# https://www.mdpi.com/2673-8392/2/1/32?utm_source=chatgpt.com#B33-encyclopedia-02-00032


Desc(df_grambank$Completeness)

### lang coverage ------

n_langs = nrow(df_grambank)
print(n_langs) #2149


lang_coverage = n_langs / nrow(df_langs) * 100
print(lang_coverage) # 31.72

# language types  
df_grambank %>%
  filter(family %in% c("Isolate",
                        "Artificial Language",
                        "Bookkeeping",
                        "Mixed Language",
                        "Pidgin",
                        "Sign Language",
                        "Speech Register",
                        "Unattested",
                        "Unclassifiable")) %>%
  count(family) # 72 Isolates and 2 sign language

### family coverage ------
n_families = df_grambank %>%
  filter(!family %in% c("Isolate",
                       "Sign Language"
                       )) %>%
  count(family) %>% nrow()
print(n_families) # 1196

n_families_ethno = df_langs %>%
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
print(family_coverage) #91.59

### speakers -------
n_speakers = sum(df_grambank$population)
print(n_speakers) # 5084612782

n_speakers_ethno = sum(df_langs$population)
speakers_coverage = n_speakers / n_speakers_ethno * 100
print(speakers_coverage) # 66.6

#### completeness ----------
df_grambank %>% 
  summarize(mean_completeness = mean(Completeness)) # 75.8% completeness of data

Desc(df_grambank$Completeness)

# table with summary per macroarea ----------
tab_langs = df_grambank %>%
  group_by(macroarea) %>%
  summarize(n_lang = n(),
            sum_pop = sum(population),
            completeness = mean(Completeness))

tab_fam = df_grambank %>% 
  group_by(macroarea) %>%
  distinct(family) %>%
  filter(!family %in% c("Isolate", "Sign Language")) %>%
  summarize(n_families = n())

tab_summary = tab_fam %>% left_join(
  tab_langs,
  join_by("macroarea" == "macroarea")
) %>% 
  relocate(macroarea, n_lang)
tab_summary

xtable(tab_summary)


##### remove langs with less than 25 % completenes -> 49 features defined

ecdf_function = ecdf(df_grambank$Completeness)
ecdf_function(25) # 0.02 -> 2% of data has a completeness lower than 25 %

plot(ecdf(df_grambank$Completeness), col = "black", lwd = 2, verticals = TRUE, do.points = FALSE,
     xlab = "Completeness", ylab = "Cumulative Probability",
     main = NULL)
abline(v = 25)

df_grambank = df_grambank %>% 
  filter(Completeness > 25)

#### summary statistics after removing low completeness

#n_langs
df_grambank %>% 
  summarize(n_langs = n()) # 2108

# n family
df_grambank %>%
  filter(!family %in% c("Isolate",
                        "Sign Language")) %>% 
  distinct(family) %>%
  nrow() # 195
df_grambank %>%
  filter(family %in% c("Isolate",
                        "Sign Language")) %>% 
  count(family) # 68 isolates no sign languages

# n speakers
df_grambank %>%
  summarize(speakers = sum(population)) #5065185564



# completeness
df_grambank %>%
  summarize(avg_comp = mean(Completeness))# 76.9

#### macroarea plots -----
# count langs per Macroarea
macro_gram = df_grambank %>%
  group_by(macroarea) %>%
  summarize(n_gram = n()) %>%
  mutate(percent_gram = n_gram/sum(n_gram) * 100)

macro_ethno = df_langs %>%
  group_by(macroarea) %>%
  summarize(n_ethno = n()) %>%
  mutate(percent_ethno = n_ethno/sum(n_ethno) * 100)

# merge
macro = macro_gram %>% left_join(macro_ethno, by = "macroarea")

# plot
prop_comp_macro = macro %>%
  select(macroarea, percent_gram, percent_ethno) %>%
  pivot_longer(cols = c(percent_gram, percent_ethno),
               values_to = "percent",
               names_to = "data") %>%
  mutate(data = case_when(
    data == "percent_gram" ~ "Grambank",
    data == "percent_ethno" ~ "Baseline"
  )) %>%
  ggplot(aes(y = percent,
             x = macroarea,
             fill = data)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  geom_text(aes(label = paste(round(percent), "%", sep = "")),
            position = position_dodge(width = 0.9),
            vjust = -0.2) + 
  ylim(0,34) + 
  labs(x = " ",
       y = "Percent (%)")
prop_comp_macro

# calculate percent absolute difference

diff_macro = macro %>%
  mutate(abs_percent_diff = percent_gram - percent_ethno) %>%
  ggplot(aes(y = abs_percent_diff,
             x = macroarea,
             fill = macroarea)) + 
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none") + 
  geom_text(aes(label = paste(round(abs_percent_diff, 2), "%", sep = "")),
            vjust = -0.2) +
  ylim(-7.2, 3.2) +
  labs(y = "Absolute Percent Difference",
       x = "Macroarea")

macro = prop_comp_macro / diff_macro
ggsave("Grambank/plots/macro_diffs.png", plot = macro, width = 10, height = 6, dpi = 300)

#### speaker plots --------

category_grambank = df_grambank %>%
  group_by(population_category) %>%
  summarize(n_grambank = n()) %>%
  mutate(percent_grambank = n_grambank / sum(n_grambank) *100)

category_ethno = df_langs %>%
  group_by(population_category) %>%
  summarize(n_ethno = n()) %>%
  mutate(percent_ethno = n_ethno / sum(n_ethno) * 100)

# merge
pop = category_grambank %>% left_join(category_ethno, by = "population_category")
pop$population_category = factor(pop$population_category, levels = c("Low", "Lower-Mid", "Upper-Mid", "High"))

# plot
prop_comp_speaker = pop %>%
  select(population_category, percent_grambank, percent_ethno) %>%
  pivot_longer(cols = c(percent_grambank, percent_ethno),
               values_to = "percent",
               names_to = "data") %>%
  mutate(data = case_when(
    data == "percent_grambank" ~ "Grambank",
    data == "percent_ethno" ~ "Baseline"
  )) %>%
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
  mutate(abs_percent_diff = percent_grambank - percent_ethno) %>%
  ggplot(aes(y = abs_percent_diff,
             x = population_category,
             fill = population_category)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = paste(round(abs_percent_diff, 2), "%", sep = "")),
            vjust = -0.2) +
  ylim(-5, 5.5) + 
  labs(y = "Absolute Percent Difference",
       x = "Speakers Category") +
  theme(legend.position = "none")

speaker_rep = prop_comp_speaker / diff_pop
ggsave("Grambank/plots/speaker_diffs.png", plot = speaker_rep, width = 10, height = 6, dpi = 300)

# speakers not present in grambank
not_in_grambank = df_langs %>% 
  filter(!ISO6393 %in% df_grambank$ISO6393) %>%
  slice_max(population, n = 50)


tree_gram = ggplot(data = not_in_grambank,
       aes(area = population, fill = family, label = paste(language, "\n", population))) + 
  geom_treemap(color = "black") + 
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) + 
  labs(fill = "Language Family")

ggsave("Grambank/plots/tree_gram.png", plot = tree_gram, width = 10, height = 6, dpi = 300)
#### country coverage

df_country = readRDS("Clean_data/distances/grambank_coverage_similarity_population.rds")

df_country = df_country %>% mutate(country_code = case_when(country == "Namibia"~ "NA",
                      TRUE ~ country_code))

head(df_country)
test = df_country %>%
  pull(def_length_matrix) 
test = test[148]


get_mean_reliability = function(m) {
  # takes a matrix with definition lengths for country language pairs in Grammbank and 
  # returns their mean number of shared feautres
  if(length(m) == 1){
    return(m[[1]])
  }else{
  v = m[upper.tri(m)]
  return(mean(v))
  }
}

df_country = df_country %>%
  mutate(mean_reliability = get_mean_reliability(def_length_matrix))

# reliability

Desc(df_country$mean_reliability)

# coverage
Desc(df_country$gram_lang_coverage)
hist = df_country %>%
  ggplot(aes(x = gram_lang_coverage)) + 
  geom_histogram(binwidth = 10, color = "black", fill = "lightblue") + 
  labs(y = "Number of Countries",
       x = "Coverage in Grambank (%)") + 
  theme(axis.title.x = element_blank())

box = df_country %>%
  ggplot(aes(x = gram_lang_coverage)) + 
  geom_boxplot(fill = "lightblue") + 
  labs(y = "",
       x = "Coverage in Grambank (%)") + 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())


hist/box + plot_layout(heights = c(3, 1))


nrow(df_country)
head(df_country)

## Violin plot

head(df_country)
coverage_long = df_country %>%
  pivot_longer(cols = c(gram_lang_coverage, gram_speaker_coverage), values_to = "coverage")


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

Desc(df_country$gram_lang_coverage)
Desc(df_country$gram_speaker_coverage)

df_country %>% filter(gram_lang_coverage == 0)

ggsave("Grambank/plots/violin.png", plot = violin, width = 10, height = 4, dpi = 300)

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
  geom_sf(aes(geometry = geometry, fill = gram_lang_coverage)) +
  scale_fill_viridis_c() + 
  labs(fill = "Coverage (%)",
       title = "a) Languages Covered") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())


speakers = ggplot(data = df_map) +
  geom_sf(aes(geometry = geometry, fill = gram_speaker_coverage)) +
  scale_fill_viridis_c() + 
  labs(fill = "Coverage (%)",
       title = "b) Speakers Covered") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

reliability = ggplot(data = df_map) +
  geom_sf(aes(geometry = geometry, fill = mean_reliability)) +
  scale_fill_viridis_c(limits = c(80, 195),
                       breaks = c(80, 110, 140, 170, 195)) +
  labs(fill = "Number of Features",
       title = "C) Mean Shared Features") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())


maps = richness / speakers / reliability

ggsave("Grambank/plots/maps.png", maps, width = 10, height = 12, dpi = 300)

##### distance from equator associated with more shared features

View(world_map)

data("world.cities")
coords = world.cities %>%
  group_by(country.etc) %>%
  top_n(1, pop) %>%  # pick the most populous city (likely the capital)
  filter(country.etc %in% df_country$country) %>%
  select(country = country.etc, lat, long) %>% as_tibble()

coords = coords %>% 
  left_join(df_country, join_by("country" == "country"))

dist_eq = coords %>%
  select(mean_reliability, lat) %>%
  mutate(lat = abs(lat))

ggplot(dist_eq,
       aes(y = mean_reliability, lat)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  geom_qq()

cor.test(dist_eq$mean_reliability, dist_eq$lat)

