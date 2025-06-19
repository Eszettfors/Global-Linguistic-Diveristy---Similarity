library(tidyverse)
library(stringr)
library(lingtypology)
library(readxl)
library(patchwork)
library(DescTools)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(treemapify)


# this script analyses the coverage of the phoible database
# it looks at country level, a family level and status level

# read data -----
# read phoible
phoible = read_csv("Clean_data/language_lvl_data/phoible_data_long.csv")

# read speaker
df_langs =  read_csv("Clean_data/language_lvl_data/lang_pop.csv")

# country_coverage_data
df_country = readRDS("Clean_data/distances/phoible_coverage_similarity_population.rds")

##set theme for plots
theme_set(theme_bw())

#subset to langs present in phoible
df_phoible = df_langs %>%
  filter(ISO6393 %in% phoible$ISO6393)

#### lang and speaker coverage ####
n_langs = df_phoible %>%
  distinct(ISO6393) %>%
  nrow()
print(n_langs) #1820 languages present in Phoible
nrow(df_langs) #6757

n_langs / nrow(df_langs) * 100
# 26.93% of languages covered

# langs per macroarea
df_phoible %>%
  group_by(macroarea) %>%
  summarize(n_langs = n())

#families
df_phoible %>% 
  filter(!family %in% c("Isolate",
         "Artificial Language",
         "Bookkeeping",
         "Mixed Language",
         "Pidgin",
         "Sign Language",
         "Speech Register",
         "Unattested",
         "Unclassifiable")) %>%
  group_by(macroarea) %>%
  distinct(family) %>%
  group_by(macroarea) %>%
  summarize(n_families = n())


n_fam_phob = df_phoible %>%
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

print(n_fam_phob)

# not "family categories"
df_phoible %>% 
  filter(family %in% c("Isolate",
                        "Artificial Language",
                        "Bookkeeping",
                        "Mixed Language",
                        "Pidgin",
                        "Sign Language",
                        "Speech Register",
                        "Unattested",
                        "Unclassifiable")) %>%
  count(family) # 1 book keeping, 51 isolates 1 pidgin


n_fam_ethno = df_langs %>%
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
print(n_fam_ethno) # 214

n_fam_phob/n_fam_ethno * 100 # 73.76%

#speakers
df_phoible %>% 
  group_by(macroarea) %>%
  summarize(tot_speakers = sum(population))

tot_speakers = df_langs %>%
  summarize(sum = sum(population)) %>%
  pull(sum)
print(tot_speakers) # 7634061189

tot_speakers_phoible = df_langs %>% 
  filter(ISO6393 %in% phoible$ISO6393) %>%
  summarize(sum = sum(population)) %>%
  pull(sum)
print(tot_speakers_phoible)# 6440585093

language_coverage = tot_speakers_phoible / tot_speakers * 100
print(language_coverage) #82.30 % of speakers covered  

### languages not in phoible according to speakers
not_in_phoible = df_langs %>% 
  filter(!ISO6393 %in% df_phoible$ISO6393) %>%
  slice_max(population, n = 50)

not_in_phoible
  
tree_not_in_phoible = ggplot(data = not_in_phoible,
       aes(area = population, fill = family, label = paste(language, "\n", population))) + 
  geom_treemap(color = "black") + 
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) + 
  labs(fill = "Language Family")

ggsave("PHOIBLE/plots/tree_map_not_users.png", plot = tree_not_in_phoible, width = 10, height = 6, dpi = 300)

#### macroarea #### 

# count langs per Macroarea
macro_phoible = df_phoible %>%
  group_by(macroarea) %>%
  summarize(n_phoible = n()) %>%
  mutate(percent_phoible = n_phoible/sum(n_phoible) * 100)

macro_ethno = df_langs %>%
  group_by(macroarea) %>%
  summarize(n_ethno = n()) %>%
  mutate(percent_ethno = n_ethno/sum(n_ethno) * 100)

# merge
macro = macro_phoible %>% left_join(macro_ethno, by = "macroarea")

# plot
prop_comp_macro = macro %>%
  select(macroarea, percent_phoible, percent_ethno) %>%
  pivot_longer(cols = c(percent_phoible, percent_ethno),
               values_to = "percent",
               names_to = "data") %>%
  mutate(data = case_when(
    data == "percent_phoible" ~ "PHOIBLE",
    data == "percent_ethno" ~ "Baseline"
  )) %>%
  ggplot(aes(y = percent,
             x = macroarea,
             fill = data)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  geom_text(aes(label = paste(round(percent), "%", sep = "")),
            position = position_dodge(width = 0.9),
            vjust = -0.2) + 
  labs(x = "",
       y = "Percent (%)") + 
  ylim(0, 41)
prop_comp_macro

# calculate percent absolute difference

diff_macro = macro %>%
  mutate(abs_percent_diff = percent_phoible - percent_ethno) %>%
  ggplot(aes(y = abs_percent_diff,
             x = macroarea,
             fill = macroarea)) + 
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none") + 
  geom_text(aes(label = paste(round(abs_percent_diff, 2), "%", sep = "")),
            vjust = -0.2) +
  labs(y = "Absolute Percent Difference",
       x = "Macroarea")

macro_plot = prop_comp_macro / diff_macro

ggsave("PHOIBLE/plots/macro_bars.png", plot = macro_plot, width = 10, height = 6, dpi = 300)

#### speakers ####

# count langs per population category
category_phoible = df_phoible %>%
  group_by(population_category) %>%
  summarize(n_phoible = n()) %>%
  mutate(percent_phoible = n_phoible / sum(n_phoible) *100)

category_ethno = df_langs %>%
  group_by(population_category) %>%
  summarize(n_ethno = n()) %>%
  mutate(percent_ethno = n_ethno / sum(n_ethno) * 100)

# merge
pop = category_phoible %>% left_join(category_ethno, by = "population_category")
pop$population_category = factor(pop$population_category, levels = c("Low", "Lower-Mid", "Upper-Mid", "High"))

# plot
prop_comp_speaker = pop %>%
  select(population_category, percent_phoible, percent_ethno) %>%
  pivot_longer(cols = c(percent_phoible, percent_ethno),
               values_to = "percent",
               names_to = "data") %>%
  mutate(data = case_when(
    data == "percent_phoible" ~ "PHOIBLE",
    data == "percent_ethno" ~ "Baseline"
  )) %>%
  ggplot(aes(y = percent,
             x = population_category,
             fill = data)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  geom_text(aes(label = paste(round(percent), "%", sep = "")),
            position = position_dodge(width = 0.9),
            vjust = -0.1) + 
  labs(x = "",
       y = "Percent (%)") + 
  ylim(0, 42)


diff_pop = pop %>%
  mutate(abs_percent_diff = percent_phoible - percent_ethno) %>%
  ggplot(aes(y = abs_percent_diff,
             x = population_category,
             fill = population_category)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = paste(round(abs_percent_diff, 2), "%", sep = "")),
            vjust = -0.2) +
  labs(y = "Absolute Percent Difference",
       x = "Speakers Category") +
  ylim(-9, 17) + 
  theme(legend.position = "none")


speaker_categ_plot = prop_comp_speaker / diff_pop
ggsave("PHOIBLE/plots/speaker_categ.png", plot = speaker_categ_plot, width = 10, height = 6, dpi = 300)

### countries ####

View(df_country)


Desc(df_country$phon_lang_coverage)
hist = df_country %>%
  ggplot(aes(x = phon_lang_coverage)) + 
  geom_histogram(binwidth = 10, color = "black", fill = "lightblue") + 
  labs(y = "Number of Countries",
       x = "Coverage in Phoible (%)") + 
  theme(axis.title.x = element_blank())

box = df_country %>%
  ggplot(aes(x = phon_lang_coverage)) + 
  geom_boxplot(fill = "lightblue") + 
  labs(y = "",
       x = "Coverage in Phoible (%)") + 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

hist/box + plot_layout(heights = c(3, 1))


nrow(df_country)
View(df_country)

## Violin plot

head(df_country)
coverage_long = df_country %>%
  pivot_longer(cols = c(phon_lang_coverage, phon_speaker_coverage), values_to = "coverage")


phoible_violin = ggplot(data = coverage_long, aes(y = coverage,fill = name,  x = name)) + 
  geom_violin(trim = TRUE, alpha = 0.7, color = "black") +
  geom_boxplot(width = 0.07, fill = "white") + 
  coord_flip() + 
  labs(x = NULL,
       y = "Coverage (%)") + 
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14),  # Increases horizontal axis text size
        axis.text.y = element_text(size = 14)) + 
  scale_x_discrete(labels = c("Languages", "Speakers"))
print(phoible_violin)
ggsave("PHOIBLE/plots/violin.png", plot = phoible_violin, width = 10, height = 4, dpi = 300)

Desc(df_country$phon_lang_coverage)
Desc(df_country$phon_speaker_coverage)
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
  geom_sf(aes(geometry = geometry, fill = phon_lang_coverage)) +
  scale_fill_viridis_c() + 
  labs(fill = "Coverage (%)",
       title = "a) Languages Covered") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())
plot(richness)
  
speakers = ggplot(data = df_map) +
  geom_sf(aes(geometry = geometry, fill = phon_speaker_coverage)) +
  scale_fill_viridis_c() + 
  labs(fill = "Coverage (%)",
       title = "b) Speakers Covered") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

maps_phoible = richness / speakers
ggsave("PHOIBLE/plots/maps.png", plot = maps_phoible, width = 10, height = 6, dpi = 300)




