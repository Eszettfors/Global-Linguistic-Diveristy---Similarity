library(tidyverse)
library(DescTools)
library(tidytext)
library(treemapify)
library(patchwork)
library(xtable)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


### This script contains the analysis of the ethnologue-joshua data###

# read country_lang pair data
df_country = read_csv("Clean_data/country_lvl_data/country_lang_pop.csv")
df_langs = read_csv("Clean_data/language_lvl_data/lang_pop.csv")

df_country = df_country %>% mutate(country_code = case_when(country == "Namibia"~ "NA", TRUE ~ country_code ))

n_pair = nrow(df_country) 
print(n_pair)#12261

df_country %>% 
  distinct(country_code, country) %>% xtable()
   # 239 territories/countries

theme_set(theme_bw())


# description
nrow(df_langs) # 6757 languages
df_langs %>%
  count(family) %>% nrow() # 223 language families (isolate, unattested, pidgin etc. included)

# filter categories from Glottolog
df_langs %>%
  filter(!family %in% c("Isolate", "Bookkeeping", "Sign Language",
                        "Unclassifiable",
                        "Pidgin", "Unattested",
                        "Artificial Language",
                        "Speech Register",
                        "Mixed Language")) %>%
  count(family) %>%
  nrow() # 9 are not language families -> 214 language families


df_langs %>% filter(family == "Bookkeeping")

df_langs %>%
  filter(family %in% c("Isolate", "Bookkeeping", "Sign Language",
                       "Unclassifiable",
                       "Pidgin", "Unattested",
                       "Artificial Language",
                       "Speech Register",
                       "Mixed Language")) %>%
  group_by(family)%>%
  summarize(n = n(),
            pop = sum(population)) %>%
  xtable()

df_langs %>%
  filter(family %in% c("Isolate", "Bookkeeping", "Sign Language",
                       "Unclassifiable",
                       "Pidgin", "Unattested",
                       "Artificial Language",
                       "Speech Register",
                       "Mixed Language")) %>%
  summarize(pop = sum(population))

# create a factor of population

df_langs$population_category = factor(df_langs$population_category,
                                         levels = c("Low",
                                                    "Lower-Mid",
                                                    "Upper-Mid",
                                                    "High"))

# macroarea

macro = df_langs %>% 
  count(macroarea) %>%
  mutate(percentage = round(n/sum(n) * 100,2)) %>%
  ggplot(aes(y = n, x = macroarea, fill = macroarea)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = paste(percentage, "%", sep = ""), vjust = -0.2)) + 
  labs(y = "Number of Languages",
       x = "Macroarea",
       title ="Global Distribution of Languages According to Macroarea") + 
  theme(legend.position = "none")
print(macro)
ggsave("Language_speakers/plots/macroarea_distribution.png", plot = macro, width = 10, height = 4, dpi = 300)

#### family ####
# subset to only include language families
family = df_langs %>%
  filter(!family %in% c("Isolate", "Bookkeeping", "Sign Language",
                       "Unclassifiable",
                       "Pidgin", "Unattested",
                       "Artificial Language",
                       "Speech Register",
                       "Mixed Language"))

tab_fams = family %>% 
  group_by(macroarea) %>%
  summarize(n_family = n_distinct(family)) %>%
  mutate(percentage = round(n_family/sum(n_family) * 100, 2))

xtable(tab_fams,include.rownames = FALSE)

family %>% 
  count(family) %>%
  mutate(percentage = round(n/sum(n) * 100)) %>%
  ggplot(aes(y = n, x = reorder(family, -n), fill = family)) +
  geom_bar(stat = "identity") +  
  labs(y = "number of languages",
       x = "language family",
       title = "Distribution of language families in Ethnologue") + 
  theme(legend.position = "none",
        axis.text.x = element_blank())

# family and macroarea
fams = family %>%
  group_by(macroarea) %>%
  summarize(n_families = n_distinct(family)) %>%
  mutate(percentage = round(n_families/sum(n_families) * 100, 2)) %>%
  ggplot(aes(y = n_families, x = macroarea, fill = macroarea)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = paste(percentage, "%", sep = ""), vjust = -0.2)) + 
  labs(y = "Number of Language Families",
       x = "Macroarea",
       title = "a) Distribution of Language Families Across Macroareas") + 
  ylim(0,80) + 
  theme(legend.position = "none")

fams

family %>% 
  group_by(macroarea) %>%
  count(family) %>%
  mutate(percentage = round(n/sum(n) * 100)) %>%
  ggplot(aes(y = reorder_within(family, n, macroarea), x = percentage), fill = family) +
  geom_bar(stat = "identity") + 
  facet_wrap(~macroarea,
             scales = "free") +
  scale_y_reordered() + 
  labs(y = "Language Families",
       x = "Percentage of Languages")

# treemaps for language families
family %>% 
  count(family) %>%
  mutate(percentage = round(n / sum(n) * 100)) %>%
  ggplot(aes(area = n, fill = family, label = paste(family, "\n", n))) +  
  geom_treemap(color = "black") +  
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) + 
  labs(title = "a) Distribution of Language Families across Macroareas") + 
  theme(legend.position = "none")


tree_map = family %>%
  group_by(macroarea, family) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percentage = round(n / sum(n) * 100)) %>%
  ggplot(aes(area = n, fill = family, label = paste(family, "\n", n))) +
  geom_treemap(color = "black") +  
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
  facet_wrap(~macroarea) +
  labs(title = "b) Languages per Family and Macroarea") +
  theme(legend.position = "none")

fam_tree = fams / tree_map
print(fam_tree)
ggsave("Language_speakers/plots/bar_tree_fam_users.png", plot = fam_tree, width = 10, height = 6, dpi = 300)

### Speakers ###
tab_speak = df_langs %>% 
  group_by(macroarea) %>%
  summarize(n_langs = n(),
            n_speakers = sum(population)) %>%
  mutate(percentage_speakers = n_speakers/sum(n_speakers) * 100,
         mean_speaker_lang = n_speakers/n_langs)
tab_speak

sum(tab_speak$n_speakers)
sum(tab_speak$n_langs)
avg_total = sum(tab_speak$n_speakers) / sum(tab_speak$n_langs)
xtable(tab_speak)  
print(avg_total)

df_langs %>% 
  group_by(macroarea) %>%
  summarize(med_pop = median(population))
median(df_langs$population)

# speaker and macro area
speak_mac = df_langs %>% 
  group_by(macroarea) %>%
  summarize(n_speakers = sum(population)) %>%
  mutate(percentage = round(n_speakers/sum(n_speakers) * 100, 5)) %>%
  mutate(n_speakers_log = log10(n_speakers)) %>%
  ggplot(aes(y = n_speakers_log, x = macroarea, fill = macroarea)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = paste(percentage, "%", sep = ""), vjust = -0.5)) + 
  labs(y = "number of speakers log10",
       title = "Distribution of speakers in Ethnologue according to macroarea") + 
  theme(legend.position = "none")


# speaker and family

tree_map_speaker = df_langs %>%
  group_by(family) %>%
  summarise(n = sum(population), .groups = "drop") %>%
  mutate(percentage = round(n / sum(n) * 100)) %>%
  ggplot(aes(area = n, fill = family, label = paste(family, "\n", n))) +
  geom_treemap(color = "black") +  
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
  labs(title = "a) Languages of the World") +
  theme(legend.position = "none") 

tree_map_speaker

tree_map_speaker_mac = df_langs %>%
  group_by(macroarea, family) %>%
  summarise(n = sum(population), .groups = "drop") %>%
  mutate(percentage = round(n / sum(n) * 100)) %>%
  ggplot(aes(area = n, fill = family, label = paste(family, "\n", n))) +
  geom_treemap(color = "black") +  
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
  facet_wrap(~macroarea) + 
  labs(title = "b) Languages per Macroarea") +
  theme(legend.position = "none")

speaker_category = df_langs %>%
  group_by(macroarea) %>%
  count(population_category) %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  ggplot(aes(y = n, x = population_category, fill = population_category)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~macroarea) + 
  geom_text(aes(label = paste(percentage, "%", sep = ""), vjust = -0.3)) + 
  labs(y = "Number of Languages",
       x = "Speaker Category",
       title = "Distribution of Languages and User Categories Across Macroareas") + 
  ylim(0, 950) + 
  theme(legend.position = "none")

print(speaker_category)
ggsave("Language_speakers/plots/bar_macro_userse.png", plot = speaker_category, width = 10, height = 4, dpi = 300)

tree_maps = tree_map_speaker/tree_map_speaker_mac
print(tree_maps)
ggsave("Language_speakers/plots/tree_map_users.png", plot = tree_maps, width = 10, height = 6, dpi = 300)

### create table with countries
world_map = ne_countries(scale = "large", returnclass = "sf")


df_country = df_country %>% mutate(country_code = case_when(country == "Namibia" ~ "NA",
                                                TRUE ~ country_code))

df_country %>%
  filter(!country_code %in% world_map$iso_a2_eh) %>% # some territories can't be plotted
  distinct(country, country_code)


# join geo data with coverage data
df_map = df_country %>%
  distinct(country_code) %>% left_join(world_map %>%
                                    select(iso_a2_eh,name, continent, geometry),
                                  by = join_by("country_code" == "iso_a2_eh"))


xtable(df_country %>% distinct(country_code, country))

##### most present language in different countries?

df_country
df_tree = df_country %>%
  group_by(ISO6393) %>%
  summarize(language = first(language),
            family = unique(family),
    countries_present = n_distinct(country_code),
    speakers = sum(population)) %>%  slice_max(n = 20, order_by = countries_present)


connected = ggplot(data = df_tree,
       (aes(area = countries_present,
        fill = family,
        label = paste(language, "\n", countries_present)))) +
               geom_treemap(color = "black") +  
               geom_treemap_text(colour = "white", place = "centre", grow = TRUE)

ggsave("Language_speakers/plots/countries_per_lang.png", plot = connected, width = 10, height = 6, dpi = 300)



# correlation between countries present and population
countries = df_country %>%
  group_by(ISO6393) %>%
  summarize(language = first(language),
            family = unique(family),
            macroarea = unique(macroarea),
            countries_present = n_distinct(country_code),
            population = sum(population))

countries %>%
  arrange(desc(countries_present)) %>%
  select(language, family, macroarea, population, countries_present) %>%
  rename("countries" = countries_present) %>%
  slice_head(n = 20) %>% xtable()



scatter = ggplot(countries,
       aes(y = log(countries_present),
           x = log(population)
           )) + 
  geom_point(aes(color = macroarea)) + 
  labs(y = "Log no. Countries Present",
       x = "Log no. Speakers")


ggsave("Language_speakers/plots/scatter.png", plot = scat_tab, width = 10, height = 10, dpi = 300)

cor.test(countries$countries_present, countries$population, method = "spearman")

# number of langs per country
richness = df_country %>%
  group_by(country_code) %>%
  summarize(n_langs = n_distinct(ISO6393))

Desc(richness$n_langs)
