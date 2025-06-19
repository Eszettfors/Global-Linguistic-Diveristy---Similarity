library(tidyverse)

# in this script, an aggregated comparison of typological databases is carried out

# read data
df_langs = read_csv("Clean_data/language_lvl_data/lang_pop.csv")
df_grambank = read_csv("Clean_data/language_lvl_data/grambank_data_wide.csv")
df_phoible = read_csv("Clean_data/language_lvl_data/phoible_data_long.csv")
df_asjp = read_csv("Clean_data/language_lvl_data/asjp_wide.csv")

df_country = read_csv("Clean_data/country_lvl_data/country_lang_pop.csv")

df_country = df_country %>% mutate(country_code = case_when(
  country == "Namibia" ~ "NA",
  TRUE ~ country_code
))


# filter asjp to 28
df_asjp = df_asjp %>%
  filter(ISO6393 %in% df_langs$ISO6393) %>%
  filter(words > 27)


#filter grambank to more than 75% completeness
df_grambank = df_grambank %>%
  filter(Completeness > 25)

df_phoible = df_langs %>%
  filter(ISO6393 %in% df_phoible$ISO6393)
  

### create barplots showing language coverage, speaker coverage, family coverage, country coverage, median country language coverage, 
langs_baseline = df_langs$ISO6393

df_grambank = df_grambank %>%
  filter(ISO6393 %in% langs_baseline)

df_asjp %>%
  filter(ISO6393 %in% langs_baseline)

### add a categorical variable denoting the database and bind them

df_langs = df_langs %>%
  mutate(data = "Baseline")
df_phoible = df_phoible %>%
  mutate(data = "Phoible")
df_grambank = df_grambank %>%
  mutate(data = "Grambank")
df_asjp = df_asjp %>%
  mutate(data = "ASJP")


# select columns
df_langs = df_langs %>%
  select(ISO6393, language, family, macroarea, population, data)

df_phoible = df_phoible %>%
  select(ISO6393, language, family, macroarea, data)

df_grambank = df_grambank %>%
  select(ISO6393, Language_name, Family_name, Macroarea, data) %>%
  rename("language" = Language_name,
         "family" = Family_name,
         "macroarea" = Macroarea)

df_asjp = df_asjp %>%
  select(ISO6393, language, family, macroarea, data)

# add population

df_phoible = df_phoible %>%
  left_join(df_langs %>%
              select(ISO6393, population),
            join_by(ISO6393))

df_asjp = df_asjp %>%
  left_join(df_langs %>%
              select(ISO6393, population),
            join_by(ISO6393))

df_grambank = df_grambank %>%
  left_join(df_langs %>%
              select(ISO6393, population),
            join_by(ISO6393))

# align columns
df_phoible = df_phoible %>%
  relocate(population)

df_langs = df_langs %>%
  relocate(population)

df_grambank = df_grambank %>%
  relocate(population)

df_asjp = df_asjp %>%
  relocate(population)


# bind
df_full = df_langs %>%
  rbind(df_phoible) %>%
  rbind(df_grambank) %>%
  rbind(df_asjp)


#### plot bar


bar_macro = df_full %>%
  group_by(data, macroarea) %>%
  summarize(langs = n()) %>%
  mutate(percent = langs/sum(langs)*100) %>%
  mutate(pos = cumsum(percent) - percent / 2) %>%
  ggplot(aes(x = percent, y = data, fill = macroarea)) + 
  geom_bar(position = "stack", stat = "identity", color = "black") + 
  geom_text(aes(x = 100-pos, 
                label = paste(round(percent,2), "%", sep = "")))

ggsave(filename = "DB comparison/plots/bar_macro.png", bar_macro, dpi = 300, height = 4, width = 8)


tot_lang = df_full %>%
  filter(data == "Baseline") %>%
  distinct(ISO6393) %>% nrow()

tot_speaker = df_full %>%
  filter(data == "Baseline") %>%
  summarize(pop = sum(population)) %>%
  pull(pop)

df_asjp %>%
  summarize(fam = n_distinct(family))


bar_lang_speaker = df_full %>%
  group_by(data) %>%
  summarize(langs = n_distinct(language),
            speakers = sum(population)) %>%
  mutate(Languages = langs / tot_lang * 100,
         Speakers = speakers / tot_speaker * 100) %>%
  pivot_longer(cols = c(Languages, Speakers),
               names_to = "measure",
               values_to = "percent") %>%
  ggplot(aes(y = percent,
             x = measure,
             fill = data)) + 
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           color = "black") + 
  geom_text(aes(label = paste0(round(percent, 2), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5) + 
  labs(y = "Coverage (%)",
       x = element_blank())

ggsave(filename = "DB comparison/plots/bar_lang_speaker.png", bar_lang_speaker, dpi = 300, height = 6, width = 8)


#### country level

df_country = read_csv("~/Projekt/Master_arbeit_2_new_data/Clean_data/country_lvl_data/countries_mean_similarities.csv")


violins = df_country %>% 
  mutate(coverage_langs_phoible = langs_phoible / langs,
         coverage_speakers_phoible = speakers_phoible / speakers,
         coverage_langs_grambank = langs_grambank / langs,
         coverage_speakers_grambank = speakers_grambank / speakers,
         coverage_langs_asjp = langs_asjp / langs,
         coverage_speakers_asjp = speakers_asjp / speakers) %>%
  select(starts_with("coverage")) %>%
  pivot_longer(
    cols = contains("coverage"),
    names_to = "coverage_type",
    values_to = "coverage_value"
  ) %>%
  ggplot(aes(x = coverage_value, y = coverage_type, fill = coverage_type)) + 
  geom_violin() + 
  theme(legend.position = "None") + 
  scale_y_discrete(labels=c("Speakers Phoible", "Speakers Grambank", "Speakers ASJP",
                            "Languages Phoible", "languages Grambank", "Languages ASJP")) +
  labs(y = element_blank(),
       x = "Coverage")

ggsave(filename = "DB comparison/plots/violin_speaker_lang.png", violins, dpi = 300, width = 8, height = 4)
