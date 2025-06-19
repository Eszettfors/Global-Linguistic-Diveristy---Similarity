library(tidyverse)
library(stringr)
library(lingtypology)
library(readxl)

# this script takes the data from ethno joshua with speaker and adds macro area and language family to the data
# the script outputs two files with country-language pairs: speaker and EGIDS
# and three files with only languages: speaker, EGIDS and (speaker and EGIDS)
# dataframe with speakers only contain instances where population data exists

# read data -----

# read speaker number data
df_speakers = read_csv("Raw_data/josh_ethno_no_macro.csv")
df_speakers = df_speakers %>%
  rename_with(everything(), .fn = ~ tolower(.)) %>%
  rename_with(everything(), .fn = ~ gsub(" ", "_", .)) %>%
  select(country_code, country, language_code, language, population) %>%
  rename(ISO6393 = "language_code")
df_speakers %>% distinct(ISO6393) %>% nrow() #6758 languages

### add glottolog data----
# check NA
colSums((is.na(df_speakers)))

df_speakers = df_speakers %>%
  mutate(country_code = case_when(
    country == "Namibia" ~ "NA",
    TRUE ~ country_code
  ))

#check for speaker data with population = 0
df_speakers %>% filter(population == "0")

# add language family to data, glottocode and macroarea ########

# access glottolog
df_glotto = lingtypology::glottolog

# subset to langs present in df_speakers, extract language family and macroarea
df_speakers_glotto = df_glotto %>%
  filter(iso %in% df_speakers$ISO6393) %>% 
  mutate(affiliation = gsub(",.*", "", affiliation)) %>%
  select(iso, glottocode, area, affiliation)

# make representation of isolates explicit

df_speakers_glotto = df_speakers_glotto %>%
  mutate(affiliation = case_when(affiliation == "" ~ "Isolate",
                                 TRUE ~ affiliation)) %>% 
  rename("ISO6393" = iso, "macroarea" = area, "family" = affiliation)


# join glotto data on ethnologue data
df_speakers = df_speakers %>% left_join(df_speakers_glotto, by = "ISO6393")

# check missing values from joining
colSums(is.na(df_speakers))

df_speakers %>% filter(is.na(macroarea))

# add values manually

df_speakers = df_speakers %>% mutate(macroarea = case_when(language == "Polci" ~ "Africa",
                                                           language == "Yaleba" ~ "Papunesia",
                                                           language == "Bontok, Northern" ~ "Papunesia",
                                                           language == "Bontok, Southwestern" ~ "Papunesia",
                                                           TRUE ~ macroarea),
                                     family = case_when(language == "Polci" ~ "Afro-Asiatic",
                                                        language == "Yaleba" ~ "Austronesian",
                                                        language == "Bontok, Northern" ~ "Austronesian",
                                                        language == "Bontok, Southwestern" ~ "Austronesian",
                                                        TRUE ~ family),
                                     glottocode = case_when(language == "Polci" ~ "polc1245",
                                                            language == "Yaleba" ~ "yale1245",
                                                            language == "Bontok, Northern" ~ "",
                                                            language == "Bontok, Southwestern" ~ "",
                                                            TRUE ~ glottocode))



### nas
colSums(is.na(df_speakers))

# language names uniquely defined?

multiple_names = df_speakers %>%
  group_by(ISO6393) %>%
  summarize(n_names = n_distinct(language)) %>% 
  filter(n_names > 1) %>% pull(ISO6393)# 85 languages with multiple names


df_speakers %>%
  filter(ISO6393 %in% multiple_names) %>%
  distinct(ISO6393, language) %>% head()
# some variation in spelling -> simply pick one

df_unique_iso = df_speakers %>% 
  distinct(ISO6393, language) %>%
  group_by(ISO6393) %>%
  slice_head(n = 1)

df_speakers = df_speakers %>%
  select(!language) %>% 
  left_join(df_unique_iso, by = join_by("ISO6393" == "ISO6393"))

df_speakers %>% distinct(ISO6393) %>% nrow()

df_speakers %>%
  group_by(language) %>%
  summarize(n = n_distinct(ISO6393)) %>% head()

# write files
write_csv(df_speakers, "Clean_data/country_lvl_data/country_lang_pop.csv")


### combined file -----

# restructure with a row for each language
df_speakers %>%
  distinct(ISO6393) %>% nrow()

df_langs = df_speakers %>%
  group_by(ISO6393) %>%
  summarize(language = unique(language),
            glottocode = unique(glottocode),
            family = unique(family),
            macroarea = unique(macroarea),
            population = sum(population))
  
View(df_langs)
### create a speaker categorical variable by splitting into quartiles
df_langs = df_langs %>%
  mutate(population_category = ntile(population, 4)) %>%
  mutate(population_category = case_when(
    population_category == 1 ~ "Low",
    population_category == 2 ~ "Lower-Mid",
    population_category == 3 ~ "Upper-Mid",
    population_category == 4 ~ "High"))

df_langs %>%
  group_by(population_category) %>%
  summarize(min = min(population),
            max = max(population)) 

# write file
write_csv(df_langs, "Clean_data/language_lvl_data/lang_pop.csv")

