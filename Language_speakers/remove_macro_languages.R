library(tidyverse)
library(readxl)
library(lingtypology)
### this script solves conflicts in the speaker data caused by merging ethnologue and joshua project data were macrolanguages were kept
# from ethnologe causing doubling of speaker in e.g. Paskistan - see pashto vs pushto

df_speaker = read_excel("Raw_data/Merged_Ethnologue_Joshua.xlsx")

# languages present in the data and also macro language according
# to ISO
macro_isos = c("nep",
               "zha",
               "uzb",
               "kur",
               "ful",
               "mon",
               "kln",
               "gon",
               "bua",
               "aym",
               "doi",
               "hai",
               "kom",
               "zza",
               "pus",
               "hbs")

df_speaker = df_speaker %>% 
  filter(!language_code %in% macro_isos)

### find doublets
doublet_ISO = df_speaker %>% 
  group_by(language) %>%
  summarize(isos = n_distinct(language_code)) %>% 
  filter(isos > 1)
print(doublet_ISO)

# change cases where simply two different iso
df_speaker = df_speaker %>%
  mutate(language = case_when(
    language_code == "gnk" ~ "||Gana",
    language_code == "gim" ~ "Gimi, Eastern",
    language_code == "gip" ~ "Gimi, Western",
    language_code == "kzy" ~ "Kango, Tshopo",
    language_code == "try" ~ "Kango, Bas-Uele",
    language_code == "lom" ~ "Loma, Liberia",
    language_code == "loi" ~ "Loma, Côte",
    language_code == "tyz" ~ "Táy",
    language_code == "tmv" ~ "Motembo-Kunda",
    TRUE ~ language
  ))

# fix errrors from merging and confusing langs and countrycodes
df_speaker = df_speaker %>%
  mutate(language_code = case_when(
    language == "Saliba" & country_code %in% c("CO", "VE") ~ "slc",
    language == "Saliba" & country_code == "PG" ~ "sbe",
    TRUE ~ language_code
  ))

# fix doublets by removing ethnologue
df_speaker = df_speaker %>% filter(!(language_code == "sbe" & df_speaker$source == "Ethnologue")) %>%
  mutate(language = case_when(
    language_code == "slc" ~ "Sáliba",
    TRUE ~ language
  )) 

df_speaker = df_speaker %>%
  filter(!(language_code == "slc" & df_speaker$source == "Ethnologue"))

# fix country lang mix up
df_speaker = df_speaker %>%
  filter(!(language_code == "taw" & country == "Papua New Guinea")) %>%
  filter(!(language_code == "neb" & country  == "Papua New Guinea")) %>%
  filter(!(language_code == "don" & country  == "Côte D'Ivoire")) %>%
  filter(!language_code %in% c("bib")) %>%
  mutate(language = case_when(
    language_code == "don" ~ "Toura, Papua New Guinea",
    language_code == "neb" ~ "Toura, Côte d'Ivoire",
    TRUE ~ language
  ))


### write

write_csv(df_speaker, "raw_data/josh_ethno_no_macro.csv")

# write
