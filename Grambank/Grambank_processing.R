library(lingtypology)
library(tidyverse)


### this script reads the grambankdata and creates two dataframed with each language as a row and each attribute as a column
# one long with each language-parameter-value as a row
# one wide with each langauge as a row and paramters a columns


# read data
gb = readRDS("Raw_data/grambank_database.rds")


# get grambank linguistic data
gb_tab = gb$tables
values_tab = gb_tab$ValueTable
param_tab = gb_tab$ParameterTable
lang_tab = gb_tab$LanguageTable


head(values_tab)
head(param_tab)
head(lang_tab)


# join tabs into a single dataframe
df = values_tab %>% 
  select(Language_ID, Parameter_ID, Value, Code_ID) %>%
  left_join(
    param_tab %>%
      select(ID, Name) %>%
      rename("Parameter_name" = Name),
    join_by("Parameter_ID" == "ID")
  ) %>%
  left_join(
    lang_tab %>%
      select(ID, Name, Glottocode, Macroarea, Family_name) %>%
      rename("Language_name" = Name),
    join_by("Language_ID" == "ID")
  ) %>%
  select(Language_ID, Language_name, Parameter_ID, Parameter_name, Value, Glottocode, Macroarea, Family_name)
  

### add ISO
glotto_iso = lingtypology::iso.gltc(lang_tab$Glottocode)
glotto_iso = as.data.frame(glotto_iso) %>% rename("ISO6393" = glotto_iso)
glotto_iso$Glottocode = rownames(glotto_iso)
rownames(glotto_iso) = 1:nrow(glotto_iso)
colSums(is.na(glotto_iso)) # no missing data

df = df %>%
  left_join(glotto_iso,
            join_by("Glottocode" == "Glottocode")) %>%
  select(-Language_ID) %>%
  relocate(ISO6393)

# write long df
write_csv(df, "Clean_data/language_lvl_data/grambank_data_long.csv")

### restructure to have one language for each row

df = df %>%
  select(!Parameter_ID) %>%
  pivot_wider(
    names_from = "Parameter_name",
    values_from = "Value"
)


# write wide df
write_csv(df, "Clean_data/language_lvl_data/grambank_data_wide.csv")
zip(df)


### export parameter ID-name key

param_key = param_tab %>% select(ID, Name)


write_csv(param_key, "Clean_data/keys/grambank_parameter_key.csv")
