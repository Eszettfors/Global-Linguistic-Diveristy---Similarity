library(tidyverse)
library(lingtypology)
source("distance_analysis/all_similarities_all_db.R")

# this script creates a file with european languages to be used for cluster analysis


european_languages = data.frame(
  ISO6393 = c(
    "eng", "fra", "deu", "ita", "spa", "por", "nld",
    "dan", "swe", "nor", "fin", "isl", "ell",
    "rus", "ukr", "pol", "ces", "slk", "hun", "ron",
    "bul", "hbs", "slv", "lit", "lav", "ekk",
    "gle", "mlt", "mkd", "aln", "als", "hye", "azj", "azb",
    "bel", "kat", "tur",
    "cat", "eus", "glg", "oci", "cor", "cym", "gla", "bre",
    "frr", "frs", "fry", "lld", "lij", "lmo", "nap", "scn", "vec",
    "rup", "rmn", "rmy", "gsw", "sme", "smn", "sms",
    "vro", "szl", "ksh", "dsb", "hsb", "ast", "ext", "fit", "bar"
  ),
  language = c(
    "English", "French", "German", "Italian", "Spanish", "Portuguese", "Dutch",
    "Danish", "Swedish", "Norwegian", "Finnish", "Icelandic", "Greek",
    "Russian", "Ukrainian", "Polish", "Czech", "Slovak", "Hungarian", "Romanian",
    "Bulgarian", "Serbo-Croat", "Slovenian", "Lithuanian", "Latvian", "Estonian",
    "Irish", "Maltese", "Macedonian", "Albanian Gheg", "Albanian Tosk", "Armenian", "North Azerbaijani", "South Azerbaijani",
    "Belarusian", "Georgian", "Turkish",
    "Catalan", "Basque", "Galician", "Occitan", "Cornish", "Welsh", "Scottish Gaelic", "Breton",
    "North Frisian", "Eastern Frisian", "Western Frisian", "Ladin", "Ligurian", "Lombard",
    "Neapolitan", "Sicilian", "Venetian", "Aromanian", "Balkan Romani", "Vlax Romani",
    "Swiss German", "Northern Sami", "Inari Sami", "Skolt Sami", "Võro", "Silesian",
    "Kölsch", "Lower Sorbian", "Upper Sorbian", "Asturian", "Extremaduran", "Meänkieli", "Bavarian"
  ),
  family = c(
    "Germanic", "Romance", "Germanic", "Romance", "Romance", "Romance", "Germanic",
  "Germanic", "Germanic", "Germanic", "Uralic", "Germanic", "Hellenic",
  "Slavic", "Slavic", "Slavic", "Slavic", "Slavic", "Uralic", "Romance",
  "Slavic", "Slavic", "Slavic", "Baltic", "Baltic", "Uralic",
  "Celtic", "Afro-Asiatic", "Slavic", "Albanian", "Albanian", "Armenian", "Turkic", "Turkic",
  "Slavic", "Kartvelian", "Turkic",
  "Romance", "Language isolate", "Romance", "Romance", "Celtic", "Celtic", "Celtic", "Celtic",
  "Germanic", "Germanic", "Germanic", "Romance", "Romance", "Romance", 
  "Romance", "Romance", "Romance", 
  "Romance", "Indo-Aryan", "Indo-Aryan", 
  "Germanic", "Uralic", "Uralic", "Uralic", 
  "Uralic", "Slavic", "Germanic", "Slavic", "Slavic", "Romance", "Romance", "Uralic", "Germanic"
)
)


write_csv(european_languages, "Clean_data/language_lvl_data/all_european_languages")
nrow(european_languages) #  70 languages

#### subset to those found in all databases

scd = df_glotto %>%
  filter(ISO6393 %in% df_grambank$ISO6393) %>%
  filter(ISO6393 %in% df_asjp$ISO6393) %>% 
  filter(ISO6393 %in% df_phoible$ISO6393)

scd_europe = european_languages %>%
  filter(ISO6393 %in% scd$ISO6393)

write_csv(scd_europe, "Clean_data/language_lvl_data/european_languages_in_all_db")
