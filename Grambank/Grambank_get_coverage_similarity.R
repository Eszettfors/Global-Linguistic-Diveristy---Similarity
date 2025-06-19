library(tidyverse)

# This script reads the Grambank data and the speaker datas
# It subsets the Grambank data to only include the speaker data and calculates the coverage
# in each country based on number of languages and speakers. For each language in each country, structural distance
# is calculated using gower distance and for each country the average completeness is given


# read grambank data -------------
df_grambank = read_csv("Clean_data/language_lvl_data/grambank_data_wide.csv")

# read country_lang data ---------------
df_country = read_csv("Clean_data/country_lvl_data/country_lang_pop.csv")

df_country = df_country %>% mutate(country_code = case_when(
  country == "Namibia" ~ "NA",
  TRUE ~ country_code
))

head(df_country)
df_country %>% unique()

# define functions -----
get_n_langs = function(df, countryID){
  # takes a df and country ID and returns the number of langs in the df
  langs = df %>%
    filter(country_code == countryID) %>% pull(ISO6393)
  return(length(langs))
}

get_n_langs_in_grambank = function(df, countryID){
  # takes a df and country ID and returns the number of langs present in Grambank
  langs = df %>% filter(country_code == countryID) %>% pull(ISO6393)
  n_langs_in_grambank = df_grambank %>%
    filter(ISO6393 %in% langs) %>% 
    select(ISO6393) %>% 
    unique() %>%
    nrow()
  return(n_langs_in_grambank)
}

get_lang_coverage = function(df, countryID){
  # takes a df and country ID and returns percentage of languages present in Grambank
  langs = get_n_langs(df, countryID)
  n_langs_in_grambank = get_n_langs_in_grambank(df, countryID)
  return(n_langs_in_grambank/langs * 100)
}

get_n_speakers = function(df, countryID){
  #takes a df and countryID and returns the number of speakers in that country of any language
  n_speaker = df %>% 
    filter(country_code == countryID) %>% 
    summarise(speakers = sum(population))
  return(as.numeric(n_speaker))
}

get_n_speakers_in_grambank = function(df, countryID){
  # takes a data frame and a country and returns the number of speakers of languages present in PHOIBLE
  speakers_in_grambank = df %>%
    filter(country_code == countryID) %>%
    filter(ISO6393 %in% df_grambank$ISO6393) %>% 
    summarize(speakers = sum(population)) %>%
    pull(speakers)
  return(as.numeric(speakers_in_grambank))
}

get_speaker_coverage_grambank = function(df, countryID){
  # takes a dataframe and a country and returns percentage of speakers in the df present in PHOIBLE
  n_speakers = get_n_speakers(df, countryID)
  n_speakers_grambank = get_n_speakers_in_grambank(df, countryID)
  return(n_speakers_grambank/n_speakers * 100)
}


#### similarity

# Dahl (2008) describes measuring language similarity as: “How large a proportion of the features that are defined for both members of a language pair have different values?”
# FVO in Ploeger et al. (https://arxiv.org/html/2407.05022v2#bib.bib32)
# "...which is the average of the percentages of features that overlap between any pair of languages in the combinations of a language set."

# Hammarström and O'Connor (2016) Gower distance: The Gower coefficient simply counts the number of features where the languages
# have a different value, divided by the total number of features compared

# cropping according to completeness completeness, e.g. skirgård et al (2023), ploeger et al.(2024) Dahl (2008)

# alternative:
# 25% completeness -> minimum 49 features defined to be included. For a calcualtion to be considered, minimum 50 common features defined
# Use number of features for which both languages are defined as a measure of reliability -> return as a matrix 
# -> calculate mean reliabiliy for each country

get_feature_vector = function(ISO) {
  # takes an ISO code and returns a vector with feature values from grambank
  vec = df_grambank %>% 
    filter(ISO6393 == ISO) %>%
    select(!c(ISO6393, Completeness, Language_name, Glottocode, Macroarea, Family_name)) %>%
    t() %>%
    as.vector()

  return(vec)
}

get_feature_overlap = function(l1, l2){
  # this function takes two language vectors of size n with categorical values.
  # it subsets the vectors to features for which both vectors are defined and returns the fraction
  # of overlapping values and the number of common features as a measure of reliability
  # if the number of overlapping features are smaller than 49 -> returns NA

  # create df of languages with features as columns
  df = t(data.frame(l1, l2))

    # subset to features without any missing values = both languages are defined
  df = df[, colSums(is.na(df)) == 0]
  
  #the number of features for which both languages are defined
  def_length = ncol(df)

  # check if the length is null or smaller than 49 -> return NA and def_length
  if (is.null(def_length) || def_length < 49) {
    return(c(NA, def_length))
  }
  else{
      # count instances of the language vectors overlapping. TRUE = same, FALSE = different
    tab = table(df[1,] == df[2,])
  
      # frac overlap = TRUE / (TRUE + FALSE)
    overlap = as.numeric(tab["TRUE"] / sum(tab))
    
    return(c(overlap, def_length))
  }
}

swe_vec = get_feature_vector("swe")
dan_vec = get_feature_vector("dan")
df_test = t(data.frame(swe_vec, dan_vec))
df_test = df_test[, colSums(is.na(df_test)) == 0]
table(df_test[1,] == df_test[2,])

get_feature_overlap_matrix = function(df, countryID){
  # takes a data frame with country language pairs and a country ID. 
  # returns a list with two matrices. One with similarity between all languages in the country based on common features in Grambank
  # and one with the number of features the language pair is defined for. 
  
  # retrieve ISO code in country
  langs = df %>%
    filter(country_code == countryID) %>%
    pull(ISO6393)
  
  # retrive ISO codes present in grambank
  langs_in_gram = df_grambank %>%
    filter(ISO6393 %in% langs) %>%
    pull(ISO6393)
  
  # create an empty matrix of with every language as a row and column entry to hold similarity values
  n = length(langs_in_gram)
  sim_m = matrix(NA,
                 ncol = n,
                 nrow = n,
                 dimnames = list(langs_in_gram, langs_in_gram))
  
  # copy the empty matrix to hold the number of features for which any two languages are defined
  def_m = sim_m

  # loop through upper triangle and populate both upper and lower triangle
  i = 1
  for (lang1 in langs_in_gram){
    for (lang2 in langs_in_gram[i:length(langs_in_gram)]){
      feature_vec_1 = get_feature_vector(lang1)
      feature_vec_2 = get_feature_vector(lang2)
      
      distance_def = get_feature_overlap(feature_vec_1, feature_vec_2)
      
      sim_m[lang1, lang2] = distance_def[1]
      sim_m[lang2, lang1] = distance_def[1]
      
      def_m[lang1, lang2] = distance_def[2]
      def_m[lang2, lang1] = distance_def[2]
    }
    i = i + 1
  }
  return(list(sim_m, def_m))
}

######
run = function(){
  # test function to get run time on country with most languages (papua new guinea)
  t1 = Sys.time()
  get_feature_overlap_matrix(df_country, "PG")
  t2 = Sys.time()
  print(t2-t1)
}

#run()# 2.83 min -> theoretical max = 2.83 * 215 = 608; -> 10 hours; is not the case as the computation time exponentialy decreases with fewer languages

# remove any langauges which have completeness <= 25 % 
df_grambank = df_grambank %>% filter(Completeness > 25)


# subset to one row for each country
sim_coverage = df_country %>% 
  distinct(country_code, country)

# add calculations to the subsetted df
sim_coverage = sim_coverage %>%
  rowwise() %>%
  mutate(n_langs = get_n_langs(df_country, country_code),
         n_langs_in_grambank = get_n_langs_in_grambank(df_country, country_code),
         gram_lang_coverage = get_lang_coverage(df_country, country_code),
         n_speakers = get_n_speakers(df_country, country_code),
         n_speakers_in_grambank = get_n_speakers_in_grambank(df_country, country_code),
         gram_speaker_coverage = get_speaker_coverage_grambank(df_country, country_code),
         feat_overlap_def = list(get_feature_overlap_matrix(df_country, country_code)))


# split similarity matrix and definition length matrix into two columns
sim_coverage = sim_coverage %>%
  rowwise() %>%
  mutate(feat_sim_matrix = list(feat_overlap_def[[1]]),
         def_length_matrix = list(feat_overlap_def[[2]])) %>%
  select(!feat_overlap_def)

# write object
write_rds(sim_coverage, "Clean_data/distances/grambank_coverage_similarity_population.rds")
