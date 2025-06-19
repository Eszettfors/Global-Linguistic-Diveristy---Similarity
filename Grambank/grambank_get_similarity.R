library(tidyverse)
library(stringr)

# this script allows the computation of feature overlap distance
# between any languages in Grambank

##### set up ---------
df_grambank = read_csv("Clean_data/language_lvl_data/grambank_data_wide.csv")

get_feature_vector = function(ISO) {
  # takes an ISO code and returns a vector with feature values from grambank
  vec = df_grambank %>% 
    filter(ISO6393 == ISO) %>%
    select(!c(ISO6393, Language_name, Glottocode, Macroarea, Family_name)) %>%
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

get_feature_overlap_matrix = function(langs){
  # takes a vector of iso codes and outputs a matrix with feature overlap between them

  
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

#####

# similarity between indo-european languages


IE = df_grambank %>% 
  filter(Family_name == "Indo-European") %>%
  pull(ISO6393)


IE_ms = IE %>%
  get_feature_overlap_matrix()


IE_sim = IE_ms[[1]]
IE_reli = IE_ms[[2]]


IE_sim = IE_sim[rownames(IE_sim) != "ckb", colnames(IE_sim) != "ckb"]

table(is.na(IE_sim))

cluster = hclust(dist(IE_sim), method = "ward.D2")

plot(cluster)

