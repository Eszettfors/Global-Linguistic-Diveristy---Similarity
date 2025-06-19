library(tidyverse)

# this script calculates the completeness of the parameter data for every language in grambank

gb_full = read_csv("Clean_data/language_lvl_data/grambank_data_wide.csv")

# create a dataframe to hold langauges
languages = gb_full %>% select(c(ISO6393, Language_name, Glottocode, Macroarea, Family_name))

# separate paramteres from language metadata
gb = gb_full %>% select(!c(ISO6393, Language_name, Glottocode, Macroarea, Family_name))


get_completeness = function(vec) {
  # this function takes a vector and counts the percentage of non missing values
  
  # gives a table where TRUE = not NA and FALSE = NA 
  tab = table(!is.na(vec))
  
  # extracts the number of TRUE = Data which isn't missing
  complete_data = as.numeric(tab["TRUE"])
  
  #returns data completeness as percentage of non missing values
  return(complete_data/length(vec) * 100)
}

#apply completeness function to grambank data
completeness_vec = c()
for (row in 1:nrow(gb)) {
  lang_vec = gb[row,]
  completeness = get_completeness(lang_vec)
  completeness_vec = c(completeness_vec, completeness)
}

# add completeness to language data
languages$Completeness = completeness_vec

#write
write_csv(languages, "Clean_data/language_lvl_data/grambank_data_completeness.csv")

# merge with full GB and write
gb_full = merge(languages, gb_full)


write_csv(gb_full, "Clean_data/language_lvl_data/grambank_data_wide.csv")

### completeness per parameter ####

completeness_vec_p = c()
for (col in 1:ncol(gb)){
  param_vec = as.vector(gb[,col])[[1]]
  completeness = get_completeness(param_vec)
  completeness_vec_p = c(completeness_vec_p, completeness)
}

completeness_vec_p

param_key = read_csv("Clean_data/keys/grambank_parameter_key.csv")

param_key$completeness = completeness_vec_p

# write
write_csv(param_key, "Clean_data/parameters/grambank_parameter_completeness.csv")

# add completeness 


#### ecdf plots

par(mfrow = c(2,1))
# languages completeness
ecdf_func_1 = ecdf(languages$Completeness)
plot(ecdf(languages$Completeness), col = "black", lwd = 2, verticals = TRUE, do.points = FALSE,
     xlab = "", ylab = "Cumulative Probability",
     main = "a) ECDF Plot of Completeness per Language")
abline(v = c(25, 50, 75), col = "grey", lwd = 2, lty = 2,
       h = c(ecdf_func_1(25), ecdf_func_1(50), ecdf_func_1(75)))

#parameter
ecdf_func_2 = ecdf(param_key$completeness)
plot(ecdf(param_key$completeness), col = "black", lwd = 2, verticals = TRUE, do.points = FALSE,
     xlab = "Completeness (%)", ylab = "Cumulative Probability",
     main = "b) ECDF Plot of Completeness per Parameter")
abline(v = c(25, 50, 75), col = "grey", lwd = 2, lty = 2,
       h = c(ecdf_func_2(25), ecdf_func_2(50), ecdf_func_2(75)))


