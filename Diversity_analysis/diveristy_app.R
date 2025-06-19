library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(DT)
library(stringdist)
library(visNetwork)
library(stringr)

setwd("~/Projekt/Master_arbeit_2_new_data")

df_country = read_csv("Clean_data/country_lvl_data/country_diversity.csv") # necessary for mapping
df_pop = read_csv("Clean_data/country_lvl_data/country_lang_pop.csv") # necessary for diversity calculation 
df_lex = readRDS("~/Projekt/Master_arbeit_2_new_data/Clean_data/distances/asjp_coverage_similarity_population.rds") # necessary for non naive diversity calculation
df_asjp = read_csv("Clean_data/language_lvl_data/asjp_wide.csv")
df_asjp = df_asjp %>% filter(words > 27)
df_grambank = read_csv("Clean_data/language_lvl_data/grambank_data_wide.csv")

theme_set(theme_bw())

# fix namibia
df_country = df_country %>% 
  mutate(country_code = case_when(country == "Namibia" ~ "NA",
                                  TRUE ~ country_code))
df_pop = df_pop %>%
  mutate(country_code = case_when(country == "Namibia" ~ "NA",
                                  TRUE ~ country_code))

# get map data
df_map = rnaturalearth::ne_countries(scale = "large", type = "countries")
df_map = df_map %>% select(iso_a2_eh, geometry, continent)

df_map = df_map %>%
  mutate(iso_a2_eh = case_when(is.na(iso_a2_eh) ~ "NA",
                               TRUE ~ iso_a2_eh)) %>%
  group_by(iso_a2_eh) %>%
  summarize(iso_a2_eh = first(iso_a2_eh),
            geometry = first(geometry),
            continent = first(continent))



# add geometry
df_country = df_country %>%
  left_join(df_map, join_by ("country_code" =="iso_a2_eh"))

# fix continents
df_country %>%
  filter(continent == "Seven seas (open ocean)")



df_country = df_country %>%
  mutate(continent = case_when(
    country_code == "CC" ~ "Asia",
    country_code == "CX" ~ "Oceania",
    country_code == "GF" ~ "South America",
    country_code == "GP" ~ "North America",
    country_code == "MQ" ~ "North America",
    country_code == "RE" ~ "Africa",
    country_code == "SJ" ~ "Europe",
    country_code == "YT" ~ "Africa",
    country_code == "IO" ~ "Asia",
    country_code == "MU" ~ "Africa",
    country_code == "MV" ~ "Asia",
    country_code == "SC" ~ "Africa",
    country_code == "SH" ~ "Africa",
    country_code == "TK" ~ "Oceania",
    country_code == "FR" ~ "Europe",
    TRUE ~ continent))


# add asjp_speaker_coverage
df_country = df_country %>%
  left_join(df_lex %>%
              select(country_code, asjp_speaker_coverage),
            join_by("country_code" == "country_code")) %>%
  rename("sim_coverage" = "asjp_speaker_coverage")


# add continent to pop
df_pop = df_country %>%
  select(country_code, continent) %>%
  right_join(df_pop,
             join_by("country_code" == "country_code"))

# make it an sf_oject
df_country = st_as_sf(df_country)


# get geometry for population data
df_pop_map = df_pop %>%
  left_join(df_country %>%
              select(country_code, geometry),
            join_by("country_code" == "country_code")) %>%
  st_as_sf()


#### functions
get_shannon_diversity = function(prop, sim_m){
  # calculates diversity for q = 1 ergo shannon given a vector with proportions
  
  # for each proportion, get the expected similarity to all other proportions
  expected = log(sim_m %*% prop)
  
  # for each proportion, multiply by expected similarity to all other proportions
  # and derive entropy
  E = -1 * sum(prop * expected)
  
  # exponentiate entropy to get diversity
  D = exp(E)
  
  return(D)
}


get_diversity_q = function(prop, sim_m, q){
  # a general function to implement diversity for any q
  
  # to avoid division with zero, implement shannon diversity as a special case
  if (q == 1){
    return(get_shannon_diversity(prop, sim_m))
  }
  
  # get expected similarity to all other prop for each proportion
  expected = sim_m %*% prop
  
  # raise the expected similarity to the power of q-1
  expected_order = expected^(q-1)
  
  # multiply the expected similarity with each proportion and take the reciprocal
  D = (sum(prop * expected_order))^(1/(1-q))
  
  return(D)
}

get_naive_div_q = function(c, q) {
  # function takes a country and calculates the naive diversity of order q
  
  # subset to langs in matrix and order
  props = df_pop %>% filter(country == c) %>%
    mutate(prop = population /sum(population)) %>% pull(prop)
  
  # create an identity matrix with the length of props
  I = diag(length(props))
  
  # get diversity
  D = get_diversity_q(props, I, q)
  return(D)
}

get_lex_div_q = function(c, q) {
  # function takes a country and calculates the lexical diversity of order q
  lex_m = df_lex %>% 
    filter(country == c) %>%
    pull(ldn_sim_matrix)
  lex_m = lex_m[[1]]
  
  # subset to langs in matrix and order
  props = df_pop %>% filter(country == c) %>%
    filter(ISO6393 %in% colnames(lex_m)) %>%
    mutate(prop = population /sum(population)) %>%
    select(ISO6393, prop) %>%
    mutate(order = match(ISO6393, colnames(lex_m))) %>%
    arrange(order) %>% pull(prop)
  
  # get diversity
  D = get_diversity_q(props, lex_m, q)
  return(D)
}

get_diversity_profile = function(c, non_naive = TRUE, range = 4) {
  # this function takes a country or a vector of countries calculates its diversity profile, if not naive, it uses lexical similarity
  
  if (is.vector(c) && length(c) == 1){ # check if string
    #vecs to gold values
    div_values = c()
    qs = c()
    # loop through qs
    for (q in seq(0,range, 0.05)){
      # if naive, run applicable fucntion
      if (non_naive == TRUE){
        div_values = c(div_values, get_lex_div_q(c, q = q))
      } else {
        div_values = c(div_values, get_naive_div_q(c, q = q))
      }
      # save qs
      qs = c(qs, q)
    }
    # create dataframe for plotting
    div_prof = data.frame(qs, div_values)
    
    # plot
    p = div_prof %>%
      ggplot(aes(x = qs, y = div_values)) + 
      geom_line() + 
      labs(x = "q",
           y = "Diversity")
    
    plot(p)
    return(p)
  } else if(is.vector(c)){ # check if vector
    df_div_prof = data.frame()
    for (country in c){ 
      # get diveristy values for each country
      div_values = c()
      qs = c()
      for (q in seq(0,range, 0.05)){
        if (non_naive == TRUE) { # apply different function depending on if naive or not
          div_values = c(div_values, get_lex_div_q(country, q = q))
        } else {
          div_values = c(div_values, get_naive_div_q(country, q = q))
        }
        qs = c(qs, q)
      }
      # vector filled with country code
      name_vec = rep(country, length(div_values))
      # create a dataframe
      df_div_prof = rbind(df_div_prof, data.frame(name_vec, qs, div_values))
    }
    
    # plot the data
    p = df_div_prof %>%
      ggplot(aes(y = div_values, x = qs, fill = name_vec)) + 
      geom_line(aes(color = name_vec, linetype = name_vec)) + 
      labs(x = "q",
           y = "Diversity",
           linetype = "Country",
           color = "Country")
    plot(p)
    return(p)
  }
}


get_languages = function(c){
  langs = df_pop %>% filter(country == c) %>%
    mutate(speakers_perc = round(population / sum(population) * 100,2)) %>%
    select(language, family, population, speakers_perc) %>%
    arrange(desc(speakers_perc)) %>%
    rename("speakers (%)" = "speakers_perc",
           "speakers" = "population")
  return(langs)
}

get_languages_global = function() {
  # summarizes languages on a global level according to speakers
  df_pop %>%  
    group_by(language) %>%
    summarise(
      family = unique(family),
      speakers = sum(population, na.rm = TRUE)) %>%
    mutate(percent = round(100 * speakers / sum(speakers), 2)) %>%
    rename("speakers (%)" = "percent") %>%
    arrange(desc(speakers))
}

get_languages_by_continent = function(continents) {
  # summarizes languages on a continent according to speakers
  df_pop %>%  
    filter(continent %in% continents) %>%
    group_by(language) %>%
    summarise(
      family = unique(family),
      speakers = sum(population, na.rm = TRUE)) %>%
    mutate(percent = round(100 * speakers / sum(speakers), 2)) %>%
    rename("speakers (%)" = "percent") %>%
    arrange(desc(speakers))
}

get_concept_vector = function(ISO){
  # this function takes a language ISO code and returns a vector with concept values from ASJP
  if (!ISO %in% df_asjp$ISO6393){
    stop(error)
  }
  vec = df_asjp %>%
    filter(ISO6393 == ISO) %>%
    select(!c("lang_id", "ISO6393", "words", "completeness", "language", "macroarea", "family")) %>%
    t() %>%
    as.vector()
  return(vec)
}

get_mean_ldn_sim = function(v1, v2){
  # this function takes two language vectors with concepts and calculates the mean normalized levenshtein distance 
  # between them
  
  # get vector with normalized levenshtein distances
  ldn = stringdist(v1, v2, method = "lv") / pmax(nchar(v1), nchar(v2))
  
  # average ldn
  mean_ldn = mean(ldn, na.rm = TRUE)
  
  return(1 - mean_ldn)
}

get_ldn_sim_matrix = function(iso_vec){
  # function takes a vector of iso codes and returns a matrix with pairwise similarity
  
  sim_m = matrix(NA, ncol = length(iso_vec), nrow = length(iso_vec), dimnames = list(iso_vec, iso_vec))
  
  # populate upper triangl
  i = 1
  for (lang1 in iso_vec){
    for (lang2 in iso_vec[i:length(iso_vec)]){
      concept_vec_1 = get_concept_vector(lang1)
      concept_vec_2 = get_concept_vector(lang2)
      
      sim = get_mean_ldn_sim(concept_vec_1, concept_vec_2)
      
      sim_m[lang1, lang2] = sim
      sim_m[lang2, lang1] = sim
      
    }
    i = i + 1
  }
  return(sim_m)
}

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
  return(sim_m)
}

get_feature_overlap_matrix(c("swe", "dan"))

# add total population
df_pop_map = df_pop %>%
  group_by(country) %>%
  summarize(total_population = sum(population)) %>%
  right_join(
    df_pop_map,
    join_by("country" == "country")
  ) %>%
  st_as_sf()




######things to add - 
# ability to select a country and get similarity matrix + network
# ability to enter two language names and get a similarity measure.
# ability to enter multiple languages and get a matrix + network


##### application
df = df_country
df = df %>% rename("Richness" = "richness",
              "Exponent Shannon" = "exponent_shannon",
              "Inverse Simpson" = "inverse_simpson",
              "Mean Phonemic Similarity" = "mean_phoible_similarity",
              "Mean Morphosyntactic Similarity" = "mean_grambank_similarity",
              "Mean Lexical Similarity" = "mean_asjp_similarity",
              "Mean Phylogenetic Similarity" = "mean_glottolog_similarity",
              "Lexical Diversity (q=0)" = "lex_div_q_0",
              "Lexical Diversity (q=1)" = "lex_div_q_1",
              "Lexical Diversity (q=2)" = "lex_div_q_2",
              "Coverage for Similarity Measure (%)" = "sim_coverage")

# list of continents to select in the app
continent_choices = sort(unique(df$continent))

# UI
ui = fluidPage(
  titlePanel("Global Linguistic Diversity"),
  tabsetPanel(
    id = "main_tabs",
    
    # --- Tab 1: Diversity Dashboard ---
    tabPanel("Diversity Dashboard",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "selected_measure",
                   label = "Select a measure to visualize:",
                   choices = c(
                     "Richness", "Exponent Shannon", "Inverse Simpson",
                     "Mean Phonemic Similarity", "Mean Morphosyntactic Similarity",
                     "Mean Lexical Similarity", "Mean Phylogenetic Similarity",
                     "Lexical Diversity (q=0)", "Lexical Diversity (q=1)",
                     "Lexical Diversity (q=2)", "Coverage for Similarity Measure (%)"
                   ),
                   selected = "Richness"
                 ),
                 selectInput(
                   inputId = "selected_countries",
                   label = "Select country/countries to get diversity profile:",
                   choices = sort(unique(df$country)),
                   selected = NULL,
                   multiple = TRUE
                 ),
                 checkboxInput("non_naive", "Use lexical similarity (non-naive)?", value = FALSE),
                 checkboxGroupInput(
                   inputId = "selected_continents",
                   label = "Select continent(s):",
                   choices = continent_choices,
                   selected = continent_choices
                 ),
                 actionButton("select_all", "Select All"),
                 actionButton("deselect_all", "Deselect All"),
                 br(), br(),
                 hr(),
                 h4("Most Widely Spoken Languages"),
                 actionButton("toggle_table_view", "Show Global Language Table"),
                 DT::dataTableOutput("language_table")
               ),
               mainPanel(
                 leafletOutput("map", height = "800px"),
                 plotOutput("diversity_plot", height = "400px")
               )
             )
    ),
    
    # --- Tab 2: Diaspora View ---
    tabPanel("Linguistic Diaspora",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "selected_language",
                   label = "Search for a language to show its diaspora:",
                   choices = NULL,  # initialize with no choices
                   selected = "English",
                   multiple = FALSE
                 ),
                 checkboxInput(
                   inputId = "show_percentage",
                   label = "Show diaspora as % of country population",
                   value = FALSE
                 ),
                 h4("Distribution of Speakers"),
                 DT::dataTableOutput("language_distribution")
                 
               ),
               mainPanel(
                 leafletOutput("diaspora_map", height = "800px")
               )
             )
    ),
    # tab panel # 3 lexical similarity
    tabPanel("Lexical Similarity",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "selected_languages",
                   label = "Select languages to compare:",
                   choices = sort(unique(df_asjp$language)),
                   selected = NULL,
                   multiple = TRUE
                 ),
                 helpText("Select at least two languages to compute lexical similarity."),
                 sliderInput(
                   inputId = "similarity_threshold",
                   label = "Minimum lexical similarity to show connection:",
                   min = 0,
                   max = 0.4,
                   value = 0.1,
                   step = 0.01
                 )),
               mainPanel(
                 DT::dataTableOutput("similarity_matrix_table"),
                 visNetworkOutput("similarity_network", height = "600px")
                 
               ),
               
             )
    ),
    # tab panel 4, morph similarity
    tabPanel("Morphosyntactic Similarity",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "selected_languages_grambank",
                   label = "Select languages to compare:",
                   choices = sort(unique(df_grambank$Language_name)),
                   selected = NULL,
                   multiple = TRUE
                 ),
                 helpText("Select at least two languages to compute morphosyntactic similarity."),
                 sliderInput(
                   inputId = "grambank_similarity_threshold",
                   label = "Minimum morphosyntactic similarity to show connection:",
                   min = 0,
                   max = 1,
                   value = 0.5,
                   step = 0.01
                 )
               ),
               mainPanel(
                 DT::dataTableOutput("grambank_similarity_matrix_table"),
                 visNetworkOutput("grambank_similarity_network", height = "600px")
               )
             )
    )
    
             )
    )
    


# Server
server = function(input, output, session) {
  # --- Diversity data ---
  
  color_data = reactive({
    req(input$selected_measure, input$selected_continents)
    df %>%
      filter(continent %in% input$selected_continents) %>%
      mutate(selected_value = .[[input$selected_measure]])
  })
  
  measures_pal = reactive({
    colorNumeric(palette = "YlGnBu", domain = color_data()$selected_value, na.color = "grey")
  })
  

  
  # Diversity map
  output$map <- renderLeaflet({
    leaflet(df) %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  observe({
    data = color_data()
    pal_func = measures_pal()
    
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        layerId = ~country, 
        fillColor = ~pal_func(selected_value),
        fillOpacity = 0.7,
        color = "#BDBDC3",
        weight = 1,
        popup = ~paste0("<strong>", country, "</strong><br/>", 
                        input$selected_measure, ": ", round(selected_value, 3)),
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
        label = ~paste0(country, ": ", round(selected_value, 3))
      ) %>%
      addLegend(
        pal = pal_func,
        values = data$selected_value,
        opacity = 0.7,
        title = input$selected_measure,
        position = "bottomright",
        labFormat = labelFormat(digits = 3)
      )
  })
  
  # Diversity plot
  output$diversity_plot = renderPlot({
    req(input$selected_countries)
    get_diversity_profile(c = input$selected_countries, non_naive = input$non_naive)
  })
  
  # Continent select/deselect
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "selected_continents", selected = continent_choices)
  })
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "selected_continents", selected = character(0))
  })
  
  # Clicked country table
  clicked_country = reactiveVal(NULL)
  observeEvent(input$map_shape_click, {
    clicked_country(input$map_shape_click$id)
  })
  
  table_view_mode = reactiveVal("country")
  
  # Button toggles the view
  observeEvent(input$toggle_table_view, {
    if (table_view_mode() == "country") {
      table_view_mode("global")
      updateActionButton(session, "toggle_table_view", label = "Show Country Table")
    } else {
      table_view_mode("country")
      updateActionButton(session, "toggle_table_view", label = "Show Global Language Table")
    }
  })
  
  # Reset to country view if user clicks on map
  observeEvent(input$map_shape_click, {
    clicked_country(input$map_shape_click$id)
    table_view_mode("country")
    updateActionButton(session, "toggle_table_view", label = "Show Global Language Table")
  })
  
  output$language_table = DT::renderDataTable({
    mode <- table_view_mode()
    
    if (mode == "global") {
      # Global language table, sorted
      lang_data <- get_languages_global()  # you already have this function
    } else if (!is.null(clicked_country())) {
      lang_data <- get_languages(clicked_country())
    } else if (!is.null(input$selected_continents)) {
      lang_data <- get_languages_by_continent(input$selected_continents)
    } else {
      lang_data <- data.frame()
    }
    
    DT::datatable(lang_data, options = list(pageLength = 10), rownames = FALSE)
  })
  
  
  
  # --- Diaspora view ---
  diaspora_data = reactive({
    req(input$selected_language)
    df_pop_map %>%
      filter(language == input$selected_language) %>%
      mutate(percentage = 100 * population / total_population )
  })
  
  diaspora_pal = reactive({
    req(diaspora_data())
    colorNumeric(palette = "Blues", domain = diaspora_data()$population, na.color = "transparent")
  })
  
  
  output$diaspora_map <- renderLeaflet({
    req(input$selected_language)
    diaspora = diaspora_data()
    
    # Choose variable for fill and legend
    map_var = if (isTRUE(input$show_percentage)) diaspora$percentage else diaspora$population
    pal = colorNumeric(
      palette = if (isTRUE(input$show_percentage)) "Purples" else "Blues",
      domain = map_var,
      na.color = "transparent"
    )
    
    leaflet(diaspora) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(map_var),
        fillOpacity = 0.8,
        color = "grey",
        weight = 2,
        popup = ~paste0(
          "<strong>", country, "</strong><br/>Speakers: ", population,
          if (isTRUE(input$show_percentage)) paste0("<br/>% of Population: ", round(percentage, 2), "%") else ""
        )
      ) %>%
      addLegend(
        pal = pal,
        values = map_var,
        title = if (isTRUE(input$show_percentage))
          paste0("Speakers of ", input$selected_language, " (% of pop.)")
        else
          paste("Speakers of", input$selected_language),
        position = "topright"
      )
  })
  
  observe({
    updateSelectizeInput(
      session,
      inputId = "selected_language",
      choices = sort(unique(df_pop_map$language)),
      selected = "English",
      server = TRUE
    )
  })
  
  language_distribution_table = reactive({
    req(input$selected_language)
    lang_df = df_pop %>% 
      filter(language == input$selected_language) %>%
      group_by(language) %>%
      mutate(
        total_speakers = sum(population),
        percentage = round(100 * population / total_speakers, 2)
      ) %>%
      ungroup() %>%
      select(country, population, percentage) %>%
      arrange(desc(population))
    
    lang_df
  })
  
  output$language_distribution = DT::renderDataTable({
    DT::datatable(
      language_distribution_table(),
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Country", "Speakers", "% of Total Speakers"))})
  
  selected_iso_codes = reactive({
    req(input$selected_languages)
    df_asjp %>%
      filter(language %in% input$selected_languages) %>%
      pull(ISO6393) %>%
      unique()
  })
  
  similarity_matrix = reactive({
    iso_vec = selected_iso_codes()
    req(length(iso_vec) >= 2)
    get_ldn_sim_matrix(iso_vec)
  })
  
  output$similarity_network <- renderVisNetwork({
    sim_mat <- similarity_matrix()
    req(nrow(sim_mat) > 1)
    
    sim_vals <- as.matrix(sim_mat)
    language_names <- rownames(sim_vals)
    
    nodes <- data.frame(id = language_names, label = language_names)
    
    # Fixed threshold (adjust here as needed)
    threshold <- input$similarity_threshold
    
    # Get upper triangle indices with similarity above threshold
    edge_indices <- which(upper.tri(sim_vals) & sim_vals > threshold, arr.ind = TRUE)
    
    # Extract similarity values safely
    sim_scores <- mapply(function(i, j) sim_vals[i, j], edge_indices[, 1], edge_indices[, 2])
    
    # Create edges
    edges <- data.frame(
      from = language_names[edge_indices[, 1]],
      to = language_names[edge_indices[, 2]],
      value = sim_scores,
      title = paste0("Similarity: ", round(sim_scores, 3)),
      width = 2 + 5 * sim_scores,
      length = 300 * (1 - sim_scores)
    )
    
    visNetwork(nodes, edges) %>%
      visEdges(smooth = FALSE, color = list(color = "#97C2FC", highlight = "#3B8BEB")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 42)
  })
  
  selected_iso_codes_grambank = reactive({
    req(input$selected_languages_grambank)
    df_grambank %>%
      filter(Language_name %in% input$selected_languages_grambank) %>%
      pull(ISO6393) %>%
      unique()
  })
  
  grambank_similarity_matrix = reactive({
    iso_vec = selected_iso_codes_grambank()
    req(length(iso_vec) >= 2)
    get_feature_overlap_matrix(iso_vec)
  })
  
  output$grambank_similarity_network <- renderVisNetwork({
    sim_mat <- grambank_similarity_matrix()
    req(nrow(sim_mat) > 1)
    
    sim_vals <- as.matrix(sim_mat)
    language_names <- rownames(sim_vals)
    
    nodes <- data.frame(id = language_names, label = language_names)
    
    threshold <- input$grambank_similarity_threshold
    
    edge_indices <- which(upper.tri(sim_vals) & sim_vals > threshold, arr.ind = TRUE)
    sim_scores <- mapply(function(i, j) sim_vals[i, j], edge_indices[, 1], edge_indices[, 2])
    
    edges <- data.frame(
      from = language_names[edge_indices[, 1]],
      to = language_names[edge_indices[, 2]],
      value = sim_scores,
      title = paste0("Similarity: ", round(sim_scores, 3)),
      width = 2 + 5 * sim_scores,
      length = 1000 * (1 - sim_scores)
    )
    
    visNetwork(nodes, edges) %>%
      visEdges(smooth = FALSE, color = list(color = "#97C2FC", highlight = "#3B8BEB")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 42)
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)


