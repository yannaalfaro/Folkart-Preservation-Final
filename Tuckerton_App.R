# Tuckerton Seaport Educational Programming (FINAL CLEANED VERSION)

library(shiny)
library(dplyr)
library(tidyverse)
library(tidygeocoder)
library(sf)
library(leaflet)
library(ggplot2)
library(tigris)
library(DT)
library(RColorBrewer)
library(stringi)
library(tidyr)
library(janitor)

# -----------------------------
# Load all pre-geocoded datasets
# -----------------------------

free_program_surveys <- read_csv("free_program_surveys_geocoded.csv") %>% clean_names()
patron_data_geo <- read_csv("patron_data_geocoded.csv") %>% clean_names()
free_programs_geo <- read_csv("free_programs_geocoded.csv") %>% clean_names()
school_tours_geo <- read_csv("school_tours_geocoded.csv") %>% clean_names()
school_residencies_geo <- read_csv("school_residencies_geocoded.csv") %>% clean_names()

# -----------------------------
# bulletproof cleaning and normalization
# -----------------------------

normalize_age_group <- function(x) {
  x <- trimws(x)
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[\u2013\u2014\u2212\u2012\uFE58\uFE63\uFF0D]", "-", x)
  x <- gsub("\\s*-\\s*", "-", x)
  return(x)
}

free_program_surveys$age_group <- normalize_age_group(free_program_surveys$age_group)

# -----------------------------
# ui
# -----------------------------

ui <- fluidPage(
  tags$head(
    tags$style(".header {display: flex; align-items: center; justify-content: space-between; padding: 10px; background-color: #084594; color: white;}")
  ),
  div(class = "header",
      div("Tuckerton Seaport Educational Programming Dashboard"),
      div("Built by Adriana Alfaro — 2025")
  ),
  tabsetPanel(
    tabPanel("Interactive Map", leafletOutput("map", height = 700)),
    tabPanel("Survey Results",
             fluidRow(
               column(6, wellPanel(plotOutput("agePlot", height = 400))),
               column(6, wellPanel(plotOutput("heardPlot", height = 400))),
               column(6, wellPanel(plotOutput("firstTimePie", height = 400))),
               column(6, wellPanel(plotOutput("familiarPlot", height = 400))),
               column(6, wellPanel(plotOutput("attendAgain", height = 400))),
               column(6, wellPanel(plotOutput("priorClasses", height = 400)))
             )
    ),
    tabPanel("Open-ended Feedback", dataTableOutput("textTable"))
  )
)

# -----------------------------
# server
# -----------------------------

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = -74.5, lat = 40.1, zoom = 8) %>%
      addAwesomeMarkers(data = patron_data_geo, ~longitude, ~latitude, group = "Paid Programs Patrons", icon = awesomeIcons(icon = 'users', markerColor = 'blue'), popup = ~paste0("Zip Code: ", paid_classes_zip)) %>%
      addAwesomeMarkers(data = free_programs_geo, ~longitude, ~latitude, group = "Free Program Locations", icon = awesomeIcons(icon = 'graduation-cap', markerColor = 'red'), popup = ~paste0("Location: ", location, "<br>Address: ", full_address)) %>%
      addAwesomeMarkers(data = school_tours_geo, ~longitude, ~latitude, group = "School Tours", icon = awesomeIcons(icon = 'school', markerColor = 'green'), popup = ~paste0("School: ", school, "<br>Address: ", address)) %>%
      addAwesomeMarkers(data = school_residencies_geo, ~longitude, ~latitude, group = "School Residencies", icon = awesomeIcons(icon = 'chalkboard-teacher', markerColor = 'purple'), popup = ~paste0("School: ", school, "<br>Address: ", address)) %>%
      addAwesomeMarkers(data = free_program_surveys, ~longitude, ~latitude, group = "Free Program Patrons", icon = awesomeIcons(icon = 'users', markerColor = 'orange'), popup = ~paste0("County: ", county)) %>%
      addLayersControl(overlayGroups = c("Paid Programs Patrons", "Free Program Locations", "School Tours", "School Residencies", "Free Program Patrons"), options = layersControlOptions(collapsed = FALSE))
  })
  
  dynamic_highlight_plot <- function(df, category, clean_title, reorder_levels = NULL) {
    if (!is.null(reorder_levels)) {
      df[[category]] <- factor(df[[category]], levels = reorder_levels)
      df <- df[order(match(df[[category]], reorder_levels)), ]
    } else {
      df <- df %>% arrange(desc(n))
      df[[category]] <- factor(df[[category]], levels = df[[category]])
    }
    palette_size <- nrow(df)
    blues <- colorRampPalette(c("#084594", "#bdd7e7"))(palette_size)
    names(blues) <- df[[category]]
    ggplot(df, aes_string(x = category, y = "n", fill = category)) +
      geom_col() +
      scale_fill_manual(values = blues) +
      labs(x = clean_title, y = "Count", fill = clean_title, title = paste(clean_title, "Distribution")) +
      theme_minimal()
  }
  
  output$agePlot <- renderPlot({
    data <- free_program_surveys %>% count(age_group)
    age_order <- c("65+","50-64","30-49","18-29","<18")
    dynamic_highlight_plot(data, "age_group", "Age Group", age_order)
  })
  
  output$priorClasses <- renderPlot({
    data <- free_program_surveys %>% filter(!is.na(number_of_prior_classes), number_of_prior_classes != "") %>% count(number_of_prior_classes)
    dynamic_highlight_plot(data, "number_of_prior_classes", "Prior Classes")
  })
  
  output$familiarPlot <- renderPlot({
    data <- free_program_surveys %>% filter(!is.na(familiar_with_seaport), familiar_with_seaport != "") %>% count(familiar_with_seaport)
    dynamic_highlight_plot(data, "familiar_with_seaport", "Familiar with Seaport")
  })
  
  output$attendAgain <- renderPlot({
    data <- free_program_surveys %>% filter(!is.na(plan_to_attend_again), plan_to_attend_again != "") %>% count(plan_to_attend_again)
    dynamic_highlight_plot(data, "plan_to_attend_again", "Plan to Attend Again")
  })
  
  output$firstTimePie <- renderPlot({
    first_time <- free_program_surveys %>% filter(!is.na(first_time_attending), first_time_attending != "") %>% count(first_time_attending)
    ggplot(first_time, aes(x = "", y = n, fill = first_time_attending)) +
      geom_bar(stat = "identity", width = 1) + coord_polar("y") +
      labs(title = "First Time Attending?", fill = "Response") + theme_void() +
      geom_text(aes(label = paste0(round(n / sum(n) * 100), "%")), position = position_stack(vjust = 0.5)) +
      scale_fill_brewer(palette = "Blues")
  })
  
  output$heardPlot <- renderPlot({
    heard_data <- free_program_surveys %>% 
      select(starts_with("heard_about_class_")) %>%
      mutate(across(-heard_about_class_other, ~ ifelse(tolower(trimws(.)) == "yes", "Yes", NA))) %>%
      mutate(heard_about_class_other = ifelse(!is.na(heard_about_class_other) & trimws(heard_about_class_other) != "", "Yes", NA)) %>%
      pivot_longer(cols = everything(), names_to = "Source", values_to = "Selected") %>%
      filter(Selected == "Yes") %>%
      mutate(Source = gsub("heard_about_class_", "", Source),
             Source = gsub("_", " ", Source)) %>%
      count(Source) %>%
      arrange(desc(n))
    
    blues <- colorRampPalette(c("#084594", "#bdd7e7"))(nrow(heard_data))
    ggplot(heard_data, aes(x = reorder(Source, n), y = n, fill = Source)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = blues) +
      labs(title = "How Did You Hear About Class?", x = "", y = "Count") +
      theme_minimal()
  })
  
  output$textTable <- renderDataTable({
    free_program_surveys %>% select(suggestions) %>% filter(!is.na(suggestions), suggestions != "")
  })
}

shinyApp(ui, server)

