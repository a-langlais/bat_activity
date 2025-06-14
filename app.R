# =======================================================================================
# Titre: Application d'analyse des données d'activité Chiroptérologique
# Description:  Application intéractive pour l'importation, le calcul et la visualisation des
#               indicateurs d'activité chiroptérologique.
#
# Auteur: Alexandre LANGLAIS
# Date: 2025/06/14
# Version: 1.0
# GitHub : https://github.com/a-langlais/bat_activity
# Dépendances: shiny, readr, dplyr, here, plotly
#
# Instructions: Ce script permet de lancer une application shiny a deux onglets :
#                 - "Points actifs" : pour l'analyse des points manuels
#                 - "Points passifs" : pour l'analyse des points d'enregistrements continus
#
#               L'application permet d'importer son fichier, sélectionner les colonnes 
#               pertinentes et calculer automatiquement les indicateurs. Les résultats 
#               sont présentés sous forme d'un tableau d'une part, et de visualisations 
#               sommaires d'autre part. Le tableau est exportable au format .csv.
# =======================================================================================

library(shiny)    # 1.10.0
library(readr)    # 2.1.5
library(dplyr)    # 1.1.4
library(here)     # 1.0.1
library(plotly)   # pour les graphiques interactifs

# Chargement des fonctions personnalisées
setwd(here())
source("src/BatActive.R")
source("src/BatPlots.R")
source("src/BatPassive.R")  # Assure-toi d'avoir cette fonction pour les passifs

ui <- fluidPage(
  titlePanel("🦇 Analyse d'activité des chauves-souris"),
  tags$h6("© 2025 - Alexandre LANGLAIS (langlais.alexandre03@gmail.com)"),
  tags$br(),
  
  tabsetPanel(
    tabPanel("Points actifs",
             sidebarLayout(
               sidebarPanel(
                 fileInput("csv_file_actifs", "Charger un fichier CSV (Points actifs)", accept = ".csv"),
                 uiOutput("col_select_ui_actifs"),
                 numericInput("duration_actifs", "Durée d'enregistrement (en minutes)", value = 10, min = 1),
                 numericInput("npoint_actifs", "Nombre de points d'observation", value = 5, min = 1),
                 actionButton("run_analysis_actifs", "Lancer l’analyse"),
                 tags$div(style = "margin-top: 15px;", uiOutput("download_ui_actifs"))
               ),
               mainPanel(
                 verbatimTextOutput("file_status_actifs"),
                 h3("👀 Échantillon du jeu de données"),
                 tableOutput("preview_data_actifs"),
                 h3("🎯 Tableau des indicateurs"),
                 tableOutput("analysis_output_actifs"),
                 h3("📊 Visualisation des résultats"),
                 fluidRow(
                   column(6, plotlyOutput("behavior_pie_actifs")),
                   column(6, plotlyOutput("species_bar_actifs"))
                 )
               )
             )
    ),
    
    tabPanel("Points passifs",
             sidebarLayout(
               sidebarPanel(
                 fileInput("csv_file_passifs", "Charger un fichier CSV (Points passifs)", accept = ".csv"),
                 uiOutput("col_select_ui_passifs"),
                 selectInput("place_passifs", "Lieu le plus proche", 
                             choices = c("Paris", "Lyon", "Marseille", "Toulouse", "Bordeaux", "Brest", "Strasbourg")),
                 # selectInput("date_format_passifs", "Format de date",
                 #             choices = c("%Y-%m-%d" = "%Y-%m-%d", "%d/%m/%Y" = "%d/%m/%Y", "%m/%d/%Y" = "%m/%d/%Y")),
                 radioButtons("time_option", "Choisir le mode de saisie de l'heure:",
                              choices = c("Heure fixe" = "fixed", "Relatif au coucher du soleil" = "sunset")),
                 
                 conditionalPanel(
                   condition = "input.time_option == 'fixed'",
                   textInput("start_time", "Heure de début (HH:MM)", value = "08:00"),
                   textInput("end_time", "Heure de fin (HH:MM)", value = "22:00")
                 ),
                 
                 conditionalPanel(
                   condition = "input.time_option == 'sunset'",
                   numericInput("minutes_before_sunset", "Minutes avant le coucher du soleil", value = 30, min = 0),
                   numericInput("minutes_after_sunrise", "Minutes après le lever du soleil", value = 30, min = 0)
                 ),
                 
                 actionButton("run_analysis_passifs", "Lancer l’analyse"),
                 tags$div(style = "margin-top: 15px;", uiOutput("download_ui_passifs"))
               ),
               mainPanel(
                 verbatimTextOutput("file_status_passifs"),
                 h3("👀 Échantillon du jeu de données"),
                 tableOutput("preview_data_passifs"),
                 h3("🎯 Tableau des indicateurs"),
                 tableOutput("analysis_output_passifs"),
                 h3("📊 Visualisation des résultats"),
                 plotlyOutput("passive_plot")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  ####### Fonction de lecture #######
  read_data <- function(file_input) {
    req(file_input)
    tryCatch({
      data_raw <- read_delim(file_input$datapath, delim = ";", show_col_types = FALSE)
      
      # Convertir toutes les colonnes de type caractère en UTF-8 propre
      data_clean <- data_raw %>%
        dplyr::mutate(across(where(is.character), ~iconv(., from = "UTF-8", to = "UTF-8", sub = "")))
      
      return(data_clean)
    }, error = function(e) {
      NULL
    })
  }
  
  ####### Points actifs #######
  
  raw_data_actifs <- reactive({
    read_data(input$csv_file_actifs)
  })
  
  output$file_status_actifs <- renderText({
    if (is.null(input$csv_file_actifs)) return("Aucun fichier chargé.")
    if (is.null(raw_data_actifs())) return("Erreur lors de la lecture du fichier.")
    paste0("Fichier chargé avec ", nrow(raw_data_actifs()), " lignes et ", ncol(raw_data_actifs()), " colonnes.")
  })
  
  output$col_select_ui_actifs <- renderUI({
    req(raw_data_actifs())
    cols <- names(raw_data_actifs())
    
    tagList(
      selectInput("col_place_actifs", "Colonne : Point", choices = cols),
      selectInput("col_id_actifs", "Colonne : Espèce", choices = cols),
      selectInput("col_activity_actifs", "Colonne : Activité observée", choices = cols)
    )
  })
  
  output$preview_data_actifs <- renderTable({
    head(raw_data_actifs())
  })
  
  df_actifs_renamed <- eventReactive(input$run_analysis_actifs, {
    req(raw_data_actifs())
    raw_data_actifs() %>%
      rename(
        Place = all_of(input$col_place_actifs),
        Id = all_of(input$col_id_actifs),
        Activity = all_of(input$col_activity_actifs)
      )
  })
  
  analysis_result_actifs <- eventReactive(input$run_analysis_actifs, {
    df <- df_actifs_renamed()
    BatActive(df, duration = input$duration_actifs, npoint = input$npoint_actifs)
  })
  
  output$analysis_output_actifs <- renderTable({
    analysis_result_actifs()
  })
  
  output$download_ui_actifs <- renderUI({
    req(analysis_result_actifs())
    downloadButton("download_indicateurs_actifs", "Télécharger les résultats (.csv)", class = "btn-success")
  })
  
  output$download_indicateurs_actifs <- downloadHandler(
    filename = function() {
      paste0("indicateurs_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(analysis_result_actifs())
      write.csv(analysis_result_actifs(), file, row.names = FALSE)
    }
  )
  
  output$behavior_pie_actifs <- renderPlotly({
    req(analysis_result_actifs())
    plot_behavior_pie(analysis_result_actifs())
  })
  
  output$species_bar_actifs <- renderPlotly({
    req(df_actifs_renamed())
    plot_species_bar(df_actifs_renamed())
  })
  
  ####### Points passifs #######
  
  raw_data_passifs <- reactive({
    read_data(input$csv_file_passifs)
  })
  
  output$file_status_passifs <- renderText({
    if (is.null(input$csv_file_passifs)) return("Aucun fichier chargé.")
    if (is.null(raw_data_passifs())) return("Erreur lors de la lecture du fichier.")
    paste0("Fichier chargé avec ", nrow(raw_data_passifs()), " lignes et ", ncol(raw_data_passifs()), " colonnes.")
  })
  
  output$col_select_ui_passifs <- renderUI({
    req(raw_data_passifs())
    cols <- names(raw_data_passifs())
    
    tagList(
      selectInput("col_place_passifs", "Colonne : Point", choices = cols),
      selectInput("col_id_passifs", "Colonne : Espèce", choices = cols),
      selectInput("col_date_passifs", "Colonne : Date de la nuit", choices = cols),
      selectInput("col_time_passifs", "Colonne : Date et Heure de l'enregistrement", choices = cols)
    )
  })
  
  output$preview_data_passifs <- renderTable({
    head(raw_data_passifs())
  })
  
  df_passifs_renamed <- eventReactive(input$run_analysis_passifs, {
    req(raw_data_passifs())
    raw_data_passifs() %>%
      rename(
        Place = all_of(input$col_place_passifs),
        Id = all_of(input$col_id_passifs),
        Night_Date = all_of(input$col_date_passifs),
        Date_Time = all_of(input$col_time_passifs)
      )
  })
  
  analysis_result_passifs <- eventReactive(input$run_analysis_passifs, {
    df <- df_passifs_renamed()
    
    if (input$time_option == "fixed") {
      record_time <- c(input$start_time, input$end_time)
      sun_offsets <- NULL
    } else {
      record_time <- NULL
      sun_offsets <- c(before_sunset = input$minutes_before_sunset,
                       after_sunrise = input$minutes_after_sunrise)
    }
    
    BatPassive(
      data = df,
      city = input$place_passifs,
      record_time = record_time,
      sun_offsets = sun_offsets
    )
  })
  
  output$analysis_output_passifs <- renderTable({
    analysis_result_passifs()
  })
  
  output$download_ui_passifs <- renderUI({
    req(analysis_result_passifs())
    downloadButton("download_indicateurs_passifs", "Télécharger les résultats (.csv)", class = "btn-success")
  })
  
  output$download_indicateurs_passifs <- downloadHandler(
    filename = function() {
      paste0("indicateurs_passifs_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(analysis_result_passifs())
      write.csv(analysis_result_passifs(), file, row.names = FALSE)
    }
  )
  
  output$passive_plot <- renderPlotly({
    req(analysis_result_passifs())
    plot_passive_activity(analysis_result_passifs())
  })
  
}

  
shinyApp(ui, server)
