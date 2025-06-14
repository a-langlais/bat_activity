# =======================================================================================
# Titre: Application d'analyse des données d'activité Chiroptérologique
# Description:  Application intéractive pour l'importation, le calcul et la visualisation des
#               indicateurs d'activité chiroptérologique.
#
# Auteur: Alexandre LANGLAIS
# Date: 2025/06/14
# Version: 1.0
# GitHub : https://github.com/a-langlais/bat_activity
# Dépendances: shiny, readr, dplyr, here
#
# Instructions: Ce script permet de lancer une application shiny a deux onglets :
#                 - "Points actifs" : pour l'analyse des points manuels
#                 - "Points passifs" : pour l'analyse des points d'enregsitrement continue
#
#               L'application permet d'importer son fichier, sélectionner les colonnes 
#               pertinentes et calculer automatiquement les indicateurs. Les résultats 
#               sont présentés sous forme d'un tableau d'une part, et de visualisations 
#               sommaires d'autre part. Le tableau est exportable au format .csv.
# =======================================================================================

# Import des packages
library(shiny) # 1.10.0
library(readr) # 2.1.5
library(dplyr) # 1.1.4
library(here)  # 1.0.1

# Chargement des fonctions
setwd(here())
source("src/BatActive.R")
source("src/BatPlots.R")


### UI
ui <- fluidPage(
  titlePanel("Analyse d'activité des chauves-souris"),
  
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
                 h3("👀 Echantillon du jeu de données"),
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
                 numericInput("duration_passifs", "Durée d'enregistrement (en minutes)", value = 15, min = 1),
                 numericInput("npoint_passifs", "Nombre de points d'observation", value = 3, min = 1),
                 actionButton("run_analysis_passifs", "Lancer l’analyse")
               ),
               mainPanel(
                 verbatimTextOutput("file_status_passifs"),
                 tableOutput("preview_data_passifs"),
                 h3("Tableau des indicateurs"),
                 tableOutput("analysis_output_passifs"),
                 textOutput("extra_passifs")
               )
             )
    )
  )
)

### SERVER
server <- function(input, output, session) {
  
  ####### Utilitaires de chargement #######
  
  read_data <- function(file_input) {
    req(file_input)
    tryCatch({
      read_delim(file_input$datapath, delim = ";", show_col_types = FALSE)
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
  })
  
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
      selectInput("col_activity_passifs", "Colonne : Activité observée", choices = cols)
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
        Activity = all_of(input$col_activity_passifs)
      )
  })
  
  analysis_result_passifs <- eventReactive(input$run_analysis_passifs, {
    df <- df_passifs_renamed()
    BatActive(df, duration = input$duration_passifs, npoint = input$npoint_passifs)
  })
  
  output$analysis_output_passifs <- renderTable({
    analysis_result_passifs()
  })
  
  output$extra_passifs <- renderText({
    "Ici, tu peux ajouter une autre analyse ou graphiques spécifiques aux points passifs."
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)
