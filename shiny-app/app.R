# =======================================================================================
# Titre: Application d'analyse des données d'activité Chiroptérologique
# Description:  Application intéractive pour l'importation et le calcul des
#               indicateurs d'activité chiroptérologique.
#
# Auteur: Alexandre LANGLAIS
# Date: 2025/06/14
# Version: 1.0
# GitHub : https://github.com/a-langlais/bat_activity
# Dépendances: shiny, readr, dplyr
#
# Instructions: Ce script permet de lancer une application shiny a trois onglets :
#                 - "Standardiser" : pour convertir les exports bruts au format standard
#                 - "Points actifs" : pour l'analyse des points manuels
#                 - "Points passifs" : pour l'analyse des points d'enregistrements continus
#
#               L'application permet d'importer son fichier, sélectionner les colonnes 
#               pertinentes et calculer automatiquement les indicateurs. Les résultats 
#               sont présentés sous forme de tableaux exportables au format .csv.
# =======================================================================================

# ======================================================================
# CHARGEMENT DES RESSOURCES
# ======================================================================

# Import des packages
library(shiny)    # 1.10.0
library(readr)    # 2.1.5
library(dplyr)    # 1.1.4

# Chargement des fonctions
source(file.path("R", "BatActive.R"))
source(file.path("R", "BatPassive.R"))
source(file.path("R", "Standardize.R"))

# ======================================================================
# INTERFACE UI
# ======================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: #f7f8f7;
        color: #1f2933;
      }
      .container-fluid {
        max-width: 1280px;
      }
      .app-header {
        border-bottom: 1px solid #dfe5e2;
        margin-bottom: 20px;
        padding: 18px 0 14px;
      }
      .app-title {
        font-size: 24px;
        font-weight: 600;
        letter-spacing: 0;
        margin: 0;
      }
      .app-subtitle {
        color: #66736f;
        font-size: 14px;
        margin: 5px 0 0;
      }
      .nav-tabs {
        border-bottom-color: #dfe5e2;
        margin-bottom: 18px;
      }
      .nav-tabs > li > a {
        color: #4b5a56;
        border-radius: 4px 4px 0 0;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: #1f2933;
        font-weight: 600;
      }
      .well {
        background: #ffffff;
        border: 1px solid #dfe5e2;
        border-radius: 6px;
        box-shadow: none;
      }
      label {
        color: #33413d;
        font-weight: 500;
      }
      .btn {
        border-radius: 4px;
      }
      .btn-primary {
        background: #315b4f;
        border-color: #315b4f;
      }
      .btn-primary:hover,
      .btn-primary:focus {
        background: #274a40;
        border-color: #274a40;
      }
      .btn-default {
        background: #ffffff;
        border-color: #cfd8d4;
        color: #2f3f3a;
      }
      .section-title {
        border-bottom: 1px solid #e5ebe8;
        color: #263630;
        font-size: 15px;
        font-weight: 600;
        margin: 22px 0 12px;
        padding-bottom: 8px;
      }
      .status-text {
        color: #66736f;
        font-size: 13px;
        margin-bottom: 14px;
      }
      .app-footer {
        border-top: 1px solid #dfe5e2;
        color: #8a9692;
        font-size: 12px;
        margin-top: 28px;
        padding: 14px 0 18px;
        text-align: right;
      }
    "))
  ),
  tags$header(
    class = "app-header",
    tags$h1(class = "app-title", "BatActivity"),
    tags$p(class = "app-subtitle", "Analyse interactive des données d'activité chiroptérologique")
  ),
  
  tabsetPanel(
    tabPanel("Standardiser",
             sidebarLayout(
               sidebarPanel(
                 fileInput("csv_file_standard", "Charger un fichier CSV", accept = ".csv"),
                 selectInput("software_standard", "Format source", choices = c("SonoChiro", "Tadarida")),
                 actionButton("run_standard", "Standardiser", class = "btn-primary"),
                 tags$div(style = "margin-top: 15px;", uiOutput("download_ui_standard")),
                 width = 3
               ),
               mainPanel(
                 tags$div(class = "status-text", textOutput("file_status_standard")),
                 tags$h4(class = "section-title", "Échantillon du fichier source"),
                 tableOutput("preview_data_standard"),
                 tags$h4(class = "section-title", "Table standardisée"),
                 tableOutput("standard_output"),
                 width = 9
               )
             )
    ),

    tabPanel("Points actifs",
             sidebarLayout(
               sidebarPanel(
                 fileInput("csv_file_actifs", "Charger un fichier CSV (Points actifs)", accept = ".csv"),
                 uiOutput("col_select_ui_actifs"),
                 numericInput("duration_actifs", "Durée d'écoute (en minutes)", value = 10, min = 1),
                 numericInput("npoint_actifs", "Nombre de points d'observation", value = 5, min = 1),
                 actionButton("run_analysis_actifs", "Lancer l’analyse", class = "btn-primary"),
                 tags$div(style = "margin-top: 15px;", uiOutput("download_ui_actifs")),
                 width = 3
               ),
               mainPanel(
                 tags$div(class = "status-text", textOutput("file_status_actifs")),
                 tags$h4(class = "section-title", "Échantillon du jeu de données"),
                 tableOutput("preview_data_actifs"),
                 tags$h4(class = "section-title", "Tableau des indicateurs"),
                 tableOutput("analysis_output_actifs"),
                 width = 9
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
                 radioButtons("time_option", "Choisir le mode de saisie de l'heure:",
                              choices = c("Heures fixes" = "fixed", "Relatif aux éphémerides" = "sunset")),
                 
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
                 
                 actionButton("run_analysis_passifs", "Lancer l’analyse", class = "btn-primary"),
                 tags$div(style = "margin-top: 15px;", uiOutput("download_ui_passifs")),
                 width = 3
               ),
               mainPanel(
                 tags$div(class = "status-text", textOutput("file_status_passifs")),
                 tags$h4(class = "section-title", "Échantillon du jeu de données"),
                 tableOutput("preview_data_passifs"),
                 tags$h4(class = "section-title", "Tableau des indicateurs"),
                 tableOutput("analysis_output_passifs"),
                 width = 9
               )
             )
    )
  ),
  tags$footer(class = "app-footer", "© Alexandre LANGLAIS")
)

# ======================================================================
# BACKEND SERVER
# ======================================================================

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

  #######
  ####### Standardiser
  #######

  raw_data_standard <- reactive({
    read_data(input$csv_file_standard)
  })

  output$file_status_standard <- renderText({
    if (is.null(input$csv_file_standard)) return("Aucun fichier chargé.")
    if (is.null(raw_data_standard())) return("Erreur lors de la lecture du fichier.")
    paste0("Fichier chargé avec ", nrow(raw_data_standard()), " lignes et ", ncol(raw_data_standard()), " colonnes.")
  })

  output$preview_data_standard <- renderTable({
    head(raw_data_standard())
  })

  standardized_data <- eventReactive(input$run_standard, {
    req(raw_data_standard())
    tryCatch(
      standardize_table_app(
        data = as.data.frame(raw_data_standard()),
        software = input$software_standard
      ),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
  })

  output$standard_output <- renderTable({
    req(standardized_data())
    head(standardized_data(), 10)
  })

  output$download_ui_standard <- renderUI({
    req(standardized_data())
    downloadButton("download_standard_table", "Télécharger la table (.csv)", class = "btn-default")
  })

  output$download_standard_table <- downloadHandler(
    filename = function() {
      paste0("table_standard_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(standardized_data())
      write.table(
        standardized_data(),
        file = file,
        row.names = FALSE,
        col.names = TRUE,
        sep = ";",
        dec = ","
      )
    }
  )
  
  #######
  ####### Points actifs
  #######
  
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
    downloadButton("download_indicateurs_actifs", "Télécharger les résultats (.csv)", class = "btn-default")
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
  
  #######
  ####### Points passifs
  #######
  
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
      selectInput("col_night_date_passifs", "Colonne : Date de la nuit (YYYY-MM-DD)", choices = cols),
      radioButtons("time_choice", "Choix :", 
                   choices = list("Date et Heure" = "datetime", 
                                  "Date et Heure séparées" = "separate"),
                   selected = "datetime"),
      conditionalPanel(
        condition = "input.time_choice == 'datetime'",
        selectInput("col_time_passifs", "Colonne : Date et Heure (YYYY-MM-DD HH:MM)", choices = cols)
      ),
      conditionalPanel(
        condition = "input.time_choice == 'separate'",
        selectInput("col_date_passifs", "Colonne : Date (YYYY-MM-DD)", choices = cols),
        selectInput("col_time_passifs", "Colonne : Heure (HH:MM)", choices = cols)
      )
    )
  })
    
  
  output$preview_data_passifs <- renderTable({
    head(raw_data_passifs())
  })
  
  df_passifs_renamed <- eventReactive(input$run_analysis_passifs, {
    req(raw_data_passifs())
    df <- raw_data_passifs()
    as.POSIXct(df[[input$col_night_date_passifs]], format = "%Y-%m-%d")
    
    # Créer Date_Time selon le choix de l'utilisateur
    df$Date_Time <- if (input$time_choice == "separate") {
      req(input$col_date_passifs, input$col_time_passifs)
      
      # Combiner les colonnes Date + Time
      datetime_str <- paste(df[[input$col_date_passifs]], df[[input$col_time_passifs]])
      tryCatch({
        as.POSIXct(datetime_str, format = "%Y-%m-%d %H:%M")
      }, error = function(e) {
        showNotification("Erreur lors de la conversion Date + Heure", type = "error")
        rep(NA, nrow(df))
      })
    } else {
      req(input$col_time_passifs)
      as.POSIXct(df[[input$col_time_passifs]], format = "%Y-%m-%d %H:%M")
    }
    
    # Renommer les autres colonnes
    df <- df %>%
      rename(
        Place = all_of(input$col_place_passifs),
        Id = all_of(input$col_id_passifs),
        Night_Date = all_of(input$col_night_date_passifs)
        # Date_Time déjà gérée manuellement plus haut
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
    downloadButton("download_indicateurs_passifs", "Télécharger les résultats (.csv)", class = "btn-default")
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
  
}

# ======================================================================
# APPLICATION
# ======================================================================

shinyApp(ui, server)
