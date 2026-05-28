# =======================================================================================
# Titre: Application d'analyse des données d'activité chiroptérologique
# Description: Application interactive pour l'importation, la standardisation, le calcul
#              et la visualisation des indicateurs d'activité chiroptérologique.
#
# Auteur: Alexandre LANGLAIS
# Date: 2026/05/28
# Version: 1.2
# GitHub : https://github.com/a-langlais/bat_activity
# =======================================================================================

# ======================================================================
# CHARGEMENT DES RESSOURCES
# ======================================================================

library(shiny)    # 1.10.0
library(bslib)    # 0.9.0
library(DT)       # 0.33
library(readr)    # 2.1.5
library(dplyr)    # 1.1.4
library(tidyr)    # 1.3.1
library(here)     # 1.0.1
library(plotly)   # 4.10.4

setwd(here())
source("src/StandardTable.R")
source("src/app/BatActive.R")
source("src/app/BatPlots.R")
source("src/app/BatPassive.R")

options(shiny.maxRequestSize = 50 * 1024^2)

# ======================================================================
# OUTILS UI
# ======================================================================

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#315b4f",
  secondary = "#6b5a47",
  success = "#527d5a",
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Source Sans 3")
)

datatable_options <- list(
  pageLength = 8,
  lengthMenu = c(5, 8, 15, 25, 50),
  scrollX = TRUE,
  language = list(
    search = "Rechercher :",
    lengthMenu = "Afficher _MENU_ lignes",
    info = "_START_ à _END_ sur _TOTAL_ lignes",
    paginate = list(previous = "Précédent", `next` = "Suivant"),
    zeroRecords = "Aucune ligne à afficher"
  )
)

empty_state <- function(title, text) {
  div(
    class = "empty-state",
    h3(title),
    p(text)
  )
}

results_card <- function(title, output) {
  div(
    class = "result-section",
    h3(title),
    output
  )
}

# ======================================================================
# INTERFACE UI
# ======================================================================

ui <- fluidPage(
  theme = app_theme,
  tags$head(
    tags$style(HTML("
      body {
        background: #f6f7f4;
        color: #1f2a26;
      }
      .app-header {
        display: flex;
        align-items: center;
        gap: 14px;
        padding: 24px 8px 12px;
      }
      .app-mark {
        width: 46px;
        height: 46px;
        border-radius: 8px;
        display: grid;
        place-items: center;
        background: #315b4f;
        color: white;
        font-size: 26px;
      }
      .app-title h1 {
        margin: 0;
        font-size: clamp(1.7rem, 3vw, 2.4rem);
        letter-spacing: 0;
      }
      .app-title p {
        margin: 3px 0 0;
        color: #5d6964;
      }
      .nav-tabs {
        border-bottom-color: #d8ddd4;
      }
      .nav-tabs .nav-link {
        color: #315b4f;
        font-weight: 600;
      }
      .nav-tabs .nav-link.active {
        color: #1f2a26;
        border-top: 3px solid #315b4f;
      }
      .well {
        background: #ffffff;
        border: 1px solid #d8ddd4;
        border-radius: 8px;
        box-shadow: none;
      }
      .form-label, label {
        font-weight: 700;
        color: #1f2a26;
      }
      .btn {
        border-radius: 6px;
        font-weight: 700;
      }
      .btn-primary, .btn-default {
        background: #315b4f;
        border-color: #315b4f;
        color: white;
      }
      .btn-primary:hover, .btn-default:hover {
        background: #26483f;
        border-color: #26483f;
        color: white;
      }
      .status-box {
        padding: 12px 14px;
        margin-bottom: 18px;
        border: 1px solid #d8ddd4;
        border-radius: 8px;
        background: #ffffff;
        color: #42504a;
        font-family: inherit;
        white-space: normal;
      }
      .result-section {
        margin-bottom: 28px;
      }
      .result-section h3 {
        margin: 0 0 12px;
        font-size: 1.35rem;
      }
      details.result-section {
        border: 1px solid #d8ddd4;
        border-radius: 8px;
        background: #ffffff;
        padding: 0;
      }
      details.result-section summary {
        cursor: pointer;
        padding: 12px 14px;
        font-size: 1.1rem;
        font-weight: 700;
        color: #315b4f;
      }
      details.result-section .datatables {
        padding: 0 14px 14px;
      }
      .empty-state {
        padding: 34px;
        border: 1px dashed #b9c2ba;
        border-radius: 8px;
        background: #ffffff;
        color: #52605a;
      }
      .empty-state h3 {
        margin-top: 0;
        color: #315b4f;
      }
    "))
  ),

  div(
    class = "app-header",
    div(class = "app-mark", "🦇"),
    div(
      class = "app-title",
      h1("Analyse d'activité des chauves-souris"),
      p("Standardiser, calculer et visualiser les indicateurs chiroptérologiques."),
      p("© Alexandre LANGLAIS (2026)")
    )
  ),

  tabsetPanel(
    id = "main_tabs",
    type = "tabs",

    tabPanel(
      "Standardiser",
      sidebarLayout(
        sidebarPanel(
          fileInput("csv_file_standard", "Charger un fichier CSV à standardiser", accept = ".csv"),
          selectInput("software_standard", "Format source", choices = c("Tadarida", "SonoChiro")),
          actionButton("run_standard", "Standardiser le tableau", class = "btn-primary"),
          tags$div(style = "margin-top: 15px;", uiOutput("download_ui_standard"))
        ),
        mainPanel(
          div(class = "status-box", textOutput("file_status_standard")),
          uiOutput("standard_sections")
        )
      )
    ),

    tabPanel(
      "Points actifs",
      sidebarLayout(
        sidebarPanel(
          fileInput("csv_file_actifs", "Charger un fichier CSV (points actifs)", accept = ".csv"),
          uiOutput("col_select_ui_actifs"),
          numericInput("duration_actifs", "Durée d'écoute (en minutes)", value = 10, min = 1),
          numericInput("npoint_actifs", "Nombre de points d'observation", value = 5, min = 1),
          actionButton("run_analysis_actifs", "Analyser les points actifs", class = "btn-primary"),
          tags$div(style = "margin-top: 15px;", uiOutput("download_ui_actifs"))
        ),
        mainPanel(
          div(class = "status-box", textOutput("file_status_actifs")),
          uiOutput("active_sections")
        )
      )
    ),

    tabPanel(
      "Points passifs",
      sidebarLayout(
        sidebarPanel(
          fileInput("csv_file_passifs", "Charger un fichier CSV (points passifs)", accept = ".csv"),
          uiOutput("col_select_ui_passifs"),
          selectInput(
            "place_passifs",
            "Lieu le plus proche",
            choices = c("Paris", "Lyon", "Marseille", "Toulouse", "Bordeaux", "Brest", "Strasbourg")
          ),
          radioButtons(
            "time_option",
            "Mode de saisie de l'heure",
            choices = c("Heures fixes" = "fixed", "Relatif aux éphémérides" = "sunset")
          ),
          conditionalPanel(
            condition = "input.time_option == 'fixed'",
            textInput("start_time", "Heure de début (HH:MM)", value = "22:00"),
            textInput("end_time", "Heure de fin (HH:MM)", value = "07:00")
          ),
          conditionalPanel(
            condition = "input.time_option == 'sunset'",
            numericInput("minutes_before_sunset", "Minutes avant le coucher du soleil", value = 30, min = 0),
            numericInput("minutes_after_sunrise", "Minutes après le lever du soleil", value = 30, min = 0)
          ),
          actionButton("run_analysis_passifs", "Analyser les points passifs", class = "btn-primary"),
          tags$div(style = "margin-top: 15px;", uiOutput("download_ui_passifs"))
        ),
        mainPanel(
          div(class = "status-box", textOutput("file_status_passifs")),
          uiOutput("passive_sections")
        )
      )
    )
  )
)

# ======================================================================
# BACKEND SERVER
# ======================================================================

server <- function(input, output, session) {
  read_data <- function(file_input) {
    req(file_input)
    tryCatch({
      data_raw <- read_delim(file_input$datapath, delim = ";", col_types = cols(.default = "c"), show_col_types = FALSE)

      data_raw %>%
        mutate(across(where(is.character), ~iconv(., from = "UTF-8", to = "UTF-8", sub = "")))
    }, error = function(e) {
      NULL
    })
  }

  selected_or_default <- function(cols, preferred) {
    matched <- preferred[preferred %in% cols]
    if (length(matched) > 0) {
      matched[[1]]
    } else {
      cols[[1]]
    }
  }

  parse_date_column <- function(x) {
    x <- trimws(as.character(x))
    x <- sub("\\s+(UTC|GMT|CEST|CET)$", "", x, ignore.case = TRUE)
    parsed <- rep(as.Date(NA), length(x))
    numeric_value <- suppressWarnings(as.numeric(gsub(",", ".", x)))
    numeric_dates <- is.na(parsed) & !is.na(numeric_value) & numeric_value > 20000
    parsed[numeric_dates] <- as.Date(floor(numeric_value[numeric_dates]), origin = "1899-12-30")

    date_formats <- c("%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d")

    for (fmt in date_formats) {
      missing <- is.na(parsed) & !is.na(x) & x != ""
      if (!any(missing)) break
      parsed[missing] <- as.Date(x[missing], format = fmt)
    }

    missing <- is.na(parsed) & !is.na(x) & x != ""

    if (any(missing)) {
      parsed_datetime <- parse_datetime_column(x[missing])
      parsed[missing] <- as.Date(parsed_datetime)
    }

    parsed
  }

  parse_datetime_column <- function(x) {
    x <- trimws(as.character(x))
    x <- sub("\\s+(UTC|GMT|CEST|CET)$", "", x, ignore.case = TRUE)
    parsed <- rep(as.POSIXct(NA, tz = "UTC"), length(x))
    numeric_value <- suppressWarnings(as.numeric(gsub(",", ".", x)))
    numeric_datetimes <- is.na(parsed) & !is.na(numeric_value) & numeric_value > 20000
    parsed[numeric_datetimes] <- as.POSIXct(
      (numeric_value[numeric_datetimes] - 25569) * 86400,
      origin = "1970-01-01",
      tz = "UTC"
    )

    datetime_formats <- c(
      "%Y-%m-%d %H:%M:%OS",
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d %H:%M",
      "%Y-%m-%dT%H:%M:%OS",
      "%Y-%m-%dT%H:%M:%S",
      "%Y-%m-%dT%H:%M:%OSZ",
      "%Y-%m-%dT%H:%M:%SZ",
      "%d/%m/%Y %H:%M:%OS",
      "%d/%m/%Y %H:%M:%S",
      "%d/%m/%Y %H:%M"
    )

    for (fmt in datetime_formats) {
      missing <- is.na(parsed) & !is.na(x) & x != ""
      if (!any(missing)) break
      parsed[missing] <- as.POSIXct(strptime(x[missing], format = fmt, tz = "UTC"))
    }

    parsed
  }

  is_time_only_column <- function(x) {
    x <- trimws(as.character(x))
    x <- x[!is.na(x) & x != ""]
    length(x) > 0 && all(grepl("^[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?$", x))
  }

  combine_date_time_columns <- function(date_values, time_values) {
    date_parsed <- parse_date_column(date_values)
    time_values <- trimws(as.character(time_values))
    parse_datetime_column(paste(format(date_parsed, "%Y-%m-%d"), time_values))
  }

  format_parse_examples <- function(values, parsed, label) {
    raw <- trimws(as.character(values))
    failed <- is.na(parsed) & !is.na(raw) & raw != ""
    if (!any(failed)) {
      return(NULL)
    }

    examples <- unique(raw[failed])
    examples <- examples[seq_len(min(3, length(examples)))]
    paste0(label, " non convertie(s) : ", paste(shQuote(examples), collapse = ", "))
  }

  ####### Standardisation #######

  raw_data_standard <- reactive({
    req(input$csv_file_standard)
    read_data(input$csv_file_standard)
  })

  output$file_status_standard <- renderText({
    if (is.null(input$csv_file_standard)) {
      return("Chargez un export SonoChiro ou Tadarida pour créer un tableau standard BatActivity.")
    }
    if (is.null(raw_data_standard())) {
      return("Erreur lors de la lecture du fichier.")
    }
    paste0("Fichier chargé avec ", nrow(raw_data_standard()), " lignes et ", ncol(raw_data_standard()), " colonnes.")
  })

  standard_result <- eventReactive(input$run_standard, {
    req(raw_data_standard())
    tryCatch({
      StandardTable(raw_data_standard(), sftw = input$software_standard, write_file = FALSE)
    }, error = function(e) {
      showNotification(e$message, type = "error")
      NULL
    })
  })

  output$standard_sections <- renderUI({
    if (is.null(input$csv_file_standard)) {
      return(empty_state("Standardisation des tableaux", "Importez un fichier CSV, choisissez le logiciel source, puis lancez la standardisation."))
    }

    tagList(
      results_card("Aperçu du fichier source", DTOutput("preview_data_standard")),
      if (!is.null(standard_result())) {
        results_card("Tableau standardisé", DTOutput("standard_output"))
      }
    )
  })

  output$preview_data_standard <- renderDT({
    req(raw_data_standard())
    datatable(head(raw_data_standard(), 25), options = datatable_options, rownames = FALSE)
  })

  output$standard_output <- renderDT({
    req(standard_result())
    datatable(standard_result(), options = datatable_options, rownames = FALSE)
  })

  output$download_ui_standard <- renderUI({
    req(standard_result())
    downloadButton("download_standard", "Télécharger le tableau standard (.csv)", class = "btn-success")
  })

  output$download_standard <- downloadHandler(
    filename = function() {
      paste0("bat_activity_standard_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(standard_result())
      write.table(standard_result(), file, row.names = FALSE, col.names = TRUE, sep = ";", dec = ",")
    }
  )

  ####### Points actifs #######

  raw_data_actifs <- reactive({
    req(input$csv_file_actifs)
    read_data(input$csv_file_actifs)
  })

  output$file_status_actifs <- renderText({
    if (is.null(input$csv_file_actifs)) {
      return("Chargez un fichier CSV de points actifs pour commencer.")
    }
    if (is.null(raw_data_actifs())) {
      return("Erreur lors de la lecture du fichier.")
    }
    paste0("Fichier chargé avec ", nrow(raw_data_actifs()), " lignes et ", ncol(raw_data_actifs()), " colonnes.")
  })

  output$col_select_ui_actifs <- renderUI({
    req(raw_data_actifs())
    cols <- names(raw_data_actifs())

    tagList(
      selectInput("col_place_actifs", "Colonne : point", choices = cols, selected = selected_or_default(cols, c("Place", "Site", "Point"))),
      selectInput("col_id_actifs", "Colonne : espèce", choices = cols, selected = selected_or_default(cols, c("Id", "Species", "Taxon"))),
      selectInput("col_activity_actifs", "Colonne : activité observée", choices = cols, selected = selected_or_default(cols, c("Activity", "Activité", "Activite", "Contact")))
    )
  })

  df_actifs_renamed <- eventReactive(input$run_analysis_actifs, {
    req(raw_data_actifs())
    raw_data_actifs() %>%
      rename_with(~ ifelse(. != input$col_place_actifs, "Place", .), all_of(input$col_place_actifs)) %>%
      rename_with(~ ifelse(. != input$col_id_actifs, "Id", .), all_of(input$col_id_actifs)) %>%
      rename_with(~ ifelse(. != input$col_activity_actifs, "Activity", .), all_of(input$col_activity_actifs))
  })

  analysis_result_actifs <- eventReactive(input$run_analysis_actifs, {
    df <- df_actifs_renamed()
    tryCatch({
      BatActive(df, duration = input$duration_actifs, npoint = input$npoint_actifs)
    }, error = function(e) {
      showNotification(e$message, type = "error")
      NULL
    })
  })

  output$active_sections <- renderUI({
    if (is.null(input$csv_file_actifs)) {
      return(empty_state("Analyse des points actifs", "Importez un fichier, associez les colonnes, puis lancez l'analyse."))
    }

    tagList(
      tags$details(
        class = "result-section",
        tags$summary("Échantillon du jeu de données"),
        DTOutput("preview_data_actifs")
      ),
      if (!is.null(analysis_result_actifs())) {
        tagList(
          tags$details(
            class = "result-section",
            open = NA,
            tags$summary("Tableau des indicateurs"),
            DTOutput("analysis_output_actifs")
          ),
          tags$details(
            class = "result-section",
            open = NA,
            tags$summary("Visualisation des résultats"),
            fluidRow(
              column(6, plotlyOutput("behavior_pie_actifs")),
              column(6, plotlyOutput("species_bar_actifs"))
            )
          )
        )
      }
    )
  })

  output$preview_data_actifs <- renderDT({
    req(raw_data_actifs())
    datatable(head(raw_data_actifs(), 25), options = datatable_options, rownames = FALSE)
  })

  output$analysis_output_actifs <- renderDT({
    req(analysis_result_actifs())
    datatable(analysis_result_actifs(), options = datatable_options, rownames = FALSE)
  })

  output$download_ui_actifs <- renderUI({
    req(analysis_result_actifs())
    downloadButton("download_indicateurs_actifs", "Télécharger les résultats (.csv)", class = "btn-success")
  })

  output$download_indicateurs_actifs <- downloadHandler(
    filename = function() {
      paste0("indicateurs_actifs_", Sys.Date(), ".csv")
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
    req(input$csv_file_passifs)
    read_data(input$csv_file_passifs)
  })

  output$file_status_passifs <- renderText({
    if (is.null(input$csv_file_passifs)) {
      return("Chargez un fichier CSV de points passifs pour commencer.")
    }
    if (is.null(raw_data_passifs())) {
      return("Erreur lors de la lecture du fichier.")
    }
    paste0("Fichier chargé avec ", nrow(raw_data_passifs()), " lignes et ", ncol(raw_data_passifs()), " colonnes.")
  })

  output$col_select_ui_passifs <- renderUI({
    req(raw_data_passifs())
    cols <- names(raw_data_passifs())

    tagList(
      selectInput("col_place_passifs", "Colonne : point", choices = cols, selected = selected_or_default(cols, c("Place", "Site", "Point"))),
      selectInput("col_id_passifs", "Colonne : espèce", choices = cols, selected = selected_or_default(cols, c("Id", "Species", "Taxon"))),
      selectInput("col_night_date_passifs", "Colonne : date de la nuit", choices = cols, selected = selected_or_default(cols, c("Night_Date", "Night.Date"))),
      radioButtons(
        "time_choice",
        "Format temporel",
        choices = list("Date et heure dans une seule colonne" = "datetime", "Date et heure séparées" = "separate"),
        selected = "datetime"
      ),
      conditionalPanel(
        condition = "input.time_choice == 'datetime'",
        selectInput("col_time_passifs", "Colonne : date et heure", choices = cols, selected = selected_or_default(cols, c("Date_Time", "Date.Time")))
      ),
      conditionalPanel(
        condition = "input.time_choice == 'separate'",
        selectInput("col_date_passifs", "Colonne : date", choices = cols, selected = selected_or_default(cols, c("Date"))),
        selectInput("col_time_only_passifs", "Colonne : heure", choices = cols, selected = selected_or_default(cols, c("Time", "Heure")))
      )
    )
  })

  df_passifs_renamed <- eventReactive(input$run_analysis_passifs, {
    req(raw_data_passifs())
    df <- raw_data_passifs()

    df$Night_Date <- parse_date_column(df[[input$col_night_date_passifs]])

    df$Date_Time <- if (input$time_choice == "separate") {
      req(input$col_date_passifs, input$col_time_only_passifs)
      combine_date_time_columns(df[[input$col_date_passifs]], df[[input$col_time_only_passifs]])
    } else {
      req(input$col_time_passifs)
      parsed_datetime <- parse_datetime_column(df[[input$col_time_passifs]])

      if (any(is.na(parsed_datetime)) && is_time_only_column(df[[input$col_time_passifs]])) {
        date_col <- selected_or_default(names(df), c("Date", "Night_Date"))
        parsed_datetime <- combine_date_time_columns(df[[date_col]], df[[input$col_time_passifs]])
      }

      parsed_datetime
    }

    night_examples <- format_parse_examples(df[[input$col_night_date_passifs]], df$Night_Date, input$col_night_date_passifs)
    time_source <- if (input$time_choice == "separate") {
      paste(df[[input$col_date_passifs]], df[[input$col_time_only_passifs]])
    } else {
      df[[input$col_time_passifs]]
    }
    time_label <- if (input$time_choice == "separate") input$col_time_only_passifs else input$col_time_passifs
    time_examples <- format_parse_examples(time_source, df$Date_Time, time_label)

    if (!is.null(night_examples) || !is.null(time_examples)) {
      showNotification(
        paste(na.omit(c(night_examples, time_examples)), collapse = " | "),
        type = "error",
        duration = 12
      )
    }

    df %>%
      rename_with(~ ifelse(. != input$col_place_passifs, "Place", .), all_of(input$col_place_passifs)) %>%
      rename_with(~ ifelse(. != input$col_id_passifs, "Id", .), all_of(input$col_id_passifs)) %>%
      rename_with(~ ifelse(. != input$col_night_date_passifs, "Night_Date", .), all_of(input$col_night_date_passifs))
  })

  analysis_result_passifs <- eventReactive(input$run_analysis_passifs, {
    df <- df_passifs_renamed()

    if (input$time_option == "fixed") {
      record_time <- c(input$start_time, input$end_time)
      sun_offsets <- NULL
    } else {
      record_time <- NULL
      sun_offsets <- c(
        before_sunset = input$minutes_before_sunset,
        after_sunrise = input$minutes_after_sunrise
      )
    }

    tryCatch({
      BatPassive(
        data = df,
        city = input$place_passifs,
        record_time = record_time,
        sun_offsets = sun_offsets
      )
    }, error = function(e) {
      showNotification(e$message, type = "error")
      NULL
    })
  })

  output$passive_sections <- renderUI({
    if (is.null(input$csv_file_passifs)) {
      return(empty_state("Analyse des points passifs", "Importez un fichier, associez les colonnes temporelles, puis lancez l'analyse."))
    }

    tagList(
      tags$details(
        class = "result-section",
        tags$summary("Échantillon du jeu de données"),
        DTOutput("preview_data_passifs")
      ),
      if (!is.null(analysis_result_passifs())) {
        tagList(
          tags$details(
            class = "result-section",
            open = NA,
            tags$summary("Tableau des indicateurs"),
            DTOutput("analysis_output_passifs")
          ),
          tags$details(
            class = "result-section",
            open = NA,
            tags$summary("Visualisation des résultats"),
            plotlyOutput("passive_plot")
          )
        )
      }
    )
  })

  output$preview_data_passifs <- renderDT({
    req(raw_data_passifs())
    datatable(head(raw_data_passifs(), 25), options = datatable_options, rownames = FALSE)
  })

  output$analysis_output_passifs <- renderDT({
    req(analysis_result_passifs())
    datatable(analysis_result_passifs(), options = datatable_options, rownames = FALSE)
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
    req(df_passifs_renamed())
    plot_passive_activity(df_passifs_renamed(), input$col_id_passifs, input$place_passifs)
  })
}

# ======================================================================
# APPLICATION
# ======================================================================

shinyApp(ui, server)
