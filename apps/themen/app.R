library(shiny)
library(bslib)
library(DT)

excel_url <- "https://cloud.ph-karlsruhe.de/index.php/s/g7S755oDEnB8acQ/download"

col_stg <- "Geeignet für Studiengang"
col_kat <- "Themenkategorie"
col_bet <- "Betreuer:in"
col_emp <- "Empirisch/nicht-empirisch"
col_hide <- "Verfügbar?"
col_long <- c("Thema/Forschungsfrage", "Beschreibung des Vorhabens")

# Zeitpunkt der letzten Änderung der Excel-Datei (aus den xlsx-Metadaten)
get_last_modified <- function(xlsx_path) {
  td <- tempfile()
  on.exit(unlink(td, recursive = TRUE))
  unzip(xlsx_path, files = "docProps/core.xml", exdir = td)
  xml <- paste(readLines(file.path(td, "docProps/core.xml"), warn = FALSE), collapse = "")
  ts <- sub(".*<dcterms:modified[^>]*>([^<]+)</dcterms:modified>.*", "\\1", xml)
  t <- as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (is.na(t)) return("unbekannt")
  format(t, "%d.%m.%Y %H:%M", tz = "Europe/Berlin")
}

# Live-Abruf der Excel-Datei aus der Nextcloud
load_live <- function() {
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f))
  download.file(excel_url, f, mode = "wb", quiet = TRUE)

  df <- as.data.frame(readxl::read_excel(f, sheet = 1), check.names = FALSE)
  # Zeilenumbrüche in Spaltennamen entfernen
  names(df) <- gsub("\\s*[\r\n]+\\s*", " ", sub("/\\s*[\r\n]+\\s*", "/", names(df)))
  df[] <- lapply(df, function(x) {
    if (is.character(x)) trimws(x, whitespace = "[\\h\\v]") else x
  })

  list(data = df, stand = get_last_modified(f))
}

cache <- new.env()

get_data <- function(max_age = 300) {
  now <- Sys.time()
  expired <- is.null(cache$data) ||
    as.numeric(difftime(now, cache$time, units = "secs")) > max_age

  if (expired) {
    new_data <- tryCatch(load_live(), error = function(e) NULL)
    if (!is.null(new_data)) {
      cache$data <- new_data
      cache$time <- now
    }
  }
  cache$data
}

ui <- page_sidebar(
  fillable = FALSE,
  sidebar = sidebar(
    width = 300,
    selectizeInput("studiengang", col_stg, choices = NULL, multiple = TRUE),
    selectizeInput("themenkategorie", col_kat, choices = NULL, multiple = TRUE),
    selectizeInput("betreuung", col_bet, choices = NULL, multiple = TRUE),
    selectizeInput("empirisch", col_emp, choices = NULL, multiple = TRUE)
  ),
  tags$style("
    table.dataTable td { vertical-align: top; }
    td.expandable { cursor: pointer; min-width: 300px; }
    .clamp {
      display: -webkit-box;
      -webkit-line-clamp: 3;
      -webkit-box-orient: vertical;
      overflow: hidden;
    }
    .clamp.open { display: block; overflow: visible; }
  "),
  uiOutput("stand"),
  uiOutput("content")
)

server <- function(input, output, session) {
  res <- reactive(get_data())

  output$stand <- renderUI({
    if (is.null(res())) return(NULL)
    p(paste0("Stand: ", res()$stand))
  })

  output$content <- renderUI({
    if (is.null(res()) || nrow(res()$data) == 0) {
      return(p("Daten aktuell nicht abrufbar."))
    }
    DTOutput("table")
  })

  observe({
    req(res())
    df <- res()$data
    updateSelectizeInput(session, "studiengang",
                         choices = sort(unique(df[[col_stg]])), server = FALSE)
    updateSelectizeInput(session, "themenkategorie",
                         choices = sort(unique(df[[col_kat]])), server = FALSE)
    updateSelectizeInput(session, "betreuung",
                         choices = sort(unique(df[[col_bet]])), server = FALSE)
    updateSelectizeInput(session, "empirisch",
                         choices = sort(unique(df[[col_emp]])), server = FALSE)
  })

  filtered_data <- reactive({
    req(res())
    df <- res()$data
    if (length(input$studiengang))
      df <- df[df[[col_stg]] %in% input$studiengang, , drop = FALSE]
    if (length(input$themenkategorie))
      df <- df[df[[col_kat]] %in% input$themenkategorie, , drop = FALSE]
    if (length(input$betreuung))
      df <- df[df[[col_bet]] %in% input$betreuung, , drop = FALSE]
    if (length(input$empirisch))
      df <- df[df[[col_emp]] %in% input$empirisch, , drop = FALSE]
    # Spalte nur ausblenden; gefiltert wird davor
    df[, setdiff(names(df), col_hide), drop = FALSE]
  })

  output$table <- renderDT({
    df <- filtered_data()

    # Texte für die HTML-Darstellung maskieren (escape = FALSE unten)
    df[] <- lapply(df, function(x) {
      if (is.character(x)) ifelse(is.na(x), NA_character_, htmltools::htmlEscape(x)) else x
    })
    
    # 0-basierte Indizes der aufklappbaren Spalten (rownames = FALSE)
    long_cols <- which(names(df) %in% col_long) - 1

    column_defs <- list(list(className = "dt-left", targets = "_all"))
    if (length(long_cols) > 0) {
      column_defs <- c(column_defs, list(list(
        targets = as.list(long_cols),
        className = "expandable",
        # Nur die Anzeige wird verpackt; Suche und Filter nutzen den Rohtext
        render = JS("function(data, type) {
          return type === 'display' && data !== null
            ? '<div class=\"clamp\">' + data + '</div>' : data;
        }")
      )))
    }

    datatable(
      df,
      escape = FALSE,
      filter = "top",
      rownames = FALSE,
      callback = JS("table.on('click', 'td.expandable', function() {
        $(this).find('.clamp').toggleClass('open');
      });"),
      options = list(pageLength = 10, columnDefs = column_defs)
    )
  })
}

shinyApp(ui, server)
