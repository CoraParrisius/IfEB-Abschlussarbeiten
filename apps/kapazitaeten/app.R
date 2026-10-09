library(shiny)
library(bslib)

excel_url <- "https://cloud.ph-karlsruhe.de/index.php/s/g7S755oDEnB8acQ/download"

# Zeilensumme über alle Personenspalten (alle Spalten außer der ersten)
row_totals <- function(df) {
  data.frame(
    studiengang = df[[1]],
    total = rowSums(sapply(df[-1], as.numeric), na.rm = TRUE)
  )
}

add_status <- function(df) {
  df$belegt[is.na(df$belegt)] <- 0
  df$max[is.na(df$max)] <- 0
  df$frei <- df$max - df$belegt
  df$status <- ifelse(df$max == 0, "gray",
                ifelse(df$frei > 2, "green",
                ifelse(df$frei >= 1, "orange", "red")))
  df
}

# Live-Abruf der Excel-Datei aus der Nextcloud
load_live <- function() {
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f))
  download.file(excel_url, f, mode = "wb", quiet = TRUE)

  belegt <- row_totals(readxl::read_excel(f, sheet = 2, .name_repair = "minimal"))
  max <- row_totals(readxl::read_excel(f, sheet = 3, .name_repair = "minimal"))
  names(belegt)[2] <- "belegt"
  names(max)[2] <- "max"

  list(
    data = add_status(merge(belegt, max, by = "studiengang", all = TRUE, sort = FALSE)),
    stand = get_last_modified(f)
  )
}

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

ui <- page_fluid(
  uiOutput("header"),
  uiOutput("capacity_boxes")
)

server <- function(input, output, session) {
  res <- reactive({
    tryCatch(load_live(), error = function(e) NULL)
  })

  output$header <- renderUI({
    if (is.null(res())) return(NULL)
    p("Aktuelle Kapazitäten im Jahr ", strong(format(Sys.Date(), "%Y")),
      paste0(" (Stand: ", res()$stand, ")"))
  })

  output$capacity_boxes <- renderUI({
    if (is.null(res())) return(p("Daten aktuell nicht abrufbar."))
    kap <- res()$data
    if (nrow(kap) == 0) return(p("Daten aktuell nicht abrufbar."))

    cards <- lapply(seq_len(nrow(kap)), function(i) {
      row <- kap[i, ]
      color <- switch(row$status,
        green = "lightgreen", orange = "orange", red = "tomato", "lightgray")

      card(
        card_header(row$studiengang),
        div(
          style = paste0("background-color:", color,
                         "; padding:15px; border-radius:10px; text-align:center;"),
          h3(if (row$status == "gray") "Keine Kapazität"
             else paste0(row$frei, if (row$frei == 1) " Platz frei" else " Plätze frei")),
          p(paste("Belegt:", row$belegt, "| Max:", row$max))
        )
      )
    })
    layout_columns(col_widths = c(6, 6), !!!cards)
  })
}

shinyApp(ui, server)
