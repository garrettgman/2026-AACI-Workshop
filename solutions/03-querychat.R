library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)
library(DT)
library(querychat)

source("create_mortality_map.R")

# Load datasets
georgia_mortality <- read.csv("georgia_mortality.csv")
georgia_population <- read.csv("georgia_population.csv")

qc <- QueryChat$new(georgia_mortality, greeting = "How can I help you explore cancer mortality in Georgia?")


ui <- page_sidebar(
  title = "Cancer Mortality in Georgia (2021–2025)",
  sidebar = qc$sidebar(),
  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header("Geographic Distribution"),
      leafletOutput("map", height = 500)
    ),
    tagList(
      card(
        card_header("Summary Statistics"),
        uiOutput("summary")
      ),
      card(
        card_header("Detailed Data Table"),
        DTOutput("table")
      )
    )
  )
)

server <- function(input, output, session) {

  qc_vals <- qc$server()

  filtered <- reactive({
    qc_vals$df()
  })

  output$map <- renderLeaflet({
    create_mortality_map(filtered(), georgia_population)
  })

  output$summary <- renderUI({
    df <- filtered()
    total  <- sum(df$n)
    avg    <- round(total / 5)
    counties <- n_distinct(df$County)
    tags$ul(
      tags$li(tags$b("Total deaths (2021–2025): "), format(total, big.mark = ",")),
      tags$li(tags$b("Average per year: "),         format(avg,   big.mark = ",")),
      tags$li(tags$b("Counties with recorded deaths: "), counties)
    )
  })

  output$table <- renderDT({
    filtered() |>
      select(-FIPS, -State) |>
      datatable(filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)
