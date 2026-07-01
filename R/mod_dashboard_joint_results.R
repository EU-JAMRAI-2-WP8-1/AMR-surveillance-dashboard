
mod_dashboard_joint_results_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    class = "map-tab-container",

    column(
      width = 9,
      tags$div(
        class = "map-and-source-container",
        tags$div(
          class = "map-container",
          plotlyOutput(outputId = ns("scoresMap"), width = "100%", height = "780px")
        ),
        tags$div(
          class = "map-source-text medium-grey-text",
          "Map source: ",
          tags$a(
            href   = "https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries",
            "Eurostat",
            target = "_blank"
          )
        )
      )
    ),

    column(
      width = 3,
      tags$div(
        class = "map-info-container",
        HTML("<i>See the \"Info\" tab for information about scores</i>"),
        DT::dataTableOutput(ns("scoresTable"))
      )
    )
  )
}

mod_dashboard_joint_results_server <- function(id, country_scores, non_participating, participating, geojson_europe) {
  moduleServer(id, function(input, output, session) {

    output$scoresMap <- renderPlotly({

      themeBgColor <- "#ffffff"
      themeFgColor <- "#1D1F21"

      scoresMap <- plot_ly()

      scoresMap <- scoresMap %>% add_trace(
        type        = 'choropleth',
        geojson     = geojson_europe,
        locations   = participating()$Country,
        z           = participating()$Score,
        zmin        = 0,
        zmax        = 100,
        text        = participating()$Country,
        hoverinfo   = "text+z",
        showscale   = TRUE,
        colors      = c("#cc8888", "#d5b47f", "#dddd77", "#0fdbd5", "#008aab"),
        reversescale = FALSE,
        colorbar    = list(
          outlinewidth = 0,
          thickness    = 20,
          color        = themeBgColor,
          tickcolor    = themeFgColor,
          x            = 0.05,
          y            = 0.8,
          tickfont     = list(color = themeFgColor),
          title        = list(
            text = "Score",
            font = list(color = themeFgColor)
          )
        ),
        marker = list(line = list(width = 1.4, color = themeBgColor))
      )

      scoresMap <- scoresMap %>% add_trace(
        name        = "Not participating",
        type        = "choropleth",
        geojson     = geojson_europe,
        locations   = non_participating(),
        z           = rep(0.7, length(non_participating())),
        zmin        = 0,
        zmax        = 1,
        text        = non_participating(),
        hoverinfo   = "text",
        showscale   = FALSE,
        colorscale  = "Greys",
        marker      = list(line = list(width = 1.4, color = themeBgColor))
      )

      scoresMap %>% layout(
        geo = list(
          scope         = "europe",
          showcountries = FALSE,
          showframe     = FALSE,
          showland      = FALSE,
          bgcolor       = "rgba(0,0,0,0)",
          showcoastline = FALSE,
          projection    = list(scale = 1.7),
          center        = list(lat = 54, lon = 14)
        ),
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        margin        = list(t = 32, r = 0, l = 0, b = 32),
        dragmode      = FALSE,
        autosize      = TRUE
      )
    })

    output$scoresTable <- DT::renderDT(
      participating(),
      rownames = FALSE,
      colnames = c("Country", "Score (/100)", "% Answered"),
      options  = list(dom = 't', pageLength = 100)
    )

  })
}
