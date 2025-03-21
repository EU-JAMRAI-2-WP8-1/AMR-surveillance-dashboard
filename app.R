## SETUP ##

# import libraries
library(shiny)
library(shinyWidgets)
library(readxl)
library(rjson)
library(dplyr)
library(countrycode)
library(ggplot2)
library(echarts4r)
library(plotly)
library(geojsonR)
library(geojsonio)
library(leaflet)
library(gapminder)
library(bslib)
library(thematic)

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# add resource directory to server
shiny::addResourcePath('www', '/srv/shiny-server/www')

# set app files directory
appFilesDirectory = "/home/shiny-app/files"

## for layout --> see help at https://shiny.posit.co/r/articles/build/layout-guide/
## jamrai logo colors: #0fdbd5, #008aab, #26cad3

# create custom theme based on Bootstrap
custom_theme <- bs_theme(
    version = 5,
    #bg = "#ffffff",
    #fg = "#000000",
    primary = "#008aab",
    secondary = "#0fdbd5",
    base_font = "Helvetica Neue,Helvetica,Arial,sans-serif"
    #"navbar-bg" = "rgba(0, 0, 0, 0)"
)

# graph layout theme with Thematic
thematic_shiny(
  bg = "auto",
  fg = "auto",
  accent = "auto",
  font = NA,
  sequential = sequential_gradient(),
  qualitative = okabe_ito(),
  inherit = FALSE,
  session = shiny::getDefaultReactiveDomain()
)


## DATA LOAD ##

# import logos as variables
jamraiLogoHeaderLong      <- file.path("www/logos/TRANSPARENT_LONG2.png")
jamraiLogoHeaderRect      <- file.path("www/logos/TRANSPARENT_RECTANGULAR.png")
jamraiLogoHeaderRectWhite <- file.path("www/logos/TRANSPARENT_LONG1_WHITE-480x107.png")
euLogoFundWhite           <- file.path("www/logos/EN_Co-fundedbytheEU_RGB_WHITE-Outline-480x107.png")

# import Europe polygons
#geojsonEurope = FROM_GeoJson(file.path("/home/shiny-app/files/data/CNTR_RG_20M_2024_3035.geojson"))
geojsonEurope = geojson_read(file.path("/home/shiny-app/files/data/CNTR_RG_60M_2024_4326_europe_only.geojson"))
## source: https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries
#geojsonEurope = FROM_GeoJson(file.path("/home/shiny-app/files/data/europe.geojson"))

# import survey questions and replies from JSON
surveyDataFile <- file.path("/home/shiny-app/files/data/replies_formatted_20250314.json")
surveyData     <- fromJSON(paste(readLines(surveyDataFile), collapse=""))

# variables for data parsing -- might need to be updated for future versions of the survey
countryQuestionIndex <- 3

# prepare data
countryList <- c()
for (country in surveyData[[3]][["possible_answers"]]){
    countryList <- c(countryList, country$title)
}


## USER INTERFACE ##

# user interface
ui <- shinyUI(fluidPage(

    # dark mode hidden input (required)
    input_dark_mode(id = "mode") %>% tagAppendAttributes(class="hidden"),

    # set theme
    theme = custom_theme,

    # import CSS
    includeCSS(file.path("/srv/shiny-server/www/css/style.css")),

    # navigation bar
    page_navbar(
        nav_spacer(),

        # button - link to JAMRAI website
        nav_item(
            tags$a(
            tags$span(
                bsicons::bs_icon("cursor"),
                "JAMRAI"
            ),
            href = "https://eu-jamrai.eu/",
            target = "_blank"
            )
        ),

        # button - link to GitHub repo
        nav_item(
            tags$a(
            tags$span(
                bsicons::bs_icon("file-earmark-code"), "Source code"
            ),
            href = "https://github.com/",
            target = "_blank"
            )
        ),

        # button - triggers download of source data
        nav_item(
            tags$a(
            tags$span(
                bsicons::bs_icon("file-earmark-arrow-down"), "Download data"
            ),
            href = "https://github.com/",
            target = "_blank"
            )
        ),

        # light/dark mode toggle
        nav_item(
            input_dark_mode(id = "dark_mode", mode = NULL)
        ),

        position = c("fixed-top")
    ),

    # spacer - prevents overlapping of header and navbar
    div() %>% tagAppendAttributes(class="top-spacer"),

    # header
    fluidRow(
        img(src=jamraiLogoHeaderLong) %>% tagAppendAttributes(class="jamrai-logo-header-long width-auto"),
        img(src=jamraiLogoHeaderRect) %>% tagAppendAttributes(class="jamrai-logo-header-rect width-auto"),
        div() %>% tagAppendAttributes(class="vertical-line"),
        div("EU-JAMRAI 2 - Human AMR surveillance systems in Europe") %>% tagAppendAttributes(class="main-title width-auto"),
    ) %>% tagAppendAttributes(class="header-box"),

    hr(),

    # side bar (filters)
    sidebarLayout(

        position = "left",

        # sidebar content
        sidebarPanel(

            
            ##tags$h4("Filters"),

            # country filter
            #tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # remove slider little ticks
            #selectInput(
            #    inputId  = "inCountry",
            #    label    = "Country - drop down",
            #    choices  = unique(gapminder$country[gapminder$continent == "Europe"]),
            #    selected = "Sweden",
            #    width    = "256px"
            #),

            # 

            tags$h4("Country"),
            tags$div(
                id="myclass1",
                class="myclass",
                    checkboxGroupInput(
                    inputId  = "countriesSelection",
                    label    = "",
                    choices  = countryList,
                    selected = countryList,
                    inline   = FALSE,
                    width    = NULL
                )
            ) %>% tagAppendAttributes(class="filter-scroll-box"),

            hr(),

            tags$h4("Pathogens")


            #sliderInput(
            #    inputId = "yearSlider",
            #    label   = "Year",
            #    min     = 2020,
            #    max     = 2024,
            #    value   = c(2020, 2024),
            #    step    = 1,
            #    width   = "256px"
            #),

            #sliderInput("n", "Observations", 1, 100, 50, ticks = FALSE),
            #sliderInput("bins", "Scale", 1, 10, 5, step = 1, ticks = FALSE),
            #actionButton(
            #    inputId = "applyFilters",
            #    label   = "Apply filters"
            #)

        ) %>% tagAppendAttributes(class="width-auto"),

        # main dashboard container
        mainPanel(

            # tabs
            tabsetPanel(

                # map
                tabPanel(
                    tags$span(
                        bsicons::bs_icon("globe-europe-africa"),
                        tags$span("Map") %>% tagAppendAttributes(class="tab-text")
                    ),

                    fluidRow( ## todo: wrap into division
                        ##tags$span("Countries participation to the survey") %>% tagAppendAttributes(class="plot-title"), ## title - temporarily removed
                        
                        #plotlyOutput(outputId = "mainMap"), # replaced by leaflet map
                        leafletOutput("mainMap") %>% tagAppendAttributes(class="main-map-output"),

                        #tags$span
                        #    tags$span(
                        #        tags$span(
                        #            ""
                        #        ) %>% tagAppendAttributes(class="map-legend-color jamrai-blue"),
                        #        tags$span(
                        #            "Reply"
                        #        ) %>% tagAppendAttributes(class="map-legend-description"),
                        #        tags$span(
                        #            ""
                        #        ) %>% tagAppendAttributes(class="map-legend-color soft-red"),
                        #        tags$span(
                        #            "No reply"
                        #        ) %>% tagAppendAttributes(class="map-legend-description"),
                        #        tags$span(
                        #            ""
                        #        ) %>% tagAppendAttributes(class="map-legend-color soft-grey"),
                        #        tags$span(
                        #            "Not participating"
                        #        ) %>% tagAppendAttributes(class="map-legend-description")
                        #    ) %>% tagAppendAttributes(class="map-legend-row")
                        #) %>% tagAppendAttributes(class="map-legend-box")

                        tags$span(
                            "Map source: ",
                            tags$a(href="https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries", "eurostat")
                        )
                    )
                ),

                # plots
                tabPanel(
                    tags$span(
                        bsicons::bs_icon("bar-chart"),
                        tags$span("Dashboard") %>% tagAppendAttributes(class="tab-text")
                    ),
                    fluidRow(
                        plotOutput(outputId = "outChartLifeExp"),
                        plotOutput(outputId = "outChartGDP")
                    )
                ),

                
                tabPanel(
                    tags$span(
                        bsicons::bs_icon("table"),
                        tags$span("Dataset") %>% tagAppendAttributes(class="tab-text")
                    ),
                    tableOutput("myTable")
                )
            ),
        ) %>% tagAppendAttributes(class="main-panel")
    ) %>% tagAppendAttributes(class="main-box"),
    #hr(),
    fluidRow( # footer
        img(src=jamraiLogoHeaderRectWhite) %>% tagAppendAttributes(class="width-auto footer-image"),
        img(src=euLogoFundWhite) %>% tagAppendAttributes(class="width-auto footer-image")
    ) %>% tagAppendAttributes(class="footer-box")
))


## SERVER ##

server <- function(input, output, session) {
    
    # filter data and store as reactive value
    data <- reactive({
        gapminder %>%
            filter(country == input$inCountry) %>%
            group_by(year) %>%
            summarise(
                AvgLifeExp = round(mean(lifeExp)),
                AvgGdpPercap = round(mean(gdpPercap), digits = 2)
        )
    })

    # custom common properties for charts
    chart_theme <- ggplot2::theme(
        plot.title   = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12)
    )

    # render placeholder1 chart
    output$outChartLifeExp <- renderPlot({
        ggplot(data(), aes(x = year, y = AvgLifeExp)) +
            geom_col(fill = "#008aab") +
            geom_text(aes(label = AvgLifeExp), vjust = 2, size = 6, color = "#ffffff") +
            labs(title = paste("Placeholder graph - ", input$inCountry))
    })

    # render placeholder2 chart
    output$outChartGDP <- renderPlot({
        ggplot(data(), aes(x = year, y = AvgGdpPercap)) +
            geom_line(color = "#008aab", size = 2) +
            geom_point(color = "#008aab", size = 5) +
            geom_label(
                aes(label = AvgGdpPercap),
                nudge_x = 0.25,
                nudge_y = 0.25
            ) +
            labs(title = paste("Placeholder graph - ", input$inCountry))
    })

    output$plot <- renderPlot({
        hist(rnorm(input$n), breaks = input$bins, col = "#007bc2")
    })

    output$mainMap <- renderLeaflet({

        if (input$dark_mode == "dark") {
            themeBgColor = "#1D1F21"
        } else {
            themeBgColor = "#ffffff"
        }

        leaflet() %>%
        setView(lng = 22, lat = 50, zoom = 4) %>%
        #addTiles(
        #    "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        #    #{attribution: '© OpenStreetMap'}
        #) %>%
        #setView(0.34580993652344, 50.6252978589571, zoom = 3) %>%
        addGeoJSON(
            geojson = geojsonEurope,
            opacity = 1,
            fillOpacity = 0,
            color = themeBgColor,
            weight = 1,
            fillColor = "#000"
        )
    })

    #output$mainMap <- renderPlotly({ # replaced by leaflet map
    #    if (input$dark_mode == "dark") {
    #        themeBgColor = "#1D1F21"
    #    } else {
    #        themeBgColor = "#ffffff"
    #    }
    #    countryReplies %>%
    #    plot_ly(
    #        #width="100%",
    #        height=680
    #    ) %>%
    #    layout(
    #        geo = list(
    #            scope="europe",
    #            showframe=FALSE,
    #            showland=TRUE,
    #            landcolor="#cccccc", # inside countries
    #            countrycolor=themeBgColor, # lines
    #            bgcolor = themeBgColor, # bg color - inside map
    #            #coastlinecolor="#fff",
    #            showcoastline=FALSE,
    #            projection=list(
    #                scale=1.5  # initial zoom
    #            )
    #            #color=countryReplies$reply
    #        ),
    #        #plot_bgcolor = "#000000", # no effect
    #        paper_bgcolor = themeBgColor, # bg color - outside map
    #        margin=list(t=0, r=0,  l=0, b=0),
    #        autosize=TRUE
    #    ) %>%
    #    add_trace(
    #        #geojson=FROM_GeoJson("/home/shiny-app/files/data/CNTR_RG_03M_2024_3035.geojson"),
    #        type='choropleth',
    #        locationmode="country names",
    #        locations=countryReplies$country,
    #        z=countryReplies$reply,
    #        zmin=0,
    #        zmax=1,
    #        #showlegend=TRUE,
    #        #autocolorscale=FALSE,
    #        showscale=TRUE,
    #        colors=c("#cc8888","#0fdbd5"),
    #        #colorbar=list(
    #        #    title=list(
    #        #        text="Reply to the survey"
    #        #    ),
    #        #    dtick=1,
    #        #    nticks=2
    #        #    ),
    #        marker=list(line=list(width=1, color=themeBgColor))
    #    )
    #})

}

# run
shinyApp(ui = ui, server = server)
