## NOTES
# TODO -> cohenrence in snake/camel case
# mapresultsdata -> nom de variable pas top

## SETUP ##

# import libraries
library(shiny)
library(shinyWidgets)
#library(sf)
library(readxl)
library(rjson)
library(jsonlite)
library(dplyr)
library(hash)
#library(countrycode)
library(ggplot2)
library(echarts4r)
library(plotly)
#library(RColorBrewer)
#library(geojsonR)
#library(geojsonio)
#library(leaflet)
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
#geojsonEurope = FROM_GeoJson(file.path("/home/shiny-app/files/data/europe.geojson")) ## not working
#geojsonEurope2 = geojson_read(file.path("/home/shiny-app/files/data/CNTR_RG_60M_2024_4326_europe_only.geojson")) ## working
geojsonEurope = rjson::fromJSON(file = file.path("/home/shiny-app/files/data/CNTR_RG_60M_2024_4326_europe_only.geojson")) ## working

## source: https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries


# import survey questions and replies from JSON
surveyDataFile <- file.path("/home/shiny-app/files/data/replies_formatted.json")
surveyData     <- rjson::fromJSON(paste(readLines(surveyDataFile), collapse=""))
surveyDataHash <- hash(surveyData)


# variables for data parsing -- might need to be updated for future versions of the survey
countryQuestionIndex <- 3

# prepare data

# europe country list
euroCountryList <- c()
for (country in geojsonEurope$features){
    euroCountryList <- c(euroCountryList, country$id)
}
#euroCountryDf <- data.frame(country=euroCountryList, presence=rep(1, length(euroCountryList)))

# participating country list
participatingCountries <- colnames(as.data.frame(surveyData[[3]][["possible_answers"]]))

# not participating countries (grey on the map)
nonParticipatingCountries <- setdiff(euroCountryList, participatingCountries)

# set df for map results layer (color based on score, set score to zero by default)
mapResultsData <- data.frame(country=participatingCountries, score=rep(0, length(participatingCountries)))



####### TEST SCORES CALCULATION (INITIAL - ALL QUESTIONS)
## loop sur chaque question
## pour chaque pqys, on récupere sa reponse et le score correspondant, et on l'ajoute dans mapResultsData

for (surveyQuestion in ls(surveyDataHash)) {
    ##surveyQuestion  --> KEY
    ##surveyDataHash[[surveyQuestion]]  --> VALUE

    coefficientThisQuestion <- surveyDataHash[[surveyQuestion]][["coefficient"]]
    
    # treat differently multiple choice questions
    if (surveyDataHash[[surveyQuestion]][["type"]] == "MultipleChoice") {

        # take actual answers, loop over country, then loop over their answers, then check the score of that answer, add it to the country's score
        for (country in participatingCountries) {
            for (answer in ls(surveyDataHash[[surveyQuestion]][["actual_answers"]])) {
                if (answer == country) {
                    # loop over all answers for this question
                    for (subAnswer in surveyDataHash[[surveyQuestion]][["actual_answers"]][[answer]]) {
                        # for each answer, get score
                        for (possibleAnswer in ls(surveyDataHash[[surveyQuestion]][["possible_answers"]])) {
                            # get score for this reply (integrate coefficient)
                            if (possibleAnswer == subAnswer) {
                                scoreThisReply <- surveyDataHash[[surveyQuestion]][["possible_answers"]][[possibleAnswer]]
                                if (scoreThisReply != "null") {
                                    mapResultsData$score[mapResultsData$country == country] <- mapResultsData$score[mapResultsData$country == country] + (scoreThisReply * as.numeric(coefficientThisQuestion))
                                }
                            }
                        }
                    }
                }
            }
        }

        next
    }

    for (country in participatingCountries) {

        # reset
        replyThisCountry <- "" ## useful??
        scoreThisReply <- 0

        # get the reply for this country ## can't get it without looping because can't use variable after '$'
        for (answer in ls(surveyDataHash[[surveyQuestion]][["actual_answers"]])) {
            if (answer == country) {
                replyThisCountry <- surveyDataHash[[surveyQuestion]][["actual_answers"]][[answer]]
            }
        }

        # loop over possible answers to get the score for this reply
        for (possibleAnswer in ls(surveyDataHash[[surveyQuestion]][["possible_answers"]])) {
            # get score for this reply (integrate coefficient)
            if (possibleAnswer == replyThisCountry) {
                scoreThisReply <- surveyDataHash[[surveyQuestion]][["possible_answers"]][[possibleAnswer]]
            }
        }

        if (scoreThisReply != "null") {
            mapResultsData$score[mapResultsData$country == country] <- mapResultsData$score[mapResultsData$country == country] + (scoreThisReply * as.numeric(coefficientThisQuestion))
        }
        
    }

}

print(mapResultsData)##dev


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

    # header ## moved
    #fluidRow(
    #    img(src=jamraiLogoHeaderLong) %>% tagAppendAttributes(class="jamrai-logo-header-long width-auto"),
    #    img(src=jamraiLogoHeaderRect) %>% tagAppendAttributes(class="jamrai-logo-header-rect width-auto"),
    #    div() %>% tagAppendAttributes(class="vertical-line"),
    #    div("EU-JAMRAI 2 - Human AMR surveillance systems in Europe") %>% tagAppendAttributes(class="main-title width-auto"),
    #) %>% tagAppendAttributes(class="header-box"),

    #hr() %>% tagAppendAttributes(class="hr-small-margin"),

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

            img(src=jamraiLogoHeaderRect) %>% tagAppendAttributes(class="jamrai-logo-header"),#TEST

            hr() %>% tagAppendAttributes(class="hr-filters-separator"),

            tags$h4("Countries"),
            tags$div(
                class="country-filter-container",
                checkboxGroupInput(
                    inputId  = "countriesSelection",
                    label    = "",
                    choices  = participatingCountries,
                    selected = participatingCountries,
                    inline   = FALSE,
                    width    = NULL
                )
            ) %>% tagAppendAttributes(class="filter-scroll-box"),

            hr() %>% tagAppendAttributes(class="hr-filters-separator"),

            tags$h4("Pathogens"),
            tags$div(
                class="pathogen-filter-container",
                checkboxGroupInput(
                    inputId  = "pathogensSelection",
                    label    = "",
                    choices  = c("E. coli", "K. pneumoniae", "P. aeruginosa", "A. baumanii", "S. aureus", "E. faecium", "E. faecalis", "S. pneumoniae", "H. influenzae"),
                    selected = c("E. coli", "K. pneumoniae", "P. aeruginosa", "A. baumanii", "S. aureus", "E. faecium", "E. faecalis", "S. pneumoniae", "H. influenzae"),
                    inline   = FALSE,
                    width    = NULL
                )
            ) %>% tagAppendAttributes(class="filter-scroll-box"),

             hr() %>% tagAppendAttributes(class="hr-filters-separator"),

            tags$h4("Antibiotics"),
            tags$div(
                class="antibiotic-filter-container", ##check si ca fonctionne comme tagAppendAttributes -> si oui utiliser ca dès que possible
                checkboxGroupInput(
                    inputId  = "antibioticsSelection",
                    label    = "",
                    choices  = c("carbapenem", "3GC", "colistin", "methicillin", "vancomycin", "penicillin", "macrolide", "ampicillin"),
                    selected = c("carbapenem", "3GC", "colistin", "methicillin", "vancomycin", "penicillin", "macrolide", "ampicillin"),
                    inline   = FALSE,
                    width    = NULL
                )
            ) %>% tagAppendAttributes(class="filter-scroll-box"),


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

        ) %>% tagAppendAttributes(class="width-16"),

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
                        
                        tags$div(
                            plotlyOutput(outputId = "plotlyMap")
                        ) %>% tagAppendAttributes(class="map-container"),
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
                            tags$a(
                                href="https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries",
                                "eurostat",
                                target = "_blank"
                            )
                        ) %>% tagAppendAttributes(class="map-source-text")
                    )
                ),

                # plots
                tabPanel(

                    tags$span(
                        bsicons::bs_icon("bar-chart"),
                        tags$span("Dashboard") %>% tagAppendAttributes(class="tab-text")
                    ),
                    fluidRow(
                        #leafletOutput("mainMap")
                        #plotOutput(outputId = "outChartLifeExp"),
                        #plotOutput(outputId = "outChartGDP")
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

    calculateScores <- function(scoreDf) {
        for (surveyQuestion in surveyData) {
            for (countryReply in surveyQuestion["actual_answers"]) {
                scoreDf$country <- surveyQuestion[["actual_answers"]][[countryReply]]
            }

        }
        scoreDf
        return(scoreDf)
    }

    getCoutryScores <- reactive({
        calculateScores(mapResultsData)
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
    #output$outChartLifeExp <- renderPlot({
    #    ggplot(filteredData(), aes(x = year, y = AvgLifeExp)) +
    #        geom_col(fill = "#008aab") +
    #        geom_text(aes(label = AvgLifeExp), vjust = 2, size = 6, color = "#ffffff") +
    #        labs(title = paste("Placeholder graph - ", input$countriesSelection))
    #})

    # render placeholder2 chart
    #output$outChartGDP <- renderPlot({
    #    ggplot(filteredData(), aes(x = year, y = AvgGdpPercap)) +
    #        geom_line(color = "#008aab", size = 2) +
    #        geom_point(color = "#008aab", size = 5) +
    #        geom_label(
    #            aes(label = AvgGdpPercap),
    #            nudge_x = 0.25,
    #            nudge_y = 0.25
    #        ) +
    #        labs(title = paste("Placeholder graph - ", input$countriesSelection))
    #})

    #output$plot <- renderPlot({
    #    hist(rnorm(input$n), breaks = input$bins, col = "#007bc2")
    #})

    #output$mainMap <- renderLeaflet({
    #
    #    if (input$dark_mode == "dark") {
    #        themeBgColor = "#1d1f21"
    #        themeFgColor = "#ffffff"
    #    } else {
    #        themeBgColor = "#ffffff"
    #        themeFgColor = "#1d1f21"
    #    }
    #
    #    leaflet(geojsonEurope) %>%
    #    setView(lng = 22, lat = 50, zoom = 4) %>%
    #    #addTiles(
    #    #    "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
    #    #    #{attribution: '© OpenStreetMap'}
    #    #) %>%
    #    addGeoJSON(
    #        geojson = geojsonEurope,
    #        opacity = 1,
    #        fillOpacity = 1,
    #        color = themeBgColor(),
    #        weight = 1,
    #        fillColor = "#aaaaaa"
    #    )# %>%
    #    #addPolygons(
    #    #    fillColor = ~pal(density),
    #    #    fillOpacity = 1 ## function based on PARTICIPATION
    #    #)
    #})

    

    output$plotlyMap <- renderPlotly({
        if (input$dark_mode == "dark") {
            themeBgColor = "#1D1F21"
            themeFgColor = "#ffffff"
            #themeSoftGrey = 0.3
        } else {
            themeBgColor = "#ffffff"
            themeFgColor = "#1D1F21"
            #themeSoftGrey = 0.7
        }
        #countryReplies %>%
        map <- plot_ly(
            #width=,
            height=800
        )

        map <- map %>% add_trace( # displays results
            type='choropleth',
            #featureidkey='properties.NAME_ENGL', # id added directly in source in geojson
            geojson=geojsonEurope,
            locations=mapResultsData$country,#input$countriesSelection, ## to do -> formule qui ajoute/retire des pays de mapresultsdata en fonction de input$countriesSelection
            z=mapResultsData$score,#c(rep(1, length(input$countriesSelection))),
            zmin=0,
            zmax=max(mapResultsData$score) * 1.1,
            #showlegend=TRUE,
            #autocolorscale=FALSE,
            showscale=TRUE,
            colors=c("#cc8888", "#dddd77", "#0fdbd5"),
            reversescale=FALSE,
            #colors=c(""),
            colorbar=list(
                #outlinecolor=rgba(0,0,0,0),
                outlinewidth=0,
                thickness=20, #default 30
                color=themeBgColor,
                tickcolor=themeFgColor,
                y=0.8,
                tickfont=list(
                    color=themeFgColor
                ),
                title=list(
                    text="Score",
                    font=list(
                        color=themeFgColor
                    )
                )
            ),
            marker=list(
                line=list(
                    width=1.2,
                    color=themeBgColor
                )
            )
        )

        map <- map %>% add_trace( # non-participating countries
            name="Not participating",
            type='choropleth',
            geojson=geojsonEurope,
            #featureidkey='properties.NAME_ENGL', # id added directly in source in geojson
            locations=nonParticipatingCountries,
            z=rep(0.7, length(nonParticipatingCountries)),
            zmin=0,
            zmax=1,
            showscale=FALSE,
            colorscale="Greys",
            #colors=c("#aaaaaa", "#aaaaaa"), ## cannot use "colors" in both traces
            marker=list(
                line=list(
                    width=1.2,
                    color=themeBgColor
                )
            )
        )

        map <- map %>% layout(
            geo = list(
                scope="europe",
                showcountries=FALSE, # hide default map
                showframe=FALSE, # hide default map
                showland=FALSE, # hide default map
                #landcolor="#cccccc", # inside countries
                #countrycolor=themeBgColor, # lines
                bgcolor = "rgba(0,0,0,0)", # bg color - inside map
                #coastlinecolor="#fff",
                showcoastline=FALSE,
                projection=list(
                    scale=1.7  # initial zoom
                ),
                center=list(
                    lat=54, # 50 trop a en haut
                    lon=14 # 10 top a droite
                )
            ),
            paper_bgcolor = "rgba(0,0,0,0)",#themeBgColor, # bg color - outside map
            margin=list(t=0, r=0,  l=0, b=0),
            autosize=TRUE
        )
    })

}

# run
shinyApp(ui = ui, server = server)
