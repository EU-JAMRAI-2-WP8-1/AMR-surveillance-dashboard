## NOTES
# revoir le format des réponses -> stocker dans un format plus lisible par R, ou un json simplifié -> question > pays > score + un tableau a part avec les tags
## (suite) en gros on check les tags, ca renvoie quelles questions sont ON et lesquelles sont OFF, puis il n'y a plus qu'à récupérer et additionner les scores

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
library(stringr)
library(DT)


# Specify the application port
options(shiny.host = "0.0.0.0")
#options(shiny.host = "148.110.159.160") ## dev
options(shiny.port = 8180)

# Add resource directory to server
shiny::addResourcePath('www', '/srv/shiny-server/www') ## !! causes issues on Docker on Windows? --> remove and put absolute paths everywhere?

# Set the app files directory
appFilesDirectory = "/home/shiny-app/files"

## For layout --> see help at https://shiny.posit.co/r/articles/build/layout-guide/
## JAMRAI logo colors: #0fdbd5, #008aab, #26cad3

# Create custom theme based on Bootstrap
custom_theme <- bs_theme(
    version = 5,
    #bg = "#ffffff",
    #fg = "#000000",
    primary = "#008aab",
    secondary = "#0fdbd5",
    base_font = "Helvetica Neue,Helvetica,Arial,sans-serif"
    #"navbar-bg" = "rgba(0, 0, 0, 0)"
)

# Graph layout theme with Thematic
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


## DATA LOAD AND PREPARAATION ##

# Import logos as variables
jamraiLogoHeaderLong      <- file.path("www/logos/TRANSPARENT_LONG2.png")
jamraiLogoHeaderRect      <- file.path("www/logos/TRANSPARENT_RECTANGULAR.png")
jamraiLogoHeaderRectWhite <- file.path("www/logos/TRANSPARENT_LONG1_WHITE-480x107.png")
euLogoFundWhite           <- file.path("www/logos/EN_Co-fundedbytheEU_RGB_WHITE-Outline-480x107.png")

# Import Europe polygons
#geojsonEurope = FROM_GeoJson(file.path("/home/shiny-app/files/data/europe.geojson")) ## not working
#geojsonEurope2 = geojson_read(file.path("/home/shiny-app/files/data/CNTR_RG_60M_2024_4326_europe_only.geojson")) ## working
geojsonEurope = rjson::fromJSON(file = file.path("/home/shiny-app/files/data/CNTR_RG_60M_2024_4326_europe_only.geojson")) ## working

## source: https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries (modified to include only european countries)

# Import survey questions and replies from JSON
surveyDataFile <- file.path("/home/shiny-app/files/data/OUT_questions_and_replies.json")
surveyData     <- rjson::fromJSON(paste(readLines(surveyDataFile), collapse=""))
##surveyDataHash <- hash(surveyData) ##unused

# Import survey score table from CSV - set first colums as row names
countryScoreTable <- read.csv("/home/shiny-app/files/data/OUT_country_scores.csv", header=TRUE) #row.names = 1,

# Europe country list
euroCountryList <- c()
for (country in geojsonEurope$features){
    euroCountryList <- c(euroCountryList, country$id)
}
#euroCountryDf <- data.frame(country=euroCountryList, presence=rep(1, length(euroCountryList)))

# Country question index (might change in future versions of the survey)
countryQuestionIndex <- 3

# Participating country list
participatingCountries <- str_replace_all(colnames(as.data.frame(surveyData[[countryQuestionIndex]][["possible_answers"]])), "\\.", " ")
##participatingCountries <- str_replace_all(participatingCountries, "\\.", " ") ##remove if working

# Countries that have replied
repliedCountries <- str_replace_all(colnames(as.data.frame(surveyData[[countryQuestionIndex]][["actual_answers"]])), "\\.", " ")

# Not-participating countries (grey on the map)
nonParticipatingCountries <- setdiff(euroCountryList, repliedCountries)

# Filters : pathogens under surveillance / antibiotics / sample types
pathogenList <- c("E. coli", "K. pneumoniae", "P. aeruginosa", "A. baumannii", "S. aureus", "VRE", "S. pneumoniae", "H. influenzae", "C. difficile") # VRE -> "E. faecium", "E. faecalis"
antibioticList <- c("Carbapenem", "3GC", "Colistin", "Methicillin", "Vancomycin", "Penicillin", "Ampicillin")
sampleTypeList <- c("Blood", "Urine", "Respiratory tract", "Soft tissue", "Screening", "Stool")

# Filters : set sections
sections <- c("Section 1", "Section 2", "Section 3")
sectionDefinitions <- data.frame(
    "Section 1" = "Treatment and diagnostic guidelines, Antimicrobial Susceptibility Testing (AST) and genotypic confirmation.",
    "Section 2" = "Whole genome sequencing (WGS) at national reference/expert laboratory",
    "Section 3" = "Data flow of national AMR surveillance system(s)"
)
sectionInfoText     <- "Section 1: Treatment and diagnostic guidelines, Antimicrobial Susceptibility Testing (AST) and genotypic confirmation.\nSection 2: Whole genome sequencing (WGS) at national reference/expert laboratory\nSection 3: Data flow of national AMR surveillance system(s)"
pathogensInfoText   <- "Unselect all pathogens to ignore if questions are pathogen-specific or not.\nTo keep only pathogen-specific questions, select all.\nTo focus one one or several pathogens, select the pathogen(s) you need."
antibioticsInfoText <- "Unselect all antibiotics to ignore if questions are antibiotic-specific or not.\nTo keep only antibiotic-specific questions, select all.\nTo focus one one or several antibiotics, select the antibiotic(s) you need."
sampleTypeInfoText  <- "Unselect all sample types to ignore if questions are type-specific or not.\nTo keep only type-specific questions, select all.\nTo focus one one or several sample type, select the type(s) you need."


## USER INTERFACE ##

# user interface
ui <- shinyUI(fluidPage(

    # set theme
    theme = custom_theme,

    # import CSS
    includeCSS(file.path("/srv/shiny-server/www/css/style.css")),

    # import JS
    includeScript("/srv/shiny-server/www/js/script.js"),

    # add favicon
    tags$head(tags$link(rel="shortcut icon", href=file.path("www/favicons/jamrai_favicon_32x32.png"))),

    # dark mode hidden input (required)
    input_dark_mode(
        id = "mode",
        class = "hidden"
    ),

    # navigation bar
    page_navbar(

        nav_item(
            class = "navbar-header",
            "ENAMReS - European National AMR Surveillance"
        ),

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
            href = "https://github.com/EU-JAMRAI-2-WP8-1/AMR-surveillance-dashboard",
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
    div(
        class = "top-spacer"
    ),

    # Layout type
    sidebarLayout(

        position = "left",

        # side bar (filters)
        sidebarPanel(
            class = "sidebar-panel",
            width = 2,

            # logo
            img(
                src = jamraiLogoHeaderRect,
                class = "jamrai-logo-header"
            ),

            hr(
                class = "hr-filters-separator"
            ),

            tags$span(
                class = "reset-filters-wrapper",
                actionButton("resetFilters", "Reset filters")
            ),

            accordion(

                accordion_panel(
                    "Sections",
                    tags$button(
                        class = "info-button",
                        #title = sectionInfoText,
                        "?"
                    ),
                    tags$span(
                        class = "info-sections",
                        sectionInfoText
                    ),
                    checkboxGroupInput(
                        inputId  = "sectionsSelection",
                        label    = "",
                        choices  = sections,
                        selected = sections,
                        inline   = FALSE,
                        width    = NULL
                    )
                ),
                hr(
                    class = "hr-filters-separator"
                ),
                accordion_panel(
                    "Countries",
                    actionLink("selectAllCountries", "Select All"),
                    checkboxGroupInput(
                        inputId  = "countriesSelection",
                        label    = "",
                        choices  = participatingCountries,
                        selected = participatingCountries,
                        inline   = FALSE,
                        width    = NULL
                    )
                ),
                hr(
                    class = "hr-filters-separator"
                ),
                accordion_panel(
                    "Pathogens",
                    actionLink("selectAllPathogens", "Select All"),
                    tags$button(
                        class = "info-button",
                        #title = pathogensInfoText,
                        "?"
                    ),
                    tags$span(
                        class = "info-sections",
                        pathogensInfoText
                    ),
                    checkboxGroupInput(
                        inputId  = "pathogensSelection",
                        label    = "",
                        choices  = pathogenList,
                        selected = c(),
                        inline   = FALSE,
                        width    = NULL
                    )
                ),
                hr(
                    class = "hr-filters-separator"
                ),
                accordion_panel(
                    "Antibiotics",
                    actionLink("selectAllAntibiotics", "Select All"),
                    tags$button(
                        class = "info-button",
                        #title = antibioticsInfoText,
                        "?"
                    ),
                    tags$span(
                        class = "info-sections",
                        antibioticsInfoText
                    ),
                    class = "country-filter-container",
                    checkboxGroupInput(
                        inputId  = "antibioticsSelection",
                        label    = "",
                        choices  = antibioticList,
                        selected = c(),
                        inline   = FALSE,
                        width    = NULL
                    )
                    
                ),
                hr(
                    class = "hr-filters-separator"
                ),
                accordion_panel(
                    "Sample type",
                    actionLink("selectAllSampleTypes", "Select All"),
                    tags$button(
                        class = "info-button",
                        #title = sampleTypeInfoText,
                        "?"
                    ),
                    tags$span(
                        class = "info-sections",
                        sampleTypeInfoText
                    ),
                    checkboxGroupInput(
                        inputId  = "sampleTypesSelection",
                        label    = "",
                        choices  = sampleTypeList,
                        selected = c(),
                        inline   = FALSE,
                        width    = NULL
                    )
                )
            )
        ),

        # main panel
        mainPanel(
            width = 10,

            # tabs
            tabsetPanel(

                # map
                tabPanel(
                    tags$span(
                        bsicons::bs_icon("globe-europe-africa"),
                        tags$span(
                            class = "tab-text",
                            "Map"
                        )
                    ),

                    fluidRow(

                        class = "map-tab-container",

                        column(
                            width = 9,
                            tags$div(
                                class = "map-and-source-container",
                                tags$div(
                                    class = "map-container",
                                    plotlyOutput(outputId = "plotlyMap", width = "100%", height = "780")
                                ),
                                tags$div(
                                    class = "map-source-text",
                                    "Map source: ",
                                    tags$a(
                                        href="https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries",
                                        "Eurostat",
                                        target = "_blank"
                                    )
                                )
                            ),
                        ),

                        column(
                            width = 3,
                            ## ici ajouter toutes les infos nécessaires
                            tags$div(
                                class = "map-info-container",
                                HTML("<i>See the \"Info\" tab for information about scores</i>"),
                                DT::dataTableOutput("scoresTable")

                            )
                        )
                        
                    )
                ),

                # survey results
                tabPanel(
                    tags$span(
                        bsicons::bs_icon("journal-check"),
                        tags$span(
                            class = "tab-text",
                            "Survey results"
                        )
                    ),
                    DT::dataTableOutput("resultsTable"),#, width="4800px"
                    #DTOutput("resultsTable") # a tester
                ),

                # plots
                tabPanel(
                    tags$span(
                        bsicons::bs_icon("bar-chart"),
                        tags$span(
                            class = "tab-text",
                            "Dashboard"
                        )
                    ),
                    fluidRow(
                        #
                    )
                ),

                # info
                tabPanel(
                    tags$span(
                        bsicons::bs_icon("info-circle"),
                        tags$span(
                            class = "tab-text",
                            "Info"
                        )
                    ),
                    fluidRow(
                        tags$div(
                            class = "about-container",
                            includeHTML("/srv/shiny-server/www/html/about.html")
                        )
                    )
                )

                # dataset
                #tabPanel(
                #    tags$span(
                #        bsicons::bs_icon("table"),
                #        tags$span(
                #            class = "tab-text",
                #            "Dataset"
                #        )
                #    ),
                #    tableOutput("myTable")
                #)
            ),
        )
    ),

    # footer
    fluidRow(
        img(src=jamraiLogoHeaderRectWhite) %>% tagAppendAttributes(class="width-auto footer-image"),
        img(src=euLogoFundWhite) %>% tagAppendAttributes(class="width-auto footer-image")
    ) %>% tagAppendAttributes(class="footer-box")
))


## SERVER ##

server <- function(input, output, session) {

    ## OBSERVE ##

    # "Reset filters" button
    observeEvent(input$resetFilters, {
        updateCheckboxGroupInput(session, "sectionsSelection", choices = c("Section 1", "Section 2", "Section 3"), selected = c("Section 1", "Section 2", "Section 3"))
        updateCheckboxGroupInput(session, "countriesSelection", choices = participatingCountries, selected = participatingCountries)
        updateCheckboxGroupInput(session, "pathogensSelection", choices = pathogenList)
        updateCheckboxGroupInput(session, "antibioticsSelection", choices = antibioticList)
        updateCheckboxGroupInput(session, "sampleTypesSelection", choices = sampleTypeList)
    })

    # "Select all" button for countries
    observe({
        if (input$selectAllCountries == 0) {
            return(NULL)
        } else if (input$selectAllCountries%%2 == 0) {
            updateCheckboxGroupInput(session, "countriesSelection", choices = participatingCountries)
        } else {
            updateCheckboxGroupInput(session, "countriesSelection", choices = participatingCountries, selected = participatingCountries)
        }
    })

    # "Select all" button for pathogens
    observe({
        if (input$selectAllPathogens == 0) {
            return(NULL)
        } else if (input$selectAllPathogens%%2 == 0) {
            updateCheckboxGroupInput(session, "pathogensSelection", choices = pathogenList)
        } else {
            updateCheckboxGroupInput(session, "pathogensSelection", choices = pathogenList, selected = pathogenList)
        }
    })

    # "Select all" button for antibiotics
    observe({
        if (input$selectAllAntibiotics == 0) {
            return(NULL)
        } else if (input$selectAllAntibiotics%%2 == 0) {
            updateCheckboxGroupInput(session, "antibioticsSelection", choices = antibioticList)
        } else {
            updateCheckboxGroupInput(session, "antibioticsSelection", choices = antibioticList, selected = antibioticList)
        }
    })

    # "Select all" button for sample types
    observe({
        if (input$selectAllSampleTypes == 0) {
            return(NULL)
        } else if (input$selectAllSampleTypes%%2 == 0) {
            updateCheckboxGroupInput(session, "sampleTypesSelection", choices = sampleTypeList)
        } else {
            updateCheckboxGroupInput(session, "sampleTypesSelection", choices = sampleTypeList, selected = sampleTypeList)
        }
    })


    ## FUNCTIONS ##

    # Calculates country scores based on filters
    getCountryScores <- function() {

        # initiate output vector
        ##countryScores <- rep(0, length(intersect(repliedCountries, input$countriesSelection)))
        countryScores <- rep(0, length(repliedCountries))
        ##countryMaxScores <- rep(0, length(intersect(repliedCountries, input$countriesSelection)))
        countryMaxScores <- rep(0, length(repliedCountries))
        ##countryScoreRatios <- rep(0, length(intersect(repliedCountries, input$countriesSelection)))
        countryScoreRatios <- rep(0, length(repliedCountries))

        # loop over questions
        for (column in 2:ncol(countryScoreTable)) { # skip col 1 = country name

            ## check if question must be taken or not ##

            # get tags as vector
            tagsThisQuestion <- str_split(str_replace_all(countryScoreTable[2, column], c("\\[" = "", "\\]" = "", "'" = "")), ", ")

            # sections
            if (length(input$sectionsSelection) == 0) { # no section selected -> all zeros and stop
                return(rep(0, length(intersect(repliedCountries, input$countriesSelection))))
            }

            # no match between selected sections and question tags -> skip the question
            if (length(intersect(input$sectionsSelection, tagsThisQuestion[[1]])) == 0) {
                next
            }

            # pathogens
            # if no pathogens selected -> ignore pathogen tags and go to next filter ;
            # if at least one selected -> check for presence of the tags
            if (length(input$pathogensSelection) > 0) {
                # no match between selected pathogens and question tags -> skip the question
                if (length(intersect(input$pathogensSelection, tagsThisQuestion[[1]])) == 0) {
                    next
                }
            }

            # antibiotics
            # if no antibiotic selected -> ignore tags and go to next filter ;
            # if at least one selected -> check for presence of the tags
            if (length(input$antibioticsSelection) > 0) {
                # no match between selected antibiotics and question tags -> skip the question
                if (length(intersect(input$antibioticsSelection, tagsThisQuestion[[1]])) == 0) {
                    next
                }
            }

            # sample types
            # if no sample type selected -> ignore tags ;
            # if at least one selected -> check for presence of the tags
            if (length(input$sampleTypesSelection) > 0) {
                # no match between selected sample types and question tags -> skip the question
                if (length(intersect(input$sampleTypesSelection, tagsThisQuestion[[1]])) == 0) {
                    next
                }
            }

            # all filters passed -> question taken -> loop over countries
            ##for (row in seq(3, nrow(countryScoreTable))) { # starts at row 3 ; by step of 2 (3, 5, 7, etc.) ##OLD
            for (row in 1:((nrow(countryScoreTable)/2) - 1)) {
                # if country is not selected -> skip the country
                if (!(countryScoreTable[(row * 2) + 1, 1] %in% input$countriesSelection)) {
                    next
                }
                # if country taken, add score and max score to respective vectors
                countryScores[row] <- countryScores[row] + as.double(countryScoreTable[(row * 2) + 1, column])
                countryMaxScores[row] <- countryMaxScores[row] + as.double(countryScoreTable[(row * 2) + 2, column]) # line just below
            }
        }
    
        # calculate score ratios
        for (i in 1:length(countryScoreRatios)) {
            if (is.na(countryScores[i])) next
            countryScoreRatios[i] <- countryScores[i] / countryMaxScores[i]
        }

        # remove NaNs from vector
        countryScoreRatios <- countryScoreRatios[!is.na(countryScoreRatios)]

        # in case empty...
        if (length(countryScoreRatios) == 0) {
            countryScoreRatios <- rep(0, length(intersect(repliedCountries, input$countriesSelection)))
        }

        return(list(countryScoreRatios, countryScores, countryMaxScores))

    }

    createResultsTable <- function() {

        ## ajouter conditions de creation du DF en fonction des filtres actifs
        
        questions <- c()
        tags <- c()

        mandatoryTags <- c()

        # loop once over questions to retrieve question titles qnd tags
        i <- 1
        for (question in surveyData) {
            if ("Section 0" %in% question$tags) next # skip section 0

            if (length(intersect(input$sectionsSelection, question$tags)) == 0) next # skip sections that are not selected

            if (length(input$pathogensSelection != 0)) {
                mandatoryTags <- append(mandatoryTags, input$pathogensSelection)
            }
            if (length(input$antibioticsSelection != 0)) {
                mandatoryTags <- append(mandatoryTags, input$antibioticsSelection)
            }
            if (length(input$sampleTypesSelection != 0)) {
                mandatoryTags <- append(mandatoryTags, input$sampleTypesSelection)
            }

            if ((length(intersect(mandatoryTags, question$tags)) == 0) & (length(mandatoryTags) > 0)) next # skip questions without selected tags

            questions <- append(questions, question$title)
            tags <- append(tags, toString(question$tags))
        }

        # add columns to DF
        resultsTable <- data.frame("Question" = questions, Tags = tags)

        # loop over selected countries
        for (country in input$countriesSelection) {

            # (re)set country replies vector
            countryReplies <- c()

            # loop over questions
            for (question in surveyData) {
                if ("Section 0" %in% question$tags) next # skip section 0

                if (length(intersect(input$sectionsSelection, question$tags)) == 0) next # skip sections that are not selected

                if (length(input$pathogensSelection != 0)) {
                    mandatoryTags <- append(mandatoryTags, input$pathogensSelection)
                }
                if (length(input$antibioticsSelection != 0)) {
                    mandatoryTags <- append(mandatoryTags, input$antibioticsSelection)
                }
                if (length(input$sampleTypesSelection != 0)) {
                    mandatoryTags <- append(mandatoryTags, input$sampleTypesSelection)
                }

                if ((length(intersect(mandatoryTags, question$tags)) == 0) & (length(mandatoryTags) > 0)) next # skip questions without selected tags

                if (length(question$actual_answers[[country]]) == 0) {
                    countryReplies <- append(countryReplies, NA)

                } else {
                    countryReplies <- append(countryReplies, toString(question$actual_answers[[country]])) # toString -> to convert Multiple Choice replies
                }
            }

            # append column to DF

            resultsTable[[country]] <- countryReplies
        }

        return(resultsTable)
    }


    ## REACTIVE ##

    countryScores <- reactive({
        getCountryScores()
    })

    getNonParticipatingCountries <- reactive({
        c(setdiff(repliedCountries, input$countriesSelection), nonParticipatingCountries)
    })

    getParticipatingCountries <- reactive({
        data.frame("Country"=intersect(repliedCountries, input$countriesSelection), "Score"=round(countryScores()[[2]], 2), "Max"=round(countryScores()[[3]], 2), "Ratio"=round(countryScores()[[1]], 2))
    })


    ## OUTPUTS ##

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
            #height = 800,
            #width = 800
        )

        map <- map %>% add_trace( # displays results
            type='choropleth',
            #featureidkey='properties.NAME_ENGL', # id added directly in source in geojson -> might be different from name_engl (ex: Slovakia / Slovak Republik)
            geojson=geojsonEurope,
            locations=intersect(repliedCountries, input$countriesSelection),
            z=countryScores()[[1]],
            zmin=0,
            zmax=1, #max(countryScoresDf$totalScore) * 1.1,
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
                x=0.05,
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
                    width=1.4,
                    color=themeBgColor
                )
            )
        )

        map <- map %>% add_trace( # non-participating countries
            name="Not participating",
            type='choropleth',
            geojson=geojsonEurope,
            #featureidkey='properties.NAME_ENGL', # id added directly in source in geojson
            locations=getNonParticipatingCountries(),
            z=rep(0.7, length(getNonParticipatingCountries())),
            zmin=0,
            zmax=1,
            showscale=FALSE,
            colorscale="Greys",
            #colors=c("#aaaaaa", "#aaaaaa"), ## cannot use "colors" in both traces
            marker=list(
                line=list(
                    width=1.4,
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
                    lat=54,
                    lon=14
                )
            ),
            paper_bgcolor = "rgba(0, 0, 0, 0)",#themeBgColor, # bg color - outside map
            margin=list(
                t=32,
                r=0,
                l=0,
                b=32
            ),
            autosize=TRUE
        )
    })

    output$scoresTable <- DT::renderDT(
        getParticipatingCountries(),
        rownames = FALSE,
        options=list(
            dom = 't',
            pageLength = 100
        )
    )

    output$resultsTable <- DT::renderDT(
        createResultsTable(),
        rownames = FALSE,
        #colnames = c(c("Questions", "Tags"), input$countriesSelection),
        options=list(
            autowidth=TRUE,
            scrollX=TRUE,
            pageLength = 10
            #columnDefs = list(
            #    list(targets=c(0), width='400')
            #)
        )
    )

}

# run
shinyApp(ui = ui, server = server)
