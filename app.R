## NOTES
# TODO -> cohenrence in snake/camel case
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


# Specify the application port
options(shiny.host = "0.0.0.0")
#options(shiny.host = "148.110.159.160")
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


# prepare data

# europe country list
euroCountryList <- c()
for (country in geojsonEurope$features){
    euroCountryList <- c(euroCountryList, country$id)
}
#euroCountryDf <- data.frame(country=euroCountryList, presence=rep(1, length(euroCountryList)))

# variables for data parsing -- might need to be updated for future versions of the survey
countryQuestionIndex <- 3

# participating country list
participatingCountries <- colnames(as.data.frame(surveyData[[countryQuestionIndex]][["possible_answers"]]))
participatingCountries <- str_replace_all(participatingCountries, "\\.", " ")

# not participating countries (grey on the map)
nonParticipatingCountries <- setdiff(euroCountryList, participatingCountries)

# set df for map results layer (color based on score, set score to zero by default)
countryScoresDf <- data.frame(
    country       = participatingCountries,
    totalScore    = rep(0, length(participatingCountries)),
    scoreSection1 = rep(0, length(participatingCountries)),
    scoreSection2 = rep(0, length(participatingCountries)),
    scoreSection3 = rep(0, length(participatingCountries))
)

# initialise df for score gap between actual and possible score
scoreGapDf <- data.frame(
    country          = participatingCountries,
    scoreGap         = rep(0, length(participatingCountries)),
    scoreGapSection1 = rep(0, length(participatingCountries)),
    scoreGapSection2 = rep(0, length(participatingCountries)),
    scoreGapSection3 = rep(0, length(participatingCountries))
)

# initialize sum of all questions coefficient => max score
maxScore = 0
maxScoreSection1 = 0
maxScoreSection2 = 0
maxScoreSection3 = 0

# Filters : set sections
sections <- c("Section 1", "Section 2", "Section 3")


# SCORES CALCULATION

for (surveyQuestion in ls(surveyDataHash)) {
    ##surveyQuestion  --> KEY
    ##surveyDataHash[[surveyQuestion]]  --> VALUE

    coefficientThisQuestion <- surveyDataHash[[surveyQuestion]][["coefficient"]]
    maxScore = maxScore + coefficientThisQuestion
    
    # get section tag + calculate max score section-secific
    if ("Section1" %in% surveyDataHash[[surveyQuestion]][["tags"]]) {
        sectionColumn <- "scoreSection1"
        scoreGapSectionColumn <- "scoreGapSection1"
        maxScoreSection1 = maxScoreSection1 + coefficientThisQuestion
    } else if ("Section2" %in% surveyDataHash[[surveyQuestion]][["tags"]]) {
        sectionColumn <- "scoreSection2"
        scoreGapSectionColumn <- "scoreGapSection2"
        maxScoreSection2 = maxScoreSection2 + coefficientThisQuestion
    } else if ("Section3" %in% surveyDataHash[[surveyQuestion]][["tags"]]) {
        sectionColumn <- "scoreSection3"
        scoreGapSectionColumn <- "scoreGapSection3"
        maxScoreSection3 = maxScoreSection3 + coefficientThisQuestion
    } else {
        # skip section 0 questions (or questions without section if any)
        next
    }
    
    # treat differently multiple choice questions
    if (surveyDataHash[[surveyQuestion]][["type"]] == "MultipleChoice") {

        # take actual answers, loop over country, then loop over their answers, then check the score of that answer, add it to the country's score
        for (country in participatingCountries) {
            for (answer in ls(surveyDataHash[[surveyQuestion]][["actual_answers"]])) {
                if (answer == country) {
                    # loop over all answers for this question
                    scoreThisQuestion <- 0 # (re)set
                    for (subAnswer in surveyDataHash[[surveyQuestion]][["actual_answers"]][[answer]]) {
                        # for each answer, get score
                        for (possibleAnswer in ls(surveyDataHash[[surveyQuestion]][["possible_answers"]])) {
                            # get score for this reply (integrate coefficient)
                            if (possibleAnswer == subAnswer) {
                                scoreThisReply <- surveyDataHash[[surveyQuestion]][["possible_answers"]][[possibleAnswer]]
                                if (scoreThisReply != "null") {
                                    scoreThisQuestion <- scoreThisQuestion + (scoreThisReply * as.numeric(coefficientThisQuestion))
                                    #old countryScoresDf$totalScore[countryScoresDf$country == country] <- countryScoresDf$totalScore[countryScoresDf$country == country] + (scoreThisReply * as.numeric(coefficientThisQuestion))
                                } else {
                                    # if null -> add max score for this question (= coefficient) as score gap
                                    ## note: "null" in multiple choice -> "do not know" => should be the ONLY reply, thus whole coefficient for this question can in theory be added to the score gap
                                    scoreGapDf$scoreGap[scoreGapDf$country == country] <- scoreGapDf$scoreGap[scoreGapDf$country == country] + as.numeric(coefficientThisQuestion)
                                    scoreGapDf[[scoreGapSectionColumn]][scoreGapDf$country == country] <- scoreGapDf[[scoreGapSectionColumn]][scoreGapDf$country == country] + as.numeric(coefficientThisQuestion)
                                }
                            }
                        }
                    }
                    # add score for this question to total score (cap to 1) for this country + for this section
                    if (scoreThisQuestion > 1) {
                        scoreThisQuestion <- 1
                    }
                    # -> total
                    countryScoresDf$totalScore[countryScoresDf$country == country] <- countryScoresDf$totalScore[countryScoresDf$country == country] + scoreThisQuestion
                    # -> section
                    countryScoresDf[[sectionColumn]][countryScoresDf$country == country] <- countryScoresDf[[sectionColumn]][countryScoresDf$country == country] + scoreThisQuestion
                }
            }
        }

        next
    }

    # not multiple-choice
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
            # if not null -> add score to total score (total and section)
            countryScoresDf$totalScore[countryScoresDf$country == country] <- countryScoresDf$totalScore[countryScoresDf$country == country] + (scoreThisReply * as.numeric(coefficientThisQuestion))
            countryScoresDf[[sectionColumn]][countryScoresDf$country == country] <- countryScoresDf[[sectionColumn]][countryScoresDf$country == country] + (scoreThisReply * as.numeric(coefficientThisQuestion))
        } else {
            # if null -> add max score for this question (= coefficient) as score gap
            scoreGapDf$scoreGap[scoreGapDf$country == country] <- scoreGapDf$scoreGap[scoreGapDf$country == country] + as.numeric(coefficientThisQuestion)
            scoreGapDf[[scoreGapSectionColumn]][scoreGapDf$country == country] <- scoreGapDf[[scoreGapSectionColumn]][scoreGapDf$country == country] + as.numeric(coefficientThisQuestion)
        }
        
    }

}


# REACTIVATE TO DISPLAY THE 'MAX SCORES'country reply rate (bigger score = less 'do not know') -> countryMaxScore(=maxScore-countryScoreGap) / maxScore
#for (country in countryScoresDf$country) {
#    countryScoresDf$totalScore[countryScoresDf$country == country] <- (maxScore - scoreGapDf$scoreGap[scoreGapDf$country == country]) / maxScore
#}


## TEMP -> set countries with score = 0 as non-participating
countryScoresDf <- countryScoresDf[countryScoresDf$totalScore != 0,]
nonParticipatingCountries <- setdiff(euroCountryList, countryScoresDf$country)


## USER INTERFACE ##

# user interface
ui <- shinyUI(fluidPage(

    # dark mode hidden input (required)
    input_dark_mode(id = "mode") %>% tagAppendAttributes(class="hidden"),

    # set theme
    theme = custom_theme,

    # import CSS
    includeCSS(file.path("/srv/shiny-server/www/css/style.css")),

    # import JS
    includeScript("/srv/shiny-server/www/js/script.js"),

    # navigation bar
    page_navbar(

        nav_item(
            class="navbar-header",
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

    # side bar (filters)
    sidebarLayout(

        position = "left",

        # sidebar content
        sidebarPanel(

            # logo
            img(src=jamraiLogoHeaderRect) %>% tagAppendAttributes(class="jamrai-logo-header"),

            hr() %>% tagAppendAttributes(class="hr-filters-separator"),

            # Country selection
            tags$div(
                tags$div(
                    class = "filter-header-container",
                    tags$div(
                        class = "filter-header-left-part",
                        tags$button(
                            id = "select-all-countries",
                            bsicons::bs_icon("check-all"),
                            onclick = "selectAllCountries()",
                            class = "btn btn-default btn-sm btn-primary select-all-button"
                        ),
                        tags$h4(
                            class = "filter-header-text",
                            "Countries"
                        )
                    ),
                    tags$button(
                        id = "hide-country-selection",
                        bsicons::bs_icon("chevron-right", icon_type = "solid"),
                        onclick = "hideShow(\"hide-country-selection\", \"country-filter-container\")",
                        class = "hide-show-button"
                    )
                )
            ),
            tags$div(
                id="country-filter-container",
                style = "display:none;",
                checkboxGroupInput(
                    inputId  = "countriesSelection",
                    label    = "",
                    choices  = participatingCountries,
                    selected = participatingCountries,
                    inline   = FALSE,
                    width    = NULL
                )
            ) %>% tagAppendAttributes(class="filter-scroll-box"),

            # Section selection
            hr() %>% tagAppendAttributes(class="hr-filters-separator"),

            tags$div(
                tags$div(
                    class = "filter-header-container",
                    tags$div(
                        class = "filter-header-left-part",
                        tags$button(
                            id = "select-all-section",
                            bsicons::bs_icon("check-all"),
                            onclick = "selectAllSections()",
                            class = "btn btn-default btn-sm btn-primary select-all-button"
                        ),
                        tags$h4(
                            class = "filter-header-text",
                            "Sections"
                        )
                    ),
                    tags$button(
                        id = "hide-sections-selection",
                        bsicons::bs_icon("chevron-right", icon_type = "solid"),
                        onclick = "hideShow(\"hide-sections-selection\", \"section-filter-container\")",
                        class = "hide-show-button"
                    )
                )
            ),
            tags$div(
                id="section-filter-container",
                style = "display:none;",
                checkboxGroupInput(
                    inputId  = "sectionsSelection",
                    label    = "",
                    choices  = sections,
                    selected = sections,
                    inline   = FALSE,
                    width    = NULL
                )
            ) %>% tagAppendAttributes(class="filter-scroll-box"),

            hr() %>% tagAppendAttributes(class="hr-filters-separator"),

            # Pathogen selection
            tags$div(
                tags$div(
                    class = "filter-header-container",
                    tags$div(
                        class = "filter-header-left-part",
                        tags$button(
                            id = "select-all-pathogens",
                            bsicons::bs_icon("check-all"),
                            onclick = "selectAllPathogens()",
                            class = "btn btn-default btn-sm btn-primary select-all-button"
                        ),
                        tags$h4(
                            class = "filter-header-text",
                            "Pathogens"
                        )
                    ),
                    tags$button(
                        id = "hide-pathogen-selection",
                        bsicons::bs_icon("chevron-right", icon_type = "solid"),
                        onclick = "hideShow(\"hide-pathogen-selection\", \"pathogen-filter-container\")",
                        class = "hide-show-button"
                    )
                )
            ),
            tags$div(
                id="pathogen-filter-container",
                style = "display:none;",
                checkboxGroupInput(
                    inputId  = "pathogensSelection",
                    label    = "",
                    choices  = c("E. coli", "K. pneumoniae", "P. aeruginosa", "A. baumanii", "S. aureus", "E. faecium", "E. faecalis", "S. pneumoniae", "H. influenzae"),
                    selected = c("E. coli", "K. pneumoniae", "P. aeruginosa", "A. baumanii", "S. aureus", "E. faecium", "E. faecalis", "S. pneumoniae", "H. influenzae"),
                    inline   = FALSE,
                    width    = NULL
                )
            ) %>% tagAppendAttributes(class="filter-scroll-box"),

            # Antibiotics selection
            hr() %>% tagAppendAttributes(class="hr-filters-separator"),

            tags$div(
                tags$div(
                    class = "filter-header-container",
                    tags$div(
                        class = "filter-header-left-part",
                        tags$button(
                            id = "select-all-antibiotics",
                            bsicons::bs_icon("check-all"),
                            onclick = "selectAllAntibiotics()",
                            class = "btn btn-default btn-sm btn-primary select-all-button"
                        ),
                        tags$h4(
                            class = "filter-header-text",
                            "Antibiotics"
                        )
                    ),
                    tags$button(
                        id = "hide-antibiotics-selection",
                        bsicons::bs_icon("chevron-right", icon_type = "solid"),
                        onclick = "hideShow(\"hide-antibiotics-selection\", \"antibiotic-filter-container\")",
                        class = "hide-show-button"
                    )
                )
            ),
            tags$div(
                id="antibiotic-filter-container",
                style = "display:none;",
                checkboxGroupInput(
                    inputId  = "antibioticsSelection",
                    label    = "",
                    choices  = c("carbapenem", "3GC", "colistin", "methicillin", "vancomycin", "penicillin", "macrolide", "ampicillin"),
                    selected = c("carbapenem", "3GC", "colistin", "methicillin", "vancomycin", "penicillin", "macrolide", "ampicillin"),
                    inline   = FALSE,
                    width    = NULL
                )
            ) %>% tagAppendAttributes(class="filter-scroll-box")

            
            #sliderInput(
            #    inputId = "yearSlider",
            #    label   = "Year",
            #    min     = 2020,
            #    max     = 2024,
            #    value   = c(2020, 2024),
            #    step    = 1,
            #    width   = "256px"
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

                    fluidRow(
                        
                        tags$div(
                            plotlyOutput(outputId = "plotlyMap")
                        ) %>% tagAppendAttributes(class="map-container"),

                        tags$span(
                            "Map source: ",
                            tags$a(
                                href="https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries",
                                "Eurostat",
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
                        #
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

    # footer
    fluidRow(
        img(src=jamraiLogoHeaderRectWhite) %>% tagAppendAttributes(class="width-auto footer-image"),
        img(src=euLogoFundWhite) %>% tagAppendAttributes(class="width-auto footer-image")
    ) %>% tagAppendAttributes(class="footer-box")
))


## SERVER ##

server <- function(input, output, session) {

    convertInputToHeader <- function(sectionFilterInput, whichDf) {
        # convert the section names from the input to the names of the columns in the scores data frame

        activeSectionNamesAsInScoresDf <- c()

        if (whichDf == "raw") {
            for (sectionName in sectionFilterInput) {
                if (sectionName == "Section 1") {
                    activeSectionNamesAsInScoresDf <- c(activeSectionNamesAsInScoresDf, "scoreSection1")
                } else if (sectionName == "Section 2") {
                    activeSectionNamesAsInScoresDf <- c(activeSectionNamesAsInScoresDf, "scoreSection2")
                } else if (sectionName == "Section 3") {
                    activeSectionNamesAsInScoresDf <- c(activeSectionNamesAsInScoresDf, "scoreSection3")
                } else {
                    print("error in function convertInputToHeader()")
                }
            }
        } else if (whichDf == "gap") {
            for (sectionName in sectionFilterInput) {
                if (sectionName == "Section 1") {
                    activeSectionNamesAsInScoresDf <- c(activeSectionNamesAsInScoresDf, "scoreGapSection1")
                } else if (sectionName == "Section 2") {
                    activeSectionNamesAsInScoresDf <- c(activeSectionNamesAsInScoresDf, "scoreGapSection2")
                } else if (sectionName == "Section 3") {
                    activeSectionNamesAsInScoresDf <- c(activeSectionNamesAsInScoresDf, "scoreGapSection3")
                } else {
                    print("error in function convertInputToHeader()")
                }
            }
        }

        return(activeSectionNamesAsInScoresDf)
        
    }

    getCoutryScores <- function() {
        # calculate country scores based on filters

        countryScores <- c()

        for (country in input$countriesSelection) {
            if (country %in% countryScoresDf$country) {
                # calculate score
                rawScore <- sum(countryScoresDf[countryScoresDf$country == country, convertInputToHeader(input$sectionsSelection, "raw")])
                
                maxScore <- 0
                if ("Section 1" %in% input$sectionsSelection) {
                    maxScore <- maxScore + maxScoreSection1
                }
                if ("Section 2" %in% input$sectionsSelection) {
                    maxScore <- maxScore + maxScoreSection2
                }
                if ("Section 3" %in% input$sectionsSelection) {
                    maxScore <- maxScore + maxScoreSection3
                }
                maxScore <- maxScore - sum(scoreGapDf[scoreGapDf$country == country, convertInputToHeader(input$sectionsSelection, "gap")])

                countryScores <- c(countryScores, rawScore/maxScore)
            }
        }

        ## ACTIVATE TO SHOW PARTICIPATION
        #replied <- c()
        #for (countryScore in countryScores) {
        #    if (countryScore > 0) {
        #        replied <- c(replied, 1)
        #    } else {
        #        replied <- c(replied, 0)
        #    }
        #}
        #return(replied)

        return(countryScores)
    }

    coutryScores <- reactive({
        getCoutryScores()
    })

    getNonParticipatingCountries <- reactive({
        c(setdiff(countryScoresDf$country, input$countriesSelection), nonParticipatingCountries)
    })

    # EXAMPLE - custom common properties for charts
    chart_theme <- ggplot2::theme(
        plot.title   = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12)
    )

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
            #featureidkey='properties.NAME_ENGL', # id added directly in source in geojson -> might be different from name_engl (ex: Slovakia / Slovak Republik)
            geojson=geojsonEurope,
            locations=intersect(countryScoresDf$country, input$countriesSelection),#countryScoresDf$country,#input$countriesSelection, ## to do -> formule qui ajoute/retire des pays de countryscoresdf en fonction de input$countriesSelection
            z=coutryScores(),
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
