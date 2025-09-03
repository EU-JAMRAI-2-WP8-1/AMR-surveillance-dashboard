
## SETUP ##

# import libraries
library(shiny)
library(shinyWidgets)
#library(jsonlite)
library(rjson)
library(dplyr)
library(ggplot2)
library(plotly)
library(bslib)
library(thematic)
library(stringr)
library(DT)

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# Add resource directory to server
#shiny::addResourcePath(prefix = 'www', directoryPath = '/srv/shiny-server/www') ## DOCKER
shiny::addResourcePath(prefix = 'www', directoryPath = './www') ## R

# Set the app files directory
#appFilesDirectory = "/home/shiny-app/files"

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


## DATA LOAD AND PREPARATION ##

# Import logos as variables
#jamraiLogoHeaderLong      <- file.path("www/logos/TRANSPARENT_LONG2.png")
#jamraiLogoHeaderRect      <- file.path("www/logos/TRANSPARENT_RECTANGULAR.png")
#jamraiLogoHeaderRectWhite <- file.path("www/logos/TRANSPARENT_LONG1_WHITE-480x107.png")
#euLogoFundWhite           <- file.path("www/logos/EN_Co-fundedbytheEU_RGB_WHITE-Outline-480x107.png")

# Import Europe polygons
geojsonEurope <- tryCatch({
    rjson::fromJSON(file = file.path("www/data/CNTR_RG_60M_2024_4326_europe_only.geojson")) # rjson
    ##jsonlite::read_json("www/data/CNTR_RG_60M_2024_4326_europe_only.geojson") # jsonlite (unused -> slows down process)
}, error = function(e) {
    showNotification("Error loading map data", type = "error")
    return(list(features = list())) # Return empty structure
})

## source: https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries (modified to include only european countries)

# Import survey questions and replies from JSON
surveyDataFile <- file.path("www/data/OUT_questions_and_replies.json")
surveyData <- tryCatch({
    rjson::fromJSON(paste(readLines(surveyDataFile), collapse="")) # rjson
    ##jsonlite::fromJSON(surveyDataFile) # jsonlite
}, error = function(e) {
    showNotification("Error loading survey data", type = "error")
    return(NULL)
})

# Import survey score table from CSV - set first column as row names
countryScoreTable <- tryCatch({
    read.csv("www/data/OUT_country_scores.csv", header=TRUE)
}, error = function(e) {
    showNotification("Error loading country scores", type = "error")
    return(data.frame()) # Return empty dataframe
})

# Europe country list
euroCountryList <- c()
for (country in geojsonEurope$features) {
    euroCountryList <- c(euroCountryList, country$id)
}

# Country question index (/!\ might change in future versions of the survey)
countryQuestionIndex <- 3

# Participating country list
participatingCountries <- str_replace_all(colnames(as.data.frame(surveyData[[countryQuestionIndex]][["possible_answers"]])), "\\.", " ")

# Countries that have replied
repliedCountries <- str_replace_all(colnames(as.data.frame(surveyData[[countryQuestionIndex]][["actual_answers"]])), "\\.", " ")

# Not-participating countries (grey on the map)
nonParticipatingCountries <- setdiff(euroCountryList, repliedCountries)

# Filters : pathogens under surveillance / resistances / culture materials
sectionList         <- c("National surveillance", "National genomic surveillance", "National guidance") # order is reverted compared to the survey (3, 2, 1)
pathogenList        <- c("E. coli", "K. pneumoniae", "P. aeruginosa", "A. baumannii", "S. aureus", "E. faecium/faecalis", "S. pneumoniae", "H. influenzae", "C. difficile")
resistanceList      <- c("Carbapenem", "3rd-generation Cephalosporin", "Colistin", "Methicillin", "Vancomycin", "Penicillin", "Ampicillin")
cultureMaterialList <- c("Blood", "Urine", "Respiratory tract", "Soft tissue", "Screening", "Stool")

# info text
pathogensInfoText       <- "Unselect all pathogens to ignore if questions are pathogen-specific or not.\nTo keep only pathogen-specific questions, select all.\nTo focus on one or several pathogens, select the pathogen(s) you need."
resistancesInfoText     <- "Unselect all resistances to ignore if questions are resistance-specific or not.\nTo keep only resistance-specific questions, select all.\nTo focus on one or several resistances, select the resistance(s) you need."
cultureMaterialInfoText <- "Unselect all culture materials to ignore if questions are material-specific or not.\nTo keep only material-specific questions, select all.\nTo focus on one or several culture materials, select the one(s) you need."

# get all questions (short titles) for question filter + set list of multiple choice questions (short titles)
allShortTitles <- c()
multipleChoiceShortTitles <- c()
for (question in surveyData) {
    #if (question$coefficient == "0") next # skip question without scores
    if ("Section 0" %in% question$tags) next # skip section 0
    if (question$type == "FreeText") next # skip free text questions
    if (question$short_title %in% allShortTitles) next # skip if alredy in (as matrix question have the same short title)
    allShortTitles <- c(allShortTitles, question$short_title)

    if (question$type == "MultipleChoice") {
        multipleChoiceShortTitles <- c(multipleChoiceShortTitles, question$short_title)
    }
}

# initiate dicrete colors sequence for maps and plots
colorSequence <- c("#0fdbd5", "#ff6f61", "#f7c948", "#6a4c93", "#25c414", "#1982c4", "#e76f51", "#2a9d8f", "#f4a261", "#264653", "#8ecae6", "#ffb4a2", "#000000")
alternativeColorSequence <- c("#0fdbd5", "#f7c948", "#ff6f61", "#6a4c93", "#25c414", "#1982c4", "#e76f51", "#2a9d8f", "#f4a261", "#264653", "#8ecae6", "#ffb4a2", "#000000") # colors 2 and 3 reverted (quick fix for "No" reply at 3rd position)

# create a dataset for participation map
participationData <- data.frame(
    "country" = euroCountryList,
    "survey_participation" = rep(NA, length(euroCountryList))
)
for (country in euroCountryList) {
    if (country %in% repliedCountries) {
        participationData[participationData$country == country, "survey_participation"] <- 1 #"Yes"
    } else if (country %in% participatingCountries) {
        participationData[participationData$country == country, "survey_participation"] <- 2 #"No"
    } else {
        participationData[participationData$country == country, "survey_participation"] <- 3 #"Not in JAMRAI"
    }
}
participationDataOccurrences <- data.frame(
    "reply" = c("Yes", "No", "Not in JAMRAI"),
    "occurences" = c(
        sum(participationData$survey_participation == 1),
        sum(participationData$survey_participation == 2),
        sum(participationData$survey_participation == 3)
    )
)


## USER INTERFACE ##

# user interface
ui <- shinyUI(fluidPage(

    # set theme
    theme = custom_theme,

    # import CSS
    includeCSS(file.path("www/css/style.css")),

    # import JS
    includeScript("www/js/script.js"),

    # add favicon
    tags$head(tags$link(rel="shortcut icon", href=file.path("www/favicons/jamrai_favicon_32x32.png"))),

    # dark mode hidden input (required)
    #input_dark_mode(
    #    id = "mode",
    #    class = "hidden"
    #),

    # navigation bar
    #page_navbar(

        #nav_item(
        #    class = "navbar-header",
        #    #"ENAMReS - European National AMR Surveillance"
        #    HTML("<span style=\"color:#ff4444\">[TEST VERSION]</span><span> National surveillance of antimicrobial resistance (AMR) in humans in Europe 2025</span>")
        #),

        #nav_spacer(),

        # button - link to JAMRAI website
        #nav_item(
        #    tags$a(
        #    tags$span(
        #        bsicons::bs_icon("cursor"),
        #        "JAMRAI"
        #    ),
        #    href = "https://eu-jamrai.eu/",
        #    target = "_blank"
        #    )
        #),

        # button - link to GitHub repo
        #nav_item(
        #    tags$a(
        #    tags$span(
        #        bsicons::bs_icon("file-earmark-code"), "Source code"
        #    ),
        #    href = "https://github.com/EU-JAMRAI-2-WP8-1/AMR-surveillance-dashboard",
        #    target = "_blank"
        #    )
        #),

        # button - triggers download of source data
        #nav_item(
        #    tags$a(
        #    tags$span(
        #        bsicons::bs_icon("file-earmark-arrow-down"), "Download data"
        #    ),
        #    href = "https://github.com/",
        #    target = "_blank"
        #    )
        #),

        # light/dark mode toggle
        #nav_item(
        #    input_dark_mode(id = "dark_mode", mode = NULL)
        #),

        #position = c("fixed-top")
    #),

    # spacer - prevents overlapping of header and navbar
    #div(
    #    class = "top-spacer"
    #),

    # Layout type
    sidebarLayout(

        position = "left",

        # side bar (filters)
        sidebarPanel(
            class = "sidebar-panel",
            width = 2,

            # logo
            #img(
            #    src = jamraiLogoHeaderRect,
            #    class = "jamrai-logo-header"
            #),

            #hr(
            #    class = "hr-filters-separator"
            #),

            tags$span(
                class = "reset-filters-wrapper",
                actionButton("resetFilters", "Reset filters", class = "btn btn-outline-primary", icon = icon("filter-circle-xmark"))
            ),

            accordion(

                accordion_panel(
                    "Sections",
                    #tags$button(
                    #    class = "info-button",
                    #    #title = sectionInfoText,
                    #    "?"
                    #),
                    #tags$span(
                    #    class = "info-sections",
                    #    sectionInfoText
                    #),
                    checkboxGroupInput(
                        inputId  = "sectionsSelection",
                        label    = "",
                        choices  = sectionList,
                        selected = sectionList,
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
                        choices  = sort(participatingCountries),
                        selected = participatingCountries,
                        inline   = FALSE,
                        width    = NULL
                    )
                ),
                hr(
                    class = "hr-filters-separator"
                ),
                accordion_panel(
                    "Culture material",
                    actionLink("selectAllCultureMaterials", "Select All"),
                    tags$button(
                        class = "info-button",
                        #title = cultureMaterialInfoText,
                        "?"
                    ),
                    tags$span(
                        class = "info-sections",
                        cultureMaterialInfoText
                    ),
                    checkboxGroupInput(
                        inputId  = "cultureMaterialsSelection",
                        label    = "",
                        choices  = cultureMaterialList,
                        selected = c(),
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
                    "Resistances",
                    actionLink("selectAllResistances", "Select All"),
                    tags$button(
                        class = "info-button",
                        #title = resistancesInfoText,
                        "?"
                    ),
                    tags$span(
                        class = "info-sections",
                        resistancesInfoText
                    ),
                    class = "country-filter-container",
                    checkboxGroupInput(
                        inputId  = "resistancesSelection",
                        label    = "",
                        choices  = resistanceList,
                        selected = c(),
                        inline   = FALSE,
                        width    = NULL
                    )
                    
                )
            ),
            tags$div(
                class = "sidebar-filler"
            )
        ),

        # main panel
        mainPanel(
            width = 10,

            # tabs
            tabsetPanel(

                # plots
                tabPanel(
                    tags$span(
                        #bsicons::bs_icon("bar-chart"),
                        bsicons::bs_icon("globe-europe-africa"),
                        tags$span(
                            class = "tab-text",
                            "Map" #"Dashboard"
                        )
                    ),
                    fluidRow(
                        
                        column(
                            width = 5,
                            selectInput(
                                inputId   = "questionSelection",
                                label     = "Select a question - use the filter panel to narrow the selection",
                                choices   = c("Participating countries", allShortTitles),
                                selected  = c("Participating countries"),
                                multiple  = FALSE,
                                selectize = FALSE,
                                width     = "99%",
                                size      = 1
                            ),
                            tags$div(
                                class = "",
                                plotOutput("dashboardPlot")
                            )
                        ),
                        column(
                            width = 7,
                            tags$div(
                                class = "",
                                plotlyOutput(outputId = "dashboardMap", width = "100%", height = "780px")
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
                        )#,
                        
                        #column(
                        #    width = 1,
                        #    tags$div(
                        #        class = ""
                        #    )
                        #)
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
                    DT::dataTableOutput("resultsTable"),
                    tags$span(
                        downloadButton("downloadData", "Download",class = "btn btn-outline-primary")
                    )
                ),

                # map
                tabPanel(
                    tags$span(
                        bsicons::bs_icon("speedometer2"),
                        tags$span(
                            class = "tab-text",
                            "Joint results"
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
                                    plotlyOutput(outputId = "scoresMap", width = "100%", height = "780px")
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
                            tags$div(
                                class = "map-info-container",
                                HTML("<i>See the \"Info\" tab for information about scores</i>"),
                                DT::dataTableOutput("scoresTable")

                            )
                        )
                        
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
                            tryCatch({
                                includeHTML("www/html/about.html")
                            }, error = function(e) {
                                HTML("<p>About information unavailable</p>")
                            }),
                            HTML("<br><br>"),
                            actionButton("showLegalModal", "Legal Information", class = "btn btn-outline-primary", icon = icon("scale-balanced"))
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
    #fluidRow(
    #    img(src=jamraiLogoHeaderRectWhite) %>% tagAppendAttributes(class="width-auto footer-image"),
    #    img(src=euLogoFundWhite) %>% tagAppendAttributes(class="width-auto footer-image")
    #) %>% tagAppendAttributes(class="footer-box")
))


## SERVER ##

server <- function(input, output, session) {

    ## OBSERVE ##

    # "Reset filters" button
    observeEvent(input$resetFilters, {
        updateCheckboxGroupInput(session, "sectionsSelection", choices = sectionList, selected = sectionList)
        updateCheckboxGroupInput(session, "countriesSelection", choices = participatingCountries, selected = participatingCountries)
        updateCheckboxGroupInput(session, "pathogensSelection", choices = pathogenList)
        updateCheckboxGroupInput(session, "resistancesSelection", choices = resistanceList)
        updateCheckboxGroupInput(session, "cultureMaterialsSelection", choices = cultureMaterialList)
    })

    # "Select all" button for countries
    observeEvent(input$selectAllCountries, {
        if ((length(participatingCountries) - length(input$countriesSelection)) < 2) {
            updateCheckboxGroupInput(session, "countriesSelection", choices = participatingCountries, selected = c())
        } else {
            updateCheckboxGroupInput(session, "countriesSelection", choices = participatingCountries, selected = participatingCountries)
        }
    })

    # "Select all" button for pathogens
    observeEvent(input$selectAllPathogens, {
        if ((length(pathogenList) - length(input$pathogensSelection)) < 2) {
            updateCheckboxGroupInput(session, "pathogensSelection", choices = pathogenList, selected = c())
        } else {
            updateCheckboxGroupInput(session, "pathogensSelection", choices = pathogenList, selected = pathogenList)
        }
    })

    # "Select all" button for resistances
    observeEvent(input$selectAllResistances, {
        if ((length(resistanceList) - length(input$resistancesSelection)) < 2) {
            updateCheckboxGroupInput(session, "resistancesSelection", choices = resistanceList)
        } else {
            updateCheckboxGroupInput(session, "resistancesSelection", choices = resistanceList, selected = resistanceList)
        }
    })

    # "Select all" button for culture materials
    observeEvent(input$selectAllCultureMaterials, {
        if ((length(cultureMaterialList) - length(input$cultureMaterialsSelection)) < 2) {
            updateCheckboxGroupInput(session, "cultureMaterialsSelection", choices = cultureMaterialList)
        } else {
            updateCheckboxGroupInput(session, "cultureMaterialsSelection", choices = cultureMaterialList, selected = cultureMaterialList)
        }
    })

    # Legal information modal
    observeEvent(input$showLegalModal, {
        showModal(modalDialog(
            title = "Legal Information",
            tryCatch({
                includeHTML("www/html/legal.html")
            }, error = function(e) {
                HTML("<p>Legal information unavailable</p>")
            }),
            easyClose = TRUE,
            footer = modalButton("Close")
        ))
    })

    # update the question selection list
    observeEvent(input$sectionsSelection, {
        updateSelectInput(session, "questionSelection", choices = c("Participating countries", activeQuestions()[[2]]), selected = c("Participating countries"))
    }, ignoreNULL = FALSE)

    observeEvent(input$pathogensSelection, {
        updateSelectInput(session, "questionSelection", choices = c("Participating countries", activeQuestions()[[2]]), selected = c("Participating countries"))
    }, ignoreNULL = FALSE)

    observeEvent(input$resistancesSelection, {
        updateSelectInput(session, "questionSelection", choices = c("Participating countries", activeQuestions()[[2]]), selected = c("Participating countries"))
    }, ignoreNULL = FALSE)

    observeEvent(input$cultureMaterialsSelection, {
        updateSelectInput(session, "questionSelection", choices = c("Participating countries", activeQuestions()[[2]]), selected = c("Participating countries"))
    }, ignoreNULL = FALSE)


    ## FUNCTIONS ##

    # Calculates country scores based on filters
    getCountryScores <- function() {

        ### this function returns a list
        #     - scores (ratios on 1) for each country (vector)
        #     - max scores (vector)
        #     - amount of questions active (numeric)

        # check sections: if none selected -> all zeros and stop
        if (length(input$sectionsSelection) == 0) { # 
            return(
                list(
                    rep(0, length(intersect(repliedCountries, input$countriesSelection))),
                    rep(0, length(intersect(repliedCountries, input$countriesSelection))),
                    0 
                )
            )
        }

        # initiate output vector
        countryScores <- rep(0, length(repliedCountries))
        countryMaxScores <- rep(0, length(repliedCountries))

        # initiate questions counter (counts based on coefficient, 1 Complex Table question = 1 question)
        activeQuestionsAmount <- 0

        # loop over questions
        for (column in 2:ncol(countryScoreTable)) { # skip col 1 = country name

            ## check if question must be taken or not ##

            # if question not in selected question list, skip it
            if (!(countryScoreTable[1, column] %in% activeQuestions()[[1]])) next

            activeQuestionsAmount <- activeQuestionsAmount + as.double(countryScoreTable[3, column])

            # loop over countries
            for (row in 1:(((nrow(countryScoreTable) - 3) / 2))) { # -3 -> ignore question title, tags and coefficient rows ; /2 because 2 lines per country (score + max score)
                # if country is not selected -> skip the country
                if (!(countryScoreTable[(row * 2) + 2, 1] %in% input$countriesSelection)) {
                    next
                }
                # if country taken, add score and max score to respective vectors
                countryScores[row] <- countryScores[row] + as.double(countryScoreTable[(row * 2) + 2, column])
                countryMaxScores[row] <- countryMaxScores[row] + as.double(countryScoreTable[(row * 2) + 3, column]) # line just below
            }
        }

        # init - create vectors for scores without unselected countries
        countryScoreRatios <- c()
        countryScoresClean <- c()
        countryMaxScoresClean <- c()
    
        # calculate score ratios + create vectors without unselected countries
        for (i in 1:length(repliedCountries)) {

            if (countryMaxScores[i] != 0) {
                countryScoreRatios <- c(countryScoreRatios, countryScores[i] / countryMaxScores[i])
                countryScoresClean <- c(countryScoresClean, countryScores[i])
                countryMaxScoresClean <- c(countryMaxScoresClean, countryMaxScores[i]/activeQuestionsAmount)
            }
        }

        # in case empty...
        if (length(countryScoreRatios) == 0) {
            countryScoreRatios <- rep(0, length(intersect(repliedCountries, input$countriesSelection)))
        }

        return(list(countryScoreRatios, countryMaxScoresClean))

    }

    createResultsTable <- function() {

        # returns a datafrme for the Survey Results tab
        
        # initiate vectors
        questions <- c()
        tags <- c()

        # get active questions (titles)
        activeQuestions <- activeQuestions()[[1]]

        # loop once over questions to retrieve question titles and tags
        for (question in surveyData) {

            if (question$title %in% activeQuestions) {
                questions <- c(questions, question$title)
                tags <- c(tags, toString(question$tags))
            }

        }

        # add columns to DF
        resultsTable <- data.frame("Question" = questions, "Tags" = tags)

        # loop over selected countries
        for (country in input$countriesSelection) {

            # (re)set country replies vector
            countryReplies <- c()

            # loop over questions
            for (question in surveyData) {

                if (question$title %in% activeQuestions) {

                    if (length(question$actual_answers[[country]]) == 0) {
                        countryReplies <- c(countryReplies, NA)

                    } else {
                        countryReplies <- c(countryReplies, toString(question$actual_answers[[country]])) # toString -> to convert Multiple Choice replies
                    }
                }
            }

            resultsTable[[country]] <- countryReplies
        }

        return(resultsTable)

    }

    getSingleQuestionReplies <- function() {
        # 

        # if selected question is "Participating countries", just return output
        if (input$questionSelection == "Participating countries"){
            return(
                list(
                    rep(1, length(intersect(repliedCountries, input$countriesSelection))),
                    colorSequence[1],
                    c("Yes"),
                    c(length(intersect(repliedCountries, input$countriesSelection)), 0)
                )
            )
        }

        # loop over questions to find it/them
        for (question in surveyData) {
            # check filters - rem.: the question selection is pre-filtered based on the selected "main" filters. Except for Complex Table sub-questions, which share the same short title
            if (
                (question[["short_title"]] == input$questionSelection) # check question selection
                & (question[["type"]] == "SingleChoice") # check type
            ) {

                # get possible answers
                possibleAnswers <- question[["possible_answers"]]

                # get country answers
                actualAnswers <- question[["actual_answers"]]

                # change replies to numeric matching color scale
                answersNumericReference <- (1:length(possibleAnswers))
                answersNumeric <- c()

                for (answer in names(actualAnswers)) {
                    if (!(answer %in% input$countriesSelection)) next # skip if country not selected
                    i <- 1
                    for (possibleAnswer in names(possibleAnswers)) {
                        if (actualAnswers[[answer]] == possibleAnswer) {
                            answersNumeric <- c(answersNumeric, answersNumericReference[i])
                        }
                        i <- i + 1
                    }
                    
                }

                # get possible answer keys only
                possibleAnswerText <- c()
                possibleAnswerOccurences <- c()
                for (possibleAnswer in names(possibleAnswers)) {
                    possibleAnswerText <- c(possibleAnswerText, possibleAnswer)

                    # get occurences of each possible reply
                    occurencesCounter <- 0
                    for (answer in names(actualAnswers)) {
                        if (!(answer %in% input$countriesSelection)) next # skip if country not selected
                        if (actualAnswers[[answer]] == possibleAnswer) {
                            occurencesCounter <- occurencesCounter + 1
                        }
                    }
                    possibleAnswerOccurences <- c(possibleAnswerOccurences, occurencesCounter)
                }

                # for map: create a custom discrete color scale
                if (length(possibleAnswerText) > 2) {
                    if (substr(possibleAnswerText[3], 1, 2) == "No") {
                        ## !! quick fix (to be replaced by better method) - put No reply in red by using color sscale where colors 2 and 3 are reverted
                        customColorScale <- alternativeColorSequence[1:length(possibleAnswerText)]
                    }
                    else {
                        customColorScale <- colorSequence[1:length(possibleAnswerText)]
                    }
                } else {
                    customColorScale <- colorSequence[1:length(possibleAnswerText)]
                }

                # convert to percentage
                possibleAnswerPercentReplied = (possibleAnswerOccurences/length(intersect(input$countriesSelection, repliedCountries)) * 100)
                
                return(
                    list(
                        answersNumeric,
                        customColorScale,
                        possibleAnswerText,
                        possibleAnswerPercentReplied
                    )
                )

            }

            else if ((question[["short_title"]] == input$questionSelection) & (question[["type"]] == "MultipleChoice")) {

                # get possible answers
                possibleAnswers <- question[["possible_answers"]]

                # get country answers and stack them in a single vector
                actualAnswers <- question[["actual_answers"]]
                allActualAnswers <- unlist(actualAnswers, use.names = FALSE)

                # loop over possible answers and get occurence of each
                possibleAnswerOccurences <- c()
                possibleAnswerText <- c()
                for (possibleAnswer in names(possibleAnswers)) {
                    possibleAnswerText <- c(possibleAnswerText, possibleAnswer)
                    possibleAnswerOccurences <- c(possibleAnswerOccurences, sum(allActualAnswers == possibleAnswer))
                }

                possibleAnswerPercentReplied = (possibleAnswerOccurences/length(intersect(input$countriesSelection, repliedCountries)) * 100)
                
                return(
                    list(
                        answersNumeric = NULL, # not used for MultipleChoice, only for the map
                        customColorScale = colorSequence[1:length(possibleAnswerText)],
                        possibleAnswerText,
                        possibleAnswerPercentReplied
                    )
                )
            }
        }

        return(
            list(
                answersNumeric = NULL,
                customColorScale = NULL,
                possibleAnswerText = NULL,
                possibleAnswerOccurences = NULL
            )
        )
        
    }

    getActiveQuestions <- function() {

        # used to filter the question selection list

        activeQuestionTitles <- c()
        activeQuestionShortTitles <- c()

        for (question in surveyData) {

            # skip section 0
            if ("Section 0" %in% question$tags) next

            # skip sections that are not selected
            if (length(intersect(input$sectionsSelection, question$tags)) == 0) next

            # skip free text questions
            if (question$type == "FreeText") next

            # check if question tags and active filters do match
            if (
                (length(input$pathogensSelection) != 0) & (length(intersect(input$pathogensSelection, question$tags)) == 0)
            ) next

            if (
                (length(input$resistancesSelection) != 0) & (length(intersect(input$resistancesSelection, question$tags)) == 0)
            ) next

            if (
                (length(input$cultureMaterialsSelection) != 0) & (length(intersect(input$cultureMaterialsSelection, question$tags)) == 0)
            ) next

            # question has to be taken -> append short title to the vector
            activeQuestionTitles <- c(activeQuestionTitles, question[["title"]])
            activeQuestionShortTitles <- c(activeQuestionShortTitles, question[["short_title"]])
            
        }

        return(list(activeQuestionTitles, activeQuestionShortTitles))

    }


    ## REACTIVE ##

    countryScores <- reactive({
        getCountryScores()
    })

    countryReplies <- reactive({
        getSingleQuestionReplies()
    })

    activeQuestions <- reactive({
        getActiveQuestions()
        # use activeQuestions()[[1]] for titles
        # use activeQuestions()[[2]] for short titles
    })

    getNonParticipatingCountries <- reactive({
        c(setdiff(repliedCountries, input$countriesSelection), nonParticipatingCountries)
    })

    getParticipatingCountries <- reactive({
        data.frame(
            "Country" = intersect(repliedCountries, input$countriesSelection),
            "Score" = round(countryScores()[[1]]* 100, 1),
            "Answered" = round((countryScores()[[2]] / (length(activeQuestions()[[1]]) / 100)), 1)
        )
    })


    ## OUTPUTS ##

    output$scoresMap <- renderPlotly({
        #if (input$dark_mode == "dark") {
        #    themeBgColor = "#1D1F21"
        #    themeFgColor = "#ffffff"
        #    #themeSoftGrey = 0.3
        #} else {
        #    themeBgColor = "#ffffff"
        #    themeFgColor = "#1D1F21"
        #    #themeSoftGrey = 0.7
        #}

        themeBgColor = "#ffffff"
        themeFgColor = "#1D1F21"

        #countryReplies %>%
        scoresMap <- plot_ly(
            #height = 800,
            #width = 800
        )

        scoresMap <- scoresMap %>% add_trace( # displays results
            type = 'choropleth',
            #featureidkey='properties.NAME_ENGL', # id added directly in source in geojson -> might be different from name_engl (ex: Slovakia / Slovak Republik)
            geojson = geojsonEurope,
            locations = intersect(repliedCountries, input$countriesSelection),
            z = countryScores()[[1]] * 100,
            zmin = 0,
            zmax = 100,
            text = intersect(repliedCountries, input$countriesSelection),
            hoverinfo = "text+z",
            #showlegend = TRUE,
            #autocolorscale = FALSE,
            showscale = TRUE,
            colors = c("#cc8888", "#d5b47f", "#dddd77", "#0fdbd5", "#008aab"),
            reversescale = FALSE,
            colorbar = list(
                #outlinecolor = rgba(0,0,0,0),
                outlinewidth = 0,
                thickness = 20, #default 30
                color = themeBgColor,
                tickcolor = themeFgColor,
                x = 0.05,
                y = 0.8,
                tickfont = list(
                    color = themeFgColor
                ),
                title = list(
                    text = "Score",
                    font = list(
                        color = themeFgColor
                    )
                )
            ),
            marker = list(
                line = list(
                    width = 1.4,
                    color = themeBgColor
                )
            )
        )

        scoresMap <- scoresMap %>% add_trace( # non-participating countries
            name = "Not participating",
            type = "choropleth",
            geojson = geojsonEurope,
            #featureidkey = 'properties.NAME_ENGL', # id added directly in source in geojson
            locations = getNonParticipatingCountries(),
            z = rep(0.7, length(getNonParticipatingCountries())),
            zmin = 0,
            zmax = 1,
            text = getNonParticipatingCountries(),
            hoverinfo = "text",
            showscale = FALSE,
            colorscale = "Greys",
            #colors = c("#aaaaaa", "#aaaaaa"), ## cannot use "colors" in both traces
            marker = list(
                line = list(
                    width = 1.4,
                    color = themeBgColor
                )
            )
        )

        scoresMap <- scoresMap %>% layout(
            geo = list(
                scope = "europe",
                showcountries = FALSE, # hide default map
                showframe = FALSE, # hide default map
                showland = FALSE, # hide default map
                #landcolor = "#cccccc", # inside countries
                #countrycolor = themeBgColor, # lines
                bgcolor = "rgba(0,0,0,0)", # bg color - inside map
                #coastlinecolor = "#fff",
                showcoastline = FALSE,
                projection = list(
                    scale = 1.7  # initial zoom
                ),
                center = list(
                    lat = 54,
                    lon = 14
                )
            ),
            paper_bgcolor = "rgba(0, 0, 0, 0)", # bg color - outside map (here transparent)
            margin = list(
                t = 32,
                r = 0,
                l = 0,
                b = 32
            ),
            dragmode = FALSE,
            autosize = TRUE
        )
    })

    output$scoresTable <- DT::renderDT(
        getParticipatingCountries(),
        rownames = FALSE,
        colnames = c("Score (/100)", "% Answered"),
        options=list(
            dom = 't',
            pageLength = 100
        )
    )

    output$resultsTable <- DT::renderDT(
        createResultsTable(),
        rownames = FALSE,
        #colnames = c(c("Questions", "Tags"), input$countriesSelection),
        options = list(
            autowidth = TRUE,
            scrollX = TRUE,
            pageLength = 10
            #columnDefs = list(
            #    list(targets=c(0), width='400')
            #)
        )
    )

    # 
    output$downloadData <- downloadHandler(
        #filename = paste0("amr_watch_europe-export-", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv"),
        filename = paste0("amr_watch_europe-export.csv"),
        content = function(file) {
            write.csv(createResultsTable(), file, row.names = FALSE)
        }
    )

    output$dashboardMap <- renderPlotly({

        #if (input$dark_mode == "dark") {
        #    themeBgColor = "#1D1F21"
        #    themeFgColor = "#ffffff"
        #} else {
        #    themeBgColor = "#ffffff"
        #    themeFgColor = "#1D1F21"
        #}

        themeBgColor = "#ffffff"
        themeFgColor = "#1D1F21"

        dashboardMap <- plot_ly(
            #height = 800,
            #width = 800
        )

        if (input$questionSelection == "Participating countries") {

            dashboardMap <- dashboardMap %>% add_trace( # displays results
                type = 'choropleth',
                #featureidkey = 'properties.NAME_ENGL', # id added directly in source in geojson -> might be different from name_engl (ex: Slovakia / Slovak Republik)
                geojson = geojsonEurope,
                locations = participationData$country,
                z = participationData$survey_participation,
                zmin = 1,
                zmax = 3,
                text = participationData$country,
                hoverinfo = "text",
                showlegend = FALSE,
                #autocolorscale = FALSE,
                showscale = FALSE,
                #colorscale = countryReplies()[[2]],#c("#cc8888", "#dddd77", "#0fdbd5"),
                reversescale = FALSE,
                colors = c(colorSequence[1], colorSequence[2], "#b3b3b3"),
                marker = list(
                    line = list(
                        width = 1,
                        color = themeBgColor
                    )
                )
            )
        }

        else if (input$questionSelection %in% multipleChoiceShortTitles){
            
            return(NULL)
            
        }
        
        else {

            dashboardMap <- dashboardMap %>% add_trace( # displays results
                type = 'choropleth',
                #featureidkey = 'properties.NAME_ENGL', # id added directly in source in geojson -> might be different from name_engl (ex: Slovakia / Slovak Republik)
                geojson = geojsonEurope,
                locations = intersect(repliedCountries, input$countriesSelection),
                z = countryReplies()[[1]],
                zmin = 1,
                zmax = length(countryReplies()[[2]]),
                text = intersect(repliedCountries, input$countriesSelection),
                hoverinfo = "text",
                showlegend = FALSE,
                #autocolorscale = FALSE,
                showscale = FALSE,
                #colorscale = countryReplies()[[2]],#c("#cc8888", "#dddd77", "#0fdbd5"),
                reversescale = FALSE,
                colors = countryReplies()[[2]],
                marker = list(
                    line = list(
                        width = 1,
                        color = themeBgColor
                    )
                )
            )

            dashboardMap <- dashboardMap %>% add_trace( # non-participating countries
                name = "Not participating",
                type = 'choropleth',
                geojson = geojsonEurope,
                #featureidkey = 'properties.NAME_ENGL', # id added directly in source in geojson
                locations = getNonParticipatingCountries(),
                z = rep(0.7, length(getNonParticipatingCountries())),
                zmin = 0,
                zmax = 1,
                text = getNonParticipatingCountries(),
                hoverinfo = "text",
                showscale = FALSE,
                colorscale = "Greys",
                #colors = c("#aaaaaa", "#aaaaaa"), ## cannot use "colors" in both traces
                marker = list(
                    line = list(
                        width = 1,
                        color = themeBgColor
                    )
                )
            )

        }

        dashboardMap <- dashboardMap %>% layout(
            geo = list(
                scope = "europe",
                showcountries = FALSE, # hide default map
                showframe = FALSE, # hide default map
                showland = FALSE, # hide default map
                #landcolor = "#cccccc", # inside countries
                #countrycolor = themeBgColor, # lines
                bgcolor = "rgba(0, 0, 0, 0)", # bg color - inside map (here transparent)
                #coastlinecolor = "#fff",
                showcoastline = FALSE,
                projection = list(
                    scale = 1.7  # initial zoom
                ),
                center = list(
                    lat = 54,
                    lon = 12
                )
            ),
            paper_bgcolor = "rgba(0, 0, 0, 0)", # bg color - outside map (here transparent)
            margin = list(
                t = 32,
                r = 0,
                l = 0,
                b = 32
            ),
            dragmode = FALSE,
            autosize = TRUE
        )

        
    })

    output$dashboardPlot <- renderPlot({

        if (input$questionSelection == "Participating countries"){

            dashboardPlot <- ggplot(
                data = participationDataOccurrences,
                aes(
                    x = reply,
                    y = occurences
                )
            ) +
            geom_bar(
                aes(x = factor(reply, level = rev(unique(reply))), y = occurences),
                stat = "identity",
                fill = c(colorSequence[1], colorSequence[2], "#b3b3b3"),
                width = 0.4
            )
        }
        
        else {

            dashboardPlot <- ggplot(
                data = data.frame(reply=countryReplies()[[3]], occurences=countryReplies()[[4]]),
                aes(
                    x = reply,
                    y = occurences
                )
            ) +
            geom_bar(
                aes(x = factor(reply, level = rev(unique(countryReplies()[[3]]))), y = occurences),
                stat = "identity",
                fill = countryReplies()[[2]],
                width = 0.4
            )
        }

        dashboardPlot <- dashboardPlot +
        scale_y_reverse() +
        coord_flip() +
        labs(
            x = "Replies", y = "% of selected countries"
        ) +
        scale_x_discrete(
            labels = function(x) {

                if (max(nchar(x)) <= 24) { # no need to split
                    return(x)
                }

                threshold <- min(c(ceiling(max(nchar(x))/3), 24)) # max 24 characters per line

                # split long labels into 2 or 3 lines (4 is too much for quesstions with many replies)
                for (i in 1:length(x)) {

                    split <- strsplit(x[i], " ")[[1]] # split by space

                    newLabel <- ""
                    thisRow <- ""
                    rowCount <- 1

                    for (j in 1:length(split)) {
                        newLabel <- paste0(newLabel, " ", split[j])
                        thisRow <- paste0(thisRow, " ", split[j])
                        if (nchar(thisRow) > threshold) {
                            rowCount <- rowCount + 1
                            if (rowCount > 3) { # max 3 lines
                                newLabel <- paste0(newLabel, "...")
                                break
                            }
                            newLabel <- paste0(newLabel, "\n") # new line
                            thisRow <- ""
                        }
                    }

                    x[i] <- newLabel

                }

                return(x)

            },
            position = "top"
        ) +
        theme(
            axis.title.y = element_blank(),       # y axis label (remove)
            axis.title.x = element_text(size=16), # x axis label
            axis.text.y = element_text(size=18),  # axis ticks
            axis.text.x = element_text(size=16, angle = 90, vjust = 0.5, hjust=1) # rotate 
        )

        dashboardPlot

    }, height = 700
    )

}

# run
shinyApp(ui = ui, server = server)
