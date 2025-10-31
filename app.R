
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
library(openxlsx)

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
pathogenList        <- c("E. coli", "K. pneumoniae", "P. aeruginosa", "A. baumannii", "S. aureus", "E. faecium/faecalis", "S. pneumoniae", "H. influenzae", "C. difficile", "Not pathogen related")
resistanceList      <- c("Carbapenem", "3rd-generation Cephalosporin", "Colistin", "Methicillin", "Vancomycin", "Penicillin", "Ampicillin", "Not resistance related")
cultureMaterialList <- c("Blood", "Urine", "Respiratory tract", "Soft tissue", "Screening", "Stool", "Not culture material related")

# info text
pathogensInfoText       <- "Select the pathogen(s) you want to filter by. You can also select 'Not pathogen related' to include questions that are not specific to any pathogen."
resistancesInfoText     <- "Select the resistance(s) you want to filter by. You can also select 'Not resistance related' to include questions that are not specific to any resistance."
cultureMaterialInfoText <- "Select the culture material(s) you want to filter by. You can also select 'Not culture material related' to include questions that are not specific to any culture material."

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

# Convert raw counts to percentages
participationDataOccurrences$occurences <- (participationDataOccurrences$occurences / sum(participationDataOccurrences$occurences)) * 100


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

    # JavaScript to update body class based on section selection
    tags$script(HTML("
        $(document).on('shiny:inputchanged', function(event) {
            if (event.name === 'sectionsSelection') {
                $('body').removeClass('section-1 section-2 section-3');
                if (event.value === 'National surveillance') {
                    $('body').addClass('section-1');
                } else if (event.value === 'National genomic surveillance') {
                    $('body').addClass('section-2');
                } else if (event.value === 'National guidance') {
                    $('body').addClass('section-3');
                }
            }
        });
        // Set initial class on page load
        $(document).ready(function() {
            $('body').addClass('section-1');
        });
    ")),

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
                    title = HTML('<i class="fa fa-globe accordion-icon accordion-icon-countries"></i> Countries'),
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
                    title = HTML('<i class="fa fa-flask accordion-icon accordion-icon-culture"></i> Culture material'),
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
                        selected = cultureMaterialList,
                        inline   = FALSE,
                        width    = NULL
                    )
                ),
                hr(
                    class = "hr-filters-separator"
                ),
                accordion_panel(
                    title = HTML('<i class="fa fa-bacteria accordion-icon accordion-icon-pathogens"></i> Pathogens'),
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
                        selected = pathogenList,
                        inline   = FALSE,
                        width    = NULL
                    )
                ),
                hr(
                    class = "hr-filters-separator"
                ),
                accordion_panel(
                    title = HTML('<i class="fa fa-triangle-exclamation accordion-icon accordion-icon-resistances"></i> Resistances'),
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
                        selected = resistanceList,
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

            # Sections filter - horizontal at top
            tags$div(
                class = "sections-wrapper",
                radioGroupButtons(
                    inputId  = "sectionsSelection",
                    label    = NULL,
                    choiceNames = list(
                        HTML('<i class="fa fa-eye"></i><span class="btn-text"> National surveillance</span>'),
                        HTML('<i class="fa fa-dna"></i><span class="btn-text"> National genomic surveillance</span>'),
                        HTML('<i class="fa fa-book-open"></i><span class="btn-text"> National guidance</span>')
                    ),
                    choiceValues = sectionList,
                    selected = sectionList[1],
                    individual = FALSE,
                    checkIcon = list(),
                    status = "primary"
                ),
                tags$a(
                    href = "https://github.com/EU-JAMRAI-2-WP8-1/AMR-surveillance-dashboard",
                    target = "_blank",
                    class = "github-link-btn",
                    tags$i(class = "fa fa-brands fa-github")
                )
            ),

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
                            selectizeInput(
                                inputId   = "questionSelection",
                                label     = "Use the left filter panel to narrow the question selection, then select a question here:",
                                choices   = c("Participating countries", allShortTitles),
                                selected  = c("Participating countries"),
                                multiple  = FALSE,
                                width     = "99%",
                                options   = list(
                                    render = I("{
                                        option: function(item, escape) {
                                            return '<div>' + item.label + '</div>';
                                        },
                                        item: function(item, escape) {
                                            return '<div>' + item.label + '</div>';
                                        }
                                    }")
                                )
                            ),
                            uiOutput("multipleChoiceAnswerSelector"),
                            tags$div(
                                class = "",
                                uiOutput("dashboardPlotUI")
                            )
                        ),
                        column(
                            width = 7,
                            tags$div(
                                class = "full-question-title-box",
                                uiOutput("fullQuestionTitle")
                            ),
                            tags$div(
                                class = "dashboard-map-box",
                                plotlyOutput(outputId = "dashboardMap", width = "100%", height = "780px")
                            ),
                            tags$div(
                                class = "map-source-text medium-grey-text",
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
                    uiOutput("noQuestionsMessage"),
                    DT::dataTableOutput("resultsTable"),
                    tags$div(
                        style = "margin-top: 10px;",
                        downloadButton("downloadDataCSV", "Download CSV", class = "btn btn-outline-primary"),
                        downloadButton("downloadDataExcel", "Download Excel", class = "btn btn-outline-primary", style = "margin-left: 10px;")
                    )
                ),

                # map - Joint results tab (commented out for beta release)
                # tabPanel(
                #     tags$span(
                #         bsicons::bs_icon("speedometer2"),
                #         tags$span(
                #             class = "tab-text",
                #             "Joint results"
                #         )
                #     ),
                #
                #     fluidRow(
                #
                #         class = "map-tab-container",
                #
                #         column(
                #             width = 9,
                #             tags$div(
                #                 class = "map-and-source-container",
                #                 tags$div(
                #                     class = "map-container",
                #                     plotlyOutput(outputId = "scoresMap", width = "100%", height = "780px")
                #                 ),
                #                 tags$div(
                #                     class = "map-source-text medium-grey-text",
                #                     "Map source: ",
                #                     tags$a(
                #                         href="https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries",
                #                         "Eurostat",
                #                         target = "_blank"
                #                     )
                #                 )
                #             ),
                #         ),
                #
                #         column(
                #             width = 3,
                #             tags$div(
                #                 class = "map-info-container",
                #                 HTML("<i>See the \"Info\" tab for information about scores</i>"),
                #                 DT::dataTableOutput("scoresTable")
                #
                #             )
                #         )
                #
                #     )
                # ),

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
        updateRadioButtons(session, "sectionsSelection", selected = sectionList[1])
        updateCheckboxGroupInput(session, "countriesSelection", choices = participatingCountries, selected = participatingCountries)
        updateCheckboxGroupInput(session, "pathogensSelection", choices = pathogenList, selected = pathogenList)
        updateCheckboxGroupInput(session, "resistancesSelection", choices = resistanceList, selected = resistanceList)
        updateCheckboxGroupInput(session, "cultureMaterialsSelection", choices = cultureMaterialList, selected = cultureMaterialList)
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
            updateCheckboxGroupInput(session, "resistancesSelection", choices = resistanceList, selected = c())
        } else {
            updateCheckboxGroupInput(session, "resistancesSelection", choices = resistanceList, selected = resistanceList)
        }
    })

    # "Select all" button for culture materials
    observeEvent(input$selectAllCultureMaterials, {
        if ((length(cultureMaterialList) - length(input$cultureMaterialsSelection)) < 2) {
            updateCheckboxGroupInput(session, "cultureMaterialsSelection", choices = cultureMaterialList, selected = c())
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
        # Create named vector: names = HTML with badges (for display), values = short titles (for selection)
        choicesWithBadges <- setNames(c("Participating countries", activeQuestions()[[2]]), c("Participating countries", activeQuestions()[[3]]))
        updateSelectizeInput(session, "questionSelection", choices = choicesWithBadges, selected = c("Participating countries"))
    }, ignoreNULL = FALSE)

    observeEvent(input$pathogensSelection, {
        choicesWithBadges <- setNames(c("Participating countries", activeQuestions()[[2]]), c("Participating countries", activeQuestions()[[3]]))
        updateSelectizeInput(session, "questionSelection", choices = choicesWithBadges, selected = c("Participating countries"))
    }, ignoreNULL = FALSE)

    observeEvent(input$resistancesSelection, {
        choicesWithBadges <- setNames(c("Participating countries", activeQuestions()[[2]]), c("Participating countries", activeQuestions()[[3]]))
        updateSelectizeInput(session, "questionSelection", choices = choicesWithBadges, selected = c("Participating countries"))
    }, ignoreNULL = FALSE)

    observeEvent(input$cultureMaterialsSelection, {
        choicesWithBadges <- setNames(c("Participating countries", activeQuestions()[[2]]), c("Participating countries", activeQuestions()[[3]]))
        updateSelectizeInput(session, "questionSelection", choices = choicesWithBadges, selected = c("Participating countries"))
    }, ignoreNULL = FALSE)

    # Remove country from selection when cross button is clicked
    observeEvent(input$remove_country, {
        countryToRemove <- input$remove_country$country
        if (!is.null(countryToRemove) && countryToRemove %in% input$countriesSelection) {
            updatedSelection <- setdiff(input$countriesSelection, countryToRemove)
            updateCheckboxGroupInput(session, "countriesSelection", selected = updatedSelection)
        }
    })

    # Dynamic UI for the plot with adjusted height
    output$dashboardPlotUI <- renderUI({
        if (input$questionSelection %in% multipleChoiceShortTitles) {
            # Smaller height when selection box is present
            plotlyOutput("dashboardPlot", height = "520px")
        } else {
            # Normal height when no selection box
            plotlyOutput("dashboardPlot", height = "700px")
        }
    })

    # Display full question title above the map
    output$fullQuestionTitle <- renderUI({
        if (input$questionSelection == "Participating countries") {
            tags$div(
                class = "question-title-text",
                tags$strong("Question: "),
                "Which countries participated in the survey?"
            )
        } else {
            # Find the full title for the selected short title
            fullTitle <- NULL
            for (question in surveyData) {
                if (question$short_title == input$questionSelection) {
                    fullTitle <- question$title
                    break
                }
            }

            if (!is.null(fullTitle)) {
                tags$div(
                    class = "question-title-text",
                    tags$strong("Question: "),
                    fullTitle
                )
            }
        }
    })

    # Dynamic UI for multiple-choice answer selection
    output$multipleChoiceAnswerSelector <- renderUI({
        if (input$questionSelection %in% multipleChoiceShortTitles) {
            # Get possible answers for this multiple-choice question
            possibleAnswers <- c()
            for (question in surveyData) {
                if (question$short_title == input$questionSelection && question$type == "MultipleChoice") {
                    possibleAnswers <- names(question$possible_answers)
                    break
                }
            }

            if (length(possibleAnswers) > 0) {
                tags$div(
                    style = "margin-top: 10px; margin-bottom: 15px; background-color: #f5f5f5; padding: 15px; border-radius: 4px;",
                    tags$label(
                        style = "font-weight: 500; margin-bottom: 8px; display: block;",
                        "Multiple Selection Question: Choose an answer option to view on the map:"
                    ),
                    selectInput(
                        inputId = "selectedAnswer",
                        label = NULL,
                        choices = possibleAnswers,
                        selected = possibleAnswers[1],
                        width = "100%"
                    ),
                    tags$div(
                        style = "margin-top: 12px; display: flex; justify-content: center; align-items: center; gap: 20px; font-size: 1em;",
                        tags$span(
                            style = "display: flex; align-items: center; gap: 8px;",
                            tags$span(
                                style = "display: inline-block; width: 24px; height: 24px; background-color: #0fdbd5;"
                            ),
                            tags$span("Selected")
                        ),
                        tags$span(
                            style = "display: flex; align-items: center; gap: 8px;",
                            tags$span(
                                style = "display: inline-block; width: 24px; height: 24px; background-color: #ff6f61;"
                            ),
                            tags$span("Not selected")
                        )
                    )
                )
            }
        }
    })


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

        # Helper function to get badge color (only for sections)
        getBadgeColor <- function(tag) {
            # Sections - use specific colors
            if (tag == "National surveillance") return("#2a9d8f")
            if (tag == "National genomic surveillance") return("#d4a843")
            if (tag == "National guidance") return("#cc8888")
            # All other filters - gray
            return("#888888")
        }

        # Helper function to get badge icon
        getBadgeIcon <- function(tag) {
            # Sections
            if (tag %in% sectionList) return("")
            # Culture material
            if (tag %in% cultureMaterialList) return('<i class="fa fa-flask"></i> ')
            # Pathogens
            if (tag %in% pathogenList) return('<i class="fa fa-bacteria"></i> ')
            # Resistances
            if (tag %in% resistanceList) return('<i class="fa fa-triangle-exclamation"></i> ')
            # Default
            return("")
        }

        formatQuestionWithBadges <- function(questionTitle, tagsString) {
            tagsList <- strsplit(tagsString, ", ")[[1]]
            # Remove "Section 0" and "Not ... related" tags (don't display these)
            tagsList <- tagsList[tagsList != "Section 0"]
            tagsList <- tagsList[!grepl("^Not .* related$", tagsList)]

            if (length(tagsList) == 0) {
                return(questionTitle)
            }

            # Create badge HTML for each tag
            badgesHtml <- paste(
                sapply(tagsList, function(tag) {
                    color <- getBadgeColor(tag)
                    icon <- getBadgeIcon(tag)
                    paste0('<span style="display: inline-block; background-color: ', color,
                           '; color: white; padding: 2px 8px; margin: 2px; border-radius: 3px; font-size: 0.75em; font-weight: 500;">',
                           icon, tag, '</span>')
                }),
                collapse = " "
            )

            # Combine badges and question on separate lines
            return(paste0(badgesHtml, '<br/>', questionTitle))
        }

        # initiate vectors
        questionsWithBadges <- c()

        # get active questions (titles)
        activeQuestions <- activeQuestions()[[1]]

        # loop once over questions to retrieve question titles and format with tags
        for (question in surveyData) {

            if (question$title %in% activeQuestions) {
                formattedQuestion <- formatQuestionWithBadges(question$title, toString(question$tags))
                questionsWithBadges <- c(questionsWithBadges, formattedQuestion)
            }

        }

        # add columns to DF
        resultsTable <- data.frame("Question" = questionsWithBadges, stringsAsFactors = FALSE)

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

                # For the MAP: create binary data for selected answer
                answersNumeric <- NULL
                if (!is.null(input$selectedAnswer)) {
                    answersNumeric <- c()
                    for (country in intersect(repliedCountries, input$countriesSelection)) {
                        if (country %in% names(actualAnswers)) {
                            countryAnswers <- actualAnswers[[country]]
                            # Check if the selected answer is in this country's list of answers
                            if (input$selectedAnswer %in% countryAnswers) {
                                answersNumeric <- c(answersNumeric, 1) # Selected
                            } else {
                                answersNumeric <- c(answersNumeric, 2) # Not selected
                            }
                        } else {
                            answersNumeric <- c(answersNumeric, 2) # Not selected
                        }
                    }
                }

                # Use light blue for selected, red for not selected (for MAP only)
                customColorScaleMap <- c("#0fdbd5", "#ff6f61")

                return(
                    list(
                        answersNumeric,  # For map (binary: selected/not selected)
                        customColorScaleMap,  # For map
                        possibleAnswerText,  # For bar chart (all answers)
                        possibleAnswerPercentReplied,  # For bar chart (all percentages)
                        colorSequence[1:length(possibleAnswerText)]  # For bar chart colors
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

        # Helper function to get badge color (only for sections)
        getBadgeColor <- function(tag) {
            # Sections - use specific colors
            if (tag == "National surveillance") return("#2a9d8f")
            if (tag == "National genomic surveillance") return("#d4a843")
            if (tag == "National guidance") return("#cc8888")
            # All other filters - gray
            return("#888888")
        }

        # Helper function to get badge icon
        getBadgeIcon <- function(tag) {
            # Sections
            if (tag %in% sectionList) return("")
            # Culture material
            if (tag %in% cultureMaterialList) return('<i class="fa fa-flask"></i> ')
            # Pathogens
            if (tag %in% pathogenList) return('<i class="fa fa-bacteria"></i> ')
            # Resistances
            if (tag %in% resistanceList) return('<i class="fa fa-triangle-exclamation"></i> ')
            # Default
            return("")
        }

        formatShortTitleWithBadges <- function(shortTitle, tagsString) {
            tagsList <- strsplit(tagsString, ", ")[[1]]
            # Remove "Section 0" and "Not ... related" tags (don't display these)
            tagsList <- tagsList[tagsList != "Section 0"]
            tagsList <- tagsList[!grepl("^Not .* related$", tagsList)]

            if (length(tagsList) == 0) {
                return(shortTitle)
            }

            badgesHtml <- paste(
                sapply(tagsList, function(tag) {
                    color <- getBadgeColor(tag)
                    icon <- getBadgeIcon(tag)
                    paste0('<span style="display: inline-block; background-color: ', color,
                           '; color: white; padding: 1px 6px; margin-right: 4px; border-radius: 3px; font-size: 0.7em; font-weight: 500;">',
                           icon, tag, '</span>')
                }),
                collapse = ""
            )

            return(paste0(badgesHtml, '<br/>', shortTitle))
        }

        activeQuestionTitles <- c()
        activeQuestionShortTitles <- c()
        activeQuestionShortTitlesWithBadges <- c()

        for (question in surveyData) {

            # skip section 0
            if ("Section 0" %in% question$tags) next

            # skip sections that are not selected
            if (length(intersect(input$sectionsSelection, question$tags)) == 0) next

            # skip free text questions
            if (question$type == "FreeText") next

            # check if question tags and active filters do match
            if (length(intersect(input$pathogensSelection, question$tags)) == 0) next

            if (length(intersect(input$resistancesSelection, question$tags)) == 0) next

            if (length(intersect(input$cultureMaterialsSelection, question$tags)) == 0) next

            # question has to be taken -> append titles to vectors
            activeQuestionTitles <- c(activeQuestionTitles, question[["title"]])
            activeQuestionShortTitles <- c(activeQuestionShortTitles, question[["short_title"]])

            # Add short title with badges
            formattedShortTitle <- formatShortTitleWithBadges(question[["short_title"]], toString(question$tags))
            activeQuestionShortTitlesWithBadges <- c(activeQuestionShortTitlesWithBadges, formattedShortTitle)

        }

        return(list(activeQuestionTitles, activeQuestionShortTitles, activeQuestionShortTitlesWithBadges))

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
            "Answered" = round(countryScores()[[2]] * 100, 1)
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
        options = list(
            dom = 't',
            pageLength = 100
        )
    )

    output$noQuestionsMessage <- renderUI({
        if (length(activeQuestions()[[1]]) == 0) {
            tags$div(
                style = "padding: 40px; text-align: center; color: #666; font-size: 1.1em;",
                tags$i(class = "fa fa-info-circle", style = "font-size: 2em; margin-bottom: 10px;"),
                tags$br(),
                "No questions match the active filters.",
                tags$br(),
                tags$span(style = "font-size: 0.9em;", "Try adjusting your filter selections.")
            )
        }
    })

    output$resultsTable <- DT::renderDT(
        createResultsTable(),
        rownames = FALSE,
        escape = FALSE,  # Allow HTML rendering in cells
        #colnames = c(c("Questions", "Tags"), input$countriesSelection),
        extensions = c('ColReorder', 'FixedColumns'),
        options = list(
            autowidth = TRUE,
            scrollX = TRUE,
            pageLength = 10,
            lengthMenu = list(c(5, 10, 20), c(5, 10, 20)),
            ordering = FALSE,  # Disable row sorting
            colReorder = list(
                realtime = TRUE,  # Show changes as you drag
                fixedColumnsLeft = 1  # Keep first column (Question) fixed for reordering
            ),
            fixedColumns = list(
                left = 0  # Start with no fixed columns
            ),
            headerCallback = JS(
                "function(thead, data, start, end, display) {",
                "  var table = this.api();",
                "  var th0 = $(thead).find('th').eq(0);",
                "  ",
                "  // Only add lock button if not already present",
                "  if (th0.find('.lock-btn').length === 0) {",
                "    var currentText = th0.text();",
                "    th0.html('<div style=\"display: flex; justify-content: space-between; align-items: center;\"><span>' + currentText + '</span><span class=\"lock-btn\" style=\"cursor: pointer;\" title=\"Click to lock/unlock this column when scrolling horizontally\"><i class=\"fa fa-lock-open\" style=\"font-size: 0.9em;\"></i></span></div>');",
                "    // Colors are now handled by CSS based on selected section",
                "    ",
                "    // Add click handler for lock button",
                "    th0.find('.lock-btn').on('click', function(e) {",
                "      e.stopPropagation();",
                "      var icon = $(this).find('i');",
                "      var isLocked = icon.hasClass('fa-lock');",
                "      ",
                "      if (isLocked) {",
                "        // Unlock",
                "        icon.removeClass('fa-lock').addClass('fa-lock-open');",
                "        $(this).attr('title', 'Click to lock/unlock this column when scrolling horizontally');",
                "        table.fixedColumns().left(0);",
                "      } else {",
                "        // Lock",
                "        icon.removeClass('fa-lock-open').addClass('fa-lock');",
                "        $(this).attr('title', 'Click to lock/unlock this column when scrolling horizontally');",
                "        table.fixedColumns().left(1);",
                "      }",
                "    });",
                "  }",
                "  ",
                "  // Style other columns (country columns)",
                "  $(thead).find('th:gt(0)').each(function(index) {",
                "    // Only add remove button if not already present",
                "    if ($(this).find('.remove-country-btn').length === 0) {",
                "      var currentText = $(this).text();",
                "      $(this).html('<div style=\"display: flex; justify-content: space-between; align-items: center;\"><span>' + currentText + '</span><span style=\"display: flex; align-items: center; gap: 6px;\"><i class=\"fa fa-arrows-h\" style=\"font-size: 0.8em; cursor: move;\"></i><span class=\"remove-country-btn\" style=\"cursor: pointer; font-weight: bold; font-size: 1.3em;\" title=\"Remove this country from selection\">&times;</span></span></div>');",
                "      ",
                "      // Add click handler for remove button",
                "      $(this).find('.remove-country-btn').on('click', function(e) {",
                "        e.stopPropagation();",
                "        var countryName = currentText;",
                "        Shiny.setInputValue('remove_country', {country: countryName, timestamp: Date.now()});",
                "      });",
                "    }",
                "    // Colors are now handled by CSS based on selected section",
                "    $(this).css({'cursor': 'move'});",
                "  });",
                "}"
            ),
            initComplete = JS(
                "function(settings, json) {",
                "  var wrapper = $(this.api().table().container());",
                "  var scroll = wrapper.find('.dataTables_scrollBody');",
                "  if (scroll.length > 0) {",
                "    var topScroll = $('<div class=\"dataTables_scrollHead_top\" style=\"overflow-x: auto; overflow-y: hidden;\"><div style=\"height: 1px; width: ' + scroll[0].scrollWidth + 'px;\"></div></div>');",
                "    topScroll.insertBefore(wrapper.find('.dataTables_scroll'));",
                "    topScroll.on('scroll', function() {",
                "      scroll.scrollLeft($(this).scrollLeft());",
                "    });",
                "    scroll.on('scroll', function() {",
                "      topScroll.scrollLeft($(this).scrollLeft());",
                "    });",
                "  }",
                "}"
            ),
            columnDefs = list(
                list(
                    targets = 0,
                    className = 'first-column-cell'
                )
            )
        )
    )

    # CSV download
    output$downloadDataCSV <- downloadHandler(
        filename = function() {
            paste0("amr_watch_europe-export-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
            write.csv(createResultsTable(), file, row.names = FALSE)
        }
    )

    # Excel download
    output$downloadDataExcel <- downloadHandler(
        filename = function() {
            paste0("amr_watch_europe-export-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
        },
        content = function(file) {
            write.xlsx(createResultsTable(), file, rowNames = FALSE)
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

        if (input$questionSelection == "Participating countries") {

            dashboardMap <- plot_ly(
                #height = 800,
                #width = 800
            )

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

            # Check if answer is selected (handles NULL case)
            if (is.null(countryReplies()[[1]]) || is.null(countryReplies()[[2]])) {
                return(NULL)
            }

            # Multiple choice questions - show map for selected answer option
            dashboardMap <- plot_ly(
                #height = 800,
                #width = 800
            )

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
        
        else {
            
            dashboardMap <- plot_ly(
                #height = 800,
                #width = 800
            )

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
        ) %>%
        config(
            displaylogo = FALSE
        )


    })

    output$dashboardPlot <- renderPlotly({

        if (input$questionSelection == "Participating countries"){

            # Calculate dynamic height based on number of bars
            numBars <- nrow(participationDataOccurrences)
            plotHeight <- max(400, numBars * 90 + 150)  # min 400px, 90px per bar + 150px for margins

            dashboardPlot <- ggplot(
                data = participationDataOccurrences,
                aes(
                    x = reply,
                    y = occurences,
                    text = paste0(reply, ": ", sprintf("%.2f", occurences), "%")
                )
            ) +
            geom_bar(
                aes(x = factor(reply, level = rev(unique(reply))), y = occurences),
                stat = "identity",
                fill = c(colorSequence[1], colorSequence[2], "#b3b3b3"),
                width = 0.5
            ) +
            # WHITE LABELS INSIDE BARS: For values >= 20%
            # Formula: y = occurences - 8.6
            # Fixed offset of 8.6 units from the left edge of the bar
            # With fixed 0-100 scale, this works consistently for all bar sizes >= 20%
            # hjust = 0: Left-align the text (text starts at the y position)
            geom_text(
                data = participationDataOccurrences %>% filter(occurences >= 20),
                aes(x = factor(reply, level = rev(unique(reply))), y = occurences - 8.6, label = sprintf("%.2f", occurences)),
                hjust = 0,
                size = 5.5,
                color = "white"
            ) +
            # BLACK LABELS OUTSIDE BARS: For values < 20%
            # Formula: y = occurences + 7.5
            # Fixed offset of 7.5 units to the right (outside bar)
            # For small bars where white label won't fit inside
            # hjust = 0: Left-align the text
            geom_text(
                data = participationDataOccurrences %>% filter(occurences < 20),
                aes(x = factor(reply, level = rev(unique(reply))), y = occurences + 7.5, label = sprintf("%.2f", occurences)),
                hjust = 0,
                size = 5.5,
                color = "black"
            )
        }

        else {
            # Check if data is available (handles NULL case for multiple-choice questions)
            if (is.null(countryReplies()[[3]]) || is.null(countryReplies()[[4]])) {
                return(NULL)
            }

            # Calculate dynamic height based on number of bars
            numBars <- length(countryReplies()[[3]])
            plotHeight <- max(400, numBars * 90 + 150)  # min 400px, 90px per bar + 150px for margins

            # For multiple-choice questions, use light grey; for others, use custom colors
            barColors <- if (input$questionSelection %in% multipleChoiceShortTitles && length(countryReplies()) >= 5) {
                rep("#008aab", length(countryReplies()[[3]]))
            } else {
                countryReplies()[[2]]
            }

            dashboardPlot <- ggplot(
                data = data.frame(reply=countryReplies()[[3]], occurences=countryReplies()[[4]]),
                aes(
                    x = reply,
                    y = occurences,
                    text = paste0(reply, ": ", sprintf("%.2f", occurences), "%")
                )
            ) +
            geom_bar(
                aes(x = factor(reply, level = rev(unique(countryReplies()[[3]]))), y = occurences),
                stat = "identity",
                fill = barColors,
                width = 0.5
            ) +
            # WHITE LABELS INSIDE BARS: For values >= 20%
            # Formula: y = occurences - 8.6
            # Fixed offset of 8.6 units from the left edge of the bar
            # With fixed 0-100 scale, this works consistently for all bar sizes >= 20%
            # hjust = 0: Left-align the text (text starts at the y position)
            geom_text(
                data = data.frame(reply=countryReplies()[[3]], occurences=countryReplies()[[4]]) %>% filter(occurences >= 20),
                aes(x = factor(reply, level = rev(unique(countryReplies()[[3]]))), y = occurences - 8.6, label = sprintf("%.2f", occurences)),
                hjust = 0,
                size = 5.5,
                color = "white"
            ) +
            # BLACK LABELS OUTSIDE BARS: For values < 20%
            # Formula: y = occurences + 7.5
            # Fixed offset of 7.5 units to the right (outside bar)
            # For small bars where white label won't fit inside
            # hjust = 0: Left-align the text
            geom_text(
                data = data.frame(reply=countryReplies()[[3]], occurences=countryReplies()[[4]]) %>% filter(occurences < 20),
                aes(x = factor(reply, level = rev(unique(countryReplies()[[3]]))), y = occurences + 7.5, label = sprintf("%.2f", occurences)),
                hjust = 0,
                size = 5.5,
                color = "black"
            )
        }

        # Set axis label based on question type
        yAxisLabel <- if (input$questionSelection == "Participating countries") {
            "% of European countries"
        } else {
            "% of selected countries"
        }

        dashboardPlot <- dashboardPlot +
        scale_y_reverse(limits = c(100, 0)) +
        coord_flip() +
        labs(
            x = "Replies", y = yAxisLabel
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
        theme_minimal() +
        theme(
            axis.title.y = element_blank(),       # y axis label (remove)
            axis.title.x = element_text(size=16, margin = margin(t = 20), family = "Arial"), # x axis label with increased top margin
            axis.text.y = element_text(size=14, hjust=0, family = "Arial"),  # axis ticks - left align, reduced font size
            axis.text.x = element_text(size=16, angle = 90, vjust = 0.5, hjust=1, family = "Arial") # rotate
        )

        ggplotly(dashboardPlot, height = plotHeight, tooltip = "text") %>%
            layout(
                yaxis = list(side = "right"),
                font = list(family = "Arial, sans-serif"),
                autosize = FALSE,
                margin = list(l = 50, r = 250, t = 50, b = 100)
            ) %>%
            config(
                displaylogo = FALSE,
                responsive = TRUE
            )

    })

}

# run
shinyApp(ui = ui, server = server)
