
## SETUP ##

# import libraries
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(rjson)
library(plotly)
library(bslib)
library(thematic)
library(DT)
library(openxlsx)
library(ggiraph)
library(systemfonts)
library(glue)
library(patchwork)
library(tidyverse)

# Source all modules from R/
# NOTE: only needed when running via Docker (Rscript app.R). When launching
# through shiny::runApp(), Shiny auto-sources the R/ directory automatically.
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# Load SMTP credentials from environment variables
SMTP_SERVER <- Sys.getenv("SMTP_SERVER")
SMTP_PORT <- Sys.getenv("SMTP_PORT")
SMTP_USERNAME <- Sys.getenv("SMTP_USERNAME")
SMTP_PASSWORD <- Sys.getenv("SMTP_PASSWORD")
SENDER_EMAIL <- Sys.getenv("SENDER_EMAIL")
RECIPIENT_EMAIL <- Sys.getenv("RECIPIENT_EMAIL")

# Check if email sending is configured
EMAIL_ENABLED <- (SMTP_SERVER != "" && SMTP_USERNAME != "" && SMTP_PASSWORD != "" && SENDER_EMAIL != "" && RECIPIENT_EMAIL != "")

# Add resource directory to server
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

# Import Europe polygons
geojsonEurope <- tryCatch({
  rjson::fromJSON(file = file.path("data/CNTR_RG_60M_2024_4326-modified.geojson")) # rjson
}, error = function(e) {
  showNotification("Error loading map data", type = "error")
  return(list(features = list())) # Return empty structure
})

## source: https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries (modified to include only european countries)

# Import survey questions and replies from JSON
surveyDataFile <- file.path("data/OUT_questions_and_replies.json")
surveyData <- tryCatch({
  rjson::fromJSON(paste(readLines(surveyDataFile), collapse="")) # rjson
  ##jsonlite::fromJSON(surveyDataFile) # jsonlite
}, error = function(e) {
  showNotification("Error loading survey data", type = "error")
  return(NULL)
})

# Import survey score table from CSV - set first column as row names
countryScoreTable <- tryCatch({
  read.csv("data/OUT_country_scores.csv", header=TRUE)
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
participatingCountries <- names(surveyData[[countryQuestionIndex]][["possible_answers"]])

# Countries that have replied
repliedCountries <- names(surveyData[[countryQuestionIndex]][["actual_answers"]])

# Not-participating countries (grey on the map)
nonParticipatingCountries <- setdiff(euroCountryList, repliedCountries)

# Filters : pathogens under surveillance / resistances / culture materials
sectionList         <- c("National surveillance", "National genomic surveillance", "National guidance") # order is reverted compared to the survey (3, 2, 1)
pathogenList        <- c("E. coli", "K. pneumoniae", "P. aeruginosa", "A. baumannii", "S. aureus", "E. faecium/faecalis", "S. pneumoniae", "H. influenzae", "C. difficile", "Not pathogen related")
resistanceList      <- c("Carbapenem", "3rd-generation Cephalosporin", "Colistin", "Methicillin", "Vancomycin", "Penicillin", "Ampicillin", "Not resistance related")
cultureMaterialList <- c("Blood/CSF", "Urine", "Respiratory tract", "Wound/tissue", "Stool", "Screening", "Not culture material related")

# No special display modification needed - just use the lists as-is
pathogenChoiceNames <- pathogenList
resistanceChoiceNames <- resistanceList
cultureMaterialChoiceNames <- cultureMaterialList

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
colorSequence <- c("#0fdbd5", "#df2e1a", "#f7c948", "#6a4c93", "#25c414", "#1982c4", "#e76f51", "#2a9d8f", "#f4a261", "#264653", "#8ecae6", "#ffb4a2", "#000000") # old red: #ff6f61

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

## Insight tab data ----
it1   <- readRDS("data/data_insighttab_1.rds")
it2   <- readRDS("data/data_insighttab_2.rds")
it2_2 <- readRDS("data/data_insighttab_2_2.rds")
it3   <- readRDS("data/data_insighttab_3.rds")



## USER INTERFACE ##

# user interface
ui <- shinyUI(fluidPage(
  
  # set theme
  theme = custom_theme,
  
  # enable shinyjs
  useShinyjs(),
  
  # import CSS
  includeCSS(file.path("www/css/style.css")),
  
  # import JS
  includeScript("www/js/script.js"),
  
  # add favicon
  tags$head(tags$link(rel="shortcut icon", href=file.path("www/favicons/jamrai_favicon_32x32.png"))),
  
  # Layout type
  sidebarLayout(
    
    position = "left",
    
    # side bar (filters)
    sidebarPanel(
      class = "sidebar-panel",
      width = 2,
      
      # logo
      tags$div(
        class = "sidebar-logo-wrapper",
        tags$img(
          src = "www/logos/Jamreye_primary-Colour-RGB.svg",
          class = "sidebar-logo",
          alt = "JAMREYE Logo"
        )
      ),
      
      tags$span(
        class = "reset-filters-wrapper",
        actionButton("showInstructions", "Instructions", class = "btn btn-outline-info", icon = icon("circle-info"))
      ),
      
      # Toggle between Graphics and Table views
      tags$div(
        class = "view-toggle-wrapper",
        radioGroupButtons(
          inputId = "viewToggle",
          label = NULL,
          choiceNames = list(
            HTML('<i class="fa fa-globe"></i><span class="btn-text"> Graphics</span>'),
            HTML('<i class="fa fa-table"></i><span class="btn-text"> Table</span>')
          ),
          choiceValues = c("graphics", "table"),
          selected = "graphics",
          individual = FALSE,
          checkIcon = list(),
          status = "primary"
        )
      ),
      
      mod_filters_ui("filters"),
      
      # Toggle between Dashboard and Insight views
      tags$div(
        class = "outer-toggle-wrapper",
        radioGroupButtons(
          inputId = "outerToggle",
          label = NULL,
          choiceNames = list(
            HTML('<i class="fa fa-chart-bar"></i><span class="btn-text"> Dashboard</span>'),
            HTML('<i class="fa fa-lightbulb"></i><span class="btn-text"> Insight</span>')
          ),
          choiceValues = c("dashboard", "insight"),
          selected = "dashboard",
          individual = FALSE,
          checkIcon = list(),
          status = "primary"
        )
      ),
      
      # Bottom buttons: Info, Legal, Contact
      tags$div(
        class = "sidebar-bottom-buttons",
        actionButton("showInfoModal", "Info", class = "btn btn-outline-secondary btn-sm", icon = icon("info-circle")),
        actionButton("showLegalModalSidebar", "Legal", class = "btn btn-outline-secondary btn-sm", icon = icon("scale-balanced")),
        actionButton("showContactModal", "Contact", class = "btn btn-outline-secondary btn-sm", icon = icon("envelope"))
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
        id = "outerTabs",
        
        # dashboard tab (contains graphics + table)
        tabPanel(
          "",
          value = "dashboard",
          tabsetPanel(
            id = "mainTabs",
            
            # map and bar chart
            tabPanel(
              "",
              value = "graphics",
              mod_dashboard_graphics_ui("graphics")
            ),

            # survey results (table)
            tabPanel(
              "",
              value = "table",
              mod_dashboard_table_ui("table")
            ),
            
            # map - Joint results tab (commented out for beta release)
            # tabPanel(
            #     tags$span(
            #         bsicons::bs_icon("speedometer2"),
            #         tags$span(class = "tab-text", "Joint results")
            #     ),
            #     mod_dashboard_joint_results_ui("joint_results")
            # ),
          ) # /mainTabs
        ), # /dashboard tabPanel
        
        # insight tab
        tabPanel(
          "",
          value = "insight",
          mod_insight_ui("insight")
        )
        
      ), # /outerTabs
    )
  ),
))


## SERVER ##

server <- function(input, output, session) {
  
  ## OBSERVE ##
  
  ## Modals ----
  setup_modals(input, output, session, email_config = list(
    enabled         = EMAIL_ENABLED,
    smtp_server     = SMTP_SERVER,
    smtp_port       = SMTP_PORT,
    smtp_username   = SMTP_USERNAME,
    smtp_password   = SMTP_PASSWORD,
    sender_email    = SENDER_EMAIL,
    recipient_email = RECIPIENT_EMAIL
  ))

  ## Filters module ----
  filters_mod <- mod_filters_server("filters")
  filters     <- filters_mod$filters
  filters_ns  <- NS("filters")

  # Outer toggle - switch between Dashboard and Insight tabs
  observeEvent(input$outerToggle, {
    if (input$outerToggle == "dashboard") {
      updateTabsetPanel(session, "outerTabs", selected = "dashboard")
      shinyjs::enable("viewToggle")
      shinyjs::enable(filters_ns("sectionsSelection"))
      shinyjs::enable(filters_ns("cultureMaterialsSelection"))
      shinyjs::enable(filters_ns("pathogensSelection"))
      shinyjs::enable(filters_ns("resistancesSelection"))
      shinyjs::enable(filters_ns("countriesSelection"))
      shinyjs::enable(filters_ns("resetFilters"))
      shinyjs::removeClass(selector = ".sidebar-panel", class = "filters-inactive")
    } else if (input$outerToggle == "insight") {
      updateTabsetPanel(session, "outerTabs", selected = "insight")
      shinyjs::disable("viewToggle")
      shinyjs::disable(filters_ns("sectionsSelection"))
      shinyjs::disable(filters_ns("cultureMaterialsSelection"))
      shinyjs::disable(filters_ns("pathogensSelection"))
      shinyjs::disable(filters_ns("resistancesSelection"))
      shinyjs::disable(filters_ns("countriesSelection"))
      shinyjs::disable(filters_ns("resetFilters"))
      shinyjs::addClass(selector = ".sidebar-panel", class = "filters-inactive")
    }
  })
  
  # View toggle - switch between Graphics and Table tabs
  observeEvent(input$viewToggle, {
    if (input$viewToggle == "graphics") {
      updateTabsetPanel(session, "mainTabs", selected = "graphics")
    } else if (input$viewToggle == "table") {
      updateTabsetPanel(session, "mainTabs", selected = "table")
    }
  })
  
  
  ## Dashboard modules ----
  activeQuestions <- reactive({ getActiveQuestions() })

  mod_dashboard_graphics_server("graphics", filters = filters, active_questions = activeQuestions)
  mod_dashboard_table_server("table",    filters = filters, active_questions = activeQuestions)
  
  # Remove country from selection when cross button is clicked
  observeEvent(input$remove_country, {
    countryToRemove <- input$remove_country$country
    if (!is.null(countryToRemove) && countryToRemove %in% filters()$countries) {
      updatedSelection <- setdiff(filters()$countries, countryToRemove)
      filters_mod$update_countries(updatedSelection)
    }
  })
  
  ## FUNCTIONS ##

  # getCountryScores, createResultsTable, getSingleQuestionReplies moved to their respective modules.
  # getActiveQuestions stays here: consumed by the activeQuestions reactive which is passed to both modules.

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
    activeQuestionPositions <- c()
    
    for (question in surveyData) {
      
      # skip section 0
      if ("Section 0" %in% question$tags) next
      
      # skip sections that are not selected
      if (length(intersect(filters()$sections, question$tags)) == 0) next
      
      # skip free text questions
      if (question$type == "FreeText") next
      
      # check if question tags and active filters do match
      if (length(intersect(filters()$pathogens, question$tags)) == 0) next
      
      if (length(intersect(filters()$resistances, question$tags)) == 0) next
      
      if (length(intersect(filters()$culture_materials, question$tags)) == 0) next
      
      # question has to be taken -> append titles to vectors
      activeQuestionTitles <- c(activeQuestionTitles, question[["title"]])
      activeQuestionShortTitles <- c(activeQuestionShortTitles, question[["short_title"]])
      
      # Add short title with badges
      formattedShortTitle <- formatShortTitleWithBadges(question[["short_title"]], toString(question$tags))
      activeQuestionShortTitlesWithBadges <- c(activeQuestionShortTitlesWithBadges, formattedShortTitle)
      
      # Add position
      activeQuestionPositions <- c(activeQuestionPositions, ifelse(is.null(question[["position"]]), 9999, question[["position"]]))
      
    }
    
    # Sort by position
    if (length(activeQuestionPositions) > 0) {
      sortOrder <- order(activeQuestionPositions)
      activeQuestionTitles <- activeQuestionTitles[sortOrder]
      activeQuestionShortTitles <- activeQuestionShortTitles[sortOrder]
      activeQuestionShortTitlesWithBadges <- activeQuestionShortTitlesWithBadges[sortOrder]
    }
    
    return(list(activeQuestionTitles, activeQuestionShortTitles, activeQuestionShortTitlesWithBadges))
    
  }
  
  
  ## Insight ----
  mod_insight_server("insight", it1 = it1, it2 = it2, it2_2 = it2_2, it3 = it3)

}

# run
shinyApp(ui = ui, server = server)
