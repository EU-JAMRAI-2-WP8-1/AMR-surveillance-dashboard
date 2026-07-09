
# Source all modules from R/
# On shinyapps.io, Shiny also auto-sources R/ after this (harmless redefinition).
# On Docker (Rscript app.R), this is the only sourcing mechanism.
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)


## CONSTANTS ##

# Filters : pathogens under surveillance / resistances / culture materials
sectionList         <- c("National surveillance", "National genomic surveillance", "National guidance") # order is reverted compared to the survey (3, 2, 1)
pathogenList        <- c("E. coli", "K. pneumoniae", "P. aeruginosa", "A. baumannii", "S. aureus", "E. faecium/faecalis", "S. pneumoniae", "H. influenzae", "C. difficile", "Not pathogen related")
resistanceList      <- c("Carbapenem", "3rd-generation Cephalosporin", "Colistin", "Methicillin", "Vancomycin", "Penicillin", "Ampicillin", "Not resistance related")
cultureMaterialList <- c("Blood/CSF", "Urine", "Respiratory tract", "Wound/tissue", "Stool", "Screening", "Not culture material related")

pathogenChoiceNames        <- pathogenList
resistanceChoiceNames      <- resistanceList
cultureMaterialChoiceNames <- cultureMaterialList


## DATA LOAD AND PREPARATION ##

# Import Europe polygons
# source: https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries (modified to include only european countries)
geojsonEurope <- tryCatch({
  rjson::fromJSON(file = file.path("data/CNTR_RG_60M_2024_4326-modified.geojson"))
}, error = function(e) {
  message("Error loading map data: ", e$message)
  list(features = list())
})

# Import survey questions and replies from JSON
surveyDataFile <- file.path("data/OUT_questions_and_replies.json")
surveyData <- tryCatch({
  rjson::fromJSON(paste(readLines(surveyDataFile), collapse=""))
}, error = function(e) {
  message("Error loading survey data: ", e$message)
  NULL
})

# Import survey score table from CSV
countryScoreTable <- tryCatch({
  read.csv("data/OUT_country_scores.csv", header = TRUE)
}, error = function(e) {
  message("Error loading country scores: ", e$message)
  data.frame()
})

# Europe country list
euroCountryList <- c()
for (country in geojsonEurope$features) {
  euroCountryList <- c(euroCountryList, country$id)
}

# Country question index (/!\ might change in future versions of the survey)
countryQuestionIndex <- 3

# Participating country list
participatingCountries    <- names(surveyData[[countryQuestionIndex]][["possible_answers"]])
repliedCountries          <- names(surveyData[[countryQuestionIndex]][["actual_answers"]])
nonParticipatingCountries <- setdiff(euroCountryList, repliedCountries)

# Question short-title lists (used for question filter and multiple-choice detection)
allShortTitles <- c()
multipleChoiceShortTitles <- c()
for (question in surveyData) {
  if ("Section 0" %in% question$tags) next
  if (question$type == "FreeText") next
  if (question$short_title %in% allShortTitles) next
  allShortTitles <- c(allShortTitles, question$short_title)
  if (question$type == "MultipleChoice") {
    multipleChoiceShortTitles <- c(multipleChoiceShortTitles, question$short_title)
  }
}

# Discrete color sequence for maps and plots
colorSequence <- c("#0fdbd5", "#df2e1a", "#f7c948", "#6a4c93", "#25c414", "#1982c4", "#e76f51", "#2a9d8f", "#f4a261", "#264653", "#8ecae6", "#ffb4a2", "#000000")

# Participation map dataset
participationData <- data.frame(
  "country"            = euroCountryList,
  "survey_participation" = rep(NA, length(euroCountryList))
)
for (country in euroCountryList) {
  if (country %in% repliedCountries) {
    participationData[participationData$country == country, "survey_participation"] <- 1
  } else if (country %in% participatingCountries) {
    participationData[participationData$country == country, "survey_participation"] <- 2
  } else {
    participationData[participationData$country == country, "survey_participation"] <- 3
  }
}
participationDataOccurrences <- data.frame(
  "reply"      = c("Yes", "No", "Not in JAMRAI"),
  "occurences" = c(
    sum(participationData$survey_participation == 1),
    sum(participationData$survey_participation == 2),
    sum(participationData$survey_participation == 3)
  )
)
participationDataOccurrences$occurences <- (participationDataOccurrences$occurences / sum(participationDataOccurrences$occurences)) * 100

# Insight tab data
it1     <- tryCatch(readRDS("data/data_insighttab_1.rds"),     error = function(e) { message("Error loading insight tab 1 data: ",    e$message); NULL })
it2     <- tryCatch(readRDS("data/data_insighttab_2.rds"),     error = function(e) { message("Error loading insight tab 2 data: ",    e$message); NULL })
it2_2   <- tryCatch(readRDS("data/data_insighttab_2_2.rds"),   error = function(e) { message("Error loading insight tab 2b data: ",   e$message); NULL })
it3     <- tryCatch(readRDS("data/data_insighttab_3.rds"),     error = function(e) { message("Error loading insight tab 3 data: ",    e$message); NULL })
it3_ast <- tryCatch(readRDS("data/data_insighttab_3_ast.rds"), error = function(e) { message("Error loading insight tab 3 AST data: ", e$message); NULL })
it3_wgt <- tryCatch(readRDS("data/data_insighttab_3_wgt.rds"), error = function(e) { message("Error loading insight tab 3 WGT data: ", e$message); NULL })
