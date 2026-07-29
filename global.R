
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

# Culture materials filter for the Insight tabs - a subset of cultureMaterialList, since
# the Insight #1/#2 graphs only ever break data down by these 5 categories (no "Stool" or
# "Not culture material related"). Values must match the "type" column of the Insight
# tab datasets (data/data_insighttab_*.rds) exactly, including the embedded newlines used
# to wrap long facet labels on the graphs; choice names are the same labels without those
# newlines, purely for a clean checkbox display - "Respiratory tract" and "Wound/tissue"
# reuse the Dashboard's own naming for those categories rather than the graphs' more
# specific wording ("Lower respiratory tract", "Wound/tissue swab"); on Insight #3,
# "Respiratory tract" also covers URTI (upper respiratory) via
# insightTab3XlabToCultureMaterial below. Only the display labels differ from the
# underlying data - insightCultureMaterialList (the actual filter/match values) still has
# to match the graphs' "type" column exactly, newlines included.
insightCultureMaterialList        <- c("Blood/CSF", "Urine", "Lower respiratory/\ntract", "Wound/\nTissue swab", "Screening")
insightCultureMaterialChoiceNames <- c("Blood/CSF", "Urine", "Respiratory tract", "Wound/tissue", "Screening")

# Pathogens filter for the Insight tabs - a subset of pathogenList, since the Insight
# datasets only ever cover these 8 pathogens (no "C. difficile" or "Not pathogen related").
insightPathogenList        <- setdiff(pathogenList, c("C. difficile", "Not pathogen related"))
insightPathogenChoiceNames <- insightPathogenList

# Resistances filter for the Insight tabs - a subset of resistanceList (no "Not resistance
# related"). Only Insight #1's x-axis carries a resistance dimension at all (#2/#2b break
# down by pathogen alone), via the insightTab1XlabInfo lookup below.
insightResistanceList        <- setdiff(resistanceList, "Not resistance related")
insightResistanceChoiceNames <- insightResistanceList

# Insight #1's x-axis combines a pathogen and a resistance into a single tick - some
# explicitly (e.g. "E.coli.CR" = E. coli + Carbapenem), some as an opaque abbreviation
# that names neither dimension outright (e.g. "MRSA" = S. aureus + Methicillin, "VRE" =
# E. faecium/faecalis + Vancomycin, "PNSP" = S. pneumoniae + Penicillin). This table
# records both dimensions for every one of tab #1's x-axis codes, so either can be
# filtered on even when the tick label alone doesn't spell it out. Values match
# pathogenList/resistanceList so the Pathogens/Resistances filters can share the same
# choices as the Dashboard section.
insightTab1XlabInfo <- data.frame(
  xlab = c("E.coli.CR", "E.coli.3GC", "E.coli.CT",
           "K.pneumoniae.CR", "K.pneumoniae.3GC", "K.pneumoniae.CT",
           "A.baumannii.CR", "P.aeruginosa.CR", "H.influenzae.AMP",
           "MRSA", "VRE", "PNSP"),
  pathogen = c("E. coli", "E. coli", "E. coli",
               "K. pneumoniae", "K. pneumoniae", "K. pneumoniae",
               "A. baumannii", "P. aeruginosa", "H. influenzae",
               "S. aureus", "E. faecium/faecalis", "S. pneumoniae"),
  resistance = c("Carbapenem", "3rd-generation Cephalosporin", "Colistin",
                 "Carbapenem", "3rd-generation Cephalosporin", "Colistin",
                 "Carbapenem", "Carbapenem", "Ampicillin",
                 "Methicillin", "Vancomycin", "Penicillin"),
  stringsAsFactors = FALSE
)

# Insight #2/#2b's x-axis is already one pathogen per tick, just written without the
# space after the genus initial (e.g. "E.coli" vs pathogenList's "E. coli").
insightPathogenByXlabCode <- c(
  "A.baumannii"        = "A. baumannii",
  "E.coli"             = "E. coli",
  "E.faecium/faecalis" = "E. faecium/faecalis",
  "H.influenzae"       = "H. influenzae",
  "K.pneumoniae"       = "K. pneumoniae",
  "P.aeruginosa"       = "P. aeruginosa",
  "S.aureus"           = "S. aureus",
  "S.pneumoniae"       = "S. pneumoniae"
)

# Insight #3's top graph (bar chart + heatmap combined) breaks down by infection syndrome
# rather than culture material directly - but each syndrome implies the specimen it's
# cultured from, so the existing Culture material filter can drive it via this syndrome ->
# culture material mapping instead of needing a filter of its own. Values on the right must
# match insightCultureMaterialList exactly (including the embedded newlines used to wrap
# facet/axis labels). URTI and LRTI both map to the same "Respiratory tract" category,
# since that's the only respiratory specimen category the Culture material filter has.
insightTab3XlabToCultureMaterial <- c(
  "BSI"         = "Blood/CSF",              # Bloodstream infection -> blood culture
  "uncomp. UTI" = "Urine",                  # Uncomplicated urinary tract infection -> urine culture
  "comp. UTI"   = "Urine",                  # Complicated urinary tract infection -> urine culture
  "URTI"        = "Lower respiratory/\ntract", # Upper respiratory tract infection
  "LRTI"        = "Lower respiratory/\ntract", # Lower respiratory tract infection
  "SSTI"        = "Wound/\nTissue swab"     # Skin and soft tissue infection -> wound/tissue swab
)


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
