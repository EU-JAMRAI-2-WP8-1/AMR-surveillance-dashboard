# Main landing page - a popup shown over the app when it opens, replacing the former
# Dashboard and Insight landing pages. Three screens, switched client-side in
# www/js/script.js (no server round trip):
#   welcome   - banner + two big choices (Dashboard / Insights)
#   dashboard - pick a view (Graphics/Table) and a section, then "Open"
#   insight   - pick one of the three insights
# The popup's final choice is sent to the server as input$mainLanding
# (list(page = "dashboard", view = ..., section = 1..3) or
#  list(page = "insight", tab = "tab1".."tab3")), handled in app.R.
# The close button (and Escape) just hides the popup, leaving the app as it is.

# Banner box at the top of each screen (CSS: .insight-landing-hero)
main_landing_hero <- function(title, ..., compact = FALSE) {
  tags$div(class = paste("insight-landing-hero", if (compact) "compact"),
    tags$div(class = "insight-landing-hero-text",
      tags$h1(title),
      ...
    )
  )
}

jamreye_word <- function() tags$span(class = "jamreye-word", "JAMREYE")

# One of the two big choices on the welcome screen
main_landing_big_choice <- function(screen, card_class, icon_name, kicker, title, text, items, cta) {
  tags$button(type = "button", class = paste("main-landing-big-choice", card_class),
              `data-landing-screen` = screen,
    tags$span(class = "main-landing-big-choice-head",
      tags$span(class = "main-landing-big-choice-icon", tags$i(class = paste0("fa fa-", icon_name))),
      tags$span(
        tags$span(class = "main-landing-big-choice-kicker", kicker), tags$br(),
        tags$span(class = "main-landing-big-choice-title", title)
      )
    ),
    tags$p(text),
    tags$ul(lapply(items, function(item) {
      tags$li(tags$i(class = paste0("fa fa-", item[1])), item[2])
    })),
    tags$span(class = "main-landing-big-choice-cta", cta, tags$i(class = "fa fa-arrow-right"))
  )
}

# A selectable Dashboard card (view or section); like radio buttons within its group
main_landing_dashboard_card <- function(group, value, card_class, icon_name, title, description) {
  tags$button(type = "button",
              class = paste("insight-landing-card dashboard-landing-card", card_class),
              `data-landing-group` = group, `data-landing-value` = value,
    tags$span(class = "insight-landing-card-body",
      tags$span(class = "insight-landing-card-head",
        tags$span(class = "insight-landing-card-icon", tags$i(class = paste0("fa fa-", icon_name))),
        tags$span(class = "dashboard-landing-card-text",
          tags$span(class = "insight-landing-card-title", title),
          tags$span(class = "dashboard-landing-card-desc", description)
        )
      ),
      tags$span(class = "insight-landing-card-cta dashboard-landing-card-state",
        tags$span(class = "state-select", "Select"),
        tags$span(class = "state-selected", tags$i(class = "fa fa-check"), "Selected")
      )
    )
  )
}

# An insight card; its id gives it its colour (INSIGHT TAB COLOURS block in style.css)
main_landing_insight_card <- function(tab, icon_name, number, title) {
  tags$button(type = "button", id = paste0("landing-goto_", tab), class = "insight-landing-card",
              `data-landing-insight` = tab,
    tags$span(class = "insight-landing-card-body",
      tags$span(class = "insight-landing-card-number", paste("Insight", number)),
      tags$span(class = "insight-landing-card-head",
        tags$span(class = "insight-landing-card-icon", tags$i(class = paste0("fa fa-", icon_name))),
        tags$span(class = "insight-landing-card-title", title)
      ),
      tags$span(class = "insight-landing-card-cta", "Explore", tags$i(class = "fa fa-arrow-right"))
    )
  )
}

main_landing_back_button <- function() {
  tags$button(type = "button", class = "main-landing-back-btn", `data-landing-screen` = "welcome",
              tags$i(class = "fa fa-arrow-left"), "Back")
}

main_landing_ui <- function() {
  council_link <- tags$a(
    href = "https://health.ec.europa.eu/publications/council-recommendation-stepping-eu-actions-combat-antimicrobial-resistance-one-health-approach_en",
    target = "_blank", rel = "noopener",
    "2023 Council Recommendation on AMR"
  )

  welcome_screen <- tags$section(class = "main-landing-screen active", `data-screen` = "welcome",
    main_landing_hero(tagList("Welcome to ", jamreye_word()),
      tags$p("JAMREYE presents ", tags$strong("current national AMR surveillance information in humans"),
             " from the 30 EU-JAMRAI 2 partner countries. It is designed as a ",
             tags$strong("tool for national and international AMR experts"),
             " and policymakers to ", tags$strong("explore, compare and analyse surveillance information"),
             " across Europe."),
      tags$p("Browse the raw survey results in the Dashboard, or read our analysis of Europe's progress ",
             "towards the three policy commitments of the ", council_link, " in the Insights.")
    ),
    tags$h2("Where would you like to start?"),
    tags$p(class = "main-landing-subtitle",
           "You can switch between both at any time with the buttons at the top of the menu bar."),
    tags$div(class = "main-landing-big-choices",
      main_landing_big_choice("dashboard", "", "chart-bar", "Survey results", "Dashboard",
        "Explore the answers of each country to the survey, on a map and bar chart or in an interactive table.",
        list(c("globe",  "Maps and charts, question by question"),
             c("table",  "Full answer tables, ready to export"),
             c("filter", "Filter by pathogen, resistance, culture material and country")),
        "Go to the Dashboard"),
      main_landing_big_choice("insight", "insight", "lightbulb", "Analysis", "Insights",
        "Discover our interactive and comparative analysis of the data, built around the three policy commitments of the Council Recommendation.",
        list(c("magnifying-glass-chart", "Interactive figures with their key findings"),
             c("code-compare",           "Compare countries side by side"),
             c("landmark",               "A baseline of Europe's progress")),
        "Go to the Insights")
    )
  )

  dashboard_screen <- tags$section(class = "main-landing-screen", `data-screen` = "dashboard",
    main_landing_hero(tagList(jamreye_word(), " Dashboard - Survey Results"), compact = TRUE,
      tags$p("Choose how you want to see the results and which part of the survey to start with. ",
             "Filters can be refined afterwards in the menu bar.")
    ),
    tags$div(class = "main-landing-dash-grid",
      tags$div(
        tags$h3(class = "main-landing-step-title", "1. Choose your view"),
        tags$div(class = "dashboard-landing-cards",
          main_landing_dashboard_card("view", "graphics", "view", "globe", "Graphics",
                                      "View answers on a map and bar chart."),
          main_landing_dashboard_card("view", "table",    "view", "table", "Table",
                                      "See answers in an interactive table.")
        )
      ),
      tags$div(
        tags$h3(class = "main-landing-step-title", "2. Select a section"),
        tags$div(class = "dashboard-landing-cards",
          main_landing_dashboard_card("section", "1", "section-1", "eye",       sectionList[1],
                                      "How AMR is monitored at the national level."),
          main_landing_dashboard_card("section", "2", "section-2", "dna",       sectionList[2],
                                      "How genomic sequencing is used for AMR."),
          main_landing_dashboard_card("section", "3", "section-3", "book-open", sectionList[3],
                                      "Guidelines that use AMR data or influence surveillance.")
        )
      )
    ),
    tags$div(class = "main-landing-filters-hint",
      tags$i(class = "fa fa-sliders"),
      tags$span("Once in the dashboard, narrow your selection with the filters in the menu bar: ",
                tags$strong("culture material"), ", ", tags$strong("pathogens"), ", ",
                tags$strong("resistances"), " and ", tags$strong("countries"), ".")
    ),
    tags$div(class = "main-landing-footer",
      main_landing_back_button(),
      tags$div(
        tags$span(class = "main-landing-open-hint", "Choose a view and a section"),
        tags$button(type = "button", class = "btn dashboard-landing-open-btn main-landing-open-btn",
                    disabled = NA,
                    tags$span(class = "main-landing-open-label", "Open the Dashboard"),
                    tags$i(class = "fa fa-arrow-right"))
      )
    )
  )

  insight_screen <- tags$section(class = "main-landing-screen", `data-screen` = "insight",
    main_landing_hero(tagList(jamreye_word(), " Insights analysis"), compact = TRUE,
      tags$p("Each insight combines an interactive figure with its key findings. Hover over the figures ",
             "for details and select countries to compare them side by side.")
    ),
    tags$h3(class = "main-landing-step-title", "Choose an insight"),
    tags$div(class = "insight-landing-cards",
      main_landing_insight_card("tab1", "ranking-star",     1, "Mandatory surveillance of AMR priority pathogens"),
      main_landing_insight_card("tab2", "map-location-dot", 2, "Expansion of European surveillance beyond invasive infections"),
      main_landing_insight_card("tab3", "book",             3, "Use of surveillance data for national treatment guidance")
    ),
    tags$div(class = "main-landing-footer", main_landing_back_button())
  )

  tags$div(id = "main-landing", class = "main-landing-overlay",
    tags$div(class = "main-landing-popup", role = "dialog", `aria-modal` = "true",
             `aria-label` = "Welcome to JAMREYE",
      tags$button(type = "button", class = "main-landing-close", title = "Close",
                  `aria-label` = "Close", tags$i(class = "fa fa-xmark")),
      tags$div(class = "main-landing-body",
        welcome_screen,
        dashboard_screen,
        insight_screen
      )
    )
  )
}
