# Dashboard landing page - shown when the app opens and whenever the "Dashboard"
# button is clicked (same mechanism as the Insight landing page). Rendered from
# dashboard_landing.md with render_landing_md() (R/mod_insight.R), its
# "<!-- dashboard-steps -->" marker replaced by the three "how to use" columns,
# the question selector and the button opening the selected view.
#
# Columns follow the sidebar's order (view toggle, sections, filters).
# The section and view cards only set the sidebar's section filter and
# Graphics/Table toggle (they mirror them); the view is opened by the big button.

# A selectable box in the style of the insight landing cards (insight_landing_cards()
# in R/mod_insight.R); the "selected" class is toggled from the server.
dashboard_landing_card <- function(input_id, card_class, icon_name, title, description) {
  actionLink(input_id, class = paste("insight-landing-card dashboard-landing-card", card_class),
    label = tags$span(class = "insight-landing-card-body",
      tags$span(class = "insight-landing-card-head",
        # same raw "fa fa-*" markup as the sidebar buttons (icon() renders some icons differently)
        tags$span(class = "insight-landing-card-icon", tags$i(class = paste0("fa fa-", icon_name))),
        tags$span(class = "dashboard-landing-card-text",
          tags$span(class = "insight-landing-card-title", title),
          tags$span(class = "dashboard-landing-card-desc", description)
        )
      ),
      tags$span(class = "insight-landing-card-cta dashboard-landing-card-state",
        tags$span(class = "state-select", "Select"),
        tags$span(class = "state-selected", icon("check"), "Selected")
      )
    )
  )
}

dashboard_landing_steps <- function(ns) {
  step <- function(title, ...) {
    tags$div(class = "dashboard-landing-step",
      tags$h3(class = "dashboard-landing-step-title", title),
      ...
    )
  }

  filter_item <- function(icon_name, name, text) {
    tags$li(tags$i(class = paste0("fa fa-", icon_name)), tags$strong(name), paste0(": ", text))
  }

  tagList(
    tags$div(class = "dashboard-landing-steps",
      step("1. Choose your view",
        tags$div(class = "dashboard-landing-cards",
          dashboard_landing_card(ns("select_graphics"), "view", "globe", "Graphics",
                                 "View answers on a map and bar chart."),
          dashboard_landing_card(ns("select_table"),    "view", "table", "Table",
                                 "See answers in an interactive table.")
        )
      ),
      step("2. Select a section",
        tags$div(class = "dashboard-landing-cards",
          dashboard_landing_card(ns("select_section1"), "section-1", "eye",       "National surveillance",
                                 "How AMR is monitored at the national level."),
          dashboard_landing_card(ns("select_section2"), "section-2", "dna",       "National genomic surveillance",
                                 "How genomic sequencing is used for AMR."),
          dashboard_landing_card(ns("select_section3"), "section-3", "book-open", "National guidance",
                                 "Guidelines that use AMR data or influence surveillance.")
        )
      ),
      step("3. Make use of filters",
        tags$p("Narrow your selection by:"),
        tags$ul(class = "dashboard-landing-filters",
          filter_item("flask",                "Culture material", "Filter by specimen type (e.g., blood, urine)."),
          filter_item("bacteria",             "Pathogens",        "Focus on specific bacterial pathogens."),
          filter_item("triangle-exclamation", "Resistances",      "Select specific antibiotic resistances."),
          filter_item("globe",                "Countries",        "Show all or only specific countries.")
        )
      )
    ),

    # Question selector - only relevant to the Graphics view
    conditionalPanel(
      condition = "input.viewToggle == 'graphics'",
      tags$div(class = "dashboard-landing-question",
        question_selector_card(ns("questionSelection"),
          tip = "use the filters first to narrow the selection")
      )
    ),

    tags$div(class = "dashboard-landing-open",
      actionButton(ns("open_view"), "Open the Graphics", icon = icon("arrow-right"),
                   class = "dashboard-landing-open-btn")
    )
  )
}

mod_dashboard_landing_ui <- function(id) {
  ns <- NS(id)
  div(class = "insight-md-content insight-landing",
    render_landing_md("content/md/dashboard_landing.md", ns,
      markers = list("<!-- dashboard-steps -->" = function() dashboard_landing_steps(ns))
    )
  )
}

# view: reactive with the sidebar's Graphics/Table selection.
# select_section(section) / select_view(view): set the sidebar's section filter / view toggle.
# open_view(view, question): leaves the landing page for the view (question used by Graphics).
mod_dashboard_landing_server <- function(id, filters, active_questions, view,
                                         select_section, select_view, open_view) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$select_section1, { select_section(sectionList[1]) })
    observeEvent(input$select_section2, { select_section(sectionList[2]) })
    observeEvent(input$select_section3, { select_section(sectionList[3]) })
    observeEvent(input$select_graphics, { select_view("graphics") })
    observeEvent(input$select_table,    { select_view("table") })

    # Highlight the cards matching the sidebar's current selections
    observeEvent(filters()$sections, {
      for (i in seq_along(sectionList)) {
        shinyjs::toggleClass(paste0("select_section", i), "selected",
                             condition = sectionList[i] %in% filters()$sections)
      }
    }, ignoreNULL = FALSE)

    observeEvent(view(), {
      shinyjs::toggleClass("select_graphics", "selected", condition = identical(view(), "graphics"))
      shinyjs::toggleClass("select_table",    "selected", condition = identical(view(), "table"))
      updateActionButton(session, "open_view",
                         label = if (identical(view(), "table")) "Open the Table" else "Open the Graphics",
                         icon  = icon("arrow-right"))
    }, ignoreNULL = FALSE)

    # Same choices as the Graphics question selector
    observeEvent(filters(), {
      updateSelectizeInput(session, "questionSelection",
                           choices = question_choices(active_questions()),
                           selected = "Participating countries")
    }, ignoreNULL = FALSE)

    observeEvent(input$open_view, {
      open_view(if (is.null(view())) "graphics" else view(), input$questionSelection)
    })

    list(
      set_question = function(question) {
        updateSelectizeInput(session, "questionSelection", selected = question)
      }
    )
  })
}
