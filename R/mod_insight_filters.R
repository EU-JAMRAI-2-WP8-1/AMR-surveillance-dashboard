
mod_insight_filters_ui <- function(id) {
  ns <- NS(id)

  tagList(

    # Filter accordion
    accordion(
      accordion_panel(
        title = HTML('<div class="filter-progress-bar" id="progress-insight-countries"></div><i class="fa fa-globe accordion-icon accordion-icon-countries"></i> Countries'),
        class = "insight-country-filter-container",
        uiOutput(ns("countryPills")),
        uiOutput(ns("selectAllCountriesButton"))
      )
    ),

    # Reset filters button
    tags$span(
      class = "reset-filters-wrapper",
      actionButton(ns("resetInsightFilters"), "Reset filters",
                   class = "btn btn-outline-primary",
                   icon  = icon("filter-circle-xmark"))
    )

  )
}

mod_insight_filters_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Each country cycles through: selected -> activated -> unselected -> selected -> ...
    defaultStates <- setNames(rep("selected", length(participatingCountries)), sort(participatingCountries))
    countryStates <- reactiveVal(defaultStates)

    nextState <- function(state) {
      switch(state,
        selected   = "activated",
        activated  = "unselected",
        unselected = "selected"
      )
    }

    # A pill click (JS) advances that country to its next state
    observeEvent(input$countryClicked, {
      states <- countryStates()
      country <- input$countryClicked
      states[[country]] <- nextState(states[[country]])
      countryStates(states)
    })

    output$countryPills <- renderUI({
      states <- countryStates()
      tags$div(
        class = "insight-country-pills",
        lapply(names(states), function(country) {
          state <- states[[country]]
          iconHtml <- switch(state,
            selected   = '<i class="fa-solid fa-check"></i>',
            activated  = '<i class="fa-solid fa-star"></i>',
            unselected = ""
          )
          tags$div(
            class         = paste("country-pill", paste0("country-pill-", state)),
            `data-country` = country,
            HTML(paste0(iconHtml, '<span class="country-label">', country, '</span>'))
          )
        })
      )
    })

    # Select/deselect all — Countries
    output$selectAllCountriesButton <- renderUI({
      states <- countryStates()
      shownCount <- sum(states != "unselected")
      tags$div(
        class = "dual-button-container",
        actionButton(session$ns("deselectAllCountries"), HTML('<i class="fa fa-times"></i> Clear'),
                     class = paste("dual-button deselect-btn", if (shownCount == 0) "disabled-btn" else "")),
        actionButton(session$ns("selectAllCountries"), HTML('<i class="fa fa-check"></i> All'),
                     class = paste("dual-button select-btn", if (shownCount == length(participatingCountries)) "disabled-btn" else ""))
      )
    })

    observeEvent(input$selectAllCountries, {
      countryStates(defaultStates)
    })
    observeEvent(input$deselectAllCountries, {
      countryStates(setNames(rep("unselected", length(participatingCountries)), sort(participatingCountries)))
    })

    # Reset filters — every country back to "selected" (shown, not activated)
    observeEvent(input$resetInsightFilters, {
      countryStates(defaultStates)
    })

    # Progress bar - computed here (the source of truth) and pushed to the client directly,
    # rather than inferred from the DOM, which lags one render behind the actual state
    observe({
      states     <- countryStates()
      total      <- length(states)
      shown      <- sum(states != "unselected")
      percentage <- if (total > 0) (shown / total) * 100 else 0
      session$sendCustomMessage("insightCountriesProgress", list(percentage = percentage))
    })

    # Called when a country is selected/deselected directly on a graph, so the sidebar
    # pill reflects it too (the reverse of the "_set" push below). Only touches shown
    # countries and is a no-op when nothing actually changes, so it can't fight with the
    # filter -> graph direction or loop back on itself.
    syncActivation <- function(added = character(0), removed = character(0)) {
      states <- countryStates()
      shown  <- names(states)[states != "unselected"]
      added   <- intersect(added, shown)
      removed <- intersect(removed, shown)
      newStates <- states
      newStates[names(states) %in% removed & states == "activated"] <- "selected"
      newStates[names(states) %in% added] <- "activated"
      if (!identical(newStates, states)) {
        countryStates(newStates)
      }
    }

    list(
      filters = reactive({
        states <- countryStates()
        list(
          shown     = names(states)[states != "unselected"],
          activated = names(states)[states == "activated"]
        )
      }),
      syncActivation = syncActivation
    )

  })
}
