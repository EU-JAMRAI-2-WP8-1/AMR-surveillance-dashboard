
mod_insight_filters_ui <- function(id) {
  ns <- NS(id)

  tagList(

    # Filter accordion
    accordion(
      accordion_panel(
        title = HTML('<div class="filter-progress-bar" id="progress-insight-cultureMaterials"></div><i class="fa fa-flask accordion-icon accordion-icon-culture"></i> Culture material'),
        checkboxGroupInput(
          inputId      = ns("cultureMaterialsSelection"),
          label        = "",
          choiceNames  = insightCultureMaterialChoiceNames,
          choiceValues = insightCultureMaterialList,
          selected     = insightCultureMaterialList,
          inline       = FALSE,
          width        = NULL
        ),
        uiOutput(ns("selectAllCultureMaterialsButton"))
      ),
      accordion_panel(
        title = HTML('<div class="filter-progress-bar" id="progress-insight-pathogens"></div><i class="fa fa-bacteria accordion-icon accordion-icon-pathogens"></i> Pathogens'),
        checkboxGroupInput(
          inputId      = ns("pathogensSelection"),
          label        = "",
          choiceNames  = insightPathogenChoiceNames,
          choiceValues = insightPathogenList,
          selected     = insightPathogenList,
          inline       = FALSE,
          width        = NULL
        ),
        uiOutput(ns("selectAllPathogensButton"))
      ),
      accordion_panel(
        title = HTML('<div class="filter-progress-bar" id="progress-insight-resistances"></div><i class="fa fa-triangle-exclamation accordion-icon accordion-icon-resistances"></i> Resistances'),
        checkboxGroupInput(
          inputId      = ns("resistancesSelection"),
          label        = "",
          choiceNames  = insightResistanceChoiceNames,
          choiceValues = insightResistanceList,
          selected     = insightResistanceList,
          inline       = FALSE,
          width        = NULL
        ),
        uiOutput(ns("selectAllResistancesButton"))
      ),
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

    # Select/deselect/activate all — Countries
    output$selectAllCountriesButton <- renderUI({
      states <- countryStates()
      shownCount     <- sum(states != "unselected")
      activatedCount <- sum(states == "activated")
      tags$div(
        class = "dual-button-container",
        actionButton(session$ns("deselectAllCountries"), HTML('<i class="fa fa-times"></i> Hide all'),
                     class = paste("dual-button deselect-btn", if (shownCount == 0) "disabled-btn" else "")),
        actionButton(session$ns("selectAllCountries"), HTML('<i class="fa fa-check"></i> Show all'),
                     class = paste("dual-button select-btn", if (shownCount == length(participatingCountries)) "disabled-btn" else "")),
        actionButton(session$ns("activateAllCountries"), HTML('<i class="fa fa-star"></i> Select all'),
                     class = paste("dual-button activate-btn", if (activatedCount == length(participatingCountries)) "disabled-btn" else ""))
      )
    })

    observeEvent(input$selectAllCountries, {
      countryStates(defaultStates)
    })
    observeEvent(input$deselectAllCountries, {
      countryStates(setNames(rep("unselected", length(participatingCountries)), sort(participatingCountries)))
    })
    observeEvent(input$activateAllCountries, {
      countryStates(setNames(rep("activated", length(participatingCountries)), sort(participatingCountries)))
    })

    # Select/deselect all — Culture materials
    output$selectAllCultureMaterialsButton <- renderUI({
      selection <- input$cultureMaterialsSelection
      tags$div(
        class = "dual-button-container",
        actionButton(session$ns("deselectAllCultureMaterials"), HTML('<i class="fa fa-times"></i> Clear'),
                     class = paste("dual-button deselect-btn", if (length(selection) == 0) "disabled-btn" else "")),
        actionButton(session$ns("selectAllCultureMaterials"), HTML('<i class="fa fa-check"></i> All'),
                     class = paste("dual-button select-btn", if (length(selection) == length(insightCultureMaterialList)) "disabled-btn" else ""))
      )
    })

    observeEvent(input$selectAllCultureMaterials, {
      updateCheckboxGroupInput(session, "cultureMaterialsSelection",
                                choiceNames  = insightCultureMaterialChoiceNames,
                                choiceValues = insightCultureMaterialList,
                                selected     = insightCultureMaterialList)
    })
    observeEvent(input$deselectAllCultureMaterials, {
      updateCheckboxGroupInput(session, "cultureMaterialsSelection",
                                choiceNames  = insightCultureMaterialChoiceNames,
                                choiceValues = insightCultureMaterialList,
                                selected     = c())
    })

    # Select/deselect all — Pathogens
    output$selectAllPathogensButton <- renderUI({
      selection <- input$pathogensSelection
      tags$div(
        class = "dual-button-container",
        actionButton(session$ns("deselectAllPathogens"), HTML('<i class="fa fa-times"></i> Clear'),
                     class = paste("dual-button deselect-btn", if (length(selection) == 0) "disabled-btn" else "")),
        actionButton(session$ns("selectAllPathogens"), HTML('<i class="fa fa-check"></i> All'),
                     class = paste("dual-button select-btn", if (length(selection) == length(insightPathogenList)) "disabled-btn" else ""))
      )
    })

    observeEvent(input$selectAllPathogens, {
      updateCheckboxGroupInput(session, "pathogensSelection",
                                choiceNames  = insightPathogenChoiceNames,
                                choiceValues = insightPathogenList,
                                selected     = insightPathogenList)
    })
    observeEvent(input$deselectAllPathogens, {
      updateCheckboxGroupInput(session, "pathogensSelection",
                                choiceNames  = insightPathogenChoiceNames,
                                choiceValues = insightPathogenList,
                                selected     = c())
    })

    # Select/deselect all — Resistances
    output$selectAllResistancesButton <- renderUI({
      selection <- input$resistancesSelection
      tags$div(
        class = "dual-button-container",
        actionButton(session$ns("deselectAllResistances"), HTML('<i class="fa fa-times"></i> Clear'),
                     class = paste("dual-button deselect-btn", if (length(selection) == 0) "disabled-btn" else "")),
        actionButton(session$ns("selectAllResistances"), HTML('<i class="fa fa-check"></i> All'),
                     class = paste("dual-button select-btn", if (length(selection) == length(insightResistanceList)) "disabled-btn" else ""))
      )
    })

    observeEvent(input$selectAllResistances, {
      updateCheckboxGroupInput(session, "resistancesSelection",
                                choiceNames  = insightResistanceChoiceNames,
                                choiceValues = insightResistanceList,
                                selected     = insightResistanceList)
    })
    observeEvent(input$deselectAllResistances, {
      updateCheckboxGroupInput(session, "resistancesSelection",
                                choiceNames  = insightResistanceChoiceNames,
                                choiceValues = insightResistanceList,
                                selected     = c())
    })

    # Reset filters — every country back to "selected" (shown, not activated);
    # culture materials, pathogens and resistances back to all selected
    observeEvent(input$resetInsightFilters, {
      countryStates(defaultStates)
      updateCheckboxGroupInput(session, "cultureMaterialsSelection",
                                choiceNames  = insightCultureMaterialChoiceNames,
                                choiceValues = insightCultureMaterialList,
                                selected     = insightCultureMaterialList)
      updateCheckboxGroupInput(session, "pathogensSelection",
                                choiceNames  = insightPathogenChoiceNames,
                                choiceValues = insightPathogenList,
                                selected     = insightPathogenList)
      updateCheckboxGroupInput(session, "resistancesSelection",
                                choiceNames  = insightResistanceChoiceNames,
                                choiceValues = insightResistanceList,
                                selected     = insightResistanceList)
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
          shown             = names(states)[states != "unselected"],
          activated         = names(states)[states == "activated"],
          culture_materials = input$cultureMaterialsSelection,
          pathogens         = input$pathogensSelection,
          resistances       = input$resistancesSelection
        )
      }),
      syncActivation = syncActivation
    )

  })
}
