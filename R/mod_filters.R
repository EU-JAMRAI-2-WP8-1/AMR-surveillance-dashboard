
mod_filters_ui <- function(id) {
  ns <- NS(id)

  tagList(

    # Sections filter
    tags$div(
      class = "sections-wrapper-sidebar",
      radioGroupButtons(
        inputId     = ns("sectionsSelection"),
        label       = NULL,
        choiceNames = list(
          HTML('<i class="fa fa-eye"></i><span class="btn-text"> National surveillance</span>'),
          HTML('<i class="fa fa-dna"></i><span class="btn-text"> National genomic surveillance</span>'),
          HTML('<i class="fa fa-book-open"></i><span class="btn-text"> National guidance</span>')
        ),
        choiceValues = sectionList,
        selected     = sectionList[1],
        individual   = FALSE,
        checkIcon    = list(),
        status       = "primary"
      )
    ),

    # Filter accordion
    accordion(
      accordion_panel(
        title = HTML('<div class="filter-progress-bar" id="progress-cultureMaterials"></div><i class="fa fa-flask accordion-icon accordion-icon-culture"></i> Culture material'),
        checkboxGroupInput(
          inputId     = ns("cultureMaterialsSelection"),
          label       = "",
          choiceNames  = cultureMaterialChoiceNames,
          choiceValues = cultureMaterialList,
          selected    = cultureMaterialList,
          inline      = FALSE,
          width       = NULL
        ),
        uiOutput(ns("selectAllCultureMaterialsButton"))
      ),
      accordion_panel(
        title = HTML('<div class="filter-progress-bar" id="progress-pathogens"></div><i class="fa fa-bacteria accordion-icon accordion-icon-pathogens"></i> Pathogens'),
        checkboxGroupInput(
          inputId     = ns("pathogensSelection"),
          label       = "",
          choiceNames  = pathogenChoiceNames,
          choiceValues = pathogenList,
          selected    = pathogenList,
          inline      = FALSE,
          width       = NULL
        ),
        uiOutput(ns("selectAllPathogensButton"))
      ),
      accordion_panel(
        title = HTML('<div class="filter-progress-bar" id="progress-resistances"></div><i class="fa fa-triangle-exclamation accordion-icon accordion-icon-resistances"></i> Resistances'),
        class = "country-filter-container",
        checkboxGroupInput(
          inputId     = ns("resistancesSelection"),
          label       = "",
          choiceNames  = resistanceChoiceNames,
          choiceValues = resistanceList,
          selected    = resistanceList,
          inline      = FALSE,
          width       = NULL
        ),
        uiOutput(ns("selectAllResistancesButton"))
      ),
      accordion_panel(
        title = HTML('<div class="filter-progress-bar" id="progress-countries"></div><i class="fa fa-globe accordion-icon accordion-icon-countries"></i> Countries'),
        checkboxGroupInput(
          inputId  = ns("countriesSelection"),
          label    = "",
          choices  = sort(participatingCountries),
          selected = participatingCountries,
          inline   = FALSE,
          width    = NULL
        ),
        uiOutput(ns("selectAllCountriesButton"))
      )
    ),

    # Reset filters button
    tags$span(
      class = "reset-filters-wrapper",
      actionButton(ns("resetFilters"), "Reset filters",
                   class = "btn btn-outline-primary",
                   icon  = icon("filter-circle-xmark"))
    )

  )
}

mod_filters_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reset all filters to defaults
    observeEvent(input$resetFilters, {
      updateRadioButtons(session, "sectionsSelection", selected = sectionList[1])
      updateCheckboxGroupInput(session, "countriesSelection",      choices = participatingCountries, selected = participatingCountries)
      updateCheckboxGroupInput(session, "pathogensSelection",      choices = pathogenList,           selected = pathogenList)
      updateCheckboxGroupInput(session, "resistancesSelection",    choices = resistanceList,         selected = resistanceList)
      updateCheckboxGroupInput(session, "cultureMaterialsSelection", choices = cultureMaterialList,  selected = cultureMaterialList)
    })

    # Select/deselect all — Countries
    output$selectAllCountriesButton <- renderUI({
      tags$div(
        class = "dual-button-container",
        actionButton(session$ns("deselectAllCountries"), HTML('<i class="fa fa-times"></i> Clear'),
                     class = paste("dual-button deselect-btn", if (length(input$countriesSelection) == 0) "disabled-btn" else "")),
        actionButton(session$ns("selectAllCountries"), HTML('<i class="fa fa-check"></i> All'),
                     class = paste("dual-button select-btn", if (length(input$countriesSelection) == length(participatingCountries)) "disabled-btn" else ""))
      )
    })

    observeEvent(input$selectAllCountries, {
      updateCheckboxGroupInput(session, "countriesSelection", choices = participatingCountries, selected = participatingCountries)
    })
    observeEvent(input$deselectAllCountries, {
      updateCheckboxGroupInput(session, "countriesSelection", choices = participatingCountries, selected = c())
    })

    # Select/deselect all — Culture materials
    output$selectAllCultureMaterialsButton <- renderUI({
      tags$div(
        class = "select-all-wrapper",
        tags$div(
          class = "dual-button-container",
          actionButton(session$ns("deselectAllCultureMaterials"), HTML('<i class="fa fa-times"></i> Clear'),
                       class = paste("dual-button deselect-btn", if (length(input$cultureMaterialsSelection) == 0) "disabled-btn" else "")),
          actionButton(session$ns("selectAllCultureMaterials"), HTML('<i class="fa fa-check"></i> All'),
                       class = paste("dual-button select-btn", if (length(input$cultureMaterialsSelection) == length(cultureMaterialList)) "disabled-btn" else ""))
        )
      )
    })

    observeEvent(input$selectAllCultureMaterials, {
      updateCheckboxGroupInput(session, "cultureMaterialsSelection", choices = cultureMaterialList, selected = cultureMaterialList)
    })
    observeEvent(input$deselectAllCultureMaterials, {
      updateCheckboxGroupInput(session, "cultureMaterialsSelection", choices = cultureMaterialList, selected = c())
    })

    # Select/deselect all — Pathogens
    output$selectAllPathogensButton <- renderUI({
      tags$div(
        class = "select-all-wrapper",
        tags$div(
          class = "dual-button-container",
          actionButton(session$ns("deselectAllPathogens"), HTML('<i class="fa fa-times"></i> Clear'),
                       class = paste("dual-button deselect-btn", if (length(input$pathogensSelection) == 0) "disabled-btn" else "")),
          actionButton(session$ns("selectAllPathogens"), HTML('<i class="fa fa-check"></i> All'),
                       class = paste("dual-button select-btn", if (length(input$pathogensSelection) == length(pathogenList)) "disabled-btn" else ""))
        )
      )
    })

    observeEvent(input$selectAllPathogens, {
      updateCheckboxGroupInput(session, "pathogensSelection", choices = pathogenList, selected = pathogenList)
    })
    observeEvent(input$deselectAllPathogens, {
      updateCheckboxGroupInput(session, "pathogensSelection", choices = pathogenList, selected = c())
    })

    # Select/deselect all — Resistances
    output$selectAllResistancesButton <- renderUI({
      tags$div(
        class = "select-all-wrapper",
        tags$div(
          class = "dual-button-container",
          actionButton(session$ns("deselectAllResistances"), HTML('<i class="fa fa-times"></i> Clear'),
                       class = paste("dual-button deselect-btn", if (length(input$resistancesSelection) == 0) "disabled-btn" else "")),
          actionButton(session$ns("selectAllResistances"), HTML('<i class="fa fa-check"></i> All'),
                       class = paste("dual-button select-btn", if (length(input$resistancesSelection) == length(resistanceList)) "disabled-btn" else ""))
        )
      )
    })

    observeEvent(input$selectAllResistances, {
      updateCheckboxGroupInput(session, "resistancesSelection", choices = resistanceList, selected = resistanceList)
    })
    observeEvent(input$deselectAllResistances, {
      updateCheckboxGroupInput(session, "resistancesSelection", choices = resistanceList, selected = c())
    })

    # Return current filter values + a helper to update countries from outside
    list(
      filters = reactive(list(
        sections          = input$sectionsSelection,
        pathogens         = input$pathogensSelection,
        resistances       = input$resistancesSelection,
        culture_materials = input$cultureMaterialsSelection,
        countries         = input$countriesSelection
      )),
      update_countries = function(new_selection) {
        updateCheckboxGroupInput(session, "countriesSelection", selected = new_selection)
      }
    )

  })
}
