
mod_insight_ui <- function(id) {
  ns <- NS(id)

  tabsetPanel(
    id = ns("insightTabs"),

    tabPanel("Insight - landing page", value = "landing",
      fluidRow(
        column(12,
          h3("Insight - landing page (work in progress)")
        )
      ),
      fluidRow(
        column(12,
          actionButton(ns("goto_tab1"), "AMR priority pathogens", icon = icon("ranking-star"), class = "btn btn-outline-primary insight-landing-btn"),
          actionButton(ns("goto_tab2"), "Coverage and representativeness", icon = icon("map-location-dot"), class = "btn btn-outline-primary insight-landing-btn"),
          actionButton(ns("goto_tab3"), "Guidance - common infections", icon = icon("book"), class = "btn btn-outline-primary insight-landing-btn")
        )
      )
    ),

    tabPanel("Insight #1", value = "tab1",
      fluidRow(
        column(12, h3("National surveillance of AMR priority pathogens"))
      ),
      fluidRow(
        column(6,
          girafeOutput(ns("plot_it1"))
        ),
        column(6,
          uiOutput(ns("md_content_it1"))
        )
      ),
      fluidRow(
        column(12,
          uiOutput(ns("country_comparison_header_if1")),
          tableOutput(ns("country_context_if1"))
        )
      )
    ),

    tabPanel("Insight #2", value = "tab2",
      fluidRow(
        column(12, h3("Population coverage and geographical representativeness"))
      ),
      fluidRow(
        column(6,
          tabsetPanel(
            id = ns("it2SubTabs"),

            # Population coverage
            tabPanel(title = tagList(icon("people-group"), " Population coverage"), value = "population_coverage",
              fluidRow(
                column(12,
                  girafeOutput(ns("plot_it2"))
                )
              ),
              fluidRow(
                column(12,
                  uiOutput(ns("country_comparison_header_if2")),
                  tableOutput(ns("country_context_if2"))
                )
              )
            ),

            # Geographical representativeness
            tabPanel(title = tagList(icon("map"), " Geographical representativeness"), value = "geographical_representativeness",
              fluidRow(
                column(12,
                  girafeOutput(ns("plot_it2_2"))
                )
              ),
              fluidRow(
                column(12,
                  uiOutput(ns("country_comparison_header_if2_2")),
                  tableOutput(ns("country_context_if2_2"))
                )
              )
            )
          )
        ),
        column(6,
          uiOutput(ns("md_content_it2"))
        )
      )
    ),

    tabPanel("Insight #3", value = "tab3",
      fluidRow(
        column(12, h3("National guidance on treatment of common infections"))
      ),
      fluidRow(
        column(6,
          girafeOutput(ns("plot_it3"))
        ),
        column(6,
          uiOutput(ns("md_content_it3"))
        )
      ),
      fluidRow(
        style = "margin-top: 30px;",
        column(6,
          h4("AST data used for national treatment guidance"),
          girafeOutput(ns("plot_it3_ast"))
        ),
        column(6,
          h4("Who guides empiric antibiotic treatment"),
          girafeOutput(ns("plot_it3_wgt"))
        )
      )
    )

  )
}

# Renders an insight tab's markdown "What can we learn from this?" text with each
# "###" paragraph individually collapsible, without splitting the source file: the
# raw markdown is cut into chunks at each level-3 heading, each chunk is rendered to
# HTML on its own, and its <h3> is pulled back out so a toggle arrow can be attached
# to it while the rest of the chunk becomes the collapsible body. The heading itself
# is always shown; only the paragraph underneath it can be hidden.
render_collapsible_insight_md <- function(path, id_prefix, disclaimer = NULL) {
  lines <- readLines(path)

  h3_idx <- which(grepl("^###\\s", lines))

  preamble_end <- if (length(h3_idx)) h3_idx[1] - 1 else length(lines)
  preamble     <- lines[seq_len(preamble_end)]
  preamble_html <- HTML(markdown::markdownToHTML(paste(preamble, collapse = "\n"), fragment.only = TRUE))

  if (length(h3_idx) == 0) return(tagList(preamble_html, disclaimer))

  section_ends <- c(h3_idx[-1] - 1, length(lines))

  sections <- Map(function(start, end, i) {
    chunk_html <- markdown::markdownToHTML(paste(lines[start:end], collapse = "\n"), fragment.only = TRUE)
    chunk_html <- sub("^\\s+", "", chunk_html)

    parts        <- regmatches(chunk_html, regexec("(?s)^(<h3[^>]*>.*?</h3>)(.*)$", chunk_html, perl = TRUE))[[1]]
    heading_html <- parts[2]
    body_html    <- parts[3]

    # A heading with no text underneath (e.g. a legend-only "###" line) has nothing
    # to toggle, so it's shown as a plain heading with no arrow.
    if (!nzchar(trimws(gsub("<[^>]+>", "", body_html)))) {
      return(tags$div(class = "insight-md-subtitle", HTML(heading_html)))
    }

    section_id <- paste0(id_prefix, "-section-", i)

    tagList(
      tags$div(class = "insight-md-subtitle",
        tags$a(class = "insight-md-toggle",
          `data-bs-toggle` = "collapse",
          href             = paste0("#", section_id),
          role             = "button",
          `aria-expanded`  = "true",
          `aria-controls`  = section_id,
          tags$i(class = "fa fa-chevron-down")
        ),
        HTML(heading_html)
      ),
      tags$div(id = section_id, class = "collapse show insight-md-section",
        HTML(body_html)
      )
    )
  }, h3_idx, section_ends, seq_along(h3_idx))

  tagList(preamble_html, disclaimer, sections)
}

# The head-to-head comparison table only has content once at least one country is
# selected on the graph above it; this renders either its header or, in the meantime,
# a placeholder sentence instead of a header with nothing underneath it.
country_comparison_header <- function(selected) {
  if (length(selected) == 0) {
    tags$p(em("Select countries for head-to-head comparison !"))
  } else {
    tags$p(tags$strong("Selected countries head-to-head comparison:"))
  }
}

mod_insight_server <- function(id, it1, it2, it2_2, it3, it3_ast, it3_wgt, selected_tab, insight_filters, sync_activation, set_selected_tab) {
  moduleServer(id, function(input, output, session) {

    observeEvent(selected_tab(), {
      updateTabsetPanel(session, "insightTabs", selected = selected_tab())
    })

    ## Landing page navigation buttons ----
    observeEvent(input$goto_tab1, { set_selected_tab("tab1") })
    observeEvent(input$goto_tab2, { set_selected_tab("tab2") })
    observeEvent(input$goto_tab3, { set_selected_tab("tab3") })

    ## Country filter integration ----
    # "shown" countries are the ones displayed on the graphs (selected + activated);
    # "activated" countries are additionally pre-selected on the graphs, as if the user
    # had clicked their row/tile.
    #
    # Rebuilding a girafe widget (ggplot -> SVG -> interactive post-processing) is
    # expensive, so we're careful about what actually triggers it:
    #  - shownCountries() only changes value (and so only invalidates the plots below)
    #    when the set of displayed countries genuinely differs. Toggling a country
    #    to/from "activated" doesn't change what's shown, so it must not rebuild anything.
    #  - shownCountries() is debounced so toggling several countries in quick succession
    #    collapses into a single rebuild once the user pauses, instead of one per click.
    #  - activatedCountries() changes are pushed to the already-rendered widgets via
    #    ggiraph's lightweight "_set" client message (same mechanism as the reset
    #    buttons below), which updates the highlighted selection without regenerating
    #    the plot at all.

    shownCountriesRV <- reactiveVal(character(0))
    observe({
      newShown <- sort(insight_filters()$shown)
      if (!identical(newShown, isolate(shownCountriesRV()))) {
        shownCountriesRV(newShown)
      }
    })
    shownCountries     <- debounce(reactive({ shownCountriesRV() }), 400)
    activatedCountries <- reactive({ insight_filters()$activated })

    # Culture material / Pathogens filters — same "guard then debounce" treatment as
    # shownCountries above, and for the same reason: insight_filters() is one combined
    # reactive, so toggling *any* filter (say, a country) invalidates it and would
    # otherwise re-publish these too, even though $culture_materials/$pathogens didn't
    # actually change - reactiveVal doesn't dedupe by value on its own, so without the
    # identical() guard that spurious re-publish still invalidates the expensive girafe
    # rebuilds below, on top of the one legitimately triggered by the filter that did
    # change (i.e. the "graphs reload twice" symptom).
    cultureMaterialsRV <- reactiveVal(insightCultureMaterialList)
    observe({
      newSelection <- insight_filters()$culture_materials
      if (!identical(newSelection, isolate(cultureMaterialsRV()))) {
        cultureMaterialsRV(newSelection)
      }
    })
    selectedCultureMaterials <- debounce(reactive({ cultureMaterialsRV() }), 400)

    pathogensRV <- reactiveVal(insightPathogenList)
    observe({
      newSelection <- insight_filters()$pathogens
      if (!identical(newSelection, isolate(pathogensRV()))) {
        pathogensRV(newSelection)
      }
    })
    selectedPathogens <- debounce(reactive({ pathogensRV() }), 400)

    resistancesRV <- reactiveVal(insightResistanceList)
    observe({
      newSelection <- insight_filters()$resistances
      if (!identical(newSelection, isolate(resistancesRV()))) {
        resistancesRV(newSelection)
      }
    })
    selectedResistances <- debounce(reactive({ resistancesRV() }), 400)

    # Whether the non-country filters relevant to each tab's graph have been changed from
    # their default (all-selected) state - used to warn that the fixed narrative text next
    # to the graph describes the default/unfiltered data, not the current selection. Country
    # selection is deliberately excluded: the text is about overall European findings, and
    # narrowing to a few countries doesn't make it stale the way changing culture
    # material/pathogen/resistance does.
    it1_filters_modified <- reactive({
      !identical(sort(selectedCultureMaterials()), sort(insightCultureMaterialList)) ||
        !identical(sort(selectedPathogens()),       sort(insightPathogenList))       ||
        !identical(sort(selectedResistances()),     sort(insightResistanceList))
    })

    it2_filters_modified <- reactive({
      !identical(sort(selectedCultureMaterials()), sort(insightCultureMaterialList)) ||
        !identical(sort(selectedPathogens()),       sort(insightPathogenList))
    })

    it3_filters_modified <- reactive({
      !identical(sort(selectedCultureMaterials()), sort(insightCultureMaterialList))
    })

    insight_text_disclaimer <- tags$p(class = "insight-md-disclaimer",
      icon("triangle-exclamation"),
      "This text reflects the default (unfiltered) data and may not match your current filter selection."
    )

    # Sync graph selection -> country filter: selecting/deselecting a country directly
    # on any graph activates/deactivates the same country in the sidebar filter (the
    # reverse of the filter -> graph "_set" push in each renderGirafe block below).
    # Tracked per-graph as an add/remove delta against the *shared* filter state, so
    # switching between graphs never clobbers a country activated from a different tab.
    sync_graph_selection <- function(input_name) {
      previousSelection <- reactiveVal(isolate(activatedCountries()))
      observeEvent(input[[input_name]], {
        newSelection <- input[[input_name]]
        oldSelection <- previousSelection()
        sync_activation(
          added   = setdiff(newSelection, oldSelection),
          removed = setdiff(oldSelection, newSelection)
        )
        previousSelection(newSelection)
      }, ignoreNULL = FALSE)
    }

    sync_graph_selection("plot_it1_selected")
    sync_graph_selection("plot_it2_selected")
    sync_graph_selection("plot_it2_2_selected")
    sync_graph_selection("plot_it3_selected")

    # Re-derive the bar chart aggregates (count/total/proportion per group) from the
    # country-level heatmap data, so the bar charts stay consistent with whichever
    # countries are currently shown (the pre-computed *_bp.rds objects assume all countries).
    recompute_bp <- function(hm_data, group_vars, prop_name) {
      agg <- hm_data %>%
        group_by(!!!rlang::syms(c(group_vars, "value"))) %>%
        summarise(count = n(), .groups = "drop_last") %>%
        mutate(total = sum(count)) %>%
        ungroup() %>%
        mutate(prop = count / total)
      rename(agg, !!prop_name := prop)
    }

    # droplevels() so unselected countries/culture materials/pathogens/resistances don't
    # linger as empty rows/bars or facets (ggplot otherwise keeps unused factor levels as
    # breaks).
    #
    # it1's x-axis ticks combine a pathogen and a resistance into a single code (some
    # opaque, e.g. "MRSA"), so insightTab1XlabInfo is joined in to recover both dimensions
    # behind each one. it2/it2_2's ticks are already one pathogen each (no resistance
    # dimension at all), just spelled without the genus-initial space, so a simple lookup
    # vector is enough there.
    it1_hm_f   <- reactive({
      it1$hm %>%
        # Join via a plain-character copy of xlab, rather than overwriting xlab itself,
        # so the factor's level order (i.e. the x-axis tick order on the graphs) survives.
        mutate(xlab_key = as.character(xlab)) %>%
        left_join(insightTab1XlabInfo, by = c("xlab_key" = "xlab")) %>%
        select(-xlab_key) %>%
        filter(as.character(Country) %in% shownCountries(),
               as.character(type)    %in% selectedCultureMaterials(),
               pathogen               %in% selectedPathogens(),
               resistance             %in% selectedResistances()) %>%
        droplevels()
    })
    it1_bp_f   <- reactive({ recompute_bp(it1_hm_f(), c("type", "xlab"), "percentage") })

    it2_hm_f   <- reactive({
      it2$hm %>%
        mutate(pathogen = insightPathogenByXlabCode[as.character(xlab)]) %>%
        filter(as.character(Country) %in% shownCountries(),
               as.character(type)    %in% selectedCultureMaterials(),
               pathogen               %in% selectedPathogens()) %>%
        droplevels()
    })
    it2_bp_f   <- reactive({ recompute_bp(it2_hm_f(), c("type", "xlab"), "proportion") })

    it2_2_hm_f <- reactive({
      it2_2$hm %>%
        mutate(pathogen = insightPathogenByXlabCode[as.character(xlab)]) %>%
        filter(as.character(Country) %in% shownCountries(),
               as.character(type)    %in% selectedCultureMaterials(),
               pathogen               %in% selectedPathogens()) %>%
        droplevels()
    })
    it2_2_bp_f <- reactive({ recompute_bp(it2_2_hm_f(), c("type", "xlab"), "proportion") })

    # it3 has no culture-material column of its own - its x-axis is an infection syndrome
    # (BSI, UTI, ...), so insightTab3XlabToCultureMaterial translates each syndrome to the
    # specimen it's cultured from, letting the Culture material filter drive this graph too.
    it3_hm_f   <- reactive({
      it3$hm %>%
        mutate(culture_material = insightTab3XlabToCultureMaterial[as.character(xlab)]) %>%
        filter(as.character(Country) %in% shownCountries(),
               culture_material      %in% selectedCultureMaterials()) %>%
        droplevels()
    })
    it3_bp_f   <- reactive({ recompute_bp(it3_hm_f(), "xlab", "proportion") })

    it3_ast_f  <- reactive({ filter(it3_ast, as.character(Country) %in% shownCountries()) %>% droplevels() })
    it3_wgt_f  <- reactive({ filter(it3_wgt, as.character(Country) %in% shownCountries()) %>% droplevels() })

    ## Color scales ----

    surv_colors <- c(
      "Yes, mandatory" = "#086D6A",
      "Yes, voluntary" = "#0fdbd5",
      "No"             = "#949494"
    )

    surv_colorsPC <- c(
      "76-100%"                           = "#086D6A",
      "51-75%"                            = "#0BA4A0",
      "26-50%"                            = "#64F4F0",
      "1-25%"                             = "#CBFBFA",
      "Do not know"                       = "#949494",
      "Not part of national surveillance" = "#E3C19B"
    )

    surv_colorsGR <- c(
      "HIGH: all main geographical regions of the country are covered."   = "#086D6A",
      "MEDIUM: most geographical regions of the country are covered."     = "#0BA4A0",
      "LOW: a few geographical areas of the country are covered."         = "#CBFBFA",
      "Not part of national surveillance"                                 = "#E3C19B",
      "Do not know"                                                       = "#949494"
    )

    surv_colorsEG <- c(
      "Yes"         = "#044556",
      "No"          = "#F9BCB3",
      "Do not know" = "#949494"
    )

    ## Shared bar chart theme ----

    bp_theme <- theme_minimal() +
      theme(
        axis.title.x     = element_blank(),
        axis.title.y     = element_blank(),
        axis.text.x      = element_blank(),
        axis.ticks.x     = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position  = "none",
        strip.clip       = "off"
      )

    ## Build ggplot objects ----
    # Reactive on the filtered *_bp_f()/*_hm_f() data so the charts stay in sync with
    # the country filter (shown countries appear, unselected ones are dropped entirely).

    # --- Tab 1 ---

    gg_bp_it1 <- reactive({
      it1_bp_f() %>%
        group_by(type, xlab) %>%
        mutate(percentage = percentage / sum(percentage)) %>%
        ungroup() %>%
        ggplot(aes(x       = xlab,
                   y       = percentage,
                   fill    = value,
                   tooltip = glue("{value}: {round(percentage * 100, 1)}%"))) +
        geom_col_interactive(position = "stack") +
        geom_hline(yintercept = 0.5, color = "red", linewidth = 0.5) +
        facet_grid(cols = vars(type), scales = "free_x", space = "free") +
        scale_fill_manual(values = surv_colors) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
        bp_theme
    })

    gg_hm_it1 <- reactive({
      it1_hm_f() %>%
        ggplot(aes(x = xlab, y = Country, fill = value, data_id = Country)) +
        geom_tile_interactive(aes(tooltip = glue("In <b>{Country}</b>, the national suveillance
                                                  for <b>{abr} <i>{bug}</i> </b> in <b>{type}</b>
                                                  {surv_lab}")),
                              color = "white", linewidth = 0.5) +
        facet_grid(cols = vars(type), scales = "free_x", space = "free", switch = "both") +
        scale_fill_manual(name   = "Surveillance Type",
                          values = surv_colors,
                          breaks = c("Yes, mandatory", "Yes, voluntary", "No"),
                          labels = c("Mandatory", "Voluntary", "No")) +
        theme_minimal() +
        theme(
          axis.title.x     = element_blank(),
          axis.title.y     = element_blank(),
          axis.text.x      = element_text(angle = 90, hjust = 1, vjust = .5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position  = "none",
          strip.placement  = "outside",
          strip.clip       = "off"
        )
    })

    # --- Tab 2: population coverage ---

    gg_bp_it2 <- reactive({
      it2_bp_f() %>%
        group_by(type, xlab) %>%
        mutate(proportion = proportion / sum(proportion)) %>%
        ungroup() %>%
        ggplot(aes(x       = xlab,
                   y       = proportion,
                   fill    = value,
                   tooltip = glue("{value}: {round(proportion * 100, 1)}%"))) +
        geom_col_interactive(position = "stack") +
        geom_hline(yintercept = 0.5, color = "red", linewidth = 0.5) +
        facet_grid(cols = vars(type), scales = "free_x", space = "free") +
        scale_fill_manual(values = surv_colorsPC) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
        # guides(fill = "none") rather than relying only on bp_theme's legend.position = "none":
        # the combined plot forces legend.position = "top" via `&` so the single collected
        # legend (from the heatmap) sits above the whole figure, and that same `&` theme would
        # otherwise also un-hide this bar chart's own (redundant) legend.
        guides(fill = "none") +
        bp_theme
    })

    gg_hm_it2 <- reactive({
      it2_hm_f() |>
        mutate(tooltip = glue("In <b>{Country}")) |>
        ggplot(aes(x = xlab, y = Country, fill = value, data_id = Country)) +
        geom_tile_interactive(aes(tooltip = tooltip), color = "white", linewidth = 0.5) +
        facet_grid(cols = vars(type), scales = "free_x", space = "free", switch = "both") +
        scale_fill_manual(name   = "Population coverage",
                          values = surv_colorsPC,
                          breaks = c("76-100%", "51-75%", "26-50%", "1-25%",
                                     "Not part of national surveillance", "Do not know"),
                          labels = c("76-100%", "51-75%", "26-50%", "1-25%",
                                     "Not part of national surveillance", "Do not know")) +
        theme_minimal() +
        theme(
          axis.text.x      = element_text(angle = 90, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title       = element_blank(),
          legend.position  = "right",
          strip.placement  = "outside",
          strip.clip       = "off"
        )
    })

    # --- Tab 2: geographical representativeness ---

    gg_bp_it2_2 <- reactive({
      it2_2_bp_f() %>%
        group_by(type, xlab) %>%
        mutate(proportion = proportion / sum(proportion)) %>%
        ungroup() %>%
        ggplot(aes(x       = xlab,
                   y       = proportion,
                   fill    = value,
                   tooltip = glue("{value}: {round(proportion * 100, 1)}%"))) +
        geom_col_interactive(position = "stack") +
        geom_hline(yintercept = 0.5, color = "red", linewidth = 0.5) +
        facet_grid(cols = vars(type), scales = "free_x", space = "free") +
        scale_fill_manual(values = surv_colorsGR) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
        # See gg_bp_it2 above for why guides(fill = "none") is needed in addition to bp_theme.
        guides(fill = "none") +
        bp_theme
    })

    gg_hm_it2_2 <- reactive({
      it2_2_hm_f() |>
        mutate(tooltip = glue("In <b>{Country}")) |>
        ggplot(aes(x = xlab, y = Country, fill = value, data_id = Country)) +
        geom_tile_interactive(aes(tooltip = tooltip), color = "white", linewidth = 0.5) +
        facet_grid(cols = vars(type), scales = "free_x", space = "free", switch = "both") +
        scale_fill_manual(
          name   = "Geographical representativeness",
          values = surv_colorsGR,
          breaks = c(
            "HIGH: all main geographical regions of the country are covered.",
            "MEDIUM: most geographical regions of the country are covered.",
            "LOW: a few geographical areas of the country are covered.",
            "Not part of national surveillance",
            "Do not know"
          ),
          labels = c(
            "HIGH: all main geographical\nregions covered",
            "MEDIUM: most geographical\nregions covered",
            "LOW: a few geographical\nareas covered",
            "Not part of national\nsurveillance",
            "Do not know"
          ),
          # Forced 2-column layout: the collected legend sits above a plot that's only
          # width_svg = 6in wide, and these labels are long enough that ggplot's automatic
          # legend wrapping (which works fine for the shorter Population coverage labels)
          # lays all 5 keys out on one line here and runs off the right edge. title.position
          # = "top" stacks the (also long) legend title above the key grid instead of beside
          # it - left as the default "left", the title alone eats over a third of the 6in
          # width, squeezing the 2-column key grid enough to clip "MEDIUM: most geographical".
          guide = guide_legend(ncol = 2, byrow = TRUE, title.position = "top")
        ) +
        theme_minimal() +
        theme(
          axis.text.x      = element_text(angle = 90, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title       = element_blank(),
          legend.position  = "right",
          strip.placement  = "outside",
          strip.clip       = "off"
        )
    })

    # --- Tab 3 ---

    gg_bp_it3 <- reactive({
      it3_bp_f() |>
        group_by(xlab) %>%
        mutate(proportion = proportion / sum(proportion)) %>%
        ungroup() %>%
        ggplot(aes(x       = xlab,
                   y       = proportion,
                   fill    = value,
                   tooltip = glue("{value}: {round(proportion * 100, 1)}%"))) +
        geom_col_interactive(position = "stack") +
        geom_hline(yintercept = 0.5, color = "red", linewidth = 0.5) +
        scale_fill_manual(values = surv_colorsEG) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
        bp_theme
    })

    gg_hm_it3 <- reactive({
      it3_hm_f() |>
        mutate(tooltip = glue("In <b>{Country}")) |>
        ggplot(aes(x = xlab, y = Country, fill = value, data_id = Country)) +
        geom_tile_interactive(aes(tooltip = tooltip), color = "white", linewidth = 0.5) +
        scale_fill_manual(name   = "National guidance in place",
                          values = surv_colorsEG,
                          breaks = c("Yes", "No", "Do not know")) +
        theme_minimal() +
        theme(
          axis.title       = element_blank(),
          axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 11),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position  = "none"
        )
    })

    ## Combine bar charts and heatmaps ----

    hm_no_strip <- theme(strip.text = element_blank())

    gg_combined_it1 <- reactive({
      gg_bp_it1() + plot_spacer() + (gg_hm_it1() + hm_no_strip) +
        plot_layout(ncol = 1, heights = c(4, -0.5, 10), guides = "collect") &
        theme(text = element_text(size = 10))
    })

    gg_combined_it2 <- reactive({
      gg_bp_it2() + plot_spacer() + (gg_hm_it2() + hm_no_strip) +
        plot_layout(ncol = 1, heights = c(4, -0.5, 10), guides = "collect") &
        theme(text = element_text(size = 10), legend.position = "top")
    })

    gg_combined_it2_2 <- reactive({
      gg_bp_it2_2() + plot_spacer() + (gg_hm_it2_2() + hm_no_strip) +
        plot_layout(ncol = 1, heights = c(4, -0.5, 10), guides = "collect") &
        theme(text = element_text(size = 10), legend.position = "top")
    })

    gg_combined_it3 <- reactive({
      gg_bp_it3() + plot_spacer() + (gg_hm_it3() + hm_no_strip) +
        plot_layout(ncol = 1, heights = c(4, -0.5, 10), guides = "collect") &
        theme(text = element_text(size = 10))
    })

    ## Shared girafe options ----

    # Highlight color for "activated" countries on the graphs - keep in sync with
    # .country-pill-activated in www/css/style.css.
    #
    # Uses an inset outline rather than a stroke: adjacent heatmap tiles share their
    # border line exactly, and a stroke is centered on that shared line, so whichever
    # tile happens to come later in the SVG's paint order can partially paint over (and
    # hide) the other's border. CSS draws the outline starting at outline-offset from
    # the true edge and extending outward by outline-width, so the offset magnitude
    # must exceed the width - otherwise the outline's outer edge still lands exactly on
    # the shared boundary (as it did before: 1.5px offset + 1.5px width = 0, i.e. right
    # on the edge) where a neighboring tile's border can still paint over it. Offset here
    # is comfortably larger than the width so the whole outline sits inside the tile,
    # clear of that shared line regardless of paint order.
    selection_css <- "outline: 1px solid #ff6a00; outline-offset: -2px;"

    girafe_opts <- list(
      opts_hover_inv(css = "opacity:0.5;"),
      opts_hover(css = "stroke-width:1;cursor:pointer", reactive = TRUE),
      opts_tooltip(use_fill = TRUE, css = paste0(
        "padding:5px;border-radius:3px;color:#000;text-shadow:",
        "-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff,",
        "0 -1px 0 #fff, 0 1px 0 #fff, -1px 0 0 #fff, 1px 0 0 #fff;"
      )),
      opts_zoom(max = 5),
      opts_sizing(rescale = TRUE),
      opts_toolbar(saveaspng = TRUE, position = "bottomright", delay_mouseout = 2000)
    )

    ## Insight tab 1 ----

    output$plot_it1 <- renderGirafe({
      req(nrow(it1_hm_f()) > 0)
      girafe(code    = print(gg_combined_it1()),
             width_svg  = 6,
             height_svg = 6.5,
             options = c(girafe_opts, list(
               opts_selection(
                 css        = selection_css,
                 type       = "multiple",
                 only_shiny = TRUE,
                 selected   = isolate(activatedCountries())),
               opts_toolbar(saveaspng = TRUE, position = "bottomright",
                            pngname = "JAMREYE_InsightTab_1", delay_mouseout = 2000)
             )))
    })

    # Sync the highlighted selection to the client without rebuilding the widget.
    observeEvent(activatedCountries(), {
      session$sendCustomMessage(
        type    = paste0(session$ns("plot_it1"), "_set"),
        message = activatedCountries()
      )
    }, ignoreInit = TRUE)

    output$country_comparison_header_if1 <- renderUI({
      country_comparison_header(input$plot_it1_selected)
    })

    output$country_context_if1 <- renderTable({
      req(input$plot_it1_selected)
      it1$hm %>%
        group_by(Country, value) %>%
        summarise(Number = n(), .groups = "drop") %>%
        complete(Country,
                 value = c("Yes, mandatory", "Yes, voluntary", "No"),
                 fill  = list(Number = 0)) %>%
        group_by(Country) %>%
        mutate(Percent = 100 * Number / sum(Number)) %>%
        ungroup() |>
        filter(Country %in% input$plot_it1_selected) |>
        tidyr::pivot_wider(id_cols = "Country", names_from = value, values_from = Percent) |>
        dplyr::transmute(Country,
                         "No surveillance" = No,
                         `Yes, voluntary`,
                         `Yes, mandatory`) |>
        dplyr::mutate(across(c(`No surveillance`, `Yes, voluntary`, `Yes, mandatory`),
                             ~ paste0(round(.x, 0), "%")))
    })

    output$md_content_it1 <- renderUI({
      div(class = "insight-md-content",
        render_collapsible_insight_md("content/md/tab1.md", session$ns("it1"),
          disclaimer = if (it1_filters_modified()) insight_text_disclaimer
        )
      )
    })

    ## Insight tab 2 — population coverage ----

    output$plot_it2 <- renderGirafe({
      req(nrow(it2_hm_f()) > 0)
      girafe(code    = print(gg_combined_it2()),
             width_svg  = 6,
             height_svg = 6.5,
             options = c(girafe_opts, list(
               opts_selection(
                 css        = selection_css,
                 type       = "multiple",
                 only_shiny = TRUE,
                 selected   = isolate(activatedCountries())),
               opts_toolbar(saveaspng = TRUE, position = "bottomright",
                            pngname = "JAMREYE_InsightTab_2", delay_mouseout = 2000)
             )))
    })

    # Sync the highlighted selection to the client without rebuilding the widget.
    observeEvent(activatedCountries(), {
      session$sendCustomMessage(
        type    = paste0(session$ns("plot_it2"), "_set"),
        message = activatedCountries()
      )
    }, ignoreInit = TRUE)

    output$country_comparison_header_if2 <- renderUI({
      country_comparison_header(input$plot_it2_selected)
    })

    output$country_context_if2 <- renderTable({
      req(input$plot_it2_selected)
      it2$hm %>%
        group_by(Country, value) %>%
        summarise(Number = n(), .groups = "drop") %>%
        complete(Country,
                 value = c("76-100%", "51-75%", "26-50%", "1-25%",
                           "Not part of national surveillance", "Do not know"),
                 fill  = list(Number = 0)) %>%
        group_by(Country) %>%
        mutate(Percent = 100 * Number / sum(Number)) %>%
        ungroup() |>
        filter(Country %in% input$plot_it2_selected) |>
        tidyr::pivot_wider(id_cols = "Country", names_from = value, values_from = Percent) |>
        dplyr::mutate(across(-Country, ~ paste0(round(.x, 0), "%")))
    })

    output$md_content_it2 <- renderUI({
      div(class = "insight-md-content",
        render_collapsible_insight_md("content/md/tab2.md", session$ns("it2"),
          disclaimer = if (it2_filters_modified()) insight_text_disclaimer
        )
      )
    })

    ## Insight tab 2 — geographical representativeness ----

    output$plot_it2_2 <- renderGirafe({
      req(nrow(it2_2_hm_f()) > 0)
      girafe(code    = print(gg_combined_it2_2()),
             width_svg  = 6,
             height_svg = 6.5,
             options = c(girafe_opts, list(
               opts_selection(
                 css        = selection_css,
                 type       = "multiple",
                 only_shiny = TRUE,
                 selected   = isolate(activatedCountries())),
               opts_toolbar(saveaspng = TRUE, position = "bottomright",
                            pngname = "JAMREYE_InsightTab_2b", delay_mouseout = 2000)
             )))
    })

    # Sync the highlighted selection to the client without rebuilding the widget.
    observeEvent(activatedCountries(), {
      session$sendCustomMessage(
        type    = paste0(session$ns("plot_it2_2"), "_set"),
        message = activatedCountries()
      )
    }, ignoreInit = TRUE)

    output$country_comparison_header_if2_2 <- renderUI({
      country_comparison_header(input$plot_it2_2_selected)
    })

    output$country_context_if2_2 <- renderTable({
      req(input$plot_it2_2_selected)
      it2_2$hm %>%
        group_by(Country, value) %>%
        summarise(Number = n(), .groups = "drop") %>%
        complete(Country,
                 value = c("HIGH: all main geographical regions of the country are covered.",
                           "MEDIUM: most geographical regions of the country are covered.",
                           "LOW: a few geographical areas of the country are covered.",
                           "Not part of national surveillance", "Do not know"),
                 fill  = list(Number = 0)) %>%
        group_by(Country) %>%
        mutate(Percent = 100 * Number / sum(Number)) %>%
        ungroup() |>
        filter(Country %in% input$plot_it2_2_selected) |>
        mutate(value = case_when(
          grepl("^HIGH",   value) ~ "HIGH",
          grepl("^MEDIUM", value) ~ "MEDIUM",
          grepl("^LOW",    value) ~ "LOW",
          TRUE                    ~ value
        )) |>
        tidyr::pivot_wider(id_cols = "Country", names_from = value, values_from = Percent) |>
        dplyr::mutate(across(-Country, ~ paste0(round(.x, 0), "%")))
    })

    ## Insight tab 3 ----

    output$plot_it3 <- renderGirafe({
      req(nrow(it3_hm_f()) > 0)
      girafe(code    = print(gg_combined_it3()),
             width_svg  = 6,
             height_svg = 6.5,
             options = c(girafe_opts, list(
               opts_selection(
                 css        = selection_css,
                 type       = "multiple",
                 only_shiny = TRUE,
                 selected   = isolate(activatedCountries())),
               opts_toolbar(saveaspng = TRUE, position = "bottomright",
                            pngname = "JAMREYE_InsightTab_3", delay_mouseout = 2000)
             )))
    })

    # Sync the highlighted selection to the client without rebuilding the widget.
    observeEvent(activatedCountries(), {
      session$sendCustomMessage(
        type    = paste0(session$ns("plot_it3"), "_set"),
        message = activatedCountries()
      )
    }, ignoreInit = TRUE)

    output$md_content_it3 <- renderUI({
      div(class = "insight-md-content",
        render_collapsible_insight_md("content/md/tab3.md", session$ns("it3"),
          disclaimer = if (it3_filters_modified()) insight_text_disclaimer
        )
      )
    })

    ## Insight tab 3 — AST data used ----

    surv_colorsNTG <- c(
      "Routine AST data"        = "#044556",
      "Extended AST data"       = "#34687a",
      "AST data from NRL"       = "#81b4c8",
      "International AST data." = "#aaddf2",
      "International guidance"  = "#CBFBFA",
      "No AST data used"        = "#b5a7b6",
      "Other"                   = "#4e3751"
    )

    coverage_orderNTG <- c("Routine AST data", "Extended AST data", "AST data from NRL",
                           "International AST data.", "International guidance",
                           "No AST data used", "Other")

    gg_it3_ast <- reactive({
      it3_ast_f() |>
        ggplot(aes(x       = count,
                   y       = Country,
                   fill    = AST.data.used.for.national.treatment.guidance,
                   tooltip = glue("{AST.data.used.for.national.treatment.guidance}"))) +
        geom_col_interactive(position = "stack") +
        scale_fill_manual(name   = "Information type",
                          values = surv_colorsNTG,
                          limits = coverage_orderNTG) +
        scale_x_continuous(breaks = scales::breaks_pretty()) +
        labs(x = "Number of countries") +
        theme_minimal() +
        theme(
          axis.title.y     = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank()
        )
    })

    output$plot_it3_ast <- renderGirafe({
      req(nrow(it3_ast_f()) > 0)
      girafe(code       = print(gg_it3_ast()),
             width_svg  = 6,
             height_svg = 5,
             options    = list(
               opts_hover_inv(css = "opacity:0.5;"),
               opts_hover(css = "stroke-width:1;"),
               opts_tooltip(use_fill = TRUE, css = paste0(
                 "padding:5px;border-radius:3px;color:#000;text-shadow:",
                 "-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff,",
                 "0 -1px 0 #fff, 0 1px 0 #fff, -1px 0 0 #fff, 1px 0 0 #fff;"
               )),
               opts_sizing(rescale = TRUE),
               opts_toolbar(saveaspng = TRUE, position = "bottomright",
                            pngname = "JAMREYE_InsightTab_3_AST", delay_mouseout = 2000)
             ))
    })

    ## Insight tab 3 — Who guides empiric treatment ----

    surv_colorsWGT <- c(
      "Regional and/or local guidance" = "#f9bcb3",
      "The healthcare facility"        = "#ca9088",
      "The treating physician(s)."     = "#9c665f",
      "Clinical microbiologist(s)"     = "#703f39",
      "Other."                         = "#471b17"
    )

    coverage_orderWGT <- c("Regional and/or local guidance", "The healthcare facility",
                           "The treating physician(s).", "Clinical microbiologist(s)", "Other.")

    gg_it3_wgt <- reactive({
      it3_wgt_f() |>
        ggplot(aes(x       = count,
                   y       = Country,
                   fill    = Who.guides.empiric.antibiotic.treatment.,
                   tooltip = glue("{Who.guides.empiric.antibiotic.treatment.}"))) +
        geom_col_interactive(position = "stack") +
        scale_fill_manual(name   = "Place / person",
                          values = surv_colorsWGT,
                          limits = coverage_orderWGT) +
        scale_x_continuous(breaks = scales::breaks_pretty()) +
        labs(x = "Number of countries") +
        theme_minimal() +
        theme(
          axis.title.y       = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank()
        )
    })

    output$plot_it3_wgt <- renderGirafe({
      req(nrow(it3_wgt_f()) > 0)
      girafe(code       = print(gg_it3_wgt()),
             width_svg  = 6,
             height_svg = 5,
             options    = list(
               opts_hover_inv(css = "opacity:0.5;"),
               opts_hover(css = "stroke-width:1;"),
               opts_tooltip(use_fill = TRUE, css = paste0(
                 "padding:5px;border-radius:3px;color:#000;text-shadow:",
                 "-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff,",
                 "0 -1px 0 #fff, 0 1px 0 #fff, -1px 0 0 #fff, 1px 0 0 #fff;"
               )),
               opts_sizing(rescale = TRUE),
               opts_toolbar(saveaspng = TRUE, position = "bottomright",
                            pngname = "JAMREYE_InsightTab_3_WGT", delay_mouseout = 2000)
             ))
    })

  })
}
