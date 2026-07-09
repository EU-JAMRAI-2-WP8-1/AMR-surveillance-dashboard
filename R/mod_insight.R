
mod_insight_ui <- function(id) {
  ns <- NS(id)

  tabsetPanel(
    id = ns("insightTabs"),

    tabPanel("Insight #1", value = "tab1",
      fluidRow(
        column(12, h3("National surveillance of AMR priority pathogens"))
      ),
      fluidRow(
        column(6,
          actionButton(ns("reset_it1"), "Reset selection", class = "btn btn-outline-primary", width = "100%"),
          girafeOutput(ns("plot_it1"))
        ),
        column(6,
          uiOutput(ns("md_content_it1"))
        )
      ),
      fluidRow(
        column(12,
          tags$p(tags$strong("Selected countrie(s) head-to-head comparison:")),
          tableOutput(ns("country_context_if1"))
        )
      )
    ),

    tabPanel("Insight #2", value = "tab2",
      fluidRow(
        column(12, h3("Population coverage"))
      ),
      # Population coverage
      fluidRow(
        column(6,
          actionButton(ns("reset_it2"), "Reset selection", class = "btn btn-outline-primary", width = "100%"),
          girafeOutput(ns("plot_it2"))
        ),
        column(6,
          uiOutput(ns("md_content_it2"))
        )
      ),
      fluidRow(
        column(12,
          tags$p(tags$strong("Selected countrie(s) head-to-head comparison:")),
          tableOutput(ns("country_context_if2"))
        )
      ),
      # Geographical representativeness
      fluidRow(
        column(12, h4("Geographical representativeness"))
      ),
      fluidRow(
        column(6,
          actionButton(ns("reset_it2_2"), "Reset selection", class = "btn btn-outline-primary", width = "100%"),
          girafeOutput(ns("plot_it2_2"))
        ),
        column(6)
      ),
      fluidRow(
        column(12,
          tags$p(tags$strong("Selected countrie(s) head-to-head comparison:")),
          tableOutput(ns("country_context_if2_2"))
        )
      )
    ),

    tabPanel("Insight #3", value = "tab3",
      fluidRow(
        column(12, h3("National guidance on treatment of common infections"))
      ),
      fluidRow(
        column(6,
          actionButton(ns("reset_it3"), "Reset selection", class = "btn btn-outline-primary", width = "100%"),
          girafeOutput(ns("plot_it3"))
        ),
        column(6,
          uiOutput(ns("md_content_it3"))
        )
      ),
      fluidRow(
        column(12,
          tags$p(tags$strong("Selected countrie(s) head-to-head comparison:")),
          tableOutput(ns("country_context_if3"))
        )
      ),
      fluidRow(
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

mod_insight_server <- function(id, it1, it2, it2_2, it3, it3_ast, it3_wgt, selected_tab) {
  moduleServer(id, function(input, output, session) {

    observeEvent(selected_tab(), {
      updateTabsetPanel(session, "insightTabs", selected = selected_tab())
    })

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

    # --- Tab 1 ---

    gg_bp_it1 <- it1$bp %>%
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

    gg_hm_it1 <- it1$hm %>%
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

    # --- Tab 2: population coverage ---

    gg_bp_it2 <- it2$bp %>%
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
      bp_theme

    gg_hm_it2 <- it2$hm |>
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

    # --- Tab 2: geographical representativeness ---

    gg_bp_it2_2 <- it2_2$bp %>%
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
      bp_theme

    gg_hm_it2_2 <- it2_2$hm |>
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
          "Not part of national surveillance",
          "Do not know"
        )
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

    # --- Tab 3 ---

    gg_bp_it3 <- it3$bp |>
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

    gg_hm_it3 <- it3$hm |>
      mutate(tooltip = glue("In <b>{Country}")) |>
      ggplot(aes(x = xlab, y = Country, fill = value, data_id = Country)) +
      geom_tile_interactive(aes(tooltip = tooltip), color = "white", linewidth = 0.5) +
      scale_fill_manual(name   = "National guidance in place",
                        values = surv_colorsEG,
                        breaks = c("Yes", "No", "Do not know")) +
      theme_minimal() +
      theme(
        axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position  = "none"
      )

    ## Combine bar charts and heatmaps ----

    hm_no_strip <- theme(strip.text = element_blank())

    gg_combined_it1 <- (
      gg_bp_it1 + plot_spacer() + (gg_hm_it1 + hm_no_strip) +
        plot_layout(ncol = 1, heights = c(4, -0.5, 10), guides = "collect") &
        theme(text = element_text(size = 10))
    )

    gg_combined_it2 <- (
      gg_bp_it2 + plot_spacer() + (gg_hm_it2 + hm_no_strip) +
        plot_layout(ncol = 1, heights = c(4, -0.5, 10), guides = "collect") &
        theme(text = element_text(size = 10))
    )

    gg_combined_it2_2 <- (
      gg_bp_it2_2 + plot_spacer() + (gg_hm_it2_2 + hm_no_strip) +
        plot_layout(ncol = 1, heights = c(4, -0.5, 10), guides = "collect") &
        theme(text = element_text(size = 10))
    )

    gg_combined_it3 <- (
      gg_bp_it3 + plot_spacer() + (gg_hm_it3 + hm_no_strip) +
        plot_layout(ncol = 1, heights = c(4, -0.5, 10), guides = "collect") &
        theme(text = element_text(size = 10))
    )

    ## Shared girafe options ----

    girafe_opts <- list(
      opts_hover_inv(css = "opacity:0.5;"),
      opts_hover(css = "stroke-width:1;cursor:pointer", reactive = TRUE),
      opts_tooltip(use_fill = TRUE, css = "padding:5px;border-radius:3px;"),
      opts_zoom(max = 5),
      opts_sizing(rescale = TRUE),
      opts_toolbar(saveaspng = TRUE, position = "bottomright", delay_mouseout = 2000)
    )

    ## Insight tab 1 ----

    output$plot_it1 <- renderGirafe({
      girafe(code    = print(gg_combined_it1),
             width_svg  = 6,
             height_svg = 6.5,
             options = c(girafe_opts, list(
               opts_selection(
                 css        = "stroke: black; stroke-width: 1.5px;",
                 type       = "multiple",
                 only_shiny = TRUE,
                 selected   = input$selected_country_it1),
               opts_toolbar(saveaspng = TRUE, position = "bottomright",
                            pngname = "JAMREYE_InsightTab_1", delay_mouseout = 2000)
             )))
    })

    observeEvent(input$reset_it1, {
      session$sendCustomMessage(
        type    = paste0(session$ns("plot_it1"), "_set"),
        message = character(0)
      )
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
                             ~ paste0(round(.x, 2), "%")))
    })

    output$md_content_it1 <- renderUI({
      md <- readLines("content/md/tab1.md")
      div(class = "insight-md-content",
        HTML(markdown::markdownToHTML(paste(md, collapse = "\n"), fragment.only = TRUE))
      )
    })

    ## Insight tab 2 — population coverage ----

    output$plot_it2 <- renderGirafe({
      girafe(code    = print(gg_combined_it2),
             width_svg  = 6,
             height_svg = 6.5,
             options = c(girafe_opts, list(
               opts_selection(
                 css        = "stroke: black; stroke-width: 1.5px;",
                 type       = "multiple",
                 only_shiny = TRUE,
                 selected   = input$selected_country_it2),
               opts_toolbar(saveaspng = TRUE, position = "bottomright",
                            pngname = "JAMREYE_InsightTab_2", delay_mouseout = 2000)
             )))
    })

    observeEvent(input$reset_it2, {
      session$sendCustomMessage(
        type    = paste0(session$ns("plot_it2"), "_set"),
        message = character(0)
      )
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
        tidyr::pivot_wider(id_cols = "Country", names_from = value, values_from = Percent)
    })

    output$md_content_it2 <- renderUI({
      md <- readLines("content/md/tab2.md")
      div(class = "insight-md-content",
        HTML(markdown::markdownToHTML(paste(md, collapse = "\n"), fragment.only = TRUE))
      )
    })

    ## Insight tab 2 — geographical representativeness ----

    output$plot_it2_2 <- renderGirafe({
      girafe(code    = print(gg_combined_it2_2),
             width_svg  = 6,
             height_svg = 6.5,
             options = c(girafe_opts, list(
               opts_selection(
                 css        = "stroke: black; stroke-width: 1.5px;",
                 type       = "multiple",
                 only_shiny = TRUE,
                 selected   = input$selected_country_it2_2),
               opts_toolbar(saveaspng = TRUE, position = "bottomright",
                            pngname = "JAMREYE_InsightTab_2b", delay_mouseout = 2000)
             )))
    })

    observeEvent(input$reset_it2_2, {
      session$sendCustomMessage(
        type    = paste0(session$ns("plot_it2_2"), "_set"),
        message = character(0)
      )
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
        tidyr::pivot_wider(id_cols = "Country", names_from = value, values_from = Percent)
    })

    ## Insight tab 3 ----

    output$plot_it3 <- renderGirafe({
      girafe(code    = print(gg_combined_it3),
             width_svg  = 6,
             height_svg = 6.5,
             options = c(girafe_opts, list(
               opts_selection(
                 css        = "stroke: black; stroke-width: 1.5px;",
                 type       = "multiple",
                 only_shiny = TRUE,
                 selected   = input$selected_country_it3),
               opts_toolbar(saveaspng = TRUE, position = "bottomright",
                            pngname = "JAMREYE_InsightTab_3", delay_mouseout = 2000)
             )))
    })

    observeEvent(input$reset_it3, {
      session$sendCustomMessage(
        type    = paste0(session$ns("plot_it3"), "_set"),
        message = character(0)
      )
    })

    output$country_context_if3 <- renderTable({
      req(input$plot_it3_selected)
      it3$hm %>%
        group_by(Country, value) %>%
        summarise(Number = n(), .groups = "drop") %>%
        complete(Country,
                 value = c("Yes", "No", "Do not know"),
                 fill  = list(Number = 0)) %>%
        group_by(Country) %>%
        mutate(Percent = 100 * Number / sum(Number)) %>%
        ungroup() |>
        filter(Country %in% input$plot_it3_selected) |>
        tidyr::pivot_wider(id_cols = "Country", names_from = value, values_from = Percent)
    })

    output$md_content_it3 <- renderUI({
      md <- readLines("content/md/tab3.md")
      div(class = "insight-md-content",
        HTML(markdown::markdownToHTML(paste(md, collapse = "\n"), fragment.only = TRUE))
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

    gg_it3_ast <- it3_ast |>
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

    output$plot_it3_ast <- renderGirafe({
      girafe(code       = print(gg_it3_ast),
             width_svg  = 6,
             height_svg = 5,
             options    = list(
               opts_hover_inv(css = "opacity:0.5;"),
               opts_hover(css = "stroke-width:1;"),
               opts_tooltip(use_fill = TRUE, css = "padding:5px;border-radius:3px;"),
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

    gg_it3_wgt <- it3_wgt |>
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

    output$plot_it3_wgt <- renderGirafe({
      girafe(code       = print(gg_it3_wgt),
             width_svg  = 6,
             height_svg = 5,
             options    = list(
               opts_hover_inv(css = "opacity:0.5;"),
               opts_hover(css = "stroke-width:1;"),
               opts_tooltip(use_fill = TRUE, css = "padding:5px;border-radius:3px;"),
               opts_sizing(rescale = TRUE),
               opts_toolbar(saveaspng = TRUE, position = "bottomright",
                            pngname = "JAMREYE_InsightTab_3_WGT", delay_mouseout = 2000)
             ))
    })

  })
}
