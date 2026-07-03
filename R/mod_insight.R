
mod_insight_ui <- function(id) {
  ns <- NS(id)

  tabsetPanel(
    id = ns("insightTabs"),

    tabPanel("Insight #1", value = "tab1",
      fluidRow(
        column(12,
          h3("National surveillance of AMR priority pathogens")
        )
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
        column(12,
          h3("Population coverage and geographical representativeness")
        )
      ),
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
      )
    ),

    tabPanel("Insight #3", value = "tab3",
      fluidRow(
        column(12,
          h3("National guidance on treatment of common infections")
        )
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
      )
    )

  )
}

mod_insight_server <- function(id, it1, it2, it2_2, it3, selected_tab) {
  moduleServer(id, function(input, output, session) {

    observeEvent(selected_tab(), {
      updateTabsetPanel(session, "insightTabs", selected = selected_tab())
    })

    ## Build ggplot objects ----

    surv_colors <- c("Yes, mandatory" = "#086D6A",
                     "Yes, voluntary" = "#0fdbd5",
                     "No"             = "#949494")

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

    surv_colorsPC <- c("76-100%"                           = "#086D6A",
                       "51-75%"                            = "#0BA4A0",
                       "26-50%"                            = "#64F4F0",
                       "1-25%"                             = "#CBFBFA",
                       "Do not know"                       = "#949494",
                       "Not part of national surveillance" = "#E3C19B")

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

    surv_colorsEG <- c("Yes"         = "#044556",
                       "No"          = "#F9BCB3",
                       "Do not know" = "#949494")

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

    ## Insight tab 1 ----

    output$plot_it1 <- renderGirafe({
      girafe(code = print(gg_hm_it1),
             width_svg = 6, height_svg = 5,
             options = list(
               opts_hover_inv(css = "opacity:0.5;"),
               opts_hover(css = "stroke-width:1;cursor:pointer", reactive = TRUE),
               opts_tooltip(use_fill = TRUE, css = "padding:5px;border-radius:3px;"),
               opts_zoom(max = 5),
               opts_selection(
                 css        = "stroke: black; stroke-width: 1.5px;",
                 type       = "multiple",
                 only_shiny = TRUE,
                 selected   = input$selected_country_it1),
               opts_sizing(rescale = TRUE),
               opts_toolbar(saveaspng     = TRUE,
                            position      = "bottomright",
                            pngname       = "JAMREYE_InsightTab_1",
                            delay_mouseout = 2000)))
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

    ## Insight tab 2 ----

    output$plot_it2 <- renderGirafe({
      girafe(code = print(gg_hm_it2),
             width_svg = 6, height_svg = 5,
             options = list(
               opts_hover_inv(css = "opacity:0.5;"),
               opts_hover(css = "stroke-width:1;cursor:pointer", reactive = TRUE),
               opts_tooltip(use_fill = TRUE, css = "padding:5px;border-radius:3px;"),
               opts_zoom(max = 5),
               opts_selection(
                 css        = "stroke: black; stroke-width: 1.5px;",
                 type       = "multiple",
                 only_shiny = TRUE,
                 selected   = input$selected_country_it2),
               opts_sizing(rescale = TRUE),
               opts_toolbar(saveaspng     = TRUE,
                            position      = "bottomright",
                            pngname       = "JAMREYE_InsightTab_2",
                            delay_mouseout = 2000)))
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

    ## Insight tab 3 ----

    output$plot_it3 <- renderGirafe({
      girafe(code = print(gg_hm_it3),
             width_svg = 6, height_svg = 5,
             options = list(
               opts_hover_inv(css = "opacity:0.5;"),
               opts_hover(css = "stroke-width:1;cursor:pointer", reactive = TRUE),
               opts_tooltip(use_fill = TRUE, css = "padding:5px;border-radius:3px;"),
               opts_zoom(max = 5),
               opts_selection(
                 css        = "stroke: black; stroke-width: 1.5px;",
                 type       = "multiple",
                 only_shiny = TRUE,
                 selected   = input$selected_country_it3),
               opts_sizing(rescale = TRUE),
               opts_toolbar(saveaspng     = TRUE,
                            position      = "bottomright",
                            pngname       = "JAMREYE_InsightTab_3",
                            delay_mouseout = 2000)))
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

  })
}
