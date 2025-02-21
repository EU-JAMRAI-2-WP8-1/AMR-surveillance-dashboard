library(shiny)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(countrycode)
library(ggplot2)
library(echarts4r)
library(plotly)
library(leaflet)
library(gapminder)
library(bslib)
library(thematic)


# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

shiny::addResourcePath('www', '/srv/shiny-server/www')

appFilesDirectory = "/home/shiny-app/files"

## pour layout --> voir https://shiny.posit.co/r/articles/build/layout-guide/
## logo colors: #0fdbd5, #008aab, #26cad3

# create custom theme based on Bootstrap
custom_theme <- bs_theme(
    version = 5,
    #bg = "#fff",
    #fg = "#000",
    primary = "#008aab",
    secondary = "#0fdbd5",
    base_font = "Helvetica Neue,Helvetica,Arial,sans-serif"
    #"navbar-bg" = "rgba(0, 0, 0, 0)"
)

thematic_shiny(
  bg = "auto",
  fg = "auto",
  accent = "auto",
  font = NA,
  sequential = sequential_gradient(),
  qualitative = okabe_ito(),
  inherit = FALSE,
  session = shiny::getDefaultReactiveDomain()
)

# variables
jamraiLogoHeaderLong <- file.path("www/logos/TRANSPARENT_LONG2.png")
jamraiLogoHeaderRect <- file.path("www/logos/TRANSPARENT_RECTANGULAR.png")

# import and parse data
dataset <- read_excel("/home/shiny-app/files/data/survey_replies_20250217.xls", sheet="Content")
countryList <- na.omit(dataset[c(4:ncol(dataset)),1])


# TEST - MAP WITH PLOTLY
countryList <- read.csv("/home/shiny-app/files/data/countryList.csv")
countryReplies <- data.frame(country=countryList$Country, reply=rep(0, nrow(countryList)))
replies <- c("Ukraine", "Slovak Republic", "Denmark", "Latvia", "Estonia", "Norway", "Slovenia", "Sweden", "Luxembourg") ## todo -> take from input file instead

for (row in c(1:nrow(countryReplies))) {
  if (countryReplies[row, 1] %in% replies) {
    countryReplies[row, 2] <- 1
  }
}

### ICI

### pour mettre le dark mode sur les maps plotly -> utiliser ceci peut etre (sur serveur)



# user interface
ui <- shinyUI(fluidPage(
    input_dark_mode(id = "mode") %>% tagAppendAttributes(class="hidden"),
    theme = custom_theme,
    includeCSS(file.path("/srv/shiny-server/www/css/style.css")),
    page_navbar(
        nav_spacer(),
        nav_item(
            tags$a(
            tags$span(
                bsicons::bs_icon("cursor"),
                "JAMRAI"
            ),
            href = "https://eu-jamrai.eu/",
            target = "_blank"
            )
        ),
        nav_item(
            tags$a(
            tags$span(
                bsicons::bs_icon("file-earmark-code"), "Source code"
            ),
            href = "https://github.com/",
            target = "_blank"
            )
        ),
        nav_item(
            tags$a(
            tags$span(
                bsicons::bs_icon("file-earmark-arrow-down"), "Download data"
            ),
            href = "https://github.com/",
            target = "_blank"
            )
        ),
        nav_item(
            input_dark_mode(id = "dark_mode", mode = NULL)
        ),
        position = c("fixed-top")
    ),
    div() %>% tagAppendAttributes(class="top-spacer"),
    fluidRow(
        img(src=jamraiLogoHeaderLong) %>% tagAppendAttributes(class="jamrai-logo-header-long width-auto"),
        img(src=jamraiLogoHeaderRect) %>% tagAppendAttributes(class="jamrai-logo-header-rect width-auto"),
        div() %>% tagAppendAttributes(class="vertical-line"),
        div("EU-JAMRAI 2 - Human AMR surveillance systems in Europe") %>% tagAppendAttributes(class="main-title width-auto"),
    ) %>% tagAppendAttributes(class="header-box"),
    hr(),
    sidebarLayout(
        position = "left",
        sidebarPanel(
            tags$h4("Filters"),
            tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # remove slider little ticks
            selectInput(
                inputId  = "inCountry",
                label    = "Country - drop down",
                choices  = unique(gapminder$country[gapminder$continent == "Europe"]),
                selected = "Sweden",
                width    = "256px"
            ),
            checkboxGroupInput(
                inputId  = "countriesSelection",
                label    = "Country - checkboxes",
                choices  = unique(gapminder$country[gapminder$continent == "Europe"]),
                selected = unique(gapminder$country[gapminder$continent == "Europe"]),
                inline   = FALSE,
                width    = NULL
            ),
            sliderInput(
                inputId = "yearSlider",
                label   = "Year",
                min     = 2020,
                max     = 2024,
                value   = c(2020, 2024),
                step    = 1,
                width   = "256px"
            ),
            sliderInput("n", "Observations", 1, 100, 50, ticks = FALSE),
            sliderInput("bins", "Scale", 1, 10, 5, step = 1, ticks = FALSE),
            actionButton(
                inputId = "applyFilters",
                label   = "Apply filters"
            )
        ) %>% tagAppendAttributes(class="width-auto"),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    tags$span(
                        bsicons::bs_icon("bar-chart"),
                        tags$span("Dashboard") %>% tagAppendAttributes(class="tab-text")
                    ),
                    fluidRow(
                        plotOutput(outputId = "outChartLifeExp"),
                        plotOutput(outputId = "outChartGDP")
                    )
                ),
                tabPanel(
                    tags$span(
                        bsicons::bs_icon("globe-europe-africa"),
                        tags$span("Map") %>% tagAppendAttributes(class="tab-text")
                    ),
                    fluidRow(
                        tags$span("Countries participation to the survey") %>% tagAppendAttributes(class="plot-title"),
                        plotlyOutput(outputId = "mainMap")

                        #tags$span(
                        #    tags$span(
                        #        tags$span(
                        #            ""
                        #        ) %>% tagAppendAttributes(class="map-legend-color jamrai-blue"),
                        #        tags$span(
                        #            "Reply"
                        #        ) %>% tagAppendAttributes(class="map-legend-description"),
                        #        tags$span(
                        #            ""
                        #        ) %>% tagAppendAttributes(class="map-legend-color soft-red"),
                        #        tags$span(
                        #            "No reply"
                        #        ) %>% tagAppendAttributes(class="map-legend-description"),
                        #        tags$span(
                        #            ""
                        #        ) %>% tagAppendAttributes(class="map-legend-color soft-grey"),
                        #        tags$span(
                        #            "Not participating"
                        #        ) %>% tagAppendAttributes(class="map-legend-description")
                        #    ) %>% tagAppendAttributes(class="map-legend-row")
                        #) %>% tagAppendAttributes(class="map-legend-box")
                    )
                ),
                tabPanel(
                    tags$span(
                        bsicons::bs_icon("table"),
                        tags$span("Dataset") %>% tagAppendAttributes(class="tab-text")
                    ),
                    tableOutput("myTable")
                )
            ),
        ) %>% tagAppendAttributes(class="main-panel")
    ) %>% tagAppendAttributes(class="main-box"),
    #hr(),
    fluidRow( # footer
        img(src=file.path("www/logos/TRANSPARENT_LONG1_WHITE-480x107.png")) %>% tagAppendAttributes(class="width-auto footer-image"),
        img(src=file.path("www/logos/EN_Co-fundedbytheEU_RGB_WHITE-Outline-480x107.png")) %>% tagAppendAttributes(class="width-auto footer-image")
    ) %>% tagAppendAttributes(class="footer-box")
))

# server
server <- function(input, output, session) {
    
    # filter data and store as reactive value
    data <- reactive({
        gapminder %>%
            filter(country == input$inCountry) %>%
            group_by(year) %>%
            summarise(
                AvgLifeExp = round(mean(lifeExp)),
                AvgGdpPercap = round(mean(gdpPercap), digits = 2)
        )
    })

    # custom common properties for charts
    chart_theme <- ggplot2::theme(
        plot.title   = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12)
    )

    # render placeholder1 chart
    output$outChartLifeExp <- renderPlot({
        ggplot(data(), aes(x = year, y = AvgLifeExp)) +
            geom_col(fill = "#008aab") +
            geom_text(aes(label = AvgLifeExp), vjust = 2, size = 6, color = "#ffffff") +
            labs(title = paste("Placeholder graph - ", input$inCountry))
    })

    # render placeholder2 chart
    output$outChartGDP <- renderPlot({
        ggplot(data(), aes(x = year, y = AvgGdpPercap)) +
            geom_line(color = "#008aab", size = 2) +
            geom_point(color = "#008aab", size = 5) +
            geom_label(
                aes(label = AvgGdpPercap),
                nudge_x = 0.25,
                nudge_y = 0.25
            ) +
            labs(title = paste("Placeholder graph - ", input$inCountry))
    })

    output$plot <- renderPlot({
        hist(rnorm(input$n), breaks = input$bins, col = "#007bc2")
    })

    output$mainMap <- renderPlotly({

        if (input$dark_mode == "dark") {
            themeBgColor = "#1D1F21"
        } else {
            themeBgColor = "#ffffff"
        }

        countryReplies %>%
        plot_ly(
            #width="100%",
            height=680
        ) %>%
        add_trace(
            type='choropleth',
            locationmode="country names",
            #geojson=geojson,
            locations=countryReplies$country,
            z=countryReplies$reply,
            zmin=0,
            zmax=1,
            #showlegend=TRUE,
            #autocolorscale=FALSE,
            showscale=FALSE,
            colors=c("#cc8888","#0fdbd5"),
            #colorbar=list(
            #    title=list(
            #        text="Reply to the survey"
            #    ),
            #    dtick=1,
            #    nticks=2
            #    ),
            marker=list(line=list(width=1, color=themeBgColor))
        ) %>%
        layout(
            geo = list(
                #title="JAMRAI WP8.1 replies",
                scope='europe',
                showframe=FALSE,
                showland=TRUE,
                landcolor="#cccccc", #inside countries
                countrycolor=themeBgColor, #lines
                bgcolor = themeBgColor, # bg color - inside map
                #coastlinecolor="#fff",
                showcoastline=FALSE,
                projection=list(scale=1.5)
            ),
            #plot_bgcolor = "#000000", # no effect
            paper_bgcolor = themeBgColor, # bg color - outside map
            margin=list(t=0, r=0,  l=0, b=0),
            autosize=TRUE
        )

    })

}

# run
shinyApp(ui = ui, server = server)
