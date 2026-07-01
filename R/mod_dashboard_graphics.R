mod_dashboard_graphics_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 5,
      tags$div(
        class = "question-selector-card",
        selectizeInput(
          inputId   = ns("questionSelection"),
          label     = HTML("<strong>Select a question</strong> (tip: use the left filter panel first to narrow the selection)"),
          choices   = c("Participating countries", allShortTitles),
          selected  = "Participating countries",
          multiple  = FALSE,
          width     = "99%",
          options   = list(
            render = I("{
              option: function(item, escape) { return '<div>' + item.label + '</div>'; },
              item:   function(item, escape) { return '<div>' + item.label + '</div>'; }
            }")
          )
        )
      ),
      uiOutput(ns("multipleChoiceAnswerSelector")),
      tags$div(
        class = "dashboard-plot-container",
        uiOutput(ns("dashboardPlotUI"))
      )
    ),
    column(
      width = 7,
      tags$div(class = "full-question-title-box", uiOutput(ns("fullQuestionTitle"))),
      tags$div(
        class = "dashboard-map-box",
        plotlyOutput(outputId = ns("dashboardMap"), width = "100%", height = "780px")
      ),
      tags$div(
        class = "map-disclaimer-text",
        "Geospatial data © Eurostat | ",
        tags$a(href = "#", class = "geo-disclaimer-link", "Legal notice")
      )
    )
  )
}

mod_dashboard_graphics_server <- function(id, filters, active_questions) {
  moduleServer(id, function(input, output, session) {

    observeEvent(filters(), {
      choicesWithBadges <- setNames(
        c("Participating countries", active_questions()[[2]]),
        c("Participating countries", active_questions()[[3]])
      )
      updateSelectizeInput(session, "questionSelection",
                           choices = choicesWithBadges,
                           selected = "Participating countries")
    }, ignoreNULL = FALSE)

    getCountryScores <- function() {
      if (length(filters()$sections) == 0) {
        return(list(
          rep(0, length(intersect(repliedCountries, filters()$countries))),
          rep(0, length(intersect(repliedCountries, filters()$countries))),
          0
        ))
      }
      countryScores <- rep(0, length(repliedCountries))
      countryMaxScores <- rep(0, length(repliedCountries))
      activeQuestionsAmount <- 0
      for (column in 2:ncol(countryScoreTable)) {
        if (!(countryScoreTable[1, column] %in% active_questions()[[1]])) next
        activeQuestionsAmount <- activeQuestionsAmount + as.double(countryScoreTable[3, column])
        for (row in 1:(((nrow(countryScoreTable) - 3) / 2))) {
          if (!(countryScoreTable[(row * 2) + 2, 1] %in% filters()$countries)) next
          countryScores[row] <- countryScores[row] + as.double(countryScoreTable[(row * 2) + 2, column])
          countryMaxScores[row] <- countryMaxScores[row] + as.double(countryScoreTable[(row * 2) + 3, column])
        }
      }
      countryScoreRatios <- c()
      countryMaxScoresClean <- c()
      for (i in 1:length(repliedCountries)) {
        if (countryMaxScores[i] != 0) {
          countryScoreRatios <- c(countryScoreRatios, countryScores[i] / countryMaxScores[i])
          countryMaxScoresClean <- c(countryMaxScoresClean, countryMaxScores[i] / activeQuestionsAmount)
        }
      }
      if (length(countryScoreRatios) == 0) {
        countryScoreRatios <- rep(0, length(intersect(repliedCountries, filters()$countries)))
      }
      return(list(countryScoreRatios, countryMaxScoresClean))
    }

    getSingleQuestionReplies <- function() {
      if (input$questionSelection == "Participating countries") {
        return(list(
          rep(1, length(intersect(repliedCountries, filters()$countries))),
          colorSequence[1],
          c("Yes"),
          c(length(intersect(repliedCountries, filters()$countries)), 0)
        ))
      }
      for (question in surveyData) {
        if (question[["short_title"]] == input$questionSelection && question[["type"]] == "SingleChoice") {
          possibleAnswers <- question[["possible_answers"]]
          actualAnswers   <- question[["actual_answers"]]
          answersNumericReference <- 1:length(possibleAnswers)
          answersNumeric <- c()
          for (answer in names(actualAnswers)) {
            if (!(answer %in% filters()$countries)) next
            i <- 1
            for (possibleAnswer in names(possibleAnswers)) {
              if (actualAnswers[[answer]] == possibleAnswer) answersNumeric <- c(answersNumeric, answersNumericReference[i])
              i <- i + 1
            }
          }
          possibleAnswerText <- c(); possibleAnswerColors <- c(); possibleAnswerOccurences <- c(); colorIndex <- 1
          for (possibleAnswer in names(possibleAnswers)) {
            possibleAnswerText <- c(possibleAnswerText, possibleAnswer)
            answerColor <- possibleAnswers[[possibleAnswer]][["color"]]
            if (is.null(answerColor)) {
              fallbackIndex <- ((colorIndex - 1) %% length(colorSequence)) + 1
              answerColor <- colorSequence[fallbackIndex]
            }
            possibleAnswerColors <- c(possibleAnswerColors, answerColor)
            colorIndex <- colorIndex + 1
            occurencesCounter <- 0
            for (answer in names(actualAnswers)) {
              if (!(answer %in% filters()$countries)) next
              if (actualAnswers[[answer]] == possibleAnswer) occurencesCounter <- occurencesCounter + 1
            }
            possibleAnswerOccurences <- c(possibleAnswerOccurences, occurencesCounter)
          }
          possibleAnswerPercentReplied <- possibleAnswerOccurences / length(intersect(filters()$countries, repliedCountries)) * 100
          return(list(answersNumeric, possibleAnswerColors, possibleAnswerText, possibleAnswerPercentReplied))
        }
        else if (question[["short_title"]] == input$questionSelection && question[["type"]] == "MultipleChoice") {
          possibleAnswers <- question[["possible_answers"]]
          actualAnswers   <- question[["actual_answers"]]
          selectedCountries <- intersect(filters()$countries, repliedCountries)
          filteredActualAnswers <- actualAnswers[names(actualAnswers) %in% selectedCountries]
          allActualAnswers <- unlist(filteredActualAnswers, use.names = FALSE)
          isFollowUpQuestion <- grepl("^You answered", question[["title"]], ignore.case = FALSE)
          possibleAnswerOccurences <- c(); possibleAnswerText <- c(); possibleAnswerColors <- c(); colorIndex <- 1
          for (possibleAnswer in names(possibleAnswers)) {
            possibleAnswerText <- c(possibleAnswerText, possibleAnswer)
            possibleAnswerOccurences <- c(possibleAnswerOccurences, sum(allActualAnswers == possibleAnswer))
            answerColor <- possibleAnswers[[possibleAnswer]][["color"]]
            if (is.null(answerColor)) {
              fallbackIndex <- ((colorIndex - 1) %% length(colorSequence)) + 1
              answerColor <- colorSequence[fallbackIndex]
            }
            possibleAnswerColors <- c(possibleAnswerColors, answerColor)
            colorIndex <- colorIndex + 1
          }
          possibleAnswerPercentReplied <- possibleAnswerOccurences / length(intersect(filters()$countries, repliedCountries)) * 100
          answersNumeric <- NULL; customColorScaleMap <- NULL
          if (!is.null(input$selectedAnswer)) {
            answersNumeric <- c()
            for (country in intersect(repliedCountries, filters()$countries)) {
              hasAnswers <- FALSE
              if (country %in% names(actualAnswers)) {
                countryAnswers <- actualAnswers[[country]]
                if (!is.null(countryAnswers) && length(countryAnswers) > 0 && !all(is.na(countryAnswers))) {
                  nonEmptyAnswers <- countryAnswers[countryAnswers != ""]
                  if (length(nonEmptyAnswers) > 0) hasAnswers <- TRUE
                }
              }
              if (hasAnswers) {
                answersNumeric <- c(answersNumeric, if (input$selectedAnswer %in% countryAnswers) 1 else 2)
              } else {
                answersNumeric <- c(answersNumeric, if (isFollowUpQuestion) 3 else 2)
              }
            }
            customColorScaleMap <- if (isFollowUpQuestion) c("#0fdbd5", "#df2e1a", "#888888") else c("#0fdbd5", "#df2e1a")
          }
          return(list(answersNumeric, customColorScaleMap, possibleAnswerText, possibleAnswerPercentReplied, possibleAnswerColors))
        }
      }
      return(list(answersNumeric = NULL, customColorScale = NULL, possibleAnswerText = NULL, possibleAnswerOccurences = NULL))
    }

    countryScores <- reactive({ getCountryScores() })
    countryReplies <- reactive({ getSingleQuestionReplies() })
    getNonParticipatingCountries <- reactive({
      c(setdiff(repliedCountries, filters()$countries), nonParticipatingCountries)
    })

    output$dashboardPlotUI <- renderUI({
      if (input$questionSelection %in% multipleChoiceShortTitles) {
        plotlyOutput(session$ns("dashboardPlot"), height = "520px")
      } else {
        plotlyOutput(session$ns("dashboardPlot"), height = "700px")
      }
    })

    output$fullQuestionTitle <- renderUI({
      if (input$questionSelection == "Participating countries") {
        tags$div(class = "question-title-text", tags$strong("Question: "), "Which countries participated in the survey?")
      } else {
        fullTitle <- NULL
        for (question in surveyData) {
          if (question$short_title == input$questionSelection) { fullTitle <- question$display_title; break }
        }
        if (!is.null(fullTitle)) tags$div(class = "question-title-text", tags$strong("Question: "), HTML(fullTitle))
      }
    })

    output$multipleChoiceAnswerSelector <- renderUI({
      if (input$questionSelection %in% multipleChoiceShortTitles) {
        possibleAnswers <- c(); isFollowUpQuestion <- FALSE
        for (question in surveyData) {
          if (question$short_title == input$questionSelection && question$type == "MultipleChoice") {
            possibleAnswers <- names(question$possible_answers)
            isFollowUpQuestion <- grepl("^You answered", question$title, ignore.case = FALSE)
            break
          }
        }
        if (length(possibleAnswers) > 0) {
          legendItems <- list(
            tags$span(class = "legend-item", tags$span(class = "legend-color-swatch legend-color-selected"), tags$span("Selected")),
            tags$span(class = "legend-item", tags$span(class = "legend-color-swatch legend-color-not-selected"), tags$span("Not selected"))
          )
          if (isFollowUpQuestion) legendItems[[3]] <- tags$span(class = "legend-item", tags$span(class = "legend-color-swatch legend-color-na"), tags$span("NA"))
          tags$div(
            class = "answer-selector-container",
            tags$label(class = "answer-selector-label", "Multiple Selection Question: Choose an answer option to view on the map:"),
            selectInput(inputId = session$ns("selectedAnswer"), label = NULL, choices = possibleAnswers, selected = possibleAnswers[1], width = "100%"),
            tags$div(class = "legend-row", legendItems)
          )
        }
      }
    })

    output$dashboardMap <- renderPlotly({
      themeBgColor <- "#ffffff"
      themeFgColor <- "#1D1F21"
      if (input$questionSelection == "Participating countries") {
        dashboardMap <- plot_ly()
        dashboardMap <- dashboardMap %>% add_trace(
          type = 'choropleth', geojson = geojsonEurope,
          locations = participationData$country, z = participationData$survey_participation,
          zmin = 1, zmax = 3, text = participationData$country, hoverinfo = "text",
          showlegend = FALSE, showscale = FALSE, reversescale = FALSE,
          colors = c(colorSequence[1], colorSequence[2], "#b3b3b3"),
          marker = list(line = list(width = 1, color = themeBgColor))
        )
      } else if (input$questionSelection %in% multipleChoiceShortTitles) {
        if (is.null(countryReplies()[[1]]) || is.null(countryReplies()[[2]])) return(NULL)
        dashboardMap <- plot_ly()
        dashboardMap <- dashboardMap %>% add_trace(
          type = 'choropleth', geojson = geojsonEurope,
          locations = intersect(repliedCountries, filters()$countries), z = countryReplies()[[1]],
          zmin = 1, zmax = length(countryReplies()[[2]]),
          text = intersect(repliedCountries, filters()$countries), hoverinfo = "text",
          showlegend = FALSE, showscale = FALSE, reversescale = FALSE,
          colors = countryReplies()[[2]], marker = list(line = list(width = 1, color = themeBgColor))
        )
        dashboardMap <- dashboardMap %>% add_trace(
          name = "Not participating", type = 'choropleth', geojson = geojsonEurope,
          locations = getNonParticipatingCountries(), z = rep(0.7, length(getNonParticipatingCountries())),
          zmin = 0, zmax = 1, text = getNonParticipatingCountries(), hoverinfo = "text",
          showscale = FALSE, colorscale = "Greys", marker = list(line = list(width = 1, color = themeBgColor))
        )
      } else {
        dashboardMap <- plot_ly()
        dashboardMap <- dashboardMap %>% add_trace(
          type = 'choropleth', geojson = geojsonEurope,
          locations = intersect(repliedCountries, filters()$countries), z = countryReplies()[[1]],
          zmin = 1, zmax = length(countryReplies()[[2]]),
          text = intersect(repliedCountries, filters()$countries), hoverinfo = "text",
          showlegend = FALSE, showscale = FALSE, reversescale = FALSE,
          colors = countryReplies()[[2]], marker = list(line = list(width = 1, color = themeBgColor))
        )
        dashboardMap <- dashboardMap %>% add_trace(
          name = "Not participating", type = 'choropleth', geojson = geojsonEurope,
          locations = getNonParticipatingCountries(), z = rep(0.7, length(getNonParticipatingCountries())),
          zmin = 0, zmax = 1, text = getNonParticipatingCountries(), hoverinfo = "text",
          showscale = FALSE, colorscale = "Greys", marker = list(line = list(width = 1, color = themeBgColor))
        )
      }
      dashboardMap %>% layout(
        geo = list(
          scope = "europe", showcountries = FALSE, showframe = FALSE, showland = FALSE,
          bgcolor = "rgba(0, 0, 0, 0)", showcoastline = FALSE,
          projection = list(scale = 1.7), center = list(lat = 54, lon = 12)
        ),
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        margin = list(t = 32, r = 0, l = 0, b = 32),
        dragmode = FALSE, autosize = TRUE
      ) %>% config(displaylogo = FALSE)
    })

    output$dashboardPlot <- renderPlotly({
      wrapLabel <- function(label, maxCharsPerLine = 24) {
        if (nchar(label) <= maxCharsPerLine) return(label)
        split <- strsplit(label, " ")[[1]]
        lines <- c(); currentLine <- ""
        for (word in split) {
          testLine <- if (nchar(currentLine) == 0) word else paste(currentLine, word)
          if (nchar(testLine) > maxCharsPerLine && nchar(currentLine) > 0) {
            lines <- c(lines, currentLine); currentLine <- word
            if (length(lines) >= 3) { lines[3] <- paste0(lines[3], "..."); return(paste(lines, collapse = "<br>")) }
          } else { currentLine <- testLine }
        }
        if (nchar(currentLine) > 0) lines <- c(lines, currentLine)
        return(paste(lines, collapse = "<br>"))
      }
      if (input$questionSelection == "Participating countries") {
        numBars <- nrow(participationDataOccurrences)
        plotHeight <- max(400, numBars * 90 + 150)
        sortOrder <- order(participationDataOccurrences$occurences, decreasing = FALSE)
        replies <- participationDataOccurrences$reply[sortOrder]
        occurrences <- participationDataOccurrences$occurences[sortOrder]
        barColors <- c(colorSequence[1], colorSequence[2], "#b3b3b3")[sortOrder]
        wrappedLabels <- sapply(replies, wrapLabel)
        textLabels <- paste0(sprintf("%.0f", occurrences), "%")
        textPositions <- ifelse(occurrences >= 20, occurrences - 3, occurrences + 12)
        textColors <- ifelse(occurrences >= 20, "white", "black")
        p <- plot_ly(height = plotHeight) %>%
          add_bars(y = wrappedLabels, x = occurrences, orientation = 'h',
                   marker = list(color = barColors), width = 0.5,
                   hovertemplate = paste0("%{y}: %{x:.2f}%<extra></extra>"), showlegend = FALSE)
        for (i in seq_along(wrappedLabels)) {
          p <- p %>% add_annotations(y = wrappedLabels[i], x = textPositions[i], text = textLabels[i],
                                     xanchor = "left", showarrow = FALSE,
                                     font = list(size = 20, color = textColors[i], family = "Arial"))
        }
        p %>% layout(
          xaxis = list(title = "% of European countries", side = "top", range = c(100, 0),
                       showline = TRUE, linecolor = "gray50", linewidth = 0.5, showgrid = FALSE,
                       zeroline = FALSE, tickfont = list(size = 17, family = "Arial"),
                       titlefont = list(size = 17, family = "Arial"), fixedrange = TRUE, ticksuffix = "%"),
          yaxis = list(title = "", side = "right", showline = TRUE, linecolor = "gray50", linewidth = 0.5,
                       showgrid = FALSE, tickfont = list(size = 20, family = "Arial", weight = 700),
                       fixedrange = TRUE, automargin = TRUE, categoryorder = "array", categoryarray = wrappedLabels),
          margin = list(l = 10, r = 200, t = 50, b = 50),
          font = list(family = "Arial, sans-serif"), plot_bgcolor = "white", paper_bgcolor = "white", autosize = TRUE
        ) %>% config(displaylogo = FALSE, responsive = TRUE)
      } else {
        if (is.null(countryReplies()[[3]]) || is.null(countryReplies()[[4]])) return(NULL)
        numBars <- length(countryReplies()[[3]])
        plotHeight <- max(400, numBars * 90 + 150)
        sortOrder <- order(countryReplies()[[4]], decreasing = FALSE)
        replies <- countryReplies()[[3]][sortOrder]
        occurrences <- countryReplies()[[4]][sortOrder]
        barColors <- if (input$questionSelection %in% multipleChoiceShortTitles && length(countryReplies()) >= 5) {
          rep("#008aab", length(replies))
        } else {
          countryReplies()[[2]][sortOrder]
        }
        maxLabelLength <- max(nchar(replies))
        threshold <- if (maxLabelLength <= 24) 24 else min(ceiling(maxLabelLength / 3), 24)
        wrappedLabels <- sapply(replies, function(x) wrapLabel(x, threshold))
        textLabels <- paste0(sprintf("%.0f", occurrences), "%")
        textPositions <- ifelse(occurrences >= 20, occurrences - 3, occurrences + 12)
        textColors <- ifelse(occurrences >= 20, "white", "black")
        yAxisLabel <- "% of selected countries"
        p <- plot_ly(height = plotHeight) %>%
          add_bars(y = wrappedLabels, x = occurrences, orientation = 'h',
                   marker = list(color = barColors), width = 0.5,
                   hovertemplate = paste0("%{y}: %{x:.2f}%<extra></extra>"), showlegend = FALSE)
        for (i in seq_along(wrappedLabels)) {
          p <- p %>% add_annotations(y = wrappedLabels[i], x = textPositions[i], text = textLabels[i],
                                     xanchor = "left", showarrow = FALSE,
                                     font = list(size = 20, color = textColors[i], family = "Arial"))
        }
        p %>% layout(
          xaxis = list(title = yAxisLabel, side = "top", range = c(100, 0),
                       showline = TRUE, linecolor = "gray50", linewidth = 0.5, showgrid = FALSE,
                       zeroline = FALSE, tickfont = list(size = 17, family = "Arial"),
                       titlefont = list(size = 17, family = "Arial"), fixedrange = TRUE, ticksuffix = "%"),
          yaxis = list(title = "", side = "right", showline = TRUE, linecolor = "gray50", linewidth = 0.5,
                       showgrid = FALSE, tickfont = list(size = 20, family = "Arial", weight = 700),
                       fixedrange = TRUE, automargin = TRUE, categoryorder = "array", categoryarray = wrappedLabels),
          margin = list(l = 10, r = 200, t = 50, b = 50),
          font = list(family = "Arial, sans-serif"), plot_bgcolor = "white", paper_bgcolor = "white", autosize = TRUE
        ) %>% config(displaylogo = FALSE, responsive = TRUE)
      }
    })

  })
}
