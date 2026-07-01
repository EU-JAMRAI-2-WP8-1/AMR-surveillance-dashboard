mod_dashboard_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("noQuestionsMessage")),
    DT::dataTableOutput(ns("resultsTable")),
    tags$div(
      class = "download-buttons-wrapper",
      downloadButton(ns("downloadDataCSV"), "Download CSV", class = "btn btn-outline-primary"),
      downloadButton(ns("downloadDataExcel"), "Download Excel", class = "btn btn-outline-primary")
    )
  )
}

mod_dashboard_table_server <- function(id, filters, active_questions) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    createResultsTable <- function() {
      getBadgeColor <- function(tag) {
        if (tag == "National surveillance") return("#2a9d8f")
        if (tag == "National genomic surveillance") return("#d4a843")
        if (tag == "National guidance") return("#cc8888")
        return("#888888")
      }
      getBadgeIcon <- function(tag) {
        if (tag %in% sectionList) return("")
        if (tag %in% cultureMaterialList) return('<i class="fa fa-flask"></i> ')
        if (tag %in% pathogenList) return('<i class="fa fa-bacteria"></i> ')
        if (tag %in% resistanceList) return('<i class="fa fa-triangle-exclamation"></i> ')
        return("")
      }
      formatQuestionWithBadges <- function(questionTitle, tagsString) {
        tagsList <- strsplit(tagsString, ", ")[[1]]
        tagsList <- tagsList[tagsList != "Section 0"]
        tagsList <- tagsList[!grepl("^Not .* related$", tagsList)]
        if (length(tagsList) == 0) return(questionTitle)
        badgesHtml <- paste(sapply(tagsList, function(tag) {
          color <- getBadgeColor(tag)
          icon <- getBadgeIcon(tag)
          paste0('<span style="display: inline-block; background-color: ', color,
                 '; color: white; padding: 2px 8px; margin: 2px; border-radius: 3px; font-size: 0.75em; font-weight: 500;">',
                 icon, tag, '</span>')
        }), collapse = " ")
        return(paste0(badgesHtml, '<br/>', questionTitle))
      }

      activeQuestionTitles <- active_questions()[[1]]

      activeQuestionsData <- list()
      for (question in surveyData) {
        if (question$title %in% activeQuestionTitles) {
          activeQuestionsData[[length(activeQuestionsData) + 1]] <- list(
            question = question,
            position = ifelse(is.null(question[["position"]]), 9999, question[["position"]])
          )
        }
      }
      if (length(activeQuestionsData) > 0) {
        positions <- sapply(activeQuestionsData, function(x) x$position)
        sortOrder <- order(positions)
        activeQuestionsData <- activeQuestionsData[sortOrder]
      }
      questionsWithBadges <- c()
      for (item in activeQuestionsData) {
        formattedQuestion <- formatQuestionWithBadges(item$question$title, toString(item$question$tags))
        questionsWithBadges <- c(questionsWithBadges, formattedQuestion)
      }
      resultsTable <- data.frame("Question" = questionsWithBadges, stringsAsFactors = FALSE)
      for (country in filters()$countries) {
        countryReplies <- c()
        for (item in activeQuestionsData) {
          question <- item$question
          if (length(question$actual_answers[[country]]) == 0) {
            countryReplies <- c(countryReplies, NA)
          } else {
            countryReplies <- c(countryReplies, toString(question$actual_answers[[country]]))
          }
        }
        resultsTable[[country]] <- countryReplies
      }
      return(resultsTable)
    }

    output$noQuestionsMessage <- renderUI({
      if (length(active_questions()[[1]]) == 0) {
        tags$div(
          class = "no-questions-message",
          tags$i(class = "fa fa-info-circle no-questions-icon"),
          tags$br(),
          "No questions match the active filters.",
          tags$br(),
          tags$span(class = "no-questions-hint", "Try adjusting your filter selections.")
        )
      }
    })

    output$resultsTable <- DT::renderDT(
      createResultsTable(),
      rownames = FALSE,
      escape = FALSE,
      extensions = c('ColReorder', 'FixedColumns'),
      options = list(
        autowidth = TRUE,
        scrollX = TRUE,
        pageLength = 10,
        lengthMenu = list(c(5, 10, 20), c(5, 10, 20)),
        ordering = FALSE,
        colReorder = list(realtime = TRUE, fixedColumnsLeft = 1),
        fixedColumns = list(left = 0),
        headerCallback = JS(
          "function(thead, data, start, end, display) {",
          "  var table = this.api();",
          "  var th0 = $(thead).find('th').eq(0);",
          "  if (th0.find('.lock-btn').length === 0) {",
          "    var currentText = th0.text();",
          "    th0.html('<div style=\"display: flex; justify-content: space-between; align-items: center;\"><span>' + currentText + '</span><span class=\"lock-btn\" style=\"cursor: pointer;\" title=\"Click to lock/unlock this column when scrolling horizontally\"><i class=\"fa fa-lock-open\" style=\"font-size: 0.9em;\"></i></span></div>');",
          "    th0.find('.lock-btn').on('click', function(e) {",
          "      e.stopPropagation();",
          "      var icon = $(this).find('i');",
          "      var isLocked = icon.hasClass('fa-lock');",
          "      if (isLocked) {",
          "        icon.removeClass('fa-lock').addClass('fa-lock-open');",
          "        $(this).attr('title', 'Click to lock/unlock this column when scrolling horizontally');",
          "        table.fixedColumns().left(0);",
          "      } else {",
          "        icon.removeClass('fa-lock-open').addClass('fa-lock');",
          "        $(this).attr('title', 'Click to lock/unlock this column when scrolling horizontally');",
          "        table.fixedColumns().left(1);",
          "      }",
          "    });",
          "  }",
          "  $(thead).find('th:gt(0)').each(function(index) {",
          "    if ($(this).find('.remove-country-btn').length === 0) {",
          "      var currentText = $(this).text();",
          "      $(this).html('<div style=\"display: flex; justify-content: space-between; align-items: center;\"><span>' + currentText + '</span><span style=\"display: flex; align-items: center; gap: 6px;\"><i class=\"fa fa-arrows-h\" style=\"font-size: 0.8em; cursor: move;\"></i><span class=\"remove-country-btn\" style=\"cursor: pointer; font-weight: bold; font-size: 1.3em;\" title=\"Remove this country from selection\">&times;</span></span></div>');",
          "      $(this).find('.remove-country-btn').on('click', function(e) {",
          "        e.stopPropagation();",
          "        var countryName = currentText;",
          "        Shiny.setInputValue('remove_country', {country: countryName, timestamp: Date.now()});",
          "      });",
          "    }",
          "    $(this).css({'cursor': 'move'});",
          "  });",
          "}"
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  var wrapper = $(this.api().table().container());",
          "  var scroll = wrapper.find('.dataTables_scrollBody');",
          "  if (scroll.length > 0) {",
          "    var topScroll = $('<div class=\"dataTables_scrollHead_top\" style=\"overflow-x: auto; overflow-y: hidden;\"><div style=\"height: 1px; width: ' + scroll[0].scrollWidth + 'px;\"></div></div>');",
          "    topScroll.insertBefore(wrapper.find('.dataTables_scroll'));",
          "    topScroll.on('scroll', function() { scroll.scrollLeft($(this).scrollLeft()); });",
          "    scroll.on('scroll', function() { topScroll.scrollLeft($(this).scrollLeft()); });",
          "  }",
          "}"
        ),
        columnDefs = list(list(targets = 0, className = 'first-column-cell'))
      )
    )

    output$downloadDataCSV <- downloadHandler(
      filename = function() { paste0("jamreye_data_export-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv") },
      content = function(file) { write.csv(createResultsTable(), file, row.names = FALSE) }
    )
    output$downloadDataExcel <- downloadHandler(
      filename = function() { paste0("jamreye_data_export-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx") },
      content = function(file) { write.xlsx(createResultsTable(), file, rowNames = FALSE) }
    )
  })
}
