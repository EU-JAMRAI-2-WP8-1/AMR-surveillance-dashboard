
setup_modals <- function(input, output, session, email_config) {

  # Legal information modal (from Info tab button)
  observeEvent(input$showLegalModal, {
    showModal(modalDialog(
      title = "Legal Information",
      tryCatch({
        includeHTML("content/html/legal.html")
      }, error = function(e) {
        HTML("<p>Legal information unavailable</p>")
      }),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })

  # Legal information modal (from sidebar button)
  observeEvent(input$showLegalModalSidebar, {
    showModal(modalDialog(
      title = "Legal Information",
      tryCatch({
        includeHTML("content/html/legal.html")
      }, error = function(e) {
        HTML("<p>Legal information unavailable</p>")
      }),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })

  # Info modal (from sidebar button)
  observeEvent(input$showInfoModal, {
    showModal(modalDialog(
      title = "About",
      tryCatch({
        includeHTML("content/html/about.html")
      }, error = function(e) {
        HTML("<p>About information unavailable</p>")
      }),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })

  # Contact modal (from sidebar button)
  observeEvent(input$showContactModal, {
    showModal(modalDialog(
      title = tags$span(icon("envelope"), " Contact Us"),
      tags$div(
        class = "contact-modal-content",
        tags$p("Have questions or feedback? We'd love to hear from you. Please fill out the form below and we'll get back to you as soon as possible."),
        tags$br(),
        textInput("contactNameModal",    "Name *",    placeholder = "Your name",                        width = "100%"),
        textInput("contactEmailModal",   "Email *",   placeholder = "your.email@example.com",           width = "100%"),
        textInput("contactSubjectModal", "Subject *", placeholder = "Brief subject of your message",    width = "100%"),
        textAreaInput("contactMessageModal", "Message *", placeholder = "Your message here...",         width = "100%", height = "150px"),
        tags$p(class = "contact-required-note", "* Required fields")
      ),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submitContactModal", "Send Message", class = "btn btn-outline-primary", icon = icon("paper-plane"))
      ),
      size = "m"
    ))
  })

  # Geo data disclaimer modal
  observeEvent(input$showGeoDataDisclaimer, {
    showModal(modalDialog(
      title = "Geospatial Data - Legal Notice",
      tryCatch({
        includeHTML("content/html/geo_data_disclaimer.html")
      }, error = function(e) {
        HTML("<p>Disclaimer information unavailable</p>")
      }),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })

  # Welcome/Usage modal - shows once on page load
  observeEvent(TRUE, {
    showModal(modalDialog(
      title = tags$div(
        class = "flex-space-between",
        tags$div(
          class = "modal-header-left",
          tags$img(
            src   = "www/logos/Jamreye_primary-Colour-RGB.svg",
            class = "modal-logo",
            alt   = "JAMREYE Logo"
          ),
          tags$div(
            class = "modal-title-text",
            HTML("Welcome to JAMREYE:<br>Your AMR Surveillance Dashboard")
          )
        ),
        tags$div(
          class = "modal-header-right",
          modalButton("Start Exploring")
        )
      ),
      tryCatch({
        includeHTML("content/html/usage.html")
      }, error = function(e) {
        HTML("<p>Usage information unavailable</p>")
      }),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  }, once = TRUE, ignoreInit = FALSE)

  # Instructions button - reopens the usage modal
  observeEvent(input$showInstructions, {
    showModal(modalDialog(
      title = tags$div(
        class = "flex-space-between",
        tags$div(
          class = "modal-header-left",
          tags$img(
            src   = "www/logos/Jamreye_primary-Colour-RGB.svg",
            class = "modal-logo",
            alt   = "JAMREYE Logo"
          ),
          tags$div(
            class = "modal-title-text",
            HTML("How to Use This Dashboard")
          )
        ),
        tags$div(
          class = "modal-header-right",
          modalButton("Close")
        )
      ),
      tryCatch({
        includeHTML("content/html/usage.html")
      }, error = function(e) {
        HTML("<p>Usage information unavailable</p>")
      }),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })

  # Contact form submission
  observeEvent(input$submitContact, {
    if (!email_config$enabled) {
      output$contactStatus <- renderUI({
        tags$div(
          class = "alert alert-danger",
          tags$strong("Error: "),
          "The contact form is not configured on this instance. Please use the official deployment."
        )
      })
      return()
    }

    name    <- trimws(input$contactName)
    email   <- trimws(input$contactEmail)
    subject <- trimws(input$contactSubject)
    message <- trimws(input$contactMessage)

    if (name == "" || email == "" || subject == "" || message == "") {
      output$contactStatus <- renderUI({
        tags$div(
          class = "alert alert-danger",
          tags$strong("Error: "),
          "Please fill in all required fields."
        )
      })
      return()
    }

    emailPattern <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
    if (!grepl(emailPattern, email)) {
      output$contactStatus <- renderUI({
        tags$div(
          class = "alert alert-danger",
          tags$strong("Error: "),
          "Please enter a valid email address."
        )
      })
      return()
    }

    shinyjs::disable("submitContact")
    output$contactStatus <- renderUI({
      tags$div(
        class = "alert alert-info",
        tags$i(class = "fa fa-spinner fa-spin contact-spinner"),
        "Sending your message..."
      )
    })

    tryCatch({
      recipient_list <- trimws(unlist(strsplit(email_config$recipient_email, ",")))

      email_body_text <- paste0(
        "New contact form submission\n\n",
        "From: ",    name,    "\n",
        "Email: ",   email,   "\n",
        "Subject: ", subject, "\n\n",
        "Message:\n", message, "\n\n",
        "---\n",
        "Sent from JAMREYE AMR Surveillance Dashboard at ", format(Sys.time(), '%Y-%m-%d %H:%M:%S')
      )

      for (recipient in recipient_list) {
        email_content <- paste0(
          "From: ",    email_config$sender_email, "\r\n",
          "To: ",      recipient,                 "\r\n",
          "Subject: [Dashboard Contact] ", subject, "\r\n",
          "\r\n",
          email_body_text, "\r\n"
        )

        tmp_file <- tempfile(fileext = ".txt")
        writeLines(email_content, tmp_file)

        curl_cmd <- sprintf(
          'curl --url "smtp://%s:%s" --ssl-reqd --mail-from "%s" --mail-rcpt "%s" --upload-file "%s" --user "%s:%s" --tlsv1.2 --connect-timeout 10 --max-time 30 --silent --show-error 2>&1',
          email_config$smtp_server,
          email_config$smtp_port,
          email_config$sender_email,
          recipient,
          tmp_file,
          email_config$smtp_username,
          email_config$smtp_password
        )

        exit_code <- system(curl_cmd, ignore.stdout = TRUE, ignore.stderr = FALSE)
        unlink(tmp_file)

        if (exit_code != 0) {
          stop(paste("Failed to send email to", recipient, "- curl exit code:", exit_code))
        }
      }

      output$contactStatus <- renderUI({
        tags$div(
          class = "alert alert-success",
          tags$strong("Success! "),
          "Your message has been sent. We'll get back to you soon."
        )
      })

      updateTextInput(session, "contactName",    value = "")
      updateTextInput(session, "contactEmail",   value = "")
      updateTextInput(session, "contactSubject", value = "")
      updateTextAreaInput(session, "contactMessage", value = "")
      shinyjs::enable("submitContact")

    }, error = function(e) {
      output$contactStatus <- renderUI({
        tags$div(
          class = "alert alert-danger",
          tags$strong("Error: "),
          "There was an error sending your message. Please try again later or contact us directly."
        )
      })
      shinyjs::enable("submitContact")
      print(paste("Contact form error:", e$message))
    })
  })

}
