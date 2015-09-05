
# ui ----------------------------------------------------------------------

output$config <- renderUI({
  fluidPage(
    helpText("Please configure the COfficerPosition object before continue."),
    helpText("Load previous RData file."),
    helpText("If you left this empty, it will establish a new object with ",
             "latest position and security info."),
    selectInput("config_load_file", label = NULL, choices = "Establish New"),
    actionButton("config_refresh", "Refresh"),
    actionButton("config_load", "Submit")
  )
})

observeEvent(
  input$config_load, {
    isolate({
      if (input$config_load_file == "Establish New") {
        establish_pos_()
      } else {
        tmp <- file.path(log_folder, input$config_load_file, ".pos_.RData")
        establish_pos_(tmp)
      }
      Sys.sleep(0.01)
      # refresh the whole page
      shinyjs::js$refresh()
    })
  }
)

observe(
  if (!is.null(input$config_refresh)) {
    updateSelectInput(
      session, "config_load_file",
      choices = c("Establish New",
                  sort(dir(log_folder, full.names = FALSE), decreasing = TRUE))
    )
  }
)
