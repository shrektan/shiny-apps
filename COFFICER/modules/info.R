
# ui ----------------------------------------------------------------------

output$info <- renderUI({
  if (is.null(pos_))
    return(h3("You need to establish COfficerPosition object to continue."))
  navlistPanel(
    widths = c(2, 10),
    tabPanel(
      "Guide",
      helpText("The log tag of current session is: ", pos_$log_tag),
      helpText("The date of the based CORE data (financial data) is ",
        format(pos_$based_date, "%Y.%m.%d"), "."),
      helpText("Please ensure the added data make sense whenever you reboot the app."),
      helpText("")
    ),
    tabPanel(
      "Setting",
      helpText("", id = "info_log_folder_txt"),
      textInput("info_log_folder", "Log Folder", log_folder)
    ),
    tabPanel(
      "Updating",
      selectInput("info_target", NULL,
                  c("Security Info", "Added Data", "Based CORE Data")),
      fileInput("info_upd_file", "Updated file"),
      dataTableOutput("info_data")
    )
  )
})

