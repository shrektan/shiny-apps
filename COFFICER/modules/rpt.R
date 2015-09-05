
# ui ----------------------------------------------------------------------

output$rpt <- renderUI({
  if (is.null(pos_))
    return(h3("You need to establish COfficerPosition object to continue."))
  shiny::mainPanel(
    width = 12,
    tabsetPanel(
      id = "rpt_panel",
      tabPanel(
        "CurrentPosition",
        br(),
        downloadButton("rpt_pos_d", "Download xlsx"),
        br(),
        br(),
        dataTableOutput("rpt_pos"),
        tags$style(type = "text/css", "#rpt_pos{font-size:13px}")
      ),
      tabPanel(
        "RiskLimitCalculator",
        selectInput("rpt_ri_code", "Limit Code",
                    choices = c(COFFICER:::.ri_limit$cb[, unique(LimitCode)],
                                COFFICER:::.ri_limit$ul[, unique(LimitCode)],
                                COFFICER:::.ri_limit$gcamc[, unique(LimitCode)])),
        dataTableOutput("rpt_ri_dt"),
        tags$style(type = "text/css", "#rpt_ri_dt{font-size:13px}")
      )
    )
  )
})

# element -----------------------------------------------------------------

output$rpt_ri_dt <- renderDataTable({
  pns <- unique(COFFICER:::.find_ri(as.integer(input$rpt_ri_code))$PortName)
  r <- lapply(pns, compute_ri, ri_code = as.integer(input$rpt_ri_code),
              pos = pos_$latest, pos_ = pos_)
  r <- rbindlist(r)
  r[, .(LimitCode, LimitType, LimitClass, LimitName, LimitBound,
        PortName, LimitValue, RIValue, IfPass, PassDes)] %>%
    datatable(selection = "none",
              options = list(
                pageLength = 10, scrollX = TRUE
              )) %>%
    formatPercentage(c("RIValue", "LimitValue"), digits = 2)
})
output$rpt_pos <- renderDataTable({
  tmp <-
    pos_$latest[, .(Port_Name, Class_L1, Class_L3, IAS,
                    Sec_Code, Sec_Name, Quantity, AV_CV_LC, AV_Mix_LC, UGL_LC)]
  COFFICER:::.native_dt(tmp) %>%
    datatable(selection = 'single',
              options = list(
                pageLength = 10, scrollX = TRUE
              )) %>%
    formatCurrency(c("Quantity", "AV_CV_LC", "AV_Mix_LC", "UGL_LC"),
                   currency = "", digits = 0)
})

output$rpt_pos_d <- downloadHandler(
  filename = function() basename(tempfile(pattern = "pos_", fileext = ".xlsx")),
  content = function(file) {
    openxlsx::write.xlsx(pos_$latest, file)
  }
)

