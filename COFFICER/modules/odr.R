
# ui ----------------------------------------------------------------------

output$odr <- renderUI({
  if (is.null(pos_))
    return(h3("You need to establish COfficerPosition object to continue."))
  navlistPanel(
    id = "odr_panel",
    widths = c(2, 10),
    tabPanel(
      "O32 Order",
      br(),
      actionButton("odr_o32_refresh", "Refresh"),
      actionButton("odr_o32_add", "Add"),
      br(),
      br(),
      radioButtons("odr_o32_mode", NULL,
                   choices = c("Approving", "All"), inline = TRUE),
      dataTableOutput("odr_o32"),
      tags$style(type = "text/css", "#odr_o32{font-size:13px}")
    ),
    tabPanel(
      "Special Order",
      shinyBS::bsCollapse(
        open = "Security Universe",
        shinyBS::bsCollapsePanel(
          title = "Security Universe",
          style = "info",
          dataTableOutput("odr_sec_info")
        ),
        shinyBS::bsCollapsePanel(
          title = "Order Content",
          style = "warning",
          fluidRow(
            column(
              width = 3,
              selectInput(
                "odr_pn", label = "Port Name",
                choices = {
                  tmp <- pos_$get_private("info_port")[, .(Port_Order, Port_Name)]
                  setkey(tmp, Port_Order)
                  tmp$Port_Name
                }
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              textInput("odr_sec_code", "Sec_Code")
            ),
            column(
              width = 3,
              textInput("odr_sec_name", "Sec_Name")
            ),
            column(
              width = 3,
              textInput("odr_class_f", "Asset Class")
            )
          ),
          fluidRow(
            column(
              width = 3,
              wellPanel(
                radioButtons("odr_ias", NULL,
                             choices = c("AFS", "HTM", "HFT"), inline = TRUE),
                radioButtons("odr_b_s", NULL,
                             choices = c("BUY", "SELL"), inline = TRUE)
              )
            ),
            column(
              width = 3,
              numericInput("odr_quantity", "Quantity", value = 1000, min = 0)
            ),
            column(
              width = 3,
              numericInput("odr_amt", "Amount(RMB)", value = 1000000, min = 0)
            )
          ),
          actionButton("odr_add", "Add Order")
        )
      ),
      tags$style(type = "text/css", "#odr_sec_info{font-size:13px}")
    ),
    tabPanel(
      "ApprovalPanel",
      uiOutput("odr_ui_appr")
    ),
    tabPanel(
      "ExistingOrders",
      dataTableOutput("odr_added"),
      tags$style(type = "text/css", "#odr_added{font-size:13px}")
    ),
    tabPanel(
      "ApprovedOrders",
      dataTableOutput("odr_his"),
      tags$style(type = "text/css", "#odr_his{font-size:13px}")
    )
  )
})

# add order ---------------------------------------------------------------

odr_add_signal <- reactiveValues(x = 0)

observeEvent(
  input$odr_add, {
    isolate({
      if (nrow(pos_$odr_new) > 0) shinyjs::info("Existing order to approve.")
      validate(
        need(length(input$odr_pn) > 0, label = "Port Name"),
        need(length(input$odr_sec_code) > 0, label = "Sec Code"),
        need(length(input$odr_sec_name) > 0, label = "Sec Name"),
        need(nrow(pos_$odr_new) == 0, label = "Existing order to approve.")
      )
      pos_$odr_new <-
        data.table(
          Order_Time = Sys.time(),
          B_S = input$odr_b_s,
          Port_Name = input$odr_pn,
          Class_F = input$odr_class_f,
          Sec_Code = input$odr_sec_code,
          Sec_Name = input$odr_sec_name,
          IAS = input$odr_ias,
          Quantity = input$odr_quantity,
          Amount_LC = input$odr_amt
        )
      odr_add_signal$x <- odr_add_signal$x + 1
    })
    updateTabsetPanel(session, inputId = "odr_panel", selected = "ApprovalPanel")
    updateCollapse(session, id = "odr_ui_appr_coll", open = "Order Info")
  }
)

output$odr_approving_odr <- renderDataTable({
  odr_add_signal$x
  isolate({
    tmp <- COFFICER:::.native_dt(pos_$odr_new)
    tmp %>%
      datatable(
        selection = list(mode = "none"),
        rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE, dom = 'tip')
      )
  })
})

output$odr_obj_dt <- renderDataTable({
  odr_add_signal$x
  if (nrow(pos_$odr_new) > 0) {
    if (nrow(pos_$odr_approve) == 0) return(NULL) # if no related risk indicator
    if (input$odr_obj_mode == "All") {
      tmp <- pos_$odr_approve
    } else {
      tmp <- pos_$odr_approve[IfPass == FALSE | is.na(IfPass) == TRUE]
    }
  } else {
    tmp <- data.table()
  }
  tmp <- COFFICER:::.native_dt(tmp)
  tmp %>%
    datatable(
      selection = list(mode = "none"),
      rownames = FALSE,
      options = list(pageLength = 5, scrollX = TRUE)
    )
})

observe({
  if (!is.null(input$odr_appr_bt)) {
    if (input$odr_appr_bt > 0) {
      isolate({
        pos_$odr_approve <- list(op = (input$odr_appr_op == "Reject"),
                                 op_desc = input$odr_appr_op_desc)
        updateTabsetPanel(session, inputId = "odr_panel", selected = "ApprovedOrders")
        odr_add_signal$x <- odr_add_signal$x + 1
      })
    }
  }
})


# react to sec selection --------------------------------------------------

observe({
  if (!is.null(input$odr_sec_info_rows_selected)) {
    tmp <- pos_$info_sec[
      as.integer(input$odr_sec_info_rows_selected), ]
    updateTextInput(session, "odr_sec_code", value = tmp$Sec_Code)
    updateTextInput(session, "odr_sec_name", value = tmp$Sec_Name)
    updateTextInput(session, "odr_class_f", value = tmp$Class_F)
  }
})

# o32 ---------------------------------------------------------------------

odr_o32_data <- reactive({
  if (!is.null(input$odr_o32_refresh)) {
    odr_add_signal$x
    tmp <- pos_$read_o32 %>% COFFICER:::.native_dt(.)
    if (input$odr_o32_mode == "Approving")
      tmp <- tmp[Appr_State_CN == '未审批' & Order_State_CN == '有效']
    tmp
  }
})

output$odr_o32 <- renderDataTable({
  copy(odr_o32_data())[
    , c("Date", "if_Approved") := list(
      format(Date, "%Y/%m/%d"),
      (O32_No %in% pos_$odr_his$O32_No) * 1
    )] %>%
    datatable(
      selection = "multiple",
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    ) %>%
    formatCurrency(c("Quantity", "Amount"), "") %>%
    formatStyle(
      "if_Approved", target = "row",
      backgroundColor = styleEqual(c(1), "lightgreen")
    )
})

# o32 approve -------------------------------------------------------------

observeEvent(
  input$odr_o32_add, {
    isolate({
      validate(
        if (nrow(pos_$odr_new) > 0) {
          tmp <- "Existing order to approve."
          shinyjs::info(tmp)
          tmp
        },
        if (length(input$odr_o32_rows_selected) == 0) {
          tmp <- "Need to select at least one o32 order."
          shinyjs::info(tmp)
          tmp
        }
      )
      # trans to order
      odr <- odr_o32_data()[input$odr_o32_rows_selected]
      odr[, Class_F := pos_$info_sec[J(odr$Sec_Code), Class_F]]
      odr[, Order_Time := Sys.time()]
      setnames(odr, "Amount", "Amount_LC")
      pos_$odr_new <- odr
      odr_add_signal$x <- odr_add_signal$x + 1
    })
    updateTabsetPanel(session, inputId = "odr_panel", selected = "ApprovalPanel")
    updateCollapse(session, id = "odr_ui_appr_coll", open = "Order Info")
  }
)

# output element ----------------------------------------------------------

output$odr_added <- renderDataTable({
  odr_add_signal$x
  if (nrow(pos_$added) == 0) {
    data.table()
  } else {
    pos_$added %>%
      datatable(
        selection = "none",
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      ) %>%
      formatCurrency(c("Quantity", "Amount_LC"), "")
  }
})

output$odr_his <- renderDataTable({
  odr_add_signal$x
  if (nrow(pos_$odr_his) == 0) {
    data.table()
  } else {
    pos_$odr_his %>%
      datatable(
        selection = "none",
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      ) %>%
      formatCurrency(c("Quantity", "Amount_LC"), "")
  }
})

output$odr_sec_info <- renderDataTable(server = TRUE, {
  # ugly hack to disable the text input
  disable(id = "odr_sec_code")
  disable(id = "odr_sec_name")
  disable(id = "odr_class_f")

  # here is the real content
  f_tmp <- function(x) ifelse(Encoding(x) == "UTF-8", enc2native(x), x)
  tmp <- copy(pos_$info_sec)
  tmp <- COFFICER:::.native_dt(tmp)
  tmp <- tmp[, .(Sec_Code, Sec_Name, Class_F, Maturity_Date, Issuer,
                 Guarantor, Rating_Internal,
                 Rating_External, Issuer_Rating_Internal, SW_Sector1, Outstanding)]
  tmp %>%
    datatable(
      selection = list(mode = "single", target = "row", selected = 1),
      options = list(pageLength = 5, scrollX = TRUE)
    ) %>%
    formatCurrency(c("Outstanding"), currency = "", digits = 0)
})


# Approval Panel UI -------------------------------------------------------

output$odr_ui_appr <- renderUI({
  odr_add_signal$x
  if (nrow(pos_$odr_new) > 0) {
    shinyBS::bsCollapse(
      id = "odr_ui_appr_coll",
      multiple = TRUE,
      open = c("Order Info", "Subjective Judgement", "Objective Info", "Approve"),
      shinyBS::bsCollapsePanel(
        "Order Info",
        style = "info",
        dataTableOutput("odr_approving_odr"),
        tags$style(type = "text/css", "#odr_approving_odr{font-size:13px}")
      ),
      shinyBS::bsCollapsePanel(
        "Objective Info",
        style = "warning",
        radioButtons(
          "odr_obj_mode", label = NULL, choices = c("Failed", "All"),
          inline = TRUE
        ),
        dataTableOutput("odr_obj_dt"),
        tags$style(type = "text/css", "#odr_obj_dt{font-size:13px}")
      ),
      shinyBS::bsCollapsePanel(
        "Subjective Judgement",
        style = "warning",
        p("In process"),
        dataTableOutput("odr_subj_dt"),
        tags$style(type = "text/css", "#odr_subj_dt{font-size:13px}")
      ),
      shinyBS::bsCollapsePanel(
        "Approve",
        style = "danger",
        radioButtons(
          "odr_appr_op", label = NULL, choices = c("Reject", "Approve"),
          inline = TRUE
        ),
        tags$style(type = "text/css", "textarea {width:100%}"),
        tags$textarea(id = 'odr_appr_op_desc',
                      placeholder = 'Type your note here', rows = 4),
        actionButton("odr_appr_bt", "Submit")
      )
    )
  } else {
    h3("No order in approving process.")
  }
})
