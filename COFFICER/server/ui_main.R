output$ui <- renderUI({
  # theme united spacelab journal flatly are all ok
  navbarPage(
    id = "navbar",
    title = "COFFICER", theme = shinytheme("flatly"),
    tabPanel("HOME", value = "home", uiOutput("home")),
    tabPanel("Configure", value = "config", uiOutput("config")),
    tabPanel("Info Maintenance", value = "info", uiOutput("info")),
    tabPanel("Order Approving", value = "odr", uiOutput("odr")),
    tabPanel("Report", value = "rpt", uiOutput("rpt"))
  )
})
