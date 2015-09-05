output$home <- renderUI({
message(Sys.info()["user"])
  tagList(
    # h2("WELCOME to COFFICER App", style = "color:black;font-weight:bold"),
    h2("User Manual", style = "font-weight:bold"),
    h3("Order"),
    shinyjs::useShinyjs(),
    # add a app refresh trigger
    shinyjs::extendShinyjs(
      text = "shinyjs.refresh = function() { location.reload(); }"),
    p("Here you are able to input any trading order for O32 and ",
      "immediately know if you can approval the order or not. "),
    p("Also, you will be informed the exact reasons if the order should be rejected. "),
    p("The limit status impact would be showed in the browser as well."),
    h3("Report"),
    p("You will be able to generate the risk limit report here."),
    # h3(a("Tan, Xianying", href = "mailto:xianying.tan@gc-amc.com",
    # style = "color:black")),
    tags$footer(
      div(HTML("&nbsp;")),
      div(HTML("&nbsp;")),
      div(
        HTML("Copyright &copy; Generali China Asset Management CO. LTD"),
        style = "color: #999999"
      ),
      # div(HTML("&nbsp;")),
      style = "text-align: center; margin-bottom: 100px; font-size: 0.8em"
    )
      ) %>%
    div(class = "container")
})
