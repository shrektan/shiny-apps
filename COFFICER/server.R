## server ##

for (file in list.files("global", pattern = "*.R")) {
  source(file.path("global", file),
         local = TRUE, encoding = "UTF-8")
}

function(input, output, session) {
  isolate({
    navbarEval <- character(0)
  })

  for (file in list.files("server", pattern = "*.R")) {
    source(file.path("server", file),
           local = TRUE, encoding = "UTF-8")
  }
  
  observe({
    if (is.null(input$navbar)) return(NULL)
    if (grepl("\\W", input$navbar)) return(NULL)
    if (!input$navbar %in% navbarEval) {
      navbarEval <<- c(navbarEval, input$navbar)
      source(file.path("modules", paste0(input$navbar, ".R")),
             local = TRUE, encoding = "UTF-8")
    } else {
      return(NULL)
    }
  })
}
