if (interactive()) {
  library(shiny)
  library(shinyforms)

  questions <- list(
    list(id = "name", type = "text", title = "Name", mandatory = TRUE),
    list(id = "age", type = "numeric", title = "Age"),
    list(id = "favourite_pkg", type = "text", title = "Favourite R package"),
    list(id = "terms", type = "checkbox", title = "I agree to the terms"),
    list(id = "naps", type = "select", title = "Do you like taking naps?", choices = c("",  "Yes", "No")),
    list(id = "nap_location", type = "select", title = "Where do you nap?", choices = c("", "bed", "couch", "floor"), condition = "input.naps == 'Yes'", mandatory = TRUE)
  )
  formInfo <- list(
  id = "basicinfo",
  questions = questions,
  storage = list(
    # Right now, only flat file storage is supported
    type = STORAGE_TYPES$FLATFILE,
    # The path where responses are stored
    path = "responses"
  )
  )
  ui <- fluidPage(
    formUI(formInfo)
  )

  server <- function(input, output, session) {
    formServer(formInfo)
  }

  shinyApp(ui = ui, server = server)
  }

