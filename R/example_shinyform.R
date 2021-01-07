if (interactive()) {
  library(shiny)
  library(shinyforms)
  library(stringr)
  library(dplyr)

  questions <- list(
    list(id = "name", type = "text", title = "Name", mandatory = TRUE, prefill = TRUE),
    list(id = "age", type = "numeric", title = "Age", prefill = TRUE),
    list(id = "favourite_pkg", type = "text", title = "Favourite R package", prefill = TRUE),
    # currently no option for checkbox inputs to be prefilled
    list(id = "terms", type = "checkbox", title = "I agree to the terms"),
    list(id = "naps", type = "select", title = "Do you like taking naps?", choices = c("",  "Yes", "No"), prefill = TRUE),
    list(id = "nap_location", type = "select", title = "Where do you nap?", choices = c("", "Bed", "Couch", "Floor", "Adam's Surface"), condition = "input.naps == 'Yes'", mandatory = TRUE, prefill = TRUE)
  )
  formInfo <- list(
  id = "basicinfo",
  name = "Shiny questionnaire",
  questions = questions,
  storage = list(
    # Right now, only flat file storage is supported
    type = STORAGE_TYPES$POSTGRES,
    # The path where responses are stored
    path = "responses",
    # Name of postgres table to save the results in
    table_name = 'shinyform_test_3'
  ),
  reset = TRUE,
  password = 'shinyforms'
  )
  
  
  ui <- fluidPage(
    formUI(formInfo)
  )

  server <- function(input, output, session) {
    formServer(formInfo)
  }

  shinyApp(ui = ui, server = server)
  }

