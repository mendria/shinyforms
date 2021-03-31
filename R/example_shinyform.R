if (interactive()) {
  library(shiny)
  library(shinyforms)
  library(stringr)
  library(dplyr)
  library(shinyWidgets)


  questions <- list(
    list(id = "name", type = "text", title = "Name", mandatory = TRUE, prefill = TRUE, info = "Please enter your name."),
    list(id = "age", type = "numeric", title = "Age", mandatory = TRUE),
    list(id = "favourite_pkg", type = "text", title = "Favourite R package", prefill = TRUE, hint = "Don't spend too much time thinking about this question."),
    # currently no option for checkbox inputs to be prefilled
    list(id = "terms", type = "checkbox", title = "I agree to the terms"),
    list(id = "naps", type = "select", title = "Do you like taking naps?", choices = c("",  "Yes", "No"), prefill = TRUE, info = "There is no shame in taking a nap."),
    list(id = "nap_location", type = "select", title = "Where do you nap?", choices = c("", "Bed", "Couch", "Floor", "Adam's Surface"), condition = "input.naps == 'Yes'", mandatory = TRUE, prefill = TRUE, info = "Blah")
  )
  formInfo <- list(
  id = "basicinfo",
  name = "Napping Habit Questionnaire",
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
  password = 'shinyforms',
  # sets row filter for prefill option, if null, last entry is selected
  prefill_filter = ".$name == 'Matilda'",
  validations = list(
    list(condition = "input$terms == TRUE",
         message = "You must agree to the terms")
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

