if (interactive()) {
  library(shiny)
  library(shinyforms)
  library(stringr)
  library(dplyr)
  library(shinyWidgets)
  
  questions <- list(
    list(id = "insp_date", type = "date", title = "Inspection date"),
    list(id = "ao_type", type = "select", title = "Was this inspection performed by EPOs or OPLEs?", choices = c("EPO", "OPLE")),
    list(id = "ao_name", type = "text", title = "Inspecting officers", prefill = TRUE, mandatory = TRUE),
    list(id = "insp_number_ibis", type = "numeric", title = "Ibis inspection ID", condition = "input.ao_type == 'EPO'"),
    # currently no option for checkbox inputs to be prefilled
    list(id = "insp_number_ople", type = "text", title = "OPLE inspection number", condition = "input.ao_type == 'OPLE'", hint = "Please provide the inspection number in the format 'xx-xx-xxx-xxx"),
    list(id = "insp_report", type = "checkbox", title = "Have you created an inspection report?", choices = c("",  "Yes", "No")),
    list(id = "observations", type = "text", title = "What were your key observations at this site?"),
    list(id = "gut_feel", type = "select", title = "Do you think the duty holder will comply in the longterm?", choices = c("Yes", "No")),
    list(id = "stkp_location", type = "text", title = paste("Where was located?")),
    list(id = "frv_observations", type = "text", title = "FRV observations")
    # ,
    # list(id = "buildings", type = "checkbox_group", title = "Please select storage building numbers", choices = c(1, 2, 3, 4))
  )
  formInfo <- list(
    id = "basicinfo",
    name = "Post-Inspection Data Entry",
    questions = questions,
    storage = list(
      # Right now, only flat file storage is supported
      type = STORAGE_TYPES$POSTGRES,
      # The path where responses are stored
      path = "responses",
      # Name of postgres table to save the results in
      table_name = 'inspection_info_test'
    ),
    reset = TRUE,
    password = 'shinyforms'
    # ,
    # validations = list(
    #   list(condition = "input$insp_date == Sys.Date()",
    #        message = "Did you inspect the site today?")
  # )
  )
  
  
  ui <- fluidPage(
    formUI(formInfo)
  )
  
  server <- function(input, output, session) {
    formServer(formInfo)
  }
  
  shinyApp(ui = ui, server = server, 
           # options = list(display.mode = "showcase")
           )
  
}


