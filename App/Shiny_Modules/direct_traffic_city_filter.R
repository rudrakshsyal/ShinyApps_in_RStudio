## City filter for Direct Traffic

direct_city_filterUI <- function(id){
  
  ns <- NS(id)
  column(12, checkboxGroupButtons(
    inputId = ns("city"), label = "Select City :",
    choices = unique(users_direct$city_new),
    selected = unique(users_direct$city_new),
    justified = F,
    size = "xs",
    status = "primary",
    checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
  ))
}

direct_city_filterSERVER <- function(input, output, session){
  dt_city <- reactive({input$city})
  return(dt_city)
}