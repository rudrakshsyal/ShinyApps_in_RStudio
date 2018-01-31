## cnv_1c_column -- Conversion Unique split by Tier

conversion_tierUI <- function(id){
  ns <- NS(id)
  tabPanel("b. Conversion by Tier", 
           fluidRow(
             column(12, checkboxGroupButtons(
               inputId = ns("tier"), label = "Select Tier: ", 
               choices = unique(conversion_tier$tier), 
               selected = c("Bengaluru", "Delhi_NCR", "Hyderabad"),
               justified = F,
               size = "xs",
               status = "primary",
               checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
             )),
             column(12, plotlyOutput(ns("cnv_1c_column")))))
}

conversion_tierSERVER <- function(input,output,session,country){
  
  column <- reactive({conversion_tier %>%
      filter(country %in% country()) %>%
      mutate(tier = ifelse(tier == "Tier-2", "Tier-2", ifelse(tier == "International", "International", "Tier-1"))) %>%
      group_by(month, tier) %>%
      summarise(conversion_percentage = round((sum(unique_transactions)*100/sum(users)),2)) %>%
      ungroup()})
  
  data <- reactive({conversion_tier %>%
      filter(country %in% country()) %>%
      filter(tier %in% input$tier) %>%
      group_by(month,tier) %>%
      summarise(conversion_percentage = round((sum(unique_transactions)*100/sum(users)),2)) %>%
      ungroup()})
  
  output$cnv_1c_column <- renderPlotly({
    p <- (ggplot() +
            
            geom_bar(data = column(),
                     aes(x=month, y=conversion_percentage, fill=tier),
                     stat = "identity", position = "dodge") +
            
            geom_line(data = data(),
                     aes(x=month, y=conversion_percentage, color=tier, group=tier),
                     size = 0.5) +
            
            geom_point(data = data(),
                      aes(x=month, y=conversion_percentage, color=tier, group=tier),
                      size = 1.5) +
            
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
}