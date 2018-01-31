## cnv_1b_column -- Conversion 2

conversion_uniqueUI <- function(id){
  ns <- NS(id)
  tabPanel("a. Unique (Patients / DCH Users)", plotlyOutput(ns("cnv_1b_column")))
}

conversion_unique <- function(input,output,session,country){
  
  data <- reactive({new_conversion %>%
       filter(country %in% country()) %>%
       group_by(month) %>%
       summarise(conversion_percentage = round((sum(unique_transactions)*100/sum(users)),2)) %>%
       select(month, conversion_percentage) %>%
       ungroup()})
  
  output$cnv_1b_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data = data(),
                     aes(x=month, y=conversion_percentage),
                     stat = "identity", fill = "dodgerblue4") +
            
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  # legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")))
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
}