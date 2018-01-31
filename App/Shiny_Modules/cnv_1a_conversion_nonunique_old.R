## cnv_1a_column -- Conversion 1

conversion_nonuniqueUI <- function(id){
  ns <- NS(id)
  tabPanel("a. Total (Transactions / DCH Users)", plotlyOutput(ns("cnv_1a_column")))
}

conversion_nonunique <- function(input,output,session,country){
  
  data <- reactive({Country_Transactions_Conversion_percentage %>%
       filter(country %in% country()) %>%
       group_by(month_names) %>%
       summarise(conversion_percentage = (mean(conversion_percentage))) %>%
       select(month_names, conversion_percentage) %>%
       ungroup()})
  
  output$cnv_1a_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data = data(),
                     aes(x=month_names, y=conversion_percentage),
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