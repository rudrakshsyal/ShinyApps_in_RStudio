## cnv_1b_column -- Conversion 2

conversion_unique_sourceUI <- function(id){
  ns <- NS(id)
  tabPanel("c. BOOK Conversion by Platform", plotlyOutput(ns("cnv_1d_column")))
}

conversion_unique_sourceSERVER <- function(input,output,session,country){
  
  data <- reactive({conversion_source %>%
       filter(country %in% country()) %>%
       group_by(month, source) %>%
       summarise(conversion_percentage = round((sum(trans)*100/sum(users)),2)) %>%
       select(month, source, conversion_percentage) %>%
       ungroup()})
  
  output$cnv_1d_column <- renderPlotly({
    p <- (ggplot() +
            
            scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  # legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
          
            geom_line(data = data(),
                      aes(x=month, y=conversion_percentage, colour = source, group = source),
                      size = 0.5) +
            
            geom_point(data = data(),
                       aes(x=month, y=conversion_percentage, colour = source, group = source),
                       size = 1.5))
      
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
}