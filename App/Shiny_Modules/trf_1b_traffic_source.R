## trf_1b_line -- Traffic split by Source

users_sourceUI <- function(id){
  ns <- NS(id)
  tabPanel("b. by Source", plotlyOutput(ns("trf_1b_line"))) 
}

users_sourceSERVER <- function(input,output,session,country){
  
  data <- reactive({users_country_source %>%
          filter(country %in% country()) %>%
          group_by(month, source) %>%
          summarise(`no. of users (in millions)` = (round((sum(users)/1000000),2))) %>%
          select(month, source, `no. of users (in millions)`) %>%
          ungroup()})
  
  output$trf_1b_line <- renderPlotly({
    p <- (ggplot(data(),
                 aes(x=month, y=`no. of users (in millions)`)) +
            scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            geom_line(aes(x=month, y=`no. of users (in millions)`, colour = source, group = source),
                      size = 0.5) +
            
            geom_point(aes(x=month, y=`no. of users (in millions)`, colour = source, group = source),
                       size = 1.5))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
  })
}