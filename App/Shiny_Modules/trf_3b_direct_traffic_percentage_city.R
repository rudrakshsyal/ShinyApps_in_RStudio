## trf_3b_column -- Direct Traffic as a % of Total Traffic

users_direct_perc_monthlyUI <- function(id){
  ns <- NS(id)
  tabPanel("b. % Direct Traffic", plotlyOutput(ns('trf_3b_column')))
}

users_direct_perc_monthlySERVER <- function(input,output,session,country,city){
  
  data <- reactive({users_direct %>%
      filter(country %in% country()) %>%
      filter(city_new %in% city()) %>%
      filter(variable %in% 'direct') %>%
      group_by(month,variable) %>%
      summarise(perc = round((sum(users)*100/sum(total_users)),1))})
  
  output$trf_3b_column <- renderPlotly({
    
    p <- (ggplot() + 
            
            geom_bar(data = data(),
                     aes(x=month, y=perc, fill = variable),
                     stat = "identity", position = "dodge") +
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") + 
            theme_gdocs() + scale_fill_wsj() + scale_colour_hue() +
            
            ylab("% Direct Traffic (of Total)") +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  # legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
}