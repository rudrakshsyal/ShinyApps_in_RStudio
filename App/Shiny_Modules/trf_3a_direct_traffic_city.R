## trf_3a_column -- Direct Traffic monthly by City

users_direct_monthlyUI <- function(id){
  ns <- NS(id)
  tabPanel("a. Direct Traffic", plotlyOutput(ns('trf_3a_column')))
}

users_direct_monthlySERVER <- function(input,output,session,country,city){
  
  data <- reactive({users_direct %>%
                    filter(country %in% country()) %>%
                    filter(city_new %in% city()) %>%
                    filter(variable %in% 'direct') %>%
                    group_by(month,variable) %>%
                    summarise(users = round(sum(users)/1000,1))})
  
  output$trf_3a_column <- renderPlotly({
    
    p <- (ggplot() + 
            
            geom_bar(data = data(),
                     aes(x=month, y=users, fill = variable),
                     stat = "identity", position = "dodge") +
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") + 
            theme_gdocs() + scale_fill_wsj() + scale_colour_hue() +
            
            ylab("Users (in thousands)") +
            
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