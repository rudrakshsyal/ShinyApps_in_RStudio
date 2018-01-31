## trf_3c_column -- Direct Traffic on Listing Page

users_direct_listing_profileUI <- function(id){
  ns <- NS(id)
  tabPanel("c. Listing/Profile Page", plotlyOutput(ns('trf_3c_line')))
}

users_direct_listing_profileSERVER <- function(input,output,session,country,city){
  
  data <- reactive({users_direct %>%
      filter(country %in% country()) %>%
      filter(city_new %in% city()) %>%
      filter(variable %in% c('profile_page','listing_page')) %>%
      group_by(month,variable) %>%
      summarise(users = round(sum(users)/1000,1))})
  
  output$trf_3c_line <- renderPlotly({
    
    p <-  (ggplot(data(), 
                  aes(x=month, y=users, colour = variable, group = variable)) + 
             
             geom_line(size = 0.5) +
             geom_point(size = 1.5) +
             
             scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
             theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
             ylab('Users (in thousands)') +
             
             theme(axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   axis.title = element_text(size = 12),
                   legend.text = element_text(size = 10),
                   legend.position = "bottom",
                   legend.justification = "center",
                   legend.direction = "horizontal",
                   legend.title = element_blank(),
                   text = element_text(family = "Palatino Linotype")))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
}