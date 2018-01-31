## trf_3d_column -- Direct Traffic month-on-month growth

users_direct_growthUI <- function(id){
  ns <- NS(id)
  tabPanel("d. Growth Monthly", plotlyOutput(ns('trf_3d_column')))
}

users_direct_growthSERVER <- function(input,output,session,country,city){

  pct <- function(x) {round((x-lag(x))*100/lag(x),1)}
  
  data <- reactive({users_direct %>%
                    filter(country %in% country()) %>%
                    filter(city_new %in% city()) %>%
                    filter(variable %in% 'direct') %>%
                    group_by(variable,month) %>%
                    summarise(users = sum(users)) %>%
                    mutate_each(funs(pct), c('users'))})
  
  output$trf_3d_column <- renderPlotly({
    
    p <- (ggplot() + 
            
            geom_bar(data = data(),
                     aes(x=month, y=users, fill = variable),
                     stat = "identity", position = "dodge") +
            
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") + 
            theme_gdocs() + scale_fill_wsj() + scale_colour_hue() +
            
            ylab("Monthly Growth (%)") +
            
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