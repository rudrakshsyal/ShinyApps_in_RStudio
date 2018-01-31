## trf_1a_combo -- Traffic split by Channel

users_channelUI <- function(id){
  ns <- NS(id)
  tabPanel("a. by Channel", plotlyOutput(ns('trf_1a_combo'))) 
}

users_channelSERVER <- function(input,output,session,country){
  
  data_bar <- reactive({users_total_dch %>%
              filter(country %in% country()) %>%
              group_by(month, variable) %>%
              summarise(`no. of users (in millions)` = round((sum(users))/1000000,3)) %>%
              select(month, `no. of users (in millions)`, variable) %>%
              ungroup()})
  
  data_line  <- reactive({users_country_channel %>%
                filter(country %in% country()) %>%
                group_by(month, channel) %>%
                summarise(`no. of users (in millions)` = round((sum(users))/1000000,3)) %>%
                select(month, channel, `no. of users (in millions)`) %>%
                ungroup()})
  
output$trf_1a_combo <- renderPlotly({
  
  p <- (ggplot() + 
          
          geom_bar(data = data_bar(),
                   aes(x=month, y=`no. of users (in millions)`, fill = variable),
                   width = 10, stat = "identity", position = "dodge") +
                   scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") + 
                   theme_gdocs() + scale_fill_wsj() + scale_colour_hue() +
          
                    theme(axis.text.x = element_text(size = 10),
                          axis.text.y = element_text(size = 10),
                          axis.title = element_text(size = 12),
                          legend.text = element_text(size = 10),
                          legend.position = "bottom",
                          legend.justification = "center",
                          # legend.direction = "horizontal",
                          legend.title = element_blank(),
                          text = element_text(family = "Palatino Linotype")) +
          
          geom_line(data = data_line(),
                    aes(x=month, y=`no. of users (in millions)`, colour = channel, group = channel),
                    size = 0.5) + 
          
          geom_point(data = data_line(),
                     aes(x=month, y=`no. of users (in millions)`, colour = channel, group = channel),
                     size = 1.5))
  
  ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
  
})
}