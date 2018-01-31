## trf_2a_line -- Traffic daily run-rate

users_cumulative_dailyUI <- function(id){
  # attach("csv_data.RData")
  ns <- NS(id)
  tabPanel("b. Cumulative daily run-rate", 
           
           column(12, checkboxGroupButtons(
             inputId = ns("channel"), label = "Select Channel: ", 
             choices = unique(users_channel_daily$channel), 
             selected = c("Direct", "AMP", "Mobile App", "Organic", "Referral", "Paid", "Affiliates"),
             justified = F, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           
           column(10, plotlyOutput(ns("trf_2b_line"))))
}

users_cumulative_dailySERVER <- function(input,output,session,country){
  
  data <- reactive({users_channel_daily %>%
            filter(channel %in% input$channel) %>% 
            filter(country %in% country()) %>%
            select(-country,-channel) %>%
            mutate(date = day(date)) %>%
            mutate(month = substr(month, 1, 7)) %>%
            group_by(date,month) %>%
            summarise(users = sum(users)) %>%
            group_by(month) %>%
            mutate(`no. of users (in millions)` = round(cumsum(users)/1000000,2))})
  
  output$trf_2b_line <- renderPlotly({
    
    p <- (ggplot() +
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
            
            geom_line(data = data(), 
            aes(x=date, y=`no. of users (in millions)`, colour = month, group = month),
            size = 0.5) +
            
            geom_point(data = data(), 
            aes(x=date, y=`no. of users (in millions)`, colour = month, group = month),
            size = 1.5))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
  })
  }