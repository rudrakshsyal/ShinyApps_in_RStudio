## trf_2a_line -- Traffic daily run-rate

users_dailyUI <- function(id){
  # attach("csv_data.RData")
  ns <- NS(id)
  tabPanel("a. Daily run-rate", 
           
           column(12, checkboxGroupButtons(
             inputId = ns("channel"), label = "Select Channel: ", 
             choices = unique(users_channel_daily$channel), 
             selected = c("Direct", "AMP", "Mobile App", "Organic", "Referral", "Paid", "Affiliates"),
             justified = F, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           
           column(10, plotlyOutput(ns("trf_2a_line"))))
}

users_dailySERVER <- function(input,output,session,country){
  
  data <- reactive({users_channel_daily %>%
            filter(channel %in% input$channel) %>%
            filter(country %in% country()) %>%
            group_by(date, month) %>%
            summarise(users = sum(users))
            })
  
  output$trf_2a_line <- renderPlotly({
    
    p <- (ggplot() +
            
            geom_bar(data = data(), fill = "deepskyblue4",
                     aes(x=date, y=users),
                     stat = "identity") + 
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
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