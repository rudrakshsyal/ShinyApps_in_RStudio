## tra_3b_line -- ABS Cancellation Rate by City

trans_abs_cityUI <- function(id){
  
  ns <- NS(id)
  tabPanel("b. by City",
           column(12, checkboxGroupButtons(
             inputId = ns("city"), label = "Select City: ", 
             choices = unique(Transactions_Month_City_Data_Table$city), 
             selected = "Pune",
             justified = T, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           
           column(12, plotlyOutput(ns("tra_3b_line"))))
}

trans_abs_city <- function(input,output,session,country){
  
  data <- reactive({Cancellations_City %>%
            filter(city %in% input$city) %>%
            group_by(month_names, country, city) %>%
            summarise(cancellation_rate = cancellation_rate) %>%
            ungroup()})
  
  output$tra_3b_line <- renderPlotly({
    p <-  (ggplot() +

             geom_line(data = data(),
                       aes(x=month_names, y=cancellation_rate, colour = city, group = city),
                       size = 0.5) +

             geom_point(data = data(),
                        aes(x=month_names, y=cancellation_rate, colour = city, group = city),
                        size = 1.5) +

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
                   text = element_text(family = "Palatino Linotype")))

    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))

  })
}