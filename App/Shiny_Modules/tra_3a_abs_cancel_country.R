## tra_3a_line -- ABS Cancellation Rate by Country

trans_abs_countryUI <- function(id){
  ns <- NS(id)
  tabPanel("a. by Country", csvDownloadUI(ns("tra_3a")),
           plotlyOutput(ns("tra_3a_line")))
}

trans_abs_country <- function(input,output,session,country){
  
  data <- reactive({Cancellations_Country %>%
            filter(country %in% country()) %>%
            group_by(month_names, country) %>%
            summarise(cancellation_rate = cancellation_rate) %>%
            ungroup()})
  
  output$tra_3a_line <- renderPlotly({
    p <- (ggplot() +

            geom_line(data = data(),
            aes(x=month_names, y=cancellation_rate, colour = country, group = country),
            size = 0.5) +

            geom_point(data = data(),
            aes(x=month_names, y=cancellation_rate, colour = country, group = country),
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
  
  callModule(csvDownload, "tra_3a", data, "abs_cancellations_country")
}