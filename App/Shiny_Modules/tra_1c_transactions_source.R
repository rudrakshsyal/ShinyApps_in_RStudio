## tra_1c_line -- Transaction split by Source

trans_sourceUI <- function(id){
  ns <- NS(id)
  tabPanel("c. by Source", plotlyOutput(ns("tra_1c_line")))
}

trans_source <- function(input,output,session,country){
  
  data <- reactive({Book_Practo_Source %>%
                    filter(country %in% country()) %>%
                    group_by(month_names, sources) %>%
                    summarise(`no. of ABS Appointments (in thousands)` = (round((sum(book_practo)/1000),2))) %>%
                    select(month_names, sources, `no. of ABS Appointments (in thousands)`) %>%
                    ungroup()})
  
  output$tra_1c_line <- renderPlotly({
    p <- (ggplot() +
          scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
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
                    aes(x=month_names, y=`no. of ABS Appointments (in thousands)`, colour = sources, group = sources),
                    size = 0.5) +

          geom_point(data = data(),
                     aes(x=month_names, y=`no. of ABS Appointments (in thousands)`, colour = sources, group = sources),
                     size = 1.5))

    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
  })
}