## tra_1a_combo -- Transaction split by Mode

trans_modeUI <- function(id){
  ns <- NS(id)
  tabPanel("a. by Mode", plotlyOutput(ns("tra_1a_combo")))
}

trans_mode <- function(input,output,session,country){
  
  data <- reactive({total_transactions_country %>%
                    filter(country %in% country()) %>%
                    group_by(month_names, filter) %>%
                    summarise(`no. of transactions (in thousands)` = (round((sum(value)/1000),2))) %>%
                    ungroup()})
  
  data_bar <- reactive({total_transactions_country %>%
                        filter(!filter %in% "vn_forward") %>%
                        filter(!filter %in% "vn_connect") %>%
                        filter(country %in% country()) %>%
                        group_by(month_names) %>%
                        summarise(`no. of transactions (in thousands)` = (round((sum(value)/1000),2))) %>%
                        ungroup()})
     
  output$tra_1a_combo <- renderPlotly({
    
    p <- (ggplot() +

          geom_bar(data = data_bar(),
                   aes(x=month_names, y=`no. of transactions (in thousands)`),
                   width = 10, stat = "identity", fill = "deepskyblue4") +
          
          scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") + 
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

          geom_line(data = data(),
                    aes(x=month_names, y=`no. of transactions (in thousands)`, colour = filter, group = filter),
                    size = 0.5) +

          geom_point(data = data(),
                     aes(x=month_names, y=`no. of transactions (in thousands)`, colour = filter, group = filter),
                     size = 1.5))

    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
  })
}