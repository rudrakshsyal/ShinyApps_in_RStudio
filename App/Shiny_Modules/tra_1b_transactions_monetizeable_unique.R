## tra_1b_column -- Transactions Monetizable & Unique

## UI Function
trans_monetizeable_uniqueUI <- function(id){
  ns <- NS(id)
  tabPanel("b. Monetizable & Unique Tranxs.", 
           csvDownloadUI(ns("tra_1b")),
           plotlyOutput(ns("tra_1b_column")))
}

## Server Function
trans_monetizeable_unique <- function(input,output,session,country){
  
  data <- reactive({Monetizable_Transactions %>%
                    filter(country %in% country()) %>%
                    group_by(month_names, variable) %>%
                    summarise(`no. of transactions (in thousands)` = (round((sum(value)/1000),2))) %>%
                    ungroup()})
  
  output$tra_1b_column <- renderPlotly({
    
      p <- (ggplot(data(),
                   aes(x=month_names, y=`no. of transactions (in thousands)`)) +

              geom_bar(aes(x=month_names, y=`no. of transactions (in thousands)`, fill = variable),
                       width = 10, stat = "identity", position = "dodge") +
              
              scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
              theme_gdocs() + scale_fill_economist() + scale_colour_hue() +

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
  
  callModule(csvDownload, "tra_1b", data, "monthly_unique_transactions")
}