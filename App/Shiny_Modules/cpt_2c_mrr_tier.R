## cpt_2c_column -- MRR for Reach + CPT split by Tier

mrr_tierUI <- function(id){
  ns <- NS(id)
  tabPanel("c. MRR (by Tier)", plotlyOutput(ns("cpt_2c_column")))
}

mrr_tierSERVER <- function(input,output,session,country){
  
  data_column <- reactive({mrr_ecpt %>%
      filter(country %in% country()) %>%
      group_by(month) %>%
      summarise(mrr = round(sum(mrr)/100000,1))})
  
  data_line <- reactive({mrr_ecpt %>%
      filter(country %in% country()) %>%
      group_by(month,tier) %>%
      summarise(mrr = round(sum(mrr)/100000,1))})
  
  output$cpt_2c_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data = data_column(),
                     aes(x=month, y=mrr),
                     stat = "identity", fill = "dodgerblue4") +
            
            geom_line(data = data_line(),
                      aes(x=month, y=mrr, group=tier, color=tier), size = 0.5) +
            
            geom_point(data = data_line(),
                      aes(x=month, y=mrr, group=tier, color=tier), size = 1.5) +
            
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
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
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
}