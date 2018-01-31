## liq_1a_column -- Overall Liquidity

liquidityUI <- function(id){
  ns <- NS(id)
  tabPanel("a. Overall Liquidity", plotlyOutput(ns("liq_1a_combo")))
}

liquiditySERVER <- function(input,output,session,country){
  
  data_column <- reactive({liquidity %>%
      filter(tier != "Total") %>%
      group_by(month,tier) %>%
      summarise(liquidity = liquidity)})
  
  data_line <- reactive({liquidity %>%
      filter(tier == "Total") %>%
      group_by(month) %>%
      summarise(liquidity = liquidity)})
  
  output$liq_1a_combo <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data = data_column(),
                     aes(x=month, y=liquidity, fill=tier),
                     stat = "identity",position = "dodge") +
            
            geom_line(data = data_line(),
                      aes(x=month, y=liquidity), size = 0.5) +
            
            geom_point(data = data_line(),
                      aes(x=month, y=liquidity), size = 1.5) +
            
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