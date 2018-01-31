## liq_1b_line -- Liquidity Area under the curve

liquidity_aucUI <- function(id){
  ns <- NS(id)
  tabPanel("b. Area Under the Curve", plotlyOutput(ns("liq_1b_line")))
}

liquidity_auc <- function(input,output,session,country){
  
  output$liq_1b_line <- renderPlotly({
    p <-  (ggplot(data = temp_2, aes(x=cumm_perc, y=transactions, colour = month, group = month)) + 
             geom_line(size = 0.5) +
             geom_point(size = 1) + 
             scale_x_continuous(limits = c(0,1)) + 
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