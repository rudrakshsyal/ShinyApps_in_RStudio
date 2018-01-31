## liq_1a_column -- Liquidity month-on-month trend

liquidity_monthlyUI <- function(id){
  ns <- NS(id)
  tabPanel("a. Month-on-Month trend", plotOutput(ns("liq_1a_column")))
}

liquidity_monthly <- function(input,output,session,country){
  
  output$liq_1a_column <- renderPlot({
    ggplot(data = liquidity_mom,
           aes(x=month, y=liq)) +
      
      geom_bar(width = 15, stat="identity", fill = "deepskyblue4") +
      geom_text(size=4, vjust = -1, aes(label = liq)) + 
      scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
      scale_y_continuous(position = "right", limits = c(0,25)) +
      theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
      
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.position = "bottom",
            legend.justification = "center",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            text = element_text(family = "Palatino Linotype"))
  },
  height = 400,
  width = 1000
  )
}