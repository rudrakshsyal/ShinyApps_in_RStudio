## tra_1d_column -- Transaction split by Segment

## UI Function
trans_segmentUI <- function(id){
  ns <- NS(id)
  tabPanel("d. by Segment", plotlyOutput(ns("tra_1d_column")))
}

## Server Function
trans_segment <- function(input,output,session,country){
  
  output$tra_1d_column <- renderPlotly({

    p <- (ggplot(data = final_vpe_gmv) +
    
          geom_bar(aes(x=month, y=`Transactional Value (in percentage)`, fill = segment), 
                   position="stack", stat="identity") +
          theme_gdocs() + scale_fill_wsj() + scale_colour_hue() +
    
          theme(axis.text.x = element_text(size = 9),
                axis.text.y = element_text(size = 9),
                axis.title = element_text(size = 12),
                legend.text = element_text(size = 10),
                legend.position = "bottom",
                legend.justification = "center",
                legend.direction = "horizontal",
                legend.title = element_blank(),
                text = element_text(family = "Palatino Linotype")) +
    
          scale_x_date(date_breaks = "1 month", date_labels = "%b-%y"))

    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y")) %>%
      layout(legend = list(orientation = "h"))
  })
}