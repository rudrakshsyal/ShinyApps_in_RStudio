## cpt_3b_column -- FQS for CPT customers

cpt_practices_liveUI <- function(id){
  ns <- NS(id)
  tabPanel("b. Practices Live on CPT", 
           plotlyOutput(ns("cpt_3b_column"))
)}

cpt_practices_liveSERVER <- function(input,output,session,country){

  output$cpt_3b_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data=cpt_practices_live,aes(x=date,y=practice_count),
                     stat = "identity", position = "dodge") +
            
            ylab("#Practices") + xlab("Date") +
            
            ggtitle("#Practices Live on CPT") +
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
}