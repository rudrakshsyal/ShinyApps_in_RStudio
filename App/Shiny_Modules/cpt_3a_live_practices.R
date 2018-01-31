## cpt_3a_combo -- Practices live on CPT

cpt_practices_liveUI <- function(id){
  ns <- NS(id)
  tabPanel("a. Practices Live on CPT",
           fluidRow(
           column(3,switchInput(inputId = ns("deal_type"), value = F, size="mini", label="Type",
                                                            offLabel = "Prepaid", onLabel="All",
                                                            onStatus="danger", offStatus="success")),
           column(12,plotlyOutput(ns("cpt_3a_column"))))
)}

cpt_practices_liveSERVER <- function(input,output,session,country){
  
  data_3a <- reactive({
    fqs_cpt_final %>%
      filter(deal_type == input$deal_type*1) %>%
      group_by(date) %>%
      summarise(practice_count = sum(practice_count))
  })

  output$cpt_3a_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data=data_3a(),
                     aes(x=date,y=practice_count),
                     stat = "identity", position = "dodge") +
            
            ylab("# Practices") + xlab("Date") +
            
            ggtitle("CPT Practices Live") +
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
    
    
  })
}