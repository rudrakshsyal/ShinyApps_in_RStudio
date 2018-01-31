## cpt_3e -- CPT doctors with Book

fqs_cpt_bookUI <- function(id){
  ns <- NS(id)
  tabPanel("e. Doctors with Book",
           fluidRow(
           column(3,switchInput(inputId = ns("deal_type"), value = F, size="mini", label="Type",
                                                            offLabel = "Prepaid", onLabel="All",
                                                            onStatus="danger", offStatus="success")),
           column(12, plotlyOutput(ns("cpt_3e1_column"))),
           column(12, plotlyOutput(ns("cpt_3e2_column"))))
)}

fqs_cpt_bookSERVER <- function(input,output,session,country){

  data_3e <- reactive({
    fqs_cpt_final %>%
      filter(deal_type == input$deal_type*1) %>%
      group_by(date) %>%
      summarise(doctor_count_book = sum(doctor_count_book),
                perc_doctor_count_book = sum(doctor_count_book)*100/sum(doctor_count))
  })
  
  output$cpt_3e1_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data=data_3e(),
                     aes(x=date,y=doctor_count_book),
                     stat = "identity", position = "dodge") +
            
            ylab("# Doctors") + xlab("Date") +
            
            ggtitle("CPT Doctors with Book") +
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
  
  output$cpt_3e2_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data=data_3e(),
                     aes(x=date,y=perc_doctor_count_book),
                     stat = "identity", position = "dodge") +
            
            ylab("% Doctors of total") + xlab("Date") +
            
            ggtitle("CPT Doctors with Book (as % of Total)") +
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
}