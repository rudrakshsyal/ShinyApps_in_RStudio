## cpt_3c - CPT doctors with 20+ recos

fqs_cpt_recos20UI <- function(id){
  ns <- NS(id)
  tabPanel("c. Doctors with 20+ Recos",
           fluidRow(
           column(3,switchInput(inputId = ns("deal_type"), value = F, size="mini", label="Type",
                                                            offLabel = "Prepaid", onLabel="All",
                                                            onStatus="danger", offStatus="success")),
           column(12, plotlyOutput(ns("cpt_3c1_column"))),
           column(12, plotlyOutput(ns("cpt_3c2_column"))))
)}

fqs_cpt_recos20SERVER <- function(input,output,session,country){

  data_3c <- reactive({
    fqs_cpt_final %>%
      filter(deal_type == input$deal_type*1) %>%
      group_by(date) %>%
      summarise(docs_20recos = sum(docs_20recos),
                perc_docs_20recos = sum(docs_20recos)*100/sum(doctor_count))
  })
  
  output$cpt_3c1_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data=data_3c(),
                     aes(x=date,y=docs_20recos),
                     stat = "identity", position = "dodge") +
            
            ylab("# Doctors") + xlab("Date") +
            
            ggtitle("CPT Doctors with 20 or more Recommendations") +
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
  
  output$cpt_3c2_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data=data_3c(),
                     aes(x=date,y=perc_docs_20recos),
                     stat = "identity", position = "dodge") +
            
            ylab("% Doctors of total") + xlab("Date") +
            
            ggtitle("CPT Doctors with 20 or more Recommendations (as % of Total)") +
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
}