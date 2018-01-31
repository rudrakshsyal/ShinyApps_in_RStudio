## cpt_3d -- CPT doctors with 10+ reviews

fqs_cpt_reviews10UI <- function(id){
  ns <- NS(id)
  tabPanel("d. Doctors with 10+ Reviews",
           fluidRow(
           column(3,switchInput(inputId = ns("deal_type"), value = F, size="mini", label="Type",
                                                            offLabel = "Prepaid", onLabel="All",
                                                            onStatus="danger", offStatus="success")),
           column(12, plotlyOutput(ns("cpt_3d1_column"))),
           column(12, plotlyOutput(ns("cpt_3d2_column"))))
)}

fqs_cpt_reviews10SERVER <- function(input,output,session,country){

  data_3d <- reactive({
    fqs_cpt_final %>%
      filter(deal_type == input$deal_type*1) %>%
      group_by(date) %>%
      summarise(docs_10reviews = sum(docs_10reviews),
                perc_docs_10reviews = sum(docs_10reviews)*100/sum(doctor_count))
  })
  
  output$cpt_3d1_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data=data_3d(),
                     aes(x=date,y=docs_10reviews),
                     stat = "identity", position = "dodge") +
            
            ylab("# Doctors") + xlab("Date") +
            
            ggtitle("CPT Doctors with 10 or more Reviews") +
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
  
  output$cpt_3d2_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data=data_3d(),
                     aes(x=date,y=perc_docs_10reviews),
                     stat = "identity", position = "dodge") +
            
            ylab("% Doctors of total") + xlab("Date") +
            
            ggtitle("CPT Doctors with 10 or more Reviews (as % of Total)") +
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
} 
