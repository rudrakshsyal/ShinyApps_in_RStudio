## cpt_3a_column -- FQS for CPT customers
## cpt_3b_line -- FQS for CPT customers

fqs_cptUI <- function(id){
  ns <- NS(id)
  tabPanel("a. % Doctors with 20 or more Recommendations",
           fluidRow(
           column(3,switchInput(inputId = ns("deal_type"), value = F, size="mini", label="Type",
                                                            offLabel = "Prepaid", onLabel="All",
                                                            onStatus="danger", offStatus="success")),
           column(12, plotlyOutput(ns("cpt_3a1_column"))),
           column(12, plotlyOutput(ns("cpt_3a2_column"))),
           column(12, plotlyOutput(ns("cpt_3a3_line"))),
           column(12, plotlyOutput(ns("cpt_3a4_line"))))
)}

fqs_cptSERVER <- function(input,output,session,country){

  data_3a1 <- reactive({
    fqs_cpt_final %>%
      filter(deal_type == input$deal_type*1) %>%
      group_by(date) %>%
      summarise(docs_20recos = sum(docs_20recos))
  })
  
  output$cpt_3a1_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data=data_3a1(),
                     aes(x=date,y=docs_20recos),
                     stat = "identity", position = "dodge") +
            
            ylab("# Doctors") + xlab("Date") +
            
            ggtitle("CPT Doctors with 20 or more Recommendations") +
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
  
  data_3a2 <- reactive({
    fqs_cpt_final %>%
      filter(deal_type == input$deal_type*1) %>%
      group_by(date) %>%
      summarise(docs_10reviews = sum(docs_10reviews))
  })
  
  output$cpt_3a2_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data=data_3a2(),
                     aes(x=date,y=docs_10reviews),
                     stat = "identity", position = "dodge") +
            
            ylab("# Doctors") + xlab("Date") +
            
            ggtitle("CPT Doctors with 10 or more Reviews") +
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
  
  data_3a3 <- reactive({
    fqs_cpt_final %>%
      filter(deal_type == input$deal_type*1) %>%
      group_by(date) %>%
      summarise(doctor_count = sum(doctor_count))
  })
  
  output$cpt_3a3_line <- renderPlotly({
    p <- (ggplot() +

            geom_line(data=data_3a3(),
                      aes(x=date,y=doctor_count)) +
            
            ylab("# Doctors") + xlab("Date") +
            
            ggtitle("Total Doctors live on CPT") +
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
    })
  
  data_3a4 <- reactive({
    fqs_cpt_final %>%
      filter(deal_type == input$deal_type*1) %>%
      group_by(date) %>%
      summarise(doctor_count_book = sum(doctor_count_book))
  })
  
  output$cpt_3a4_line <- renderPlotly({
    p <- (ggplot() +

            geom_line(data=data_3a4(),
                      aes(x=date,y=doctor_count_book)) +
            
            ylab("# Doctors on Book") + xlab("Date") +
            
            ggtitle("Total Doctors live on CPT with Book enabled") +
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
    })
}