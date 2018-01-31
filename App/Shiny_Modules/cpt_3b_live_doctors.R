## cpt_3b - CPT doctors live

fqs_cpt_live_docsUI <- function(id){
  ns <- NS(id)
  tabPanel("b. CPT Live Doctors",
           fluidRow(
           column(3,switchInput(inputId = ns("deal_type"), value = F, size="mini", label="Type",
                                                            offLabel = "Prepaid", onLabel="All",
                                                            onStatus="danger", offStatus="success")),
           column(12, plotlyOutput(ns("cpt_3b_line"))))
)}

fqs_cpt_live_docsSERVER <- function(input,output,session,country){

  data_3b <- reactive({
    fqs_cpt_final %>%
      filter(deal_type == input$deal_type*1) %>%
      group_by(date) %>%
      summarise(doctor_count = sum(doctor_count))
  })
  
  output$cpt_3b_line <- renderPlotly({
    p <- (ggplot() +

            geom_line(data=data_3b(),
                      aes(x=date,y=doctor_count)) +
            
            ylab("# Doctors") + xlab("Date") +
            
            ggtitle("Total Doctors live on CPT") +
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
    })
  
}