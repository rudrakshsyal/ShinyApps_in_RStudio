## cpt_4c -- CPT Practice Acquisition

cpt_practice_acquisitionUI <- function(id){
  ns <- NS(id)
  tabPanel("c. Acquisition", 
           column(6, checkboxGroupButtons(
             inputId = ns("deal_type"), label = "Select Type: ", 
             choices = unique(cpt_practice_acquisition$deal_type), 
             selected = "Prepaid",
             justified = F, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           column(6, checkboxGroupButtons(
             inputId = ns("bu"), label = "Select BU: ", 
             choices = c('Enterprise','SMB'), 
             selected = c('Enterprise','SMB'),
             justified = F, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           column(12, checkboxGroupButtons(
             inputId = ns("tier"), label = "Select Tier: ", 
             choices = unique(cpt_practice_acquisition$tier), 
             selected = unique(cpt_practice_acquisition$tier),
             justified = F, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           column(12, plotlyOutput(ns("cpt_4c_line")))
)}

cpt_practice_acquisitionSERVER <- function(input,output,session,country){

  data_line <- reactive({
    cpt_practice_acquisition %>%
      mutate(bu = ifelse(is_enterprise == 1,'Enterprise','SMB')) %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(bu %in% input$bu) %>%
      filter(tier %in% input$tier) %>%
      group_by(start_month) %>%
      summarise(hunting = sum(hunting),
                recharge = sum(recharge),
                bfc = sum(bfc))  
  })
  
  output$cpt_4c_line <- renderPlotly({
    q <- (ggplot() +
          geom_line(data=data_line(),aes(x=start_month,y=hunting,colour="Hunting"),size=0.5) +
          geom_line(data=data_line(),aes(x=start_month,y=recharge,colour="Recharge"),size=0.5) +
          geom_line(data=data_line(),aes(x=start_month,y=bfc,colour="Back From Churn"),size=0.5) +
            
          geom_point(data=data_line(),aes(x=start_month,y=hunting,colour="Hunting"),size=1.5) +
          geom_point(data=data_line(),aes(x=start_month,y=recharge,colour="Recharge"),size=1.5) +
          geom_point(data=data_line(),aes(x=start_month,y=bfc,colour="Back From Churn"),size=1.5) +
            
          scale_color_discrete(name = "Type") +
            
          ylab("#Practices") + xlab("Month") +
          
          ggtitle("CPT Acquisition"))
  
  ggplotly(q, width = 900, height = 400, tooltip = c("x", "y"))
  })

}