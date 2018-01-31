## cpt_4a -- CPT Practice Churn 90 day

cpt_practice_churn_90dUI <- function(id){
  ns <- NS(id)
  tabPanel("a. Practice Churn - 90 day", 
           column(6, checkboxGroupButtons(
             inputId = ns("deal_type"), label = "Select Type: ", 
             choices = unique(cpt_practice_churn$deal_type), 
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
             choices = unique(cpt_practice_churn$tier), 
             selected = unique(cpt_practice_churn$tier),
             justified = F, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           column(12, plotlyOutput(ns("cpt_4a1_column"))),
           column(12, plotlyOutput(ns("cpt_4a2_line")))
)}

cpt_practice_churn_90dSERVER <- function(input,output,session,country){

  data_column <- reactive({
    cpt_practice_churn %>%
      mutate(bu = ifelse(is_enterprise == 1,'Enterprise','SMB')) %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(bu %in% input$bu) %>%
      filter(tier %in% input$tier) %>%
      group_by(renewal_month) %>%
      summarise(churn_rate_90d = round(sum(churned_90)*100/sum(upforrenewal),1))
  })
  
  data_line <- reactive({
    cpt_practice_churn %>%
      mutate(bu = ifelse(is_enterprise == 1,'Enterprise','SMB')) %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(bu %in% input$bu) %>%
      filter(tier %in% input$tier) %>%
      group_by(renewal_month) %>%
      summarise(churned_90 = sum(churned_90),
                upforrenewal = sum(upforrenewal))  
  })
  
  output$cpt_4a1_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data=data_column(),aes(x=renewal_month,y=churn_rate_90d),
                     stat = "identity", position = "dodge") +
            
            ylab("Churn Rate") + xlab("Renewal Month") +
            
            scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
            
            ggtitle("Churn Rate - 90 day"))
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
  
  output$cpt_4a2_line <- renderPlotly({
    q <- (ggplot() +
          geom_line(data=data_line(),aes(x=renewal_month,y=churned_90,colour="Churned"),size=0.5) +
          geom_line(data=data_line(),aes(x=renewal_month,y=upforrenewal,colour="Up for Renewal"),size=0.5) +
          
          geom_point(data=data_line(),aes(x=renewal_month,y=churned_90,colour="Churned"),size=1.5) +
          geom_point(data=data_line(),aes(x=renewal_month,y=upforrenewal,colour="Up for Renewal"),size=1.5) +
            
          ylab("#Practices") + xlab("Renewal Month") +
          
          scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
          scale_color_discrete(name = "Type") +
          ggtitle("#Practices - Up for renewal & Churned - 90 day"))
  
  ggplotly(q, width = 900, height = 400, tooltip = c("x", "y"))
  })

}