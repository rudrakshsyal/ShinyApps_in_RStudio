## cpt_4b -- CPT Practice Churn 30 day

cpt_practice_churn_30dUI <- function(id){
  ns <- NS(id)
  tabPanel("b. Practice Churn - 30 day", 
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
           column(12, plotlyOutput(ns("cpt_4b1_column"))),
           column(12, plotlyOutput(ns("cpt_4b2_line")))
)}

cpt_practice_churn_30dSERVER <- function(input,output,session,country){

  data_column <- reactive({
    cpt_practice_churn %>%
      mutate(bu = ifelse(is_enterprise == 1,'Enterprise','SMB')) %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(bu %in% input$bu) %>%
      filter(tier %in% input$tier) %>%
      group_by(renewal_month) %>%
      summarise(churn_rate_30d = round(sum(churned_30)*100/sum(upforrenewal),1))
  })
  
  data_line <- reactive({
    cpt_practice_churn %>%
      mutate(bu = ifelse(is_enterprise == 1,'Enterprise','SMB')) %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(bu %in% input$bu) %>%
      filter(tier %in% input$tier) %>%
      group_by(renewal_month) %>%
      summarise(churned_30 = sum(churned_30),
                upforrenewal = sum(upforrenewal))  
  })
  
  output$cpt_4b1_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data=data_column(),aes(x=renewal_month,y=churn_rate_30d),
                     stat = "identity", position = "dodge") +
            
            ylab("Churn Rate") + xlab("Renewal Month") +
            
            ggtitle("Churn Rate - 30 day") +
            
            scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
  
  output$cpt_4b2_line <- renderPlotly({
    q <- (ggplot() +
          geom_line(data=data_line(),aes(x=renewal_month,y=churned_30,colour="Churned"),size=0.5) +
          geom_line(data=data_line(),aes(x=renewal_month,y=upforrenewal,colour="Up for Renewal"),size=0.5) +
          
          geom_point(data=data_line(),aes(x=renewal_month,y=churned_30,colour="Churned"),size=1.5) +
          geom_point(data=data_line(),aes(x=renewal_month,y=upforrenewal,colour="Up for Renewal"),size=1.5) +
          
          ylab("#Practices") + xlab("Renewal Month") +
            
          scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
          scale_color_discrete(name = "Type") +
          ggtitle("#Practices - Up for renewal & Churned - 30 day"))
  
  ggplotly(q, width = 900, height = 400, tooltip = c("x", "y"))
  })

}