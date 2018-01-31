## cpt_3a_column -- eCPT for Reach & CPT

ecpt_credit_transactionsUI <- function(id){
  ns <- NS(id)
  tabPanel("c. eCPT for CPT Subscriptions", 
           column(6, checkboxGroupButtons(
             inputId = ns("deal_type"), label = "Select Type: ", 
             choices = unique(monetised_monetisable$deal_type), 
             selected = "Prepaid",
             justified = F, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           column(3, checkboxGroupButtons(
             inputId = ns("tier"), label = "Select Tier: ", 
             choices = unique(monetised_monetisable$tier), 
             selected = "Tier-1",
             justified = F, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           column(3, checkboxGroupButtons(
             inputId = ns("bu"), label = "Select BU: ", 
             choices = c('Enterprise','SMB'), 
             selected = c("Enterprise","SMB"),
             justified = F, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           column(12, plotlyOutput(ns("cpt_2c_column"))))
}

ecpt_credit_transactionsSERVER <- function(input,output,session,country){
  
  data <- reactive({
    cpt_credit_transactions %>%
      mutate(bu = ifelse(is_enterprise == 1,'Enterprise','SMB')) %>%
      filter(country %in% country()) %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(bu %in% input$bu) %>%
      filter(tier %in% input$tier) %>%
      group_by(month) %>%
      summarise(ecpt = round(sum(cpt_revenue)/sum(cpt_transactions),1)) %>%
      mutate(variable = 'eCPT')
  })
  
  output$cpt_2c_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data = data(),
                     aes(x=month, y=ecpt, fill=variable),
                     stat = "identity") +
            
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
}