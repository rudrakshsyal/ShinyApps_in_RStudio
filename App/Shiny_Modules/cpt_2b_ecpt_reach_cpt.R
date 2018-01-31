## cpt_2b_column -- eCPT for Reach & CPT

ecpt_reach_cptUI <- function(id){
  ns <- NS(id)
  tabPanel("b. eCPT", 
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
           column(12, plotlyOutput(ns("cpt_2b_combo"))))
}

ecpt_reach_cptSERVER <- function(input,output,session,country){
  
  factor <- as.integer(update_date-floor_date(update_date,"month")+1)/as.integer(floor_date(update_date,"month")+months(1)-floor_date(update_date,"month"))
  monetised_monetisable$reach_revenue[monetised_monetisable$month == floor_date(update_date,"month")] <- 
    factor*monetised_monetisable$reach_revenue[monetised_monetisable$month == floor_date(update_date,"month")]
  
  data_column <- reactive({
    monetised_monetisable %>%
      mutate(bu = ifelse(is_enterprise == 1,'Enterprise','SMB')) %>%
      filter(country %in% country()) %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(bu %in% input$bu) %>%
      filter(tier %in% input$tier) %>%
      group_by(month) %>%
      summarise(ecpt = round(((sum(reach_revenue)+sum(cpt_revenue))/sum(monetised_transactions)),1))
  })
  
  data_line <- reactive({
    bind_rows(
    monetised_monetisable %>%
      mutate(bu = ifelse(is_enterprise == 1,'Enterprise','SMB')) %>%
      filter(country %in% country()) %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(bu %in% input$bu) %>%
      filter(tier %in% input$tier) %>%
      group_by(month) %>%
      summarise(ecpt = round((sum(cpt_revenue)/sum(cpt_transactions)),1)) %>%
      ungroup() %>%
      mutate(type = 'CPT'),
    monetised_monetisable %>%
      mutate(bu = ifelse(is_enterprise == 1,'Enterprise','SMB')) %>%
      filter(country %in% country()) %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(bu %in% input$bu) %>%
      filter(tier %in% input$tier) %>%
      group_by(month) %>%
      summarise(ecpt = round((sum(reach_revenue)/sum(reach_transactions)),1)) %>%
      ungroup() %>%
      mutate(type = 'Reach'))
    })

  output$cpt_2b_combo <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data = data_column(),aes(x=month, y=ecpt),stat = "identity") +
            
            geom_line(data = data_line(),
                      aes(x=month, y=ecpt, group=type, colour=type), size = 0.5) +
            
            geom_point(data = data_line(),
                       aes(x=month, y=ecpt, group=type, colour=type), size = 1.5) +
            
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
            theme_gdocs() + scale_fill_wsj())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
}