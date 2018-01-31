## cpt_2a_column -- MRR for Reach & CPT

mrr_reach_cptUI <- function(id){
  ns <- NS(id)
  tabPanel("a. MRR", 
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
             selected = c('Enterprise','SMB'),
             justified = F, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           column(12, plotlyOutput(ns("cpt_2a_combo"))))
}

mrr_reach_cptSERVER <- function(input,output,session,country){
  
  data_column <- reactive({
    monetised_monetisable %>%
      mutate(bu = ifelse(is_enterprise == 1,'Enterprise','SMB')) %>%
      filter(country %in% country()) %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(bu %in% input$bu) %>%
      filter(tier %in% input$tier) %>%
      group_by(month) %>%
      summarise(mrr = round((sum(reach_revenue)+sum(cpt_revenue))/100000,1))
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
      summarise(mrr = round(sum(cpt_revenue)/100000,1)) %>%
      ungroup() %>%
      mutate(type = 'CPT'),
    monetised_monetisable %>%
      mutate(bu = ifelse(is_enterprise == 1,'Enterprise','SMB')) %>%
      filter(country %in% country()) %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(bu %in% input$bu) %>%
      filter(tier %in% input$tier) %>%
      group_by(month) %>%
      summarise(mrr = round(sum(reach_revenue)/100000,1)) %>%
      ungroup() %>%
      mutate(type = 'Reach'))
    })

  output$cpt_2a_combo <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data = data_column(),
                     aes(x=month, y=mrr),
                     stat = "identity", fill = "dodgerblue4") +
            
            geom_line(data = data_line(),
                      aes(x=month, y=mrr, group=type, colour=type), size = 0.5) +
            
            geom_point(data = data_line(),
                       aes(x=month, y=mrr,group=type, colour=type), size = 1.5) +
            
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
            theme_gdocs() + scale_fill_wsj())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
}