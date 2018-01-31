## cpt_1a_column -- % Transactions to Enterprise

unique_transactions_enterpriseUI <- function(id){
  ns <- NS(id)
  tabPanel("a. % Transactions to Enterprise", plotlyOutput(ns("cpt_1a_combo")))
}

unique_transactions_enterpriseSERVER <- function(input,output,session,country){
  
  data_column <- reactive({monetised_monetisable %>%
      filter(country %in% country()) %>%
      mutate(bu = ifelse(is_enterprise==1,'Enterprise','SMB')) %>%
      group_by(month,bu,tier) %>%
      summarise(trans = sum(monetisable_transactions)) %>%
      group_by(month,tier) %>%
      mutate(total = sum(trans)) %>%
      filter(bu == 'Enterprise') %>%
      summarise(perc_enterprise = (trans*100)/total)})
  
  data_line <- reactive({monetised_monetisable %>%
      filter(country %in% country()) %>%
      mutate(bu = ifelse(is_enterprise==1,'Enterprise','SMB')) %>%
      group_by(month,bu) %>%
      summarise(trans = sum(monetisable_transactions)) %>%
      group_by(month) %>%
      mutate(total = sum(trans)) %>%
      filter(bu == 'Enterprise') %>%
      summarise(perc_enterprise = (trans*100)/total)})
  
  output$cpt_1a_combo <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data = data_column(),
                     aes(x=month, y=perc_enterprise, fill=tier),
                     stat = "identity",position = "dodge") +
            
            geom_line(data = data_line(),
                      aes(x=month, y=perc_enterprise), size = 0.5) +
            
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  # legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")))
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
}