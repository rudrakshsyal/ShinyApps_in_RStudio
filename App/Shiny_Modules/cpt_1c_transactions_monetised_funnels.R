## % Transactions Monetised Funnels
## 1. Last 7 days - cpt_1c1_combo
## 2. Last 30 days - cpt_1c2_combo
## 3. Month till date - cpt_1c3_combo

transactions_monetised_funnelsUI <- function(id){
  ns <- NS(id)
  tabPanel("c. CPT Monetisation Funnels", 
           column(6, checkboxGroupButtons(
             inputId = ns("deal_type"), label = "Select Type: ", 
             choices = unique(monetised_monetisable$deal_type), 
             selected = "Prepaid",
             justified = F, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           column(6, checkboxGroupButtons(
             inputId = ns("tier"), label = "Select Tier: ", 
             choices = unique(monetised_monetisable$tier), 
             selected = "Tier-1",
             justified = F, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           
           column(12, plotlyOutput(ns("cpt_1c1_combo"))),
           column(12, hr()),
           column(12, plotlyOutput(ns("cpt_1c2_combo"))),
           column(12, hr()),
           column(12, plotlyOutput(ns("cpt_1c3_combo"))),
           column(12, hr()),
           column(12, plotlyOutput(ns("cpt_1c4_column")))
           )
}

transactions_monetised_funnelsSERVER <- function(input,output,session,country){
  
  data_column7d <- reactive({
    transaction_funnel %>%
      filter(country %in% country()) %>%
      filter(timeframe == 'rolling_7d') %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(tier %in% input$tier) %>%
      group_by(entity) %>%
      summarise(transactions = (round(sum(transactions)/1000))) %>%
      mutate(max = max(transactions)) %>%
      mutate(perc = round(transactions*100/max,1)) %>%
      mutate(entity_num = paste0(entity," - ",perc,"%"))
  })

  output$cpt_1c1_combo <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data = data_column7d(),
                     aes(x=entity_num, y=transactions),
                     stat = "identity",position="dodge", fill = "dodgerblue4", width = 0.5) +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            ggtitle("Last 7 days") + 
            
            ylab("Transactions (in thousands)") + 
            xlab("") +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  # legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
  })
  
  data_column30d <- reactive({
    transaction_funnel %>%
      filter(country %in% country()) %>%
      filter(timeframe == 'rolling_30d') %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(tier %in% input$tier) %>%
      group_by(entity) %>%
      summarise(transactions = (round(sum(transactions)/1000))) %>%
      mutate(max = max(transactions)) %>%
      mutate(perc = round(transactions*100/max,1)) %>%
      mutate(entity_num = paste0(entity," - ",perc,"%"))
  })
  
  output$cpt_1c2_combo <- renderPlotly({
    q <- (ggplot() +
            geom_bar(data = data_column30d(),
                     aes(x=entity_num, y=transactions),
                     stat = "identity",position="dodge", fill = "dodgerblue4", width = 0.5) +
            
            ggtitle("Last 30 days") + 
            
            ylab("Transactions (in thousands)") +
            xlab("") +
            
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
    
    ggplotly(q, width = 1000, height = 400, tooltip = c("x", "y"))
  })
  
  data_columnmtd <- reactive({
    transaction_funnel %>%
      filter(country %in% country()) %>%
      filter(timeframe == 'mtd') %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(tier %in% input$tier) %>%
      group_by(entity) %>%
      summarise(transactions = (round(sum(transactions)/1000))) %>%
      mutate(max = max(transactions)) %>%
      mutate(perc = round(transactions*100/max,1)) %>%
      mutate(entity_num = paste0(entity," - ",perc,"%"))
  })
  
  output$cpt_1c3_combo <- renderPlotly({
    r <- (ggplot() +
            geom_bar(data = data_columnmtd(),
                     aes(x=entity_num, y=transactions),
                     stat = "identity",position="dodge", fill = "dodgerblue4", width = 0.5) +
            
            ggtitle("Month Till Date") + 
            
            ylab("Transactions (in thousands)") +
            xlab("") +
            
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
    
    ggplotly(r, width = 1000, height = 400, tooltip = c("x", "y"))
  })
  
  data_column_compare <- reactive({
    transaction_funnel %>%
      filter(country %in% country()) %>%
      filter(timeframe %in% c('mtd','pm_month','ppm_month')) %>%
      mutate(timeframe = paste0(substr(months(start), 1, 3),"-",substr(year(start), 3, 4))) %>%
      filter(deal_type %in% input$deal_type) %>%
      filter(tier %in% input$tier) %>%
      group_by(entity,timeframe) %>%
      summarise(transactions = (round(sum(transactions)/1000, 2))) %>%
      group_by(timeframe) %>%
      mutate(max = max(transactions)) %>%
      mutate(perc = round(transactions*100/max,1)) %>%
      # mutate(entity_num = str_wrap(paste0(entity," - ",perc,"%"), width = 0.5)) %>%
      # group_by(timeframe, entity_num) %>%
      select(entity, timeframe, transactions, perc) %>%
      mutate(entity = ifelse(grepl('Total', entity), "1. Total", ifelse(grepl('Enterprise', entity), "2. Enterprise", ifelse(grepl('CPT', entity), "4. CPT", "3. Monetised"))))
  })
  
  output$cpt_1c4_column <- renderPlotly({
    s <- (ggplot() +
            geom_bar(data = data_column_compare(),
                     aes(x=entity, y=transactions),
                     stat = "identity", position="dodge", fill = "dodgerblue") +
            
            geom_text(data = data_column_compare(),
                      aes(x=entity, y = transactions/2, label = paste0(perc," %")),size=4) + 
            
            ggtitle("In comparison with last 2 months") +
            
            ylab("Transactions (in thousands)") +
            xlab("") +
            
            scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue()
            
          #   theme(axis.text.x = element_text(size = 10),
          #         axis.text.y = element_text(size = 10),
          #         axis.title = element_text(size = 12),
          #         legend.text = element_text(size = 10),
          #         legend.position = "bottom",
          #         legend.justification = "center",
          #         # legend.direction = "horizontal",
          #         legend.title = element_blank(),
          #         text = element_text(family = "Palatino Linotype"))
          )
    
    ggplotly((s + facet_grid(. ~ timeframe)), width = 1000, height = 400, tooltip = c("x", "y"))
  })
  
}