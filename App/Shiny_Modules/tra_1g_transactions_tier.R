## tra_1g_column -- Transaction split by Tier

trans_tierUI <- function(id){
  ns <- NS(id)
  tabPanel("g. Unique Tranxs. by Tier",
           column(12, checkboxGroupButtons(
             inputId = ns("tier"), label = "Select Tier: ", 
             choices = unique(transactions_unique_monthly$tier),
             selected = c("Bangalore", "Mumbai", "Navi Mumbai", "Thane", "Delhi", "Ghaziabad", "Faridabad", "Gurgaon", "Noida", "Pune", "Hyderabad", "Chennai", "Tier-2", "Kolkata"),
             justified = T, 
             size = "xs",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           csvDownloadUI(ns("tra_1g")),
           column(12, plotlyOutput(ns("tra_1g_column"))))
}

trans_tier <- function(input,output,session,country){
  
  data <- reactive({transactions_unique_monthly %>%
                    filter(country %in% country()) %>%
                    group_by(month, tier) %>%
                    filter(tier %in% input$tier) %>%
                    group_by(month) %>%
                    summarise(`no. of transactions (in thousands)` = (round((sum(trans)/1000),2))) %>%
                    ungroup()})
  
  output$tra_1g_column <- renderPlotly({
    p <- (ggplot() +
            geom_bar(data = data(),
                     aes(x=month, y=`no. of transactions (in thousands)`),
                     stat = "identity", fill = "dodgerblue4") +

            scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
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
  
  callModule(csvDownload, "tra_1g", data, "tier-wise unique transactions")
}