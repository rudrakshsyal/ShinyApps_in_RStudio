## trf_1f_column -- Traffic split by Tier

users_tierUI <- function(id){
  # attach("csv_data.RData")
  ns <- NS(id)
  tabPanel("f. by Tier",
           
           column(12, checkboxGroupButtons(
             inputId = ns("tier"), label = "Select Tier: ", 
             choices = unique(users_dch_tier$tier), 
             selected = c("Bangalore", "Mumbai", "Navi Mumbai", "Thane", "Delhi", "Ghaziabad", "Faridabad", "Gurgaon", "Noida", "Pune", "Hyderabad", "Chennai", "Tier-2", "Kolkata"),
             justified = T, 
             size = "xs",
             status = "success",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )
           ),
           
           column(12, plotlyOutput(ns("trf_1f_column"))))
}

users_tierSERVER <- function(input,output,session,country){
  
  data <- reactive({users_dch_tier %>%
                    filter(country %in% country()) %>%
                    group_by(month, tier) %>%
                    filter(tier %in% input$tier) %>%
                    group_by(month) %>%
                    summarise(`no. of users (in millions)` = (round((sum(users)/1000000),2))) %>%
                    ungroup()})
            
  output$trf_1f_column <- renderPlotly({
    p <- (ggplot(data()) + 
            geom_bar(aes(x=month, y=`no. of users (in millions)`), 
                     stat = "identity", fill = "dodgerblue4") +
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
    
    ggplotly(p, width = 800, height = 400, tooltip = c("x", "y")) 
  })
}