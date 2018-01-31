## trf_1d_line -- Traffic split by Segment-Tier

users_segmenttierUI <- function(id){
  # attach("csv_data.RData")
  ns <- NS(id)
  tabPanel("d. by Segment-Tier",
           
           column(6, checkboxGroupButtons(
             inputId = ns("segment"), label = "Select Segment: ", 
             choices = unique(users_listing_profile_segment_tier$segment), 
             selected = "doctor",
             justified = T, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           
           column(6, checkboxGroupButtons(
             inputId = ns("tier"), label = "Select Tier: ", 
             choices = unique(users_listing_profile_segment_tier$tier), 
             selected = "Tier-1",
             justified = T, 
             size = "sm",
             status = "success",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           
           column(12, plotlyOutput(ns("trf_1d_line"))))
}

users_segmenttierSERVER <- function(input,output,session,country){
  
  data <- reactive({users_listing_profile_segment_tier %>%
                    filter(segment %in% input$segment) %>%
                    filter(tier %in% input$tier) %>%
                    group_by(month, page) %>%
                    summarise(`No. of Users (in millions)` = round((sum(users))/1000000,3)) %>%
                    ungroup()})
  
  output$trf_1d_line <- renderPlotly({
    
    p <-  (ggplot(data(), 
                 aes(x=month, y=`No. of Users (in millions)`, colour = page, group = page)) + 
             
             geom_line(size = 0.5) + geom_point(size = 1.5) +
             scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
             theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
    })
}