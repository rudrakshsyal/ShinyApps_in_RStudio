## trf_1c_line -- Traffic split by Segment-Device

users_segmentdeviceUI <- function(id){
  # attach("csv_data.RData")
  ns <- NS(id)
  tabPanel("c. by Segment-Device",
           
           column(6, checkboxGroupButtons(
             inputId = ns("segment"), label = "Select Segment: ", 
             choices = unique(users_listing_profile_segment_device$segment), 
             selected = c("doctor"),
             justified = T, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           column(6, checkboxGroupButtons(
             inputId = ns("device"), label = "Select Device: ", 
             choices = unique(users_listing_profile_segment_device$device), 
             selected = "mobile",
             justified = T, 
             size = "sm",
             status = "danger",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
          column(12, plotlyOutput(ns("trf_1c_line")))
  )
}

users_segmentdeviceSERVER <- function(input,output,session,country){
  
  data <- reactive({users_listing_profile_segment_device %>%
                    filter(segment %in% input$segment) %>%
                    filter(device %in% input$device) %>%
                    group_by(month, page) %>%
                    summarise(`No. of Users (in millions)` = round((sum(users))/1000000,3)) %>%
                    ungroup()})
                
  output$trf_1c_line <- renderPlotly({
    
    p <-  (ggplot(data(),
                  aes(x=month, y=`No. of Users (in millions)`, colour = page, group = page)) + 
             
             geom_line(size = 0.5) + geom_point(size = 1.5) +
             scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
             theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
  })
}