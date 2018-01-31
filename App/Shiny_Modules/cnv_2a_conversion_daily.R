## cnv_1c_column -- Conversion Unique split by Tier

conversion_dailyUI <- function(id){
  ns <- NS(id)
  tabPanel("a. Daily Conversion", plotlyOutput(ns("cnv_2a_column")))
}

conversion_dailySERVER <- function(input,output,session,country){
  
  conversion_daily <- merge((unique_trans_daily %>%
                                group_by(date,month,country) %>%
                                summarise(transactions = sum(transactions)) %>%
                                ungroup()),
                                (users_channel_daily %>%
                                group_by(date,month,country) %>%
                                summarise(users = sum(users)) %>%
                                ungroup()), by.all = c(date, month, country), all.x = T)
  
  column <- reactive({conversion_daily %>%
      filter(country %in% country()) %>%
      group_by(date) %>%
      summarise(conversion_percentage = round((sum(transactions)*100/sum(users)),2)) %>%
      filter(!is.na(conversion_percentage)) %>%
      ungroup()})
  
  output$cnv_2a_column <- renderPlotly({
    p <- (ggplot() +
            
            geom_bar(data = column(), fill="deepskyblue4",
                     aes(x=date, y=conversion_percentage),
                     stat = "identity") +
            
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
  })
}