## tra_2a_line -- Cumulative Transaction daily run-rate

trans_dailyUI <- function(id){
  ns <- NS(id)
  tabPanel("a. Cumulative Daily Run Rate", plotlyOutput(ns("tra_2a_line")))
}

trans_daily <- function(input,output,session,country){
  
  data <- reactive({melt(as.data.frame(transactions_run_rate %>%
                               filter(country %in% country()) %>%
                               group_by(day, month_day, country) %>%
                               summarise(sum_book = sum(book), sum_vn = sum(vn), sum = sum(tranx)) %>%
                               ungroup() %>%
                               group_by(day, month_day) %>%
                               summarise(sum2_book = sum(sum_book), sum2_vn = sum(sum_vn), sum2 = sum(sum)) %>%
                               ungroup() %>%
                               group_by(month_day) %>%
                               mutate(cumulative_book_practo = (round((cumsum(sum2_book)/1000),2)),
                                      cumulative_vn_call_attempts = (round((cumsum(sum2_vn)/1000),2)),
                                      cumulative_transactions = (round((cumsum(sum2)/1000),2))) %>%
                               ungroup() %>%
                               select(day, month_day, cumulative_book_practo, cumulative_vn_call_attempts, cumulative_transactions) %>%
                               ungroup()),
               id = c("day", "month_day"),
               measure.vars = c("cumulative_book_practo", "cumulative_vn_call_attempts", "cumulative_transactions"),
               variable_name = "mode")})
  
  
  output$tra_2a_line <- renderPlotly({

    p <- (ggplot() +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +

            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  strip.text.x = element_text(size = 12, colour = "dark blue", angle = 0),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +

            geom_line(data = data(),
            aes(x=day, y=value, colour = month_day, group = month_day),
            size = 0.5) +

            geom_point(data = data(),
            aes(x=day, y=value, colour = month_day, group = month_day),
            size = 1.5))

    ggplotly((p + facet_grid(. ~ mode)), width = 900, height = 400, tooltip = c("x", "y"))

  })
}