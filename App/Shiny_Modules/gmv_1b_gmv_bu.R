## gmv_1b_combo -- GMV/VPE by Business Unit

gmv_buUI <- function(id){
  ns <- NS(id)
  tabPanel("b. by Business Unit", 
           
           column(6, checkboxGroupButtons(
             inputId = ns("bu"), label = "Please select the Business Unit: ", 
             choices = c("Clinic" = 'clinic', "Mid - Market" = 'mm', "Strategic - Accounts" = 'sa'),
             selected = "sa",
             justified = T, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )
           ),
           
           column(12, plotOutput(ns("gmv_1b_combo"))))
}

gmv_bu <- function(input,output,session,country){
  output$gmv_1b_combo <- renderPlot({
    
    p1 <- ggplot(data = 
                   (segregated_gmv %>%
                      filter(bu %in% input$bu) %>%
                      group_by(month) %>%
                      summarise(segregated_gmv = round((sum(value))/10000000,2)) %>%
                      select(month, segregated_gmv) %>%
                      ungroup()),
                 aes(x=month, y=segregated_gmv)) + 
      
      geom_bar(width = 10, stat="identity", fill = "deepskyblue4") +
      
      geom_text(size=4, vjust = -1, aes(label = segregated_gmv)) + 
      
      labs(y = "Segregated GMV (in crores)") + 
      
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.position = "bottom",
            legend.justification = "center",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            text = element_text(family = "Palatino Linotype"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank()) +
      
      theme_gdocs() + scale_fill_economist() + scale_colour_hue()
    
    p2 <- ggplot() + 
      
      
      geom_line(colour = "maroon", 
                data = 
                  (segregated_vpe %>%
                     filter(bu %in% input$bu) %>%
                     group_by(month, trans) %>%
                     summarise(multi = ((vpe*trans))) %>%
                     ungroup() %>%
                     group_by(month) %>%
                     summarise(sum_trans = sum(trans),
                               sum_multi = sum(multi)) %>%
                     ungroup() %>%
                     group_by(month) %>%
                     summarise(segregated_vpe = (sum_multi/sum_trans)) %>%
                     select(month, segregated_vpe) %>%
                     ungroup()), 
                
                aes(x=month, y=segregated_vpe), size = 1) + theme_bw() %+replace% theme(panel.background = element_rect(fill = NA),
                                                                                        panel.grid.major = element_blank(), 
                                                                                        panel.grid.minor = element_blank(), 
                                                                                        panel.border = element_blank()) +
      scale_y_continuous(limits = c(1000,10000)) +
      
      geom_point(colour = "red",
                 data = 
                   (segregated_vpe %>%
                      filter(bu %in% input$bu) %>%
                      group_by(month, trans) %>%
                      summarise(multi = ((vpe*trans))) %>%
                      ungroup() %>%
                      group_by(month) %>%
                      summarise(sum_trans = sum(trans),
                                sum_multi = sum(multi)) %>%
                      ungroup() %>%
                      group_by(month) %>%
                      summarise(segregated_vpe = (sum_multi/sum_trans)) %>%
                      select(month, segregated_vpe) %>%
                      ungroup()), 
                 
                 aes(x=month, y=segregated_vpe), size = 3) + theme_bw() %+replace% theme(panel.background = element_rect(fill = NA),
                                                                                         panel.grid.major = element_blank(), 
                                                                                         panel.grid.minor = element_blank(), 
                                                                                         panel.border = element_blank())
    
    # extract gtable
    g1 <- ggplot_gtable(ggplot_build(p1))
    g2 <- ggplot_gtable(ggplot_build(p2))
    
    # overlap the panel of 2nd plot on that of 1st plot
    pp <- c(subset(g1$layout, name == "panel", se = t:r))
    g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                         pp$l, pp$b, pp$l)
    
    # axis tweaks
    ia <- which(g2$layout$name == "axis-l")
    ga <- g2$grobs[[ia]]
    ax <- ga$children[[2]]
    ax$widths <- rev(ax$widths)
    ax$grobs <- rev(ax$grobs)
    ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
    g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
    
    # draw it
    grid.draw(g)
    # ggplotly(g, width = 500, height = 400, tooltip = c("x", "y"))
    
  },
  height = 400,
  width = 1000
  )
}