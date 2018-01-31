## gmv_1a_combo -- GMV Overall

gmv_buUI <- function(id){
  ns <- NS(id)
  tabPanel("a. GMV", 
           column(3, checkboxGroupButtons(
             inputId = ns("bu"), label = "Select BU: ", 
             choices = unique(vpe_gmv_dch$bu), 
             selected = c('Enterprise','SMB'),
             justified = F, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           column(12,plotlyOutput(ns("gmv_1a_combo"))))
}

gmv_buSERVER <- function(input,output,session,country){

  output$gmv_1a_combo <- renderPlotly({
    
    p <- ggplot() + 
      
      geom_bar(data = (vpe_gmv_dch %>% group_by(month) %>% summarise(gmv=sum(gmv)/10000000)),
                 aes(x=month, y=gmv), width = 10, stat="identity", fill = "deepskyblue4") +
      
      geom_line(data = vpe_gmv_dch %>% group_by(month, bu) %>% summarise(gmv=sum(gmv)/10000000),
                aes(x=month, y=gmv, group=bu, colour=bu), size=0.5) +
      
      geom_point(data = vpe_gmv_dch %>% group_by(month, bu) %>% summarise(gmv=sum(gmv)/10000000),
                aes(x=month, y=gmv, group=bu, colour=bu), size=1.5) +
      
      labs(x='Month', y = "GMV (in crores)")
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
      
})
}