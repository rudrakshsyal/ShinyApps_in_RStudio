## gmv_1b_combo -- VPE Overall

vpe_buUI <- function(id){
  ns <- NS(id)
  tabPanel("b. VPE", 
           column(3, checkboxGroupButtons(
             inputId = ns("bu"), label = "Select BU: ", 
             choices = unique(vpe_gmv_dch$bu), 
             selected = c('Enterprise','SMB'),
             justified = F, 
             size = "sm",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           column(12,plotlyOutput(ns("gmv_1b_combo"))))
}

vpe_buSERVER <- function(input,output,session,country){

  output$gmv_1b_combo <- renderPlotly({
    
    p <- ggplot() + 
      
      geom_bar(data = (vpe_gmv_dch %>% group_by(month,bu) %>% summarise(vpe = sum(gmv)/sum(transactions))),
                 aes(x=month, y=vpe, fill=bu), width = 10, stat="identity", position="dodge") +
      
      geom_line(data = (vpe_gmv_dch %>% group_by(month) %>% summarise(vpe = sum(gmv)/sum(transactions))),
                aes(x=month, y=vpe), size=0.5) +
      
      geom_point(data = (vpe_gmv_dch %>% group_by(month) %>% summarise(vpe = sum(gmv)/sum(transactions))),
                 aes(x=month, y=vpe), size=1.5) +
      
      labs(x='Month', y = "VPE")
    
    ggplotly(p, width = 900, height = 400, tooltip = c("x", "y"))
      
})
}