csvDownloadUI <- function(id, label = "Download CSV") {
  ns <- NS(id)
  
  downloadButton(ns("download"), label)
}

## allow users of the module to input a (reactive) data.frame to download as csv and a name for the file
csvDownload <- function(input, output, session, data, filename) {
  filename = paste0(filename, Sys.Date(), ".csv")
  output$download <- downloadHandler(
    filename = function() {
      filename
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
}
