library(shiny)
library(DT)
source("pokemon-api-investigate.R")

ui <- fluidPage(
  titlePanel("Pokemon Market Price Tracking"),
  
  downloadButton("download_csv", "Download CSV"),
  br(), br(),
  
  DTOutput("cards_table")
)

server <- function(input, output) {
  
  output$cards_table <- renderDT({
    datatable(
      cards_df_clean,
      options = list(pageLength = 25, scrollX = TRUE)
    )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("pokemon_cards_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(cards_df_clean, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)