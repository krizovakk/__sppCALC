
library(shiny)


# ---------------------------------------------------- UI

ui <- fluidPage(
  
  # theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("Čus čus"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", 
                label = "", 
                buttonLabel = "Nahraj diagram",
                placeholder = "",
                accept = c(".xls", ".xlsx")),
      tableOutput("files")
    ),
    
    mainPanel(
      
      # plotOutput("distPlot"), # distPlot ?
      actionButton("run", "Process Data"),
    
    downloadButton("download"))
    
  ))

# ---------------------------------------------------- SERVER

server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)
  })
  
  output$head <- renderTable({
    head(data(), input$n)
  })
}

# ---------------------------------------------------- APP

shinyApp(ui, server)


