library(shiny)

# ---------------------------------------------------- UI

ui <- fluidPage(
  "Hello, world!"
)

# ---------------------------------------------------- SERVER

server <- function(input, output, session) {
}

# ---------------------------------------------------- APP

shinyApp(ui, server)
