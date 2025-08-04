source('R/data.R')
source("R/ui.R")
source("R/server.R")
source("R/utils.R")

options(
  shiny.launcher.browser = T,
  shiny.host = '127.0.0.1',
  shiny.port = 5858
)

invisible(root$read())

shinyApp(ui = ui(root), server = server(root))
