# app.R - This is the main application file that calls the UI and server
# It's used to launch the app when using runApp() or deploying to ShinyServer/RStudio Connect

# Source the global.R file
source("global.R")

# Source the UI and server files
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)