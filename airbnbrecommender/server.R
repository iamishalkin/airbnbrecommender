library(shiny)
library(shinydashboard)

server <- function(input, output, session) {
  recc_predicted <- predict(object = recc_model, newdata = r.test, n = 6)
}