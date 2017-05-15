library(shiny)
library(shinydashboard)
library(DT)

server <- function(input, output, session) {
  output$aparts <- renderText({ 
   "These apartment are specially for you:"
    validate(
      need(input$caption != "", "Please enter your ID")
    )
    my_id <- filter(author, crosspredict.author_id == input$caption)
    recc_user_1 <- recc_predicted@items[[my_id$c.1.968.]]
    recc_user_1 <- data.frame(recc_user_1)
    apart_names = left_join(recc_user_1, apartchms, by = "recc_user_1")
    output$aparts = DT::renderDataTable(apart_names, server = FALSE, filter="top")
  })
}