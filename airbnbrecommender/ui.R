library(shiny)
library(shinydashboard)

dashboardPage( skin = "red",
  dashboardHeader(title= "Airbnb Recommender",
    dropdownMenu(
      type = "notifications",
                               notificationItem(
                                 text = "5 new users today",
                                 icon("users")
                               ),
                               notificationItem(
                                 text = "12 items delivered",
                                 icon("truck"),
                                 status = "success"
                               ),
                               notificationItem(
                                 text = "Server load at 86%",
                                 icon = icon("exclamation-triangle"),
                                 status = "warning"
                               )
  )),
  dashboardSidebar( 
    tags$img(height = 85, 
             width = 100,
             src = "logo.png"),
    tags$style(HTML("
      @import url('//bootswatch.com/journal/bootstrap.css');
      
      h1 {
        font-family: 'Lobster';
        font-weight: 500;
        line-height: 1.1;
        color: #fd5c63;
      }

    ")),
    tags$h1("Airbnb"),
    
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    ))
  ,
  dashboardBody( 
    tabItems(
      # First tab content
      tabItem(
        tabName = "dashboard",
              fluidRow(column (6, 
                              tags$br(), textInput("caption","Пожалуйста, введите свой ID:", ""), 
                              actionButton("summary", icon=icon("submit"), class="btn btn-primary", label = "submit"),
                              p(textInput("caption", "Если вы не знаете ID, введите логин и пароль:", "login"), 
                 textInput("caption", "", "password"), 
                              actionButton("submit", icon=icon("submit"), class="btn btn-primary", label = "submit")), 
                                DT::dataTableOutput('aparts'))
      ))
    )
  ))

  

  





