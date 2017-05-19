library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(
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
    tags$h1("Airbnb11"),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(column(6, textInput("caption", "Пожалуйста, введите свой ID:", ""), actionButton("submit", label = "submit"), textInput("caption", "Если вы не знаете ID, введите логин и пароль:", "login"), textInput("caption", "", "password"), actionButton("submit", label = "submit"), DT::dataTableOutput('aparts')))
      )
    )
  )
)





