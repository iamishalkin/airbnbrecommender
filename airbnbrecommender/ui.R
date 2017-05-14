library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(dropdownMenu(type = "notifications",
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
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(column(6, textInput("caption", "Пожалуйста, введите свой ID:", ""), actionButton("submit", label = "submit"), textInput("caption", "Если вы не знаете ID, введите логин и пароль:", "login"), textInput("caption", "", "password"), actionButton("submit", label = "submit")))
      )
    )
  )
)