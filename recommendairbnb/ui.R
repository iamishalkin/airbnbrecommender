#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

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
              fluidRow(
                spbapart <- read_csv("~/airbnbrecommender/Data-city/Saint-Petersburg--Russia.csv"),
                mskapart=read_csv("~/airbnbrecommender/Data-city/Moscow--Russia.csv"),
                klgapart=read_csv("~/airbnbrecommender/Data-city/Kaliningrad--Kaliningrad-Oblast--Russia.csv"),
                selectcolumns=function(df){
                  library(dplyr)
                  df=df[,1:77]
                  df=dplyr::filter(df, native_currency=='RUB')
                  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                                         as.factor)
                  df <- df[!duplicated(df$apart_id),]
                  #df <- df[order(df$apart_id),]
                  df=as.data.frame(df)
                  rownames(df)=df$apart_id
                  df=subset(df, select=-c(apart_name, city,description, special_offer,Paid_parking_off_premises, in_building, host_id,lat,lng, native_currency, at_page))
                  #delete inimportatant columns with NA
                  df=subset(df, select=-c(extra_price_native, cleaning_fee_native,square_feet))
                  #df$cleaning_fee_native[-is.na]=mean
                },
                spbapart=selectcolumns(spbapart),
                mskapart=selectcolumns(mskapart),
                klgapart=selectcolumns(klgapart),
                one_city_apart_fix_price=function(apart){
                  apart$fixed_price=apart$price_native- mean(apart$price_native)
                  return(apart)
                },
                mskapart=one_city_apart_fix_price(mskapart),
                spbapart=one_city_apart_fix_price(spbapart),
                klgapart=one_city_apart_fix_price(klgapart),
                aparts=rbind(mskapart, spbapart, klgapart),
                
                datatable(aparts, options = list(searchHighlight = TRUE,
                                              initComplete = JS("
                                                                function(settings, json) {
                                                                $(this.api().table().header()).css({
                                                                'background-color': '#000',
                                                                'color': '#fff'
                                                                });
                                                                }")
                )) %>%
                  formatDate('Departure.Date', 'toLocaleString') %>%
                  formatDate('Arrival.Date', 'toLocaleString') %>%
                  formatStyle('Cruise.Ship', target = 'row', backgroundColor = styleEqual(c("MSC MUSICA", "Regal Princess"), c('gray', 'yellow'))
                  )
                                              )
                )
              ),
    
    # Second tab content
    tabItem(leafletOutput("map", width="100%", height="100%")
            )
            
    )
    )