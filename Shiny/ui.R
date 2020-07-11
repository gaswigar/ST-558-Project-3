#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "Project 3"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Information", tabName = "info", icon = icon("dashboard")),
            menuItem("Exploration", tabName = "expo", icon = icon("th")),
            menuItem("Clustering/PCA", tabName = "clust", icon = icon("th")),
            menuItem("Modeling", tabName = "model", icon = icon("th")),
            menuItem("Data", tabName = "data", icon = icon("th"))
        )
    ),

    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "info",
                    fluidRow(
                        box(plotOutput("plot1", height = 250)),
                        
                        box(
                            title = "Controls",
                            sliderInput("slider", "Number of observations:", 1, 100, 50)
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "expo",
                    h2("Exploring Data")
            ),
            
            # Third tab content
            tabItem(tabName = "clust",
                    h2("Clustering Analysis")
            ),
            
            # Fourth tab content
            tabItem(tabName = "model",
                    h2("Modelling")
            ),
            
            # Fifth tab content
            tabItem(tabName = "data",
                    h2("View Data")
            )
        )
    )
)