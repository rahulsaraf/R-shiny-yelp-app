library(shiny)

shinyUI(fluidPage(
  titlePanel("Yelp Dataset Challenge"),
  sidebarLayout(
    sidebarPanel(
      h2("Business Details"),
      p("Here, details about the business are given."),
      br(),
      textInput("bussnm", "Business Name", "Gab & Eat"),      
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      
      submitButton("Update View")
    ),
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Business Details", h1("Business Details"),tableOutput("bussview")),
                  tabPanel("WordCloud", h1("WordCloud"),p("Word Cloud contains all the nouns in user tips"),imageOutput("myImage")),
                  tabPanel("Table", h1("Observations"),tableOutput("view")),
                  tabPanel("Tip Table", h1("Tips"),tableOutput("tipview"))
      )
      
      #h1("WordCloud"),
      #p("Word Cloud contains all the nouns in user tips"),
      #imageOutput("myImage"),
      #h1("Observations"),
      #tableOutput("view")
    )
  )
))