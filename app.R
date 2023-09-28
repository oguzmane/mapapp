library(rsconnect)
library(shiny)
library(readxl)
library(tidyverse)
library(reactable)

cen_select <- suppressWarnings(read.csv("cen_select.csv"))

ui <- fluidPage(
  titlePanel("England & Wales - Demographic Statistics"),
  tags$br(),
  tags$br(),
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Loading...",id="loadmessage")),
      uiOutput("selectYear"),
      uiOutput("selectCat"),
      uiOutput("selectArea"),
      uiOutput("selectLoc")
    ),
    mainPanel(
      reactableOutput("table"),
      tags$br(),
      tags$br(),
      downloadButton("downloadData", "Download")
    )
  )
  
)

server <- function(input, output) {
  
  output$selectYear <- renderUI({
    selectInput("year",
                "Census",
                choices = c("",unique(cen_select$year))
    )
  })
  
  output$selectCat <- renderUI({
    selectInput(
      inputId = "cat",
      label = "Category",
      choices = c("",unique(cen_select$cat[cen_select$year==input$year]))
    )
  })
  
  output$selectArea <- renderUI({
    selectInput(
      inputId="area",
      label="Area",
      selected = input$area,
      choices = c("","Region","County","District")
        # c("",unique(cen_select$area[cen_select$year==input$year & cen_select$cat==input$cat]))
    )
  })
  
  
  output$selectLoc<- renderUI({
    validate(need(input$year, ''))
    validate(need(input$cat, ''))
    validate(need(input$area, ''))
    selectInput(
      inputId="loc",
      label="Locations",
      selected = input$loc,
      choices = sort(unique(df_readFUN(input$year,input$cat,input$area)$area)),
        # unique(df_readFUN(input$year,input$cat,input$area)$area),
      multiple=T
    )
  })
  
  output$table <- renderReactable({

    validate(need(input$year, 'Please select a year, category, and area'))
    validate(need(input$cat, 'Please select a year, category, and area'))
    validate(need(input$area, 'Please select a year, category, and area'))

    tableFUN(input$year,input$cat,input$area,input$loc)

  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("cen",input$year,"_",input$cat,"_",input$area,".csv")
    },
    content = function(file) {
      write.csv(writeFUN(input$year,input$cat,input$area,input$loc), file)
    }
  )

}


shinyApp(ui = ui, server = server)








