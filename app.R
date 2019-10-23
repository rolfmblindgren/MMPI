source("init.R", local=TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem(tabName = "home", text = "Home", icon = icon("home")),
    menuItem(tabName = "Results", text = "The Table", icon = icon("heart")),
    menuItem(tabName = "Translation", text = "The Document", icon = icon("file-word"))
  )),
  dashboardBody(
    tabItems(
      tabItem( 
        tabName="home", 
        fluidRow(
          box(fileInput("file1", "Choose CSV File",
                        accept = c(
                          "text/csv",
                          "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                          "text/comma-separated-values,text/plain",
                          ".csv")),
              tags$hr(),
              checkboxInput("header", "Header", TRUE))
        )),
      tabItem(
        tabName="Results",
        fluidRow(
          box(DT::dataTableOutput("table"),
              tags$hr(),
              checkboxInput("terse", "Terse", FALSE),downloadButton("report", "Generate report"))
        )),
      tabItem(
        tabName="Translation",
        fluidRow(
          box(downloadLink("downloadData", "Download"))
        ))
    )
  )
)

server <- function(input, output) {

  output$report <- downloadHandler(
    filename = function(){
      paste("data-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {

      source("process.R",local=TRUE)

      print(xtable(res,align=c("l","p{4cm}","p{4cm}")),comment=FALSE,booktabs=TRUE,floating=FALSE,file=file)
    }
  ) 
  
  output$terse <- renderText({ input$terse })

#  output$downloadData <-  downloadHandler(
#    filename <- function() {
#      paste("data-", Sys.Date(), ".docx", sep="")
#    },
#    content = function(file) {
#      "<p>Hei</p>"
#    }
#  )

  output$table <- DT::renderDataTable({
    if ( is.character(input$file1$datapath)
                      && file_ext(input$file1$datapath)=="csv") {
      
      source("process.R",local=TRUE)
      
      mmpi <- DT::datatable(res)             
      
    } else {
      NULL
    }
  })
}

shinyApp(ui, server)
