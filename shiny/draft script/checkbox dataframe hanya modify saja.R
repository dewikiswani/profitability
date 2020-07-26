library(shiny)
library(shinydashboard)
library(data.table)
library(DT)


app <- shiny::shinyApp(
  ui <- dashboardPage(dashboardHeader(disable = T),
                      dashboardSidebar(disable = T),
                      dashboardBody(uiOutput("MainBody")
                                    
                      )
                      
  )
  
  
  ,
  server = shinyServer(function(input, output) {
    vals<-reactiveValues()
    
    vals$Data<-data.table(
      Brands=paste0("Brand",1:10),
      Contact=paste0("Brand",1:10,"@email.com")
    )
    
    output$MainBody<-renderUI({
      fluidPage(
        box(width=12,
            h3(strong("Actions on datatable with buttons"),align="center"),
            hr(),
            column(6,offset = 6,
                   HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
                   actionButton(inputId = "Compare_row_head",label = "Compare selected rows"),
                   HTML('</div>')
            ),
            
            column(12,dataTableOutput("Main_table")),
            tags$script(HTML('$(document).on("click", "input", function () {
  var checkboxes = document.getElementsByName("row_selected");
  var checkboxesChecked = [];
  for (var i=0; i<checkboxes.length; i++) {
     if (checkboxes[i].checked) {
        checkboxesChecked.push(checkboxes[i].value);
     }
  }
  Shiny.onInputChange("checked_rows",checkboxesChecked);
      })')),

            
        )
      )
    })
    
    output$Main_table<-renderDataTable({
      DT=vals$Data
      DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(vals$Data),'"><br>')
      datatable(DT,
                escape=F)}
    )
    
    ###Brand visualisation 
    observeEvent(input$Compare_row_head,{
      # browser()
      row_to_del=as.numeric(gsub("Row","",input$checked_rows))
      number_brands=length(row_to_del)
      vals$fake_sales=data.table(
        month=rep(1:12,number_brands),
        sales=round(rnorm(12*number_brands,1000,1000)^2)/12,
        Brands=rep(vals$Data[row_to_del,Brands],each=12)
      )
      vals$fake_sales[,Brands:=as.factor(Brands)]
      showModal(fake_sales_modal)
    }
    )
    
    
    fake_sales_modal<-modalDialog(
      fluidPage(
        h3(strong("Monthly sales of selected brands"),align="center"),
        plotOutput('sales_plot')
      ),
      size="l"
    )
    
    output$sales_plot<-renderPlot({
      require(ggplot2)
      ggplot(vals$fake_sales,aes(x=month,y=sales,color=Brands))+geom_line()
    })
    
  })
  
)
runApp(app)