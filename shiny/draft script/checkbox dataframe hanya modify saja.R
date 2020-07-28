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

    
    # vals <- reactiveValues(
    #   Brands=NULL,
    #   Contact=NULL
    # )
    
    output$MainBody<-renderUI({
      fluidPage(
        box(width=12,
            h3(strong("Actions on datatable with buttons"),align="center"),
            hr(),
            column(6,offset = 6,
                   HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
                   actionButton(inputId = "Compare_row_head",label = "Compare selected rows"),
                   actionButton(inputId = "Del_row_head",label = "Delete selected rows"),
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
            tags$script("$(document).on('click', '#Main_table button', function () {
                    Shiny.onInputChange('lastClickId',this.id);
                    Shiny.onInputChange('lastClick', Math.random())
  });")

            
        )
      )
    })
    
    vals<-reactiveValues()
    
    vals$Data<-data.table(
      Brands=paste0("Brand",1:10),
      Contact=paste0("Brand",1:10,"@email.com")
    )
    
    output$Main_table<-renderDataTable({
      # observeEvent(input$provShowDeskriptifHit, {
        # data.table(data.frame(
        # 
        #   vals$Brands <-paste0("Brand",20:30),
        #   vals$Contact <- paste0("Brand",1:10,"@email.com"),
        #   vals$Select <-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(vals$Data),'"><br>')
        # ), escape =F)
      # })

      DT=vals$Data
      DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(vals$Data),'"><br>')
      DT[["Actions"]]<-
        paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
                <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(vals$Data),'>Delete</button>
                <button type="button" class="btn btn-secondary modify"id=modify_',1:nrow(vals$Data),'>Modify</button>
             </div>
             
             ')
      datatable(DT,
                escape=F)
      }
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
    
    observeEvent(input$lastClick,{
                   
                   browser()
                   if (input$lastClickId%like%"delete")
                   {
                     row_to_del=as.numeric(gsub("delete_","",input$lastClickId))
                     vals$Data=vals$Data[-row_to_del]
                   }
                   else if (input$lastClickId%like%"modify")
                   {
                     showModal(modal_modify)
                   }
                 }
    )
    
  })
  
)
runApp(app)