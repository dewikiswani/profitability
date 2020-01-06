effects_tab <- argonTabItem(
  tabName = "effects",
  # classic cards
  argonH1("Summary Metadata", display = 4),
  argonRow(
    argonCard(
      width = 12,
      src = NULL,
      icon = icon("cogs"),
      status = "success",
      shadow = TRUE,
      border_level = 2,
      hover_shadow = TRUE,
      title = "Choose",
      argonRow(
        argonColumn(
          width = 12,
          sidebarLayout(
            sidebarPanel(
              fluidRow(
                column(6,
                       selectInput("dataset","Komoditas",choices = c("Rubber","Coffee","Coconut","OilPalm","Chocolate"))
                       #selectInput("wilayah","Wilayah",choices = c("A","B","C"))
                )
                ,
                column(6,
                       sliderInput("srate.p", "Discount Rate Private", 7.8 ,min = 0, max = 15, step = 0.01),
                       sliderInput("srate.s", "Discount Rate Social", 2.8 ,min = 0, max = 8, step = 0.01),
                       sliderInput("slabor.p", "Cost of Labor Private", 50000 ,min = 40000, max = 200000, step = 1000),
                       sliderInput("slabor.s", "Cost of Labor Social", 50000 ,min = 40000, max = 200000, step = 1000),
                       sliderInput("snilai.tukar", "Rupiah Exchange Rate", 10000 ,min = 9000, max = 20000, step = 100),
                       actionButton("s.simulate","Simulate!",icon("paper-plane"),style="color: white; 
                         background-color: green;")
                )
                
                
              )
              
            ),
            mainPanel(
              fluidRow(
                #column(1),
                
                column(5,
                       argonH1("Assumption", display = 4),
                       fluidRow(column(12, verbatimTextOutput("svalue"))),
                       fluidRow(column(12, verbatimTextOutput("svalue.2"))),
                       fluidRow(column(12, verbatimTextOutput("svalue.3"))),
                       
                ),
                column(7,
                       argonH1("Summary", display = 4),
                       fluidRow(column(12, verbatimTextOutput("snpv"))),
                       fluidRow(column(12, verbatimTextOutput("snonlabor.cost"))),
                       fluidRow(column(12, verbatimTextOutput("sestcost.print"))),
                       fluidRow(column(12, verbatimTextOutput("ssummary"))),
                       fluidRow(column(12, verbatimTextOutput("ssummary.2")))
                       
                )
              )
            )
            
            
          )
          
        )
      )
    ),
    br(), br(),
    argonColumn(
      width = 12,
      argonH1("Table", display = 4),
      argonTabSet(
        id = "stab-1",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "sm",
        width = 12,
        iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Table Price",
          active = T,
          dataTableOutput("stabel.1"),
          style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
        ),
        argonTab(
          tabName = "Table Input-Output",
          active = F,
          dataTableOutput("stabel.2"),
          style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
        ),
        argonTab(
          tabName = "Field Data",
          active = F,
          dataTableOutput("tabel.3"),
          style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
        )
      )
    ) 
  )
  
 
  
)