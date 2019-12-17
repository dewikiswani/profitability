alerts_tab <- argonTabItem(
  tabName = "alerts",
  argonH1("Upload File", display = 4),
  argonRow(
    argonCard(
      width = 12,
      title = "Assumption & Summary",
      src = NULL,
      hover_lift = TRUE,
      shadow = TRUE,
      shadow_size = NULL,
      hover_shadow = FALSE,
      border_level = 0,
      icon = argonIcon("atom"),
      status = "primary",
      background_color = NULL,
      gradient = FALSE, 
      floating = FALSE,
      
      argonRow(
        argonColumn(
          width = 12,
          sidebarLayout(
            sidebarPanel(
              fluidRow(
                column(6,
                       fileInput("file.1", "Price Tradable"),
                       fileInput("file.2", "Price Non-Tradable"),
                       fileInput("file.3", "Price Labour"),
                       fileInput("file.4", "Price Capital"),
                       fileInput("file.5", "Price Output"),
                       actionButton("simulate","Simulate!",icon("paper-plane"),style="color: white; 
                         background-color: green;")
                ),
                column(6,
                       fileInput("file.6", "I-O Tradable"),
                       fileInput("file.7", "I-O Non-Tradable"),
                       fileInput("file.8", "I-O Labour"),
                       fileInput("file.9", "I-O Capital"),
                       fileInput("file.10", "I-O Output")
                )
                
                
              )
            ),
            mainPanel(
              fluidRow(
                column(3,
                       sliderInput("rate.p", "Discount Rate Private", 7.8 ,min = 0, max = 15, step = 0.01),
                       sliderInput("rate.s", "Discount Rate Social", 2.8 ,min = 0, max = 8, step = 0.01),
                       sliderInput("labor.p", "Cost of Labor Private", 50000 ,min = 40000, max = 200000, step = 1000),
                       sliderInput("labor.s", "Cost of Labor Social", 50000 ,min = 40000, max = 200000, step = 1000),
                       sliderInput("nilai.tukar", "Rupiah Exchange Rate", 10000 ,min = 9000, max = 20000, step = 100)
                ),
                column(1),
                
                column(8,
                       argonH1("Assumption", display = 4),
                       #h4("Assumption & Summary"),
                       fluidRow(column(12, verbatimTextOutput("value"))),
                       fluidRow(column(12, verbatimTextOutput("value.2"))),
                       argonH1("Summary", display = 4),
                       #argonH1(verbatimTextOutput("npv"), display = 4),
                       #argonH1(verbatimTextOutput("nonlabor.cost"), display = 4),
                       #argonH1(verbatimTextOutput("estcost.print"), display = 4),
                       #argonH1(verbatimTextOutput("summary"), display = 4),
                       #argonH1(verbatimTextOutput("summary.2"), display = 4)
                       fluidRow(column(12, verbatimTextOutput("npv"))),
                       fluidRow(column(12, verbatimTextOutput("nonlabor.cost"))),
                       fluidRow(column(12, verbatimTextOutput("estcost.print"))),
                       fluidRow(column(12, verbatimTextOutput("summary"))),
                       fluidRow(column(12, verbatimTextOutput("summary.2")))
                )
                
                
                
                )
                
              )
              
            )
          )
          
          
        )
      )
    ),
    argonColumn(
      width = 12,
      argonH1("Table", display = 4),
      argonTabSet(
        id = "tab-1",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "sm",
        width = 12,
        iconList = lapply(X = 1:2, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Table Price",
          active = FALSE,
          dataTableOutput("tabel.1")
        ),
        argonTab(
          tabName = "Table Input-Output",
          active = TRUE,
          dataTableOutput("tabel.2")
        )
      )
    )

    
    )
