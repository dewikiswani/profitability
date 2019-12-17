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
                column(12,
                       sliderInput("srate.p", "Discount Rate Private", 7.8 ,min = 0, max = 15, step = 0.01),
                       sliderInput("srate.s", "Discount Rate Social", 2.8 ,min = 0, max = 8, step = 0.01),
                       sliderInput("slabor.p", "Cost of Labor Private", 50000 ,min = 40000, max = 200000, step = 1000),
                       sliderInput("slabor.s", "Cost of Labor Social", 50000 ,min = 40000, max = 200000, step = 1000),
                       sliderInput("snilai.tukar", "Rupiah Exchange Rate", 10000 ,min = 9000, max = 20000, step = 100),
                       actionButton("s.simulate","Simulate!",icon("paper-plane"),style="color: white; 
                         background-color: green;")
                )
                #,
                #column(1
                       #fileInput("file.6", "I-O Tradable"),
                       #fileInput("file.7", "I-O Non-Tradable"),
                       #fileInput("file.8", "I-O Labour"),
                       #fileInput("file.9", "I-O Capital"),
                       #fileInput("file.10", "I-O Output")
                
                #)
                
                
              )
              
            ),
            mainPanel(
              fluidRow(
                #column(1),
                
                column(8,
                       argonH1("Assumption", display = 4),
                       #fluidRow(column(12, verbatimTextOutput("svalue"))),
                       #fluidRow(column(12, verbatimTextOutput("svalue.2"))),
                       argonH1("Summary", display = 4),
                       fluidRow(column(12, verbatimTextOutput("snpv"))),
                       fluidRow(column(12, verbatimTextOutput("snonlabor.cost"))),
                       #fluidRow(column(12, verbatimTextOutput("sestcost.print"))),
                       #fluidRow(column(12, verbatimTextOutput("ssummary"))),
                       #fluidRow(column(12, verbatimTextOutput("ssummary.2")))
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
        iconList = lapply(X = 1:2, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Table Price",
          active = FALSE,
          dataTableOutput("stabel.1")
        ),
        argonTab(
          tabName = "Table Input-Output",
          active = TRUE,
          dataTableOutput("stabel.2")
        )
      )
    ),
    argonCard(
      width = 12,
      title = "Argon Card",
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
          width = 6,
          radioButtons(
            "dist", 
            "Distribution type:",
            c("Normal" = "norm",
              "Uniform" = "unif",
              "Log-normal" = "lnorm",
              "Exponential" = "exp")
          )
        ),
        argonColumn(width = 6, plotOutput("plot"))
      )
    ) 
  )
  ,
  br(),
 
  
)