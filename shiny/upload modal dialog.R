upload <- argonTabItem(
  tabName = "upload",
  argonH1("PROFITABILITY", display = 4),
  argonRow(
    argonCard(
      width = 12,
      title = "Informasi Umum, Asumsi Makro, & Unggah File",
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
              generalUI("profit")
            ),
            mainPanel(
              buttonUI("profit")
            )
          )
        )
      )
    )
  ),
  argonRow(
    argonCard(
      width = 12,
      title = "Hasil Analisis",
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
          fluidRow(
            column(10,
                   h1("Hasil Analisis"),
                   br(),
                   resultUI("profit")
                   # fluidRow(column(12, verbatimTextOutput("value"))),
                   # fluidRow(column(12, verbatimTextOutput("value.2"))),
                   # br(),
                   # fluidRow(column(12, verbatimTextOutput("npv"))),
                   # fluidRow(column(12, verbatimTextOutput("nlc"))),
                   # fluidRow(column(12, verbatimTextOutput("ec"))),
                   # fluidRow(column(12, verbatimTextOutput("hp"))),
                   # fluidRow(column(12, verbatimTextOutput("lr")))
            )
          )
        )
        
      )
 
    )
  )
  ,
  argonColumn(
    width = 12,
    argonH1("Tabel", display = 4),
    viewTableUI("profit")
  )
  
  
)
