analisis <- argonTabItem(
  tabName = "analisis",
  argonH1("ANALISIS SKENARIO", display = 4),
  argonRow(
    argonCard(
      width = 12,
      title = "Summary",
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
                   h1("Assumption & Summary"),
                   br(),
                   h4("area skenario")
            )
          )
        )
        
      )
      
    )
  ),
  
  argonColumn(
    width = 12,
    analisisUI("lokasi")
  )
  
  
)
