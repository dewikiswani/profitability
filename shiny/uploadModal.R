uploadModal <- argonTabItem(
  tabName = "uploadModal",
  argonH1("PROFITABILITY", display = 4),
  argonRow(
    argonCard(
      width = 12,
      title = "Unggah File, Variabel Input, Asumsi & Ringkasan Data",
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
          tagList(
            # actionButton(("modalDefineButton"),'Deskripsi Skenario'),
            # tags$br(),
            # tags$br(),
            # dataTableOutput(("ListTable")),
            # uiOutput(("daftarDefineShow")),
            # tags$div(id = ('scenarioResultPlaceholder'))
            # tabPanel("wowo", tags$br(), buttonUI("forEnergy"))
            buttonUI("forEnergy")
          )
        )
        
      )
    )
  )
)
