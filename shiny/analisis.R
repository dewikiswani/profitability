analisis <- argonTabItem(
  tabName = "analisis",
  argonH1("ANALISIS SKENARIO", display = 4),
  argonRow(
    argonCard(
      width = 12,
      title = "Kumpulan Skenario Profitability",
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
                   h1("Seluruh skenario yang sudah disimpan terkumpul pada submenu ini."),
                   br(),
                   h4("~~~keterangan~~~")
            )
          )
        )
        
      )
      
    )
  ),
  argonTabSet(
    id = "tab-analisis",
    card_wrapper = TRUE,
    horizontal = TRUE,
    circle = FALSE,
    size = "sm",
    width = 12,
    iconList = lapply(X = 1:2, FUN = argonIcon, name = "atom"),
    argonTab(
      tabName = "Berdasarkan Lokasi",
      analisisUI("lokasi"),
      active = T,
      
    ),
    argonTab(
      tabName = "Berdasarkan Komoditas",
      active = F,
      analisisUI("komoditas"),
      style = "height:600px; overflow-y: scroll;overflow-x: scroll;" #kalo mau pake scroll ini templatenya
    )
  )
  
  
)
