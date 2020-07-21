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
      #analisisUI("lokasi"),
      tagList(
        argonRow(
          argonCard(
            width = 12,
            title = "Daftar Skenario",
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
                  column(3,
                         pickerInput(("penelitiAnalisis"),
                                     label="Peneliti:", 
                                     #selected = sector[1],
                                     choices=c("alfa","beta","gamma"),options = list(`actions-box` = TRUE),multiple = T)
                  ),
                  column(3,
                         pickerInput(("sutAnalisis"),
                                     label="Sistem Usaha Tani",
                                     choices=c("Mono","Agro"),options = list(`actions-box` = TRUE),multiple = T)
                  ),
                  column(3,
                         selectInput(("tahunAnalisis"),
                                     label="Tahun Skenario",
                                     choices=c(2001:2020))
                  ),
                  column(3,
                         br(),
                         actionButton(("showAnalisisHit"),"Tampilkan tabel")
                  )
                ),
                br(),
                br(),
                tags$div(id = ('scenListAnalisisPlaceholder')), #tempat jika di klik tampilkan tabel 
                br(),
                br(),
                h4("area list skenario")
              )
            )
          )
        ),
        argonRow(
          argonCard(
            width = 12,
            title = "PLOT",
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
                         h1("area PLOT"),
                         br(),
                         h4("area plot")
                  )
                )
              )
            )
          )
        ),
        #dataTableOutput(("tab1")),
        #style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
      ),
      active = T,
      
    ),
    argonTab(
      tabName = "Berdasarkan Komoditas",
      active = F,
      #analisisUI("komoditas"),
      tagList(
        argonRow(
          argonCard(
            width = 12,
            title = "Daftar Skenario",
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
                  column(3,
                         pickerInput(("penelitiAnalisis"),
                                     label="Peneliti:", 
                                     #selected = sector[1],
                                     choices=c("alfa","beta","gamma"),options = list(`actions-box` = TRUE),multiple = T)
                  ),
                  column(3,
                         pickerInput(("sutAnalisis"),
                                     label="Sistem Usaha Tani",
                                     choices=c("Mono","Agro"),options = list(`actions-box` = TRUE),multiple = T)
                  ),
                  column(3,
                         selectInput(("tahunAnalisis"),
                                     label="Tahun Skenario",
                                     choices=c(2001:2020))
                  ),
                  column(3,
                         br(),
                         actionButton(("showAnalisisHit"),"Tampilkan tabel")
                  )
                ),
                br(),
                br(),
                tags$div(id = ('scenListAnalisisPlaceholder')), #tempat jika di klik tampilkan tabel 
                br(),
                br(),
                h4("area list skenario")
              )
            )
          )
        ),
        argonRow(
          argonCard(
            width = 12,
            title = "PLOT",
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
                         h1("area PLOT"),
                         br(),
                         h4("area plot")
                  )
                )
              )
            )
          )
        ),
        #dataTableOutput(("tab1")),
        #style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
      ),
      style = "height:600px; overflow-y: scroll;overflow-x: scroll;" #kalo mau pake scroll ini templatenya
    )
  )
  
  
)
