deskriptifPlot <- argonTabItem(
  tabName = "deskriptifPlot",
  argonH1("ANALISIS DESKRIPTIF", display = 4),
  argonTabSet(
    id = "tab-deskriptifPlot",
    card_wrapper = TRUE,
    horizontal = TRUE,
    circle = FALSE,
    size = "sm",
    width = 12,
    iconList = lapply(X = 1:2, FUN = argonIcon, name = "atom"),
    argonTab(
      tabName = "Berdasarkan Provinsi",
      tagList(
        argonRow(
            # argonColumn(
            #   width = 12,
            #   fluidRow(
            #     column(4,
            #            selectInput(("provDeskriptif"),
            #                        label="Provinsi dewi",
            #                        choices = sort(unique(indonesia$provinsi)))
            #            # pickerInput(("penelitiDeskriptif"),
            #            #             label="Peneliti:", 
            #            #             #selected = sector[1],
            #            #             choices=c("alfa","beta","gamma"),options = list(`actions-box` = TRUE),multiple = T)
            #     ),
            #     
            #     column(4,
            #            selectInput(("provTahunDeskriptif"),
            #                        label="Tahun",
            #                        choices=c(2020:2021))
            #     ),
            #     column(4,
            #            br(),
            #            actionButton(("provShowDeskriptifHit"),"Tampilkan tabel")
            #     ))),
          argonCard(
            width = 12,
            title = "Filter berdasarkan lokasi dan tahun analisis",
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
                  column(5,
                         selectInput(("provDeskriptif"),
                                     label="Provinsi",
                                     choices = sort(unique(indonesia$provinsi)))
                         # pickerInput(("penelitiDeskriptif"),
                         #             label="Peneliti:", 
                         #             #selected = sector[1],
                         #             choices=c("alfa","beta","gamma"),options = list(`actions-box` = TRUE),multiple = T)
                  ),
                  
                  column(5,
                         selectInput(("provTahunDeskriptif"),
                                     label="Tahun",
                                     choices=c(2020:2030))
                  ),
                  column(2,
                         br(),
                         actionButton(("provShowDeskriptifHit"),"Tampilkan tabel",style="color: white; 
                         background-color: green;")
                  )))))
          ),
              argonColumn(
                width = 12,
                # br(),
                # tags$div(id='teksListPamDefault'),
                # br(),
                tags$div(id = 'uiListPamDefault')
                ),
                
        # argonRow(
        #   argonCard(
        #     width = 12,
        #     title = "Daftar PAM Default",
        #     src = NULL,
        #     hover_lift = TRUE,
        #     shadow = TRUE,
        #     shadow_size = NULL,
        #     hover_shadow = FALSE,
        #     border_level = 0,
        #     icon = argonIcon("atom"),
        #     status = "primary",
        #     background_color = NULL,
        #     gradient = FALSE,
        #     floating = FALSE,
        #     argonRow(
        #       argonColumn(
        #         width = 12,
        #         br(),
        #         h4("area list PAM Default"),
        #         br(),
        #         dataTableOutput("ListPamDefault"))))
        # ),
        # argonRow(
        #   argonCard(
        #     width = 12,
        #     title = "Daftar PAM baru",
        #     src = NULL,
        #     hover_lift = TRUE,
        #     shadow = TRUE,
        #     shadow_size = NULL,
        #     hover_shadow = FALSE,
        #     border_level = 0,
        #     icon = argonIcon("atom"),
        #     status = "primary",
        #     background_color = NULL,
        #     gradient = FALSE,
        #     floating = FALSE,
        #     argonRow(
        #       argonColumn(
        #         width = 12,
        #         br(),
        #         h4("area list PAM baru"))))
        # ),
        # argonRow(
        #   argonCard(
        #     width = 12,
        #     title = "PLOT",
        #     src = NULL,
        #     hover_lift = TRUE,
        #     shadow = TRUE,
        #     shadow_size = NULL,
        #     hover_shadow = FALSE,
        #     border_level = 0,
        #     icon = argonIcon("atom"),
        #     status = "primary",
        #     background_color = NULL,
        #     gradient = FALSE,
        #     floating = FALSE,
        #     argonRow(
        #       argonColumn(
        #         width = 12,
        #         fluidRow(
        #           column(12,
        #                  h1("area PLOT"),
        #                  br(),
        #                  h4("area plot"))))))),
        #dataTableOutput(("tab1")),
        # style = "height:1000px; overflow-y: scroll;overflow-x: scroll;"
        ),
        active = T,
        style = "height:800px; overflow-y: scroll;overflow-x: scroll;" #kalo mau pake scroll ini templatenya
    ),
    argonTab(
      tabName = "Berdasarkan Komoditas",
      active = F,
      tagList(
        argonRow(
          argonCard(
            width = 12,
            title = "Daftar PAM Default",
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
                         pickerInput(("komDeskriptif"),
                                     label="Sistem Usaha Tani",
                                     choices=c("Mono","Agro"),options = list(`actions-box` = TRUE),multiple = T)
                  ),
                  column(3,
                         selectInput(("komTahunDeskriptif"),
                                     label="Tahun",
                                     choices=c(2001:2020))
                  ),
                  column(3,
                         br(),
                         actionButton(("komShowDeskriptifHit"),"Tampilkan tabel")
                  )
                ),
                br(),
                br(),
                tags$div(id = ('komScenListDeskriptifPlaceholder')), #tempat jika di klik tampilkan tabel 
                br(),
                br(),
                h4("area list PAM"))))
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
                         h4("area plot"))))))
        ),
        #dataTableOutput(("tab1")),
        #style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
      ),
      style = "height:800px; overflow-y: scroll;overflow-x: scroll;" #kalo mau pake scroll ini templatenya
    )
  )
  
  
)
