pamTemplate <- argonTabItem(
  tabName = "template",
  argonH1("Membangun PAM dari data template", display = 4),
  argonRow(
    argonCard(
      width = 12,
      title = "Membangun PAM dari data template",
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
          argonH1("Informasi Umum", display = 4),
          h5("Langkah 1: menentukan informasi umum untuk data PAM yang dibangun"),
          br(),
          fluidRow(
            column(2,
                   selectInput(("sut"),"Sistem Usaha Tani",choices = c("MONOKULTUR","AGROFORESTRI")),
            ),
            column(2,
                     selectInput("kom","Komoditas",choices = "" ),
            ),
            column(2,
                   selectInput("selected_provinsi",
                               "Pilih Wilayah:",
                               choices = ""),
            ),
            column(2,
                   selectInput(("th"),"Tahun",choices = "" ),
            ),
            column(2,
                   selectInput(("tipeLahan"),"Tipe Lahan",choices = "" ),
                   
            ),
            column(2,
                   br(),
                   actionButton(("asumsiMakro_button"),"Tentukan Asumsi Makro",icon("paper-plane"),style="color: white; 
                         background-color: green;") 
                   
            )
          )
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowMakro')

        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowTable')
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowButton')
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowResult')
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          actionButton("browser_button","Browser Button")
          
        )
      ),

    )
  )
)
