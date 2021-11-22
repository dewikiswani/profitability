pamParsial <- argonTabItem(
  tabName = "pamParsial",
  argonH1("Simulasi Parsial Data Template", display = 4),
  argonRow(
    argonCard(
      width = 12,
      title = "Simulasi Parsial Data Template",
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
                   selectInput(("sut_par"),"Sistem Usaha Tani",choices = c("MONOKULTUR","AGROFORESTRI")),
            ),
            column(2,
                   selectInput("kom_par","Komoditas",choices = "" ),
            ),
            column(2,
                   selectInput("selected_prov_par",
                               "Pilih Provinsi:",
                               choices = ""),
            ),
            column(2,
                   selectInput("selected_wilayah_par",
                               "Pilih Wilayah:",
                               choices = ""),
            ),
            column(1,
                   selectInput(("th_par"),"Tahun",choices = "" ),
            ),
            column(1,
                   selectInput(("tipeLahan_par"),"Tipe Lahan",choices = "" ),
                   
            ),
            column(2,
                   br(),
                   actionButton(("asumsiMakro_button_par"),"Tentukan Asumsi Makro",icon("paper-plane"),style="color: white; 
                         background-color: green;") 
                   
            )
          )
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowMakro_par')
          
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowTable_par')
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowButton_par')
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowResult_par')
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          actionButton("browser_button_par","Browser Button")
          
        )
      ),
      
    )
  )
)
