pamBaru <- argonTabItem(
  tabName = "pamBaru",
  argonH1("Membuat PAM Baru", display = 4),
  argonRow(
    argonCard(
      width = 12,
      title = "Membuat PAM Baru",
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
                   selectInput(("sut_new"),"Sistem Usaha Tani",choices = c("MONOKULTUR","AGROFORESTRI")),
            ),
            column(2,
                   textInput("kom_new","Komoditas", value = NULL ),
            ),
            column(2,
                   selectInput("selected_provinsi_new",
                               "Pilih Provinsi:",
                               choices = ""),
            ),
            column(1,
                   selectInput(("th_new"),"Tahun",choices = c(1995:as.integer(format(Sys.Date(), "%Y"))) ),
            ),
            column(1,
                   selectInput(("tipeLahan_new"),"Tipe Lahan",choices = c("MINERAL","GAMBUT") ),
                   
            ),
            column(2,
                   selectInput(("tipeKebun_new"),"Tipe Lahan",choices = c("CROP", "SMALLHOLDER", "LARGE SCALE") ),
                   
            ),
            column(2,
                   br(),
                   actionButton(("asumsiMakro_button_new"),"Tentukan Asumsi Makro",icon("paper-plane"),style="color: white; 
                         background-color: green;"),
                   useShinyalert()
                   
            )
          )
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowMakro_new')
          
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowTable_new')
        )
      ),
      # argonRow(
      #   argonColumn(
      #     width = 12,
      #     tags$div(id = 'uiShowButton_new')
      #   )
      # ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowResult_new')
        )
      ),
      
    )
  )
  
  
)
