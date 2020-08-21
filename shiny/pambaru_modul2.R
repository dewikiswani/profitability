pamBaru <- argonTabItem(
  tabName = "pamBaru",
  argonH1("Membuat PAM Baru", display = 4),
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
              fluidRow(
                column(12,
                       h2("Informasi umum",align = 'center')
                )
              ),
              fluidRow(
                column(6,
                       selectInput(("sut"),"Sistem Usaha Tani",choices = c("MONOKULTUR","AGROFORESTRI")),
                       br(),
                       selectInput("kom","Komoditas",choices = "" ),
                       br(),
                       selectInput("selected_provinsi",
                                   "Pilih Provinsi:",
                                   choices = ""),
                ),
                column(6,
                       selectInput(("th"),"Tahun",choices = c(2020:2021)),
                       br(),
                       selectInput(("gambut"),"Tipe Lahan",choices = c("NON-GAMBUT","GAMBUT")),
                       
                )
              )
            ),
            mainPanel(
              fluidRow(
                column(4,
                       h2("Asumsi Makro",align = 'center'),
                       br(),
                       sliderInput(("rate.p"), "Discount Rate Private", 7.4 ,min = 0, max = 15, step = 0.01),
                       br(),
                       sliderInput(("rate.s"), "Discount Rate Social", 2.4 ,min = 0, max = 8, step = 0.01),
                       br(),
                       sliderInput(("nilai.tukar"), "Nilai Tukar Rupiah", 14831 ,min = 10000, max = 20000, step = 10),
                ),
                column(4,
                       h2("Unggah File",align = 'center'),
                       br(),
                       actionButton(("modalIOButton"),'Input-Output'),
                       br(),
                       br(),
                       actionButton(("modalPriceButton"),'Harga'),
                       br(),
                       br(),
                       actionButton(("modalCapitalButton"),'Modal Kapital'),
                       br(),
                       br(),
                       h5("Status Modal Kapital:"),
                       checkboxInput(("checkKapital"),'modal kapital dimasukkan kedalam perhitungan'),
                       tags$div(id='teksStatusCapital'),
                       
                ),
                column(4,
                       h2(id="big-heading","Variable Input"),
                       tags$style(HTML("#big-heading{color: white;}")),
                       br(),
                       actionButton(("run_button"),"Jalankan Analisis!",icon("paper-plane"),style="color: white; 
                         background-color: green;")   
                )
              )
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
            column(12,
                   tags$div(id = 'uiShowResult')
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
    argonTabSet(
      id = "tab-1",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "sm",
      width = 12,
      iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
      argonTab(
        tabName = "Tabel Harga",
        active = T,
        dataTableOutput(("viewPrice")),
        style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
      ),
      argonTab(
        tabName = "Tabel Input-Output",
        active = F,
        dataTableOutput(("viewIO")),
        style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
      ),
      argonTab(
        tabName = "Tabel Modal Kapital",
        active = F,
        dataTableOutput(("viewKapital")),
        style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
      )
    )
  )
  
  
)
