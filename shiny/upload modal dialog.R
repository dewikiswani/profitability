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
              #generalUI("profit")
              fluidRow(
                column(5,
                       h2("Informasi umum",align = 'center'),
                       br(),
                       selectInput(("sut"),"Sistem Usaha Tani",choices = c("MONOKULTUR","AGROFORESTRI")),
                       br(),
                       #selectInput(("kom"),"Komoditas",choices =sort(unique(komoditas$nama_komoditas))),
                       selectInput("kom","Komoditas",choices = "" ),
                       br(),
                       selectInput(("th"),"Tahun",choices = c(2020:2021)),
                       br(),
                       selectInput(("user"),"Nama Peneliti",choices = c("A","B","C")),
                       br(),
                       selectInput(("gambut"),"Tipe Lahan",choices = c("NON-GAMBUT","GAMBUT")),
                       # useShinyalert()
                ),
                column(5,
                       h2(id="big-heading","Variable Input"),
                       tags$style(HTML("#big-heading{color: white;}")),
                       br(),
                       selectInput(("selected_provinsi"),
                                   "Pilih Provinsi:",
                                   choices = ""),
                       br(),
                       selectInput("selected_kota",
                                   "Pilih Kota/Kabupaten:",
                                   choices = ""),
                       br(),
                       selectInput("selected_kec",
                                   "Pilih Kecamatan:",
                                   choices = ""),
                       br(),
                       selectInput("selected_desa",
                                   "Pilih Desa:",
                                   choices = ""),
                       br(),
                       textInput("petani", "Nama Petani")
                       
                )
              )
            ),
            mainPanel(
              #buttonUI("profit")
              fluidRow(
                column(4,
                       h2("Asumsi Makro"),
                       #tags$style(HTML("#big-heading{color: black;}")),
                       br(),
                       sliderInput(("rate.p"), "Discount Rate Private", 7.41 ,min = 0, max = 15, step = 0.01),
                       br(),
                       sliderInput(("rate.s"), "Discount Rate Social", 2.41 ,min = 0, max = 8, step = 0.01),
                       br(),
                       # sliderInput(("labor.p"), "Upah Buruh Privat", 70000 ,min = 40000, max = 200000, step = 1000),
                       # br(),
                       # sliderInput(("labor.s"), "Upah Buruh Sosial", 60000 ,min = 40000, max = 200000, step = 1000),
                       # br(),
                       sliderInput(("nilai.tukar"), "Nilai Tukar Rupiah", 14831 ,min = 10000, max = 20000, step = 10),
                ),
                column(4,
                       h2("Unggah File"),
                       br(),
                       actionButton(("modalIOButton"),'Input-Output'),
                       br(),
                       br(),
                       actionButton(("modalPriceButton"),'Harga'),
                       br(),
                       br(),
                       actionButton(("modalCapitalButton"),'Modal Kapital'),
                       br(),
                       h5("Status Modal Kapital:"),
                       # br(),
                       checkboxInput(("checkKapital"),'modal kapital dimasukkan kedalam perhitungan'),
                       # br(),
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
            column(10,
                   fluidPage(
                     fluidRow(
                       column(4,
                              h1("Hasil Analisis"),
                              br(),
                       )
                     ),
                     br(),
                     fluidRow(
                       column(4,
                              verbatimTextOutput(("npv"))
                       ),
                       column(4,
                              verbatimTextOutput(("nlc"))
                       ),
                       column(4,
                              verbatimTextOutput(("ec"))
                       )
                     ),
                     br(),
                     br(),
                     fluidRow(
                       column(8,
                              verbatimTextOutput(("hp"))
                       ),
                       column(4,
                              verbatimTextOutput(("lr"))
                       )
                     ),
                     actionButton(("saveNewPAM"),"Simpan PAM baru",icon("paper-plane"),style="color: white; 
                         background-color: green;"),
                   )
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
    #viewTableUI("profit")
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
