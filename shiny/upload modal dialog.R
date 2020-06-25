upload <- argonTabItem(
  tabName = "upload",
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
          sidebarLayout(
            sidebarPanel(
              fluidRow(
                column(5,
                       h2("Unggah File",align = 'center'),
                       br(),
                       buttonUI("profit")
                       # actionButton("modalPriceButton",'Harga'),
                       # #br(),
                       # actionButton("modalIOButton",'Input-Output'),
                       # br(),
                       # br(),
                       # br(),
                       # checkboxInput("checkKapital",'modal kapital dimasukkan kedalam perhitungan'),
                       # actionButton("modalCapitalButton",'Modal Kapital'),
                       # br(),
                       # fileInput("file.1", "Harga Input"),
                       # fileInput("file.2", "Harga Output"),
                       # fileInput("file.3", "I-O Input"),
                       # fileInput("file.4", "I-O Output"),
                       # fileInput("file.5", "Modal Kapital Privat"),
                       # fileInput("file.6", "Modal Kapital Sosial")
                ),
                column(5,
                       h2(id="big-heading","Variable Input"),
                       tags$style(HTML("#big-heading{color: white;}")),
                       br(),
                       selectInput("sut","Sistem Usaha Tani",choices = c("Monokultur","Agroforestry")),
                       br(),
                       selectInput("kom","Komoditas",choices =""),
                       br(),
                       selectInput("th","Tahun",choices = c(2001:2019)),
                       br(),
                       selectInput("user","Nama Peneliti",choices = ""),
                       
                       
                )
              )
            ),
            mainPanel(
              fluidRow(
                column(4,
                       h2("Variable Input", align = 'center'),
                       #tags$style(HTML("#big-heading{color: black;}")),
                       br(),
                       selectInput("selected_provinsi",
                                   "Pilih Provinsi:",
                                   choices = ""),
                       br(),
                       # uiOutput("selected_kota_UI"),
                       selectInput("selected_kota",
                                   "Pilih Kota/Kabupaten:",
                                   choices = ""),
                       br(),
                       # uiOutput("selected_kec_UI"),
                       selectInput("selected_kec",
                                   "Pilih Kecamatan:",
                                   choices = ""),
                       br(),
                       # uiOutput("selected_desa_UI"),
                       selectInput("selected_desa",
                                   "Pilih Desa:",
                                   choices = "")
                ),
                column(4,
                       h2(id="big-heading","Variable Input"),
                       tags$style(HTML("#big-heading{color: white;}")),
                       br(),
                       sliderInput("rate.p", "Discount Rate Private", 7 ,min = 0, max = 15, step = 0.01),
                       br(),
                       sliderInput("rate.s", "Discount Rate Social", 2 ,min = 0, max = 8, step = 0.01),
                       br(),
                       sliderInput("labor.p", "Cost of Labor Private", 50000 ,min = 40000, max = 200000, step = 1000),
                       br(),
                       sliderInput("labor.s", "Cost of Labor Social", 50000 ,min = 40000, max = 200000, step = 1000)
                ),
                column(4,
                       h2(id="big-heading","Variable Input"),
                       tags$style(HTML("#big-heading{color: white;}")),
                       br(),
                       sliderInput("nilai.tukar", "Rupiah Exchange Rate", 10000 ,min = 9000, max = 20000, step = 100),
                       actionButton("simulate","Jalankan Analisis!",icon("paper-plane"),style="color: white; 
                         background-color: green;")
                )
              )
            )
          )
        )
        # ,
        # argonColumn(
        #   width = 12,
        #   fluidRow(
        #     column(10,
        #            #fluidRow(column(10, argonH1("Assumption & Summary", display = 4))),
        #            h1("Assumption & Summary"),
        #            br(),
        #            fluidRow(column(12, verbatimTextOutput("value"))),
        #            fluidRow(column(12, verbatimTextOutput("value.2"))),
        #            br(),
        #            #argonH1("Summary", display = 4),
        #            #argonH1(verbatimTextOutput("npv"), display = 4),
        #            #argonH1(verbatimTextOutput("nonlabor.cost"), display = 4),
        #            #argonH1(verbatimTextOutput("estcost.print"), display = 4),
        #            #argonH1(verbatimTextOutput("summary"), display = 4),
        #            #argonH1(verbatimTextOutput("summary.2"), display = 4)
        #            fluidRow(column(12, verbatimTextOutput("npv"))),
        #            fluidRow(column(12, verbatimTextOutput("nlc"))),
        #            fluidRow(column(12, verbatimTextOutput("ec"))),
        #            fluidRow(column(12, verbatimTextOutput("hp"))),
        #            fluidRow(column(12, verbatimTextOutput("lr")))
        #     )
        #   )
        # )
        
      )
    )
  ),
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
                   #fluidRow(column(10, argonH1("Assumption & Summary", display = 4))),
                   h1("Assumption & Summary"),
                   br(),
                   fluidRow(column(12, verbatimTextOutput("value"))),
                   fluidRow(column(12, verbatimTextOutput("value.2"))),
                   br(),
                   #argonH1("Summary", display = 4),
                   #argonH1(verbatimTextOutput("npv"), display = 4),
                   #argonH1(verbatimTextOutput("nonlabor.cost"), display = 4),
                   #argonH1(verbatimTextOutput("estcost.print"), display = 4),
                   #argonH1(verbatimTextOutput("summary"), display = 4),
                   #argonH1(verbatimTextOutput("summary.2"), display = 4)
                   fluidRow(column(12, verbatimTextOutput("npv"))),
                   fluidRow(column(12, verbatimTextOutput("nlc"))),
                   fluidRow(column(12, verbatimTextOutput("ec"))),
                   fluidRow(column(12, verbatimTextOutput("hp"))),
                   fluidRow(column(12, verbatimTextOutput("lr")))
            )
          )
        )
        # ,
        # argonColumn(
        #   width = 12,
        #   fluidRow(
        #     column(10,
        #            #fluidRow(column(10, argonH1("Assumption & Summary", display = 4))),
        #            h1("Assumption & Summary"),
        #            br(),
        #            fluidRow(column(12, verbatimTextOutput("value"))),
        #            fluidRow(column(12, verbatimTextOutput("value.2"))),
        #            br(),
        #            #argonH1("Summary", display = 4),
        #            #argonH1(verbatimTextOutput("npv"), display = 4),
        #            #argonH1(verbatimTextOutput("nonlabor.cost"), display = 4),
        #            #argonH1(verbatimTextOutput("estcost.print"), display = 4),
        #            #argonH1(verbatimTextOutput("summary"), display = 4),
        #            #argonH1(verbatimTextOutput("summary.2"), display = 4)
        #            fluidRow(column(12, verbatimTextOutput("npv"))),
        #            fluidRow(column(12, verbatimTextOutput("nlc"))),
        #            fluidRow(column(12, verbatimTextOutput("ec"))),
        #            fluidRow(column(12, verbatimTextOutput("hp"))),
        #            fluidRow(column(12, verbatimTextOutput("lr")))
        #     )
        #   )
        # )
        
      )
 
    )
  )
  ,
  argonColumn(
    width = 12,
    argonH1("Table", display = 4),
    argonTabSet(
      id = "tab-1",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "sm",
      width = 12,
      iconList = lapply(X = 1:2, FUN = argonIcon, name = "atom"),
      argonTab(
        tabName = "Table Price",
        active = T,
        dataTableOutput("tabel.1"),
        style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
      ),
      argonTab(
        tabName = "Table Input-Output",
        active = F,
        dataTableOutput("tabel.2"),
        style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
      )
    )
  )
  
  
)
