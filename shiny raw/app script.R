library(shiny)
library(gridExtra)
library(shinyBS)
library(rhandsontable)
library(shinyBS)
library(DT)
library(stringr)
library(shinyWidgets)
library(dplyr)
library(openxlsx)


indonesia <- read.csv("C:/dw/ICRAF/project R/theme 2/profitability/data/lusita 2.0/prov sampai desa.csv",
                      stringsAsFactors = F)
komoditas <- read.csv("C:/dw/ICRAF/project R/theme 2/profitability/data/lusita 2.0/komoditas.csv",
                      stringsAsFactors = F)
peneliti <- read.csv("C:/dw/ICRAF/project R/theme 2/profitability/data/lusita 2.0/peneliti.csv",
                     stringsAsFactors = F)
username <- "dw"

alamatFile <- paste0("skenarioData")

SLIDER = sapply(1:5, function(i) {
  sprintf('<input type="text" id="Slider%d" name="slider" value="5;15" />', i)
})

js <- c(
  "function(settings){",
  "  $('[id^=Slider]').ionRangeSlider({",
  "    type: 'double',",
  "    grid: true,",
  "    grid_num: 10,",
  "    min: 0,",
  "    max: 20",
  "  });",
  "}"
)



ui <- fluidPage( 
  titlePanel("Analisis"),
  tabsetPanel(
    tabPanel(
      h5("Profitability"),
      tags$br(),
      tags$br(),
      actionButton(("modalDefineButton"),'Deskripsi Skenario'),
      tags$br(),
      tags$br(),
      dataTableOutput(("ListTable")),
      uiOutput(("daftarDefineShow")),
      tags$div(id = ('scenarioResultPlaceholder')),
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }")
    )
  )
)



server <- function(input,output,session){
  ################################################################################
  #                                                                              #
  #                        BUTTON DEFINE                                         #
  #                                                                              #
  ################################################################################
  observeEvent(input$modalDefineButton,{
    showModal(modalDialog(sidebarLayout(sidebarPanel(
      fluidRow(
        textInput(("nameResearcher"),
                  label="Nama Peneliti"),
        selectInput("sut",
                    "Sistem Usaha Tani",choices = c("Monokultur","Agroforestry")),
        selectInput("kom",
                    "Komoditas",choices =sort(unique(komoditas$nama_komoditas))),
        selectInput(("tahunSkenario"),
                    label="Tahun",
                    choices = c(2000:2020)),
        selectInput("selected_provinsi",
                    "Pilih Provinsi:",
                    choices = sort(unique(indonesia$provinsi))),
        textInput(("locScen"),
                  label="Lokasi Penelitian")
        # selectInput("selected_kota",
        #             "Pilih Kota/Kabupaten:",
        #             choices = ""),
        # selectInput("selected_kec",
        #             "Pilih Kecamatan:",
        #             choices = ""),
        # selectInput("selected_desa",
        #             "Pilih Desa:",
        #             choices = "")
      ),
      tags$br(),
      actionButton(("descriptHit"),"tampilkan"),
      width=5
    ),
    mainPanel(
      tags$div(id = 'descriptPlaceholder'),
      width=7
    )),
    title="Deskripsi Skenario",
    footer= tagList(
      actionButton(("saveModalDef"), "simpan skenario"),
      actionButton(("cancelModalDef"), "batal")
    ),
    size="l",
    easyClose = FALSE
    ))
  })

  observeEvent(input$descriptHit, {
    insertUI(selector='#descriptPlaceholder',
             where='afterEnd',
             ui= uiOutput(('descriptUIManual'))
    )
  })
  
  output$descriptUIManual<- renderUI({
    tagList(rHandsontableOutput(('editDefine')),
    )
  })
  
  
  valDef<- reactive({
    namaSken <- input$nameResearcher
    sut <- input$sut
    kom <- input$kom
    tahunSkenario <- input$tahunSkenario
    provinsi <- input$selected_provinsi
    locScen <- input$locScen
    
    gabung <- rbind(namaSken,sut,kom, tahunSkenario, provinsi, locScen)
    tableDef <- data.frame(gabung)
    colnames(tableDef) <- "Keterangan"
    rownames(tableDef) <- c("nama skenario",
                            "sistem usaha tani",
                            "komoditas",
                            "tahun skenario",
                            "provinsi",
                            "lokasi penelitian")
    
    tableDef
  })
  
  
  output$editDefine <- renderRHandsontable({
    rhandsontable(valDef(),
                  readOnly = T,
                  rowHeaderWidth = 160,
    )%>%hot_cols(colWidths = 150)
  })
  
  listValDef <- reactive({
    newTableDef <- as.data.frame(hot_to_r(input$editDefine))
    
    namaSken <- as.character(newTableDef[1,]) #2
    sut <- as.character(newTableDef[2,]) #3
    kom <-  as.character(newTableDef[3,]) #4
    tahunSkenario <- as.numeric(trimws(newTableDef[4,])) #5
    provinsi <- as.character(newTableDef[5,]) #6
    locScen <- as.character(newTableDef[6,]) #7
    
    fdSelisih <- NULL #6
    satSelisih <- NULL #7
    
    combineDef <- list(namaSken=namaSken,sut=sut,kom=kom, tahunSkenario=tahunSkenario,
                       provinsi=provinsi, locScen=locScen,
                       fdSelisih=fdSelisih, satSelisih=satSelisih)
    
    combineDef
  })
  
  ##### simpan tabel define ke dalam folder ####
  observeEvent(input$saveModalDef,{
    waktuDefine<-Sys.time()
    simpanDefine<-gsub(" ","_",waktuDefine,fixed = TRUE)
    simpanDefine<-gsub(":","-",simpanDefine,fixed = TRUE)
    namaSken <- gsub(" ","",input$nameResearcher, fixed = TRUE)
    namafileDefine<-paste0(username,"_",simpanDefine,"_",namaSken,"-",input$tahunSkenario)
    saveRDS(listValDef(), file = paste0(alamatFile,"/",namafileDefine))
    shinyjs::js$refresh()
  })
  
  
  observeEvent(input$cancelModalDef,{
    removeModal()
  })
  
  
  ListButton_fun <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id,i), ...))
    }
    inputs
  }
  
  loadRDSAll <- reactive({
    #browser()
    nameFiles <- list.files(path = alamatFile,
                            pattern = paste0("^", username))
    dirFile <- paste0(alamatFile, "/",nameFiles)
    funcFile <- function(x){
      a <- readRDS(x)
      b <- c(x,a)
      b}
    
    r <- lapply(dirFile, funcFile)
    r
  })
  
  
  ### buat tabel daftar nama file reaktif ###
  ListTableReact <- reactive({
    if(identical(list.files(path = alamatFile,pattern = paste0("^", username)),character(0))){
      data.frame(
        Nama.Skenario =  "file tidak tersedia",
        Nama.File = "file tidak tersedia",
        Sunting.Skenario = "file tidak tersedia",
        Sunting.Modal.Kapital = "file tidak tersedia", 
        Modal.kapital.digunakan = "file tidak tersedia",
        Diskon.Rate.Privat = "file tidak tersedia",
        Diskon.Rate.Sosial = "file tidak tersedia",
        Upah.Buruh.Privat = "file tidak tersedia",
        Upah.Buruh.Sosial = "file tidak tersedia", 
        Nilai.Tukar.Rupiah = "file tidak tersedia",
        Jalankan.analisis = "file tidak tersedia",
        Hapus.skenario = "file tidak tersedia"
      )
    } else {
      data.frame(
        Nama.Skenario =  unlist(lapply(loadRDSAll(), function(x)x[[2]])),
        Nama.File = unlist(lapply(loadRDSAll(), function(x)x[[1]])), #nama file dr listValDef ada di index terakhir = 6
        Sunting.Skenario = ListButton_fun(actionButton,
                                          length(loadRDSAll()),
                                          'button_',
                                          label = "Sunting Harga dan I-O",
                                          onclick = sprintf('Shiny.onInputChange("%s", Math.random());Shiny.onInputChange("%s",this.id)',
                                                            ("select_button_trigger"), ("select_button"))),
        Sunting.Modal.Kapital = ListButton_fun(actionButton,
                                          length(loadRDSAll()),
                                          'buttonKapital_',
                                          label = "Sunting Modal Kapital",
                                          onclick = sprintf('Shiny.onInputChange("%s", Math.random());Shiny.onInputChange("%s",this.id)',
                                                            ("select_buttonKapital_trigger"), ("select_buttonKapital"))),
        Diskon.Rate.Privat = ListButton_fun(numericInput,
                                            length(loadRDSAll()),
                                            "diskonP_",
                                            label = " ",
                                            value = 7 ,min = 0, max = 15, step = 0.01, width = '150px' ),
        Diskon.Rate.Sosial = ListButton_fun(numericInput,
                                            length(loadRDSAll()),
                                            "diskonS_",
                                            label = " ",
                                            value = 2 ,min = 0, max = 8, step = 0.01, width = '150px' ),
        Upah.Buruh.Privat = ListButton_fun(numericInput,
                                            length(loadRDSAll()),
                                            "upahP_",
                                            label = " ",
                                            value = 50000 ,min = 40000, max = 200000, step = 1000, width = '150px' ),
        Upah.Buruh.Sosial = ListButton_fun(numericInput,
                                            length(loadRDSAll()),
                                            "upahS",
                                            label = " ",
                                            value = 50000 ,min = 40000, max = 200000, step = 1000, width = '150px' ),
        Nilai.Tukar.Rupiah = ListButton_fun(numericInput,
                                                  length(loadRDSAll()),
                                                  "nilaiTukar_",
                                                  label = " ",
                                                  value = 10000 ,min = 9000, max = 20000, step = 100, width = '150px' ),
        Jalankan.analisis = ListButton_fun(actionButton,
                                           length(loadRDSAll()),
                                           'buttonRun_',
                                           label = "Jalankan Analisis",
                                           onclick = sprintf('Shiny.onInputChange("%s", Math.random());Shiny.onInputChange("%s",this.id)',
                                                             ("run_button_trigger"), ("run_button"))),
        Hapus.skenario = ListButton_fun(actionButton,
                                        length(loadRDSAll()),
                                        'buttonDelete_',
                                        label = "Hapus Skenario",
                                        onclick = sprintf('Shiny.onInputChange("%s", Math.random());Shiny.onInputChange("%s",this.id)',
                                                          ("delete_button_trigger"), ("delete_button")))
      )
    }
    
    
  })
  
  ###tampilkan tabel list ###
  output$ListTable <- renderDataTable({
    ListTableReact()
  }, escape = FALSE)
  
  
  observeEvent(input$run_button_trigger,{
    #removeUI(selector=paste0('#',('scenarioResultVisualization')))
    browser()
    selectedRow <- as.numeric(strsplit(input$run_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,2]) #nama file dari ListTableReact ada di col=2
    selectedFile<-readRDS(fileName)
    
    insertUI(selector=paste0("#",("scenarioResultPlaceholder")),
             where='afterEnd',
             ui= uiOutput(('scenarioResultVisualization'))
    )
  })
  
  output$coba <- renderText({
    paste0(as.numeric(strsplit(input$diskonP,"_")[[1]][2]))
  })
  
  output$scenarioResultVisualization <- renderUI({
    tagList(
            textOutput('coba')
            )
  })
  
}


shinyApp(ui,server)

