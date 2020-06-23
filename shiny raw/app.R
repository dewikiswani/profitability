#setwd("C:/dw/ICRAF/profitability/shiny")
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

ui <- fluidPage( 
  titlePanel("Profitability"),
  tagList(
    actionButton(("modalDefineButton"),'Deskripsi Skenario'),
    tags$br(),
    tags$br(),
    #dataTableOutput(("ListTable")),
    #uiOutput(("daftarDefineShow")),
    #tags$div(id = ('scenarioResultPlaceholder')),
    #shinyjs::useShinyjs(),
    #shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }")
  )
)

server <- function(input,output,session){
 
      indonesia <- read.csv("C:/dw/ICRAF/project R/theme 2/profitability/data/lusita 2.0/prov sampai desa.csv",
                            stringsAsFactors = F)
      komoditas <- read.csv("C:/dw/ICRAF/project R/theme 2/profitability/data/lusita 2.0/komoditas.csv",
                            stringsAsFactors = F)
      peneliti <- read.csv("C:/dw/ICRAF/project R/theme 2/profitability/data/lusita 2.0/peneliti.csv",
                            stringsAsFactors = F)
      observe({
        updateSelectInput(session,
                          "kom",
                          choices =sort(unique(komoditas$nama_komoditas)))
      })

      observe({
        updateSelectInput(session,
                          "user",
                          choices =sort(unique(peneliti$nama_peneliti)))
      })

      observe({
        updateSelectInput(session,
                          "selected_provinsi",
                          choices =sort(unique(indonesia$provinsi)))
      })


      observe({
        updateSelectInput(
          session,
          "selected_kota",
          choices = indonesia %>%
            filter(provinsi == input$selected_provinsi) %>%
            select(kabkot) %>%
            .[[1]]
        )
      })

      observe({
        updateSelectInput(
          session,
          "selected_kec",
          choices = indonesia %>%
            filter(kabkot == input$selected_kota) %>%
            select(kecamatan) %>%
            .[[1]]
        )
      })

      observe({
        updateSelectInput(
          session,
          "selected_desa",
          choices = indonesia %>%
            filter(kecamatan == input$selected_kec) %>%
            select(desa) %>%
            .[[1]]
        )
      })
  
  ################################################################################
  #                                                                              #
  #                        BUTTON DEFINE                                         #
  #                                                                              #
  ################################################################################
  observeEvent(input$modalDefineButton,{
    showModal(modalDialog(sidebarLayout(sidebarPanel(
      fluidRow(
        textInput(("intervensiDef"),
                  label="Nama Peneliti"),
        selectInput("sut",
                    "Sistem Usaha Tani",choices = c("Monokultur","Agroforestry")),
        selectInput("kom",
                    "Komoditas",choices =""),
        selectInput(("tahunSkenario"),
                    label="Tahun",
                    choices = c(2000:2020)),
        selectInput("selected_provinsi",
                    "Pilih Provinsi:",
                    choices = ""),
        selectInput("selected_kota",
                    "Pilih Kota/Kabupaten:",
                    choices = ""),
        selectInput("selected_kec",
                    "Pilih Kecamatan:",
                    choices = ""),
        selectInput("selected_desa",
                    "Pilih Desa:",
                    choices = "")
      ),
      tags$br(),
      actionButton(("defHit"),"tampilkan"),
      width=5
    ),
    mainPanel(
      tags$div(id = 'defPlaceholder'),
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
  
  # 
  # observeEvent(input$defHit, {
  #   insertUI(selector='#defPlaceholder',
  #            where='afterEnd',
  #            ui= uiOutput(('defUIManual'))
  #   )
  # })
  # 
  # output$defUIManual<- renderUI({
  #   tagList(rHandsontableOutput(('editDefine')),
  #   )
  # })
  # 
  # 
  # valDef<- reactive({
  #   namaSken <- input$intervensiDef
  #   tahunAwal <- input$tahunAwal
  #   tahunAkhir <- input$tahunAkhir
  #   deskrip <- input$deskripsi
  #   gabung <- rbind(namaSken,tahunAwal,tahunAkhir, deskrip)
  #   
  #   tableDef <- data.frame(gabung)
  #   colnames(tableDef) <- "Keterangan"
  #   rownames(tableDef) <- c("nama skenario",
  #                           "tahun awal",
  #                           "tahun akhir",
  #                           "deskripsi")
  #   
  #   tableDef
  # })
  # 
  # 
  # output$editDefine <- renderRHandsontable({
  #   rhandsontable(valDef(),
  #                 readOnly = T,
  #                 rowHeaderWidth = 160,
  #   )%>%hot_cols(colWidths = 150)
  # })
  # 
  # listValDef <- reactive({
  #   newTableDef <- as.data.frame(hot_to_r(input$editDefine))
  #   
  #   namaSken <- as.character(newTableDef[1,]) #2
  #   tahunAwal <- as.numeric(trimws(newTableDef[2,])) #3
  #   tahunAkhir <- as.numeric(trimws(newTableDef[3,])) #4
  #   deskrip <- as.character(newTableDef[4,]) #5
  #   fdSelisih <- NULL #6
  #   satSelisih <- NULL #7
  #   
  #   emissionFactor <- NULL #8
  #   inputLRCRate <- NULL    #9
  #   inputPercentageDiagTPM <- NULL #10
  #   
  #   combineDef <- list(namaSken=namaSken,tahunAwal=tahunAwal,tahunAkhir=tahunAkhir, deskrip=deskrip,
  #                      fdSelisih=fdSelisih, satSelisih=satSelisih,
  #                      emissionFactor=emissionFactor,
  #                      inputLRCRate=inputLRCRate,
  #                      inputPercentageDiagTPM=inputPercentageDiagTPM)
  #   
  #   combineDef
  # })
  
  # ##### simpan tabel define ke dalam folder ####
  # observeEvent(input$saveModalDef,{
  #   waktuDefine<-Sys.time()
  #   simpanDefine<-gsub(" ","_",waktuDefine,fixed = TRUE)
  #   simpanDefine<-gsub(":","-",simpanDefine,fixed = TRUE)
  #   namaSken <- gsub(" ","",input$intervensiDef, fixed = TRUE)
  #   namafileDefine<-paste0(username,"_",selectedProv,"_",simpanDefine,"_",namaSken)
  #   saveRDS(listValDef(), file = paste0(alamatFile,"/",namafileDefine))
  #   shinyjs::js$refresh()
  # })
  # 
  # 
  # observeEvent(input$cancelModalDef,{
  #   removeModal()
  # })
  # 
  # 
  # ListButton_fun <- function(FUN, len, id, ...) {
  #   inputs <- character(len)
  #   for (i in seq_len(len)) {
  #     inputs[i] <- as.character(FUN(paste0(id,i), ...))
  #   }
  #   inputs
  # }
  # 
  # loadRDSAll <- reactive({
  #   nameFiles <- list.files(path = alamatFile,
  #                           pattern = paste0("^", username))
  #   dirFile <- paste0(alamatFile, "/",nameFiles)
  #   funcFile <- function(x){
  #     a <- readRDS(x)
  #     b <- c(x,a)
  #     b}
  # 
  #   r <- lapply(dirFile, funcFile)
  #   r
  # })
  # 
  # alamatFile <- paste0("shiny raw/skenarioData")
  # username <- "dw"
  # 
  # ### buat tabel daftar nama file reaktif ###
  # ListTableReact <- reactive({
  #   if(identical(list.files(path = alamatFile,pattern = paste0("^", username)),character(0))){
  #     data.frame(
  #       Nama.Skenario =  "file tidak tersedia",
  #       Tahun.Awal = "file tidak tersedia",
  #       Tahun.Akhir = "file tidak tersedia",
  #       Deskripsi.Skenario = "file tidak tersedia",
  #       fdSelisih = "file tidak tersedia",
  #       satSelisih = "file tidak tersedia",
  #       Nama.File = "file tidak tersedia",
  #       Sunting.Skenario = "file tidak tersedia",
  #       Jalankan.analisis = "file tidak tersedia",
  #       Hapus.skenario = "file tidak tersedia"
  #     )
  #   } else {
  #     data.frame(
  #       Nama.Skenario =  unlist(lapply(loadRDSAll(), function(x)x[[2]])),
  #       Tahun.Awal = unlist(lapply(loadRDSAll(), function(x)x[[3]])),
  #       Tahun.Akhir = unlist(lapply(loadRDSAll(), function(x)x[[4]])),
  #       Deskripsi.Skenario = unlist(lapply(loadRDSAll(), function(x)x[[5]])),
  #       Nama.File = unlist(lapply(loadRDSAll(), function(x)x[[1]])), #nama file dr listValDef ada di index terakhir = 6
  #       Sunting.Skenario = ListButton_fun(actionButton,
  #                                         length(loadRDSAll()),
  #                                         'button_',
  #                                         label = "Sunting Konstruksi Ekonomi dan Satelit Akun",
  #                                         onclick = sprintf('Shiny.onInputChange("%s", Math.random());Shiny.onInputChange("%s",this.id)',
  #                                                           ("select_button_trigger"), ("select_button"))),
  #       Jalankan.analisis = ListButton_fun(actionButton,
  #                                          length(loadRDSAll()),
  #                                          'buttonRun_',
  #                                          label = "Jalankan Analisis",
  #                                          onclick = sprintf('Shiny.onInputChange("%s", Math.random());Shiny.onInputChange("%s",this.id)',
  #                                                            ("run_button_trigger"), ("run_button"))),
  #       Hapus.skenario = ListButton_fun(actionButton,
  #                                       length(loadRDSAll()),
  #                                       'buttonDelete_',
  #                                       label = "Hapus Skenario",
  #                                       onclick = sprintf('Shiny.onInputChange("%s", Math.random());Shiny.onInputChange("%s",this.id)',
  #                                                         ("delete_button_trigger"), ("delete_button")))
  #     )
  #   }
  # 
  # 
  # })
  # 
  # ###tampilkan tabel list ###
  # output$ListTable <- renderDataTable({
  #   ListTableReact()
  # }, escape = FALSE)
  
  
}


app <- shinyApp(ui,server)
