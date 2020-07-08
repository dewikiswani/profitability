
komoditas <- read.csv("C:/dw/ICRAF/project R/theme 2/profitability/data/lusita 2.0/komoditas.csv",
                      stringsAsFactors = F)

examplePrice <- read.table("data/MONOKULTUR/KELAPA SAWIT/price template input.csv", header = T, sep = ",")
exampleIO <- read.table("data/template/io template.csv", header = T, sep = ",")



generalUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(5,
           h2("Informasi umum",align = 'center'),
           br(),
           selectInput(ns("sut"),"Sistem Usaha Tani",choices = c("MONOKULTUR","AGROFORESTRY")),
           br(),
           selectInput(ns("kom"),"Komoditas",choices =sort(unique(komoditas$nama_komoditas))),
           br(),
           selectInput(ns("th"),"Tahun",choices = c(2001:2019)),
           br(),
           selectInput(ns("user"),"Nama Peneliti",choices = c("A","B","C")),
           br(),
           selectInput(ns("gambut"),"Tipe Lahan",choices = c("GAMBUT","NON GAMBUT"))
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
}


buttonUI <- function(id) {
  ns <- NS(id)
  fluidRow(
      column(4,
             h2("Asumsi Makro"),
             #tags$style(HTML("#big-heading{color: black;}")),
             br(),
             sliderInput(ns("rate.p"), "Discount Rate Private", 7 ,min = 0, max = 15, step = 0.01),
             br(),
             sliderInput(ns("rate.s"), "Discount Rate Social", 2 ,min = 0, max = 8, step = 0.01),
             br(),
             sliderInput(ns("labor.p"), "Upah Buruh Privat", 80000 ,min = 40000, max = 200000, step = 1000),
             br(),
             sliderInput(ns("labor.s"), "Upah Buruh Sosial", 80000 ,min = 40000, max = 200000, step = 1000),
             br(),
             sliderInput(ns("nilai.tukar"), "Nilai Tukar Rupiah", 15000 ,min = 9000, max = 20000, step = 100),
      ),
      column(4,
             h2("Unggah File"),
             br(),
             actionButton(ns("modalIOButton"),'Input-Output'),
             br(),
             br(),
             actionButton(ns("modalPriceButton"),'Harga'),
             br(),
             br(),
             actionButton(ns("modalCapitalButton"),'Modal Kapital'),
             br(),
             checkboxInput(ns("checkKapital"),'modal kapital dimasukkan kedalam perhitungan'),
             br(),
             br(),
      ),
      column(4,
             h2(id="big-heading","Variable Input"),
             tags$style(HTML("#big-heading{color: white;}")),
             br(),
             actionButton(ns("simulate"),"Jalankan Analisis!",icon("paper-plane"),style="color: white; 
                         background-color: green;")   
      )
    )
}

viewTableUI <- function(id){
  ns <- NS(id)
  argonTabSet(
    id = "tab-1",
    card_wrapper = TRUE,
    horizontal = TRUE,
    circle = FALSE,
    size = "sm",
    width = 12,
    iconList = lapply(X = 1:2, FUN = argonIcon, name = "atom"),
    argonTab(
      tabName = "Tabel Harga",
      active = T,
      dataTableOutput(ns("viewPrice")),
      style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
    ),
    argonTab(
      tabName = "Tabel Input-Output",
      active = F,
      dataTableOutput(ns("viewIO")),
      style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
    )
  )
}

buttonModule <- function(input, output, session) {
  
  # load the namespace
  ns <- session$ns
  
  # Start template data ketika peneliti memulai skenario ------------------
  dataTemplate <- reactive({
    ioInput <- NULL
    ioOutput <- NULL
    
    priceInput <- NULL
    priceOutput <- NULL

    capitalPrivate <- NULL
    capitalSocial <- NULL

    combineDef <- list(ioInput=ioInput,ioOutput=ioOutput,
                       priceInput=priceInput,priceOutput=priceOutput,
                       capitalPrivate=capitalPrivate,
                       capitalSocial=capitalSocial)
    print("cek")

    combineDef
    
  })
  
  observeEvent(input$modalIOButton,{
    #browser()
    
    combineDef <- dataTemplate()
    datapath <- paste0("data/", input$sut, "/",input$kom, "/")
    fileName <- paste0(datapath,"saveDataTemplate.rds")
    print("cc")
    saveRDS(combineDef,file = fileName)

  })
  
  readDataTemplate <- eventReactive(input$modalPriceButton,{
    datapath <- paste0("data/", input$sut, "/",input$kom, "/")
    fileName <- paste0(datapath,"saveDataTemplate.rds")
    print("baca")
    readRDS(fileName)

  })
  
  
  templateIOInput <- reactive({
    datapath <- paste0("data/", input$sut, "/",input$kom, "/")
    print(datapath)
    readData <- read.table(paste0(datapath,"io template input.csv"), header = T, sep = ",")
    readData
  })
  
  templateIOOutput <- reactive({
    datapath <- paste0("data/", input$sut, "/",input$kom, "/")
    print(datapath)
    readData <- read.table(paste0(datapath,"io template output.csv"), header = T, sep = ",")
    readData
  })
  
  ################################################################################
  #                                                                              #
  #                                 BUTTON IO                                    #
  #                                                                              #
  ################################################################################
  observeEvent(input$modalIOButton,{
    showModal(
      modalDialog( 
        footer=tagList(
          actionButton(ns("closeModalIO"), "Tutup")
        ),
        #tabsetPanel(
        argonTabSet(
          #tabPanel(
          id = "tabIO",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "lg",
          width = 12,
          iconList = lapply(X = 1:2, FUN = argonIcon, name = "atom"),
          argonTab(
            tabName = "Input",
            active = T,
            sidebarLayout(
              sidebarPanel(
                fluidPage(
                  h2("Sunting Input Tabel I-O"),
                  fluidRow(
                    column(6,
                           selectInput(ns("ioYear_input"),
                                       "pilih tahun skenario:",
                                       choices = c(1:30),selected = 30)
                    ),
                    column(6,
                           selectInput(ns("ioComp_input"),
                                       "banyaknya komponen:",
                                       choices = c(1:50),selected = nrow(templateIOInput()))
                    )
                  )
                ),
                tags$br(),
                actionButton(ns("ioInputHit"),"tampilkan tabel"),
                width=12
              ),
              mainPanel(
                tags$div(id = ns('ioInputPlaceholder')),
                width=12)
            )
          ),
          argonTab(
            tabName = "Output",
            #active = T,
            sidebarLayout(
              sidebarPanel(
                fluidPage(
                  h2("Sunting Output Tabel I-O"),
                  fluidRow(
                    column(6,
                           selectInput(ns("ioYear_output"),
                                       "pilih tahun skenario:",
                                       choices = c(2:30),selected = 30)
                    ),
                    column(6,
                           selectInput(ns("ioComp_output"),
                                       "banyaknya komponen:",
                                       choices = c(1:10),selected = nrow(templateIOOutput()))
                    )
                  )
                ),
                tags$br(),
                actionButton(ns("ioOutputHit"),"tampilkan tabel"),
                width=12
              ),
              mainPanel(
                tags$div(id = ns('ioOutputPlaceholder')),
                width=12)
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    )
  })
  
  observeEvent(input$closeModalIO,{
    removeModal()
  })
  
  
  # START IO Input ----------------------------------------------------------
  observeEvent(input$ioInputHit, {
    insertUI(selector= paste0("#", ns("ioInputPlaceholder")),
             where='afterEnd',
             ui= uiOutput(ns('ioInputUI'))
    )
  })
  
  output$ioInputUI<- renderUI({
    
    tagList(
      tags$br(),
      tags$br(),
      tags$b('Sunting secara manual'),
      tags$br(),
      tags$b('(hasil salin dari file Ms.Excel pastikan berformat huruf kecil/lowcase)'),
      tags$hr(),
      tags$h5("untuk memilih tipe komponen, keterangan dan unit disediakan menu dropdown pada kolom 1, 2 dan 3"),
      tags$h5("(pastikan komponen, keterangan dan unit sesuai dengan daftar yang disediakan pada menu dropdown)"),
      tags$br(),
      rHandsontableOutput(ns('editIOInput')),
      tags$br(),
      actionButton(ns('saveIOInput'), 'simpan tabel'), 
      tags$br(), 
      tags$br(),
      tags$div(id='teksIOInputSave')
    )
  })
  
  valIO1 <- eventReactive(input$ioInputHit,{
    #browser()
    indexCol <- as.numeric(input$ioYear_input)+3
    indexRow <- as.numeric(input$ioComp_input)
    
    if(indexRow > nrow(templateIOInput())){
      dataAdd <- data.frame(matrix(data=0,nrow = indexRow - nrow(templateIOInput()) , ncol = as.numeric(input$ioYear_input)))
      colnames(dataAdd) <- paste0("Y",c(1:input$ioYear_input))
      reactData$tableIO1 <- dplyr::bind_rows(templateIOInput(),dataAdd)
      #reactData$tableIO1 <- reactData$tableIO1[1:indexRow,c(1:3,4:(indexCol))]
      reactData$tableIO1
    }else {
      reactData$tableIO1 <- templateIOInput()
      reactData$tableIO1 <- reactData$tableIO1[1:indexRow,c(1:3,4:(indexCol))]
      reactData$tableIO1
    }
    
  })
  
  output$editIOInput <- renderRHandsontable({
    rhandsontable(valIO1(),
                  rowHeaderWidth = 50,
                  fixedColumnsLeft = 2,
                  height = 500,
    )
  })
  
  observeEvent(input$saveIOInput,{
    #browser()
    removeUI(selector='#textTampil')
    
    editNew<-as.data.frame(hot_to_r(input$editIOInput))
    editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
    
    datapath <- paste0("data/", input$sut, "/",input$kom, "/")
    fileName <- paste0(datapath,"saveDataTemplate.rds")
    dataDefine <- readRDS(fileName)
    dataDefine$ioInput <- editNew
    saveRDS(dataDefine,file = fileName)
    
    insertUI(selector='#teksIOInputSave',
             where = 'afterEnd',
             ui = tags$div(id="textTampil","tabel di atas sudah tersimpan"))
  })
  # ending IO Input -----------------------------------------------------------------
  
  
  # Start IO Output ------------------------------------------------------
  observeEvent(input$ioOutputHit, {
    insertUI(selector= paste0("#", ns("ioOutputPlaceholder")),
             where='afterEnd',
             ui= uiOutput(ns('ioOutputUI'))
    )
  })
  
  output$ioOutputUI<- renderUI({
    
    tagList(
      tags$br(),
      tags$br(),
      tags$b('Sunting secara manual'),
      tags$br(),
      tags$b('(hasil salin dari file Ms.Excel pastikan berformat huruf kecil/lowcase)'),
      tags$hr(),
      tags$h5("untuk memilih tipe komponen, keterangan dan unit disediakan menu dropdown pada kolom 1, 2 dan 3"),
      tags$h5("(pastikan komponen, keterangan dan unit sesuai dengan daftar yang disediakan pada menu dropdown)"),
      tags$br(),
      rHandsontableOutput(ns('editIOOutput')),
      tags$br(),
      actionButton(ns('saveIOOutput'), 'simpan tabel'), 
      tags$br(), 
      tags$br(),
      tags$div(id='teksIOOutputSave')
    )
  })
  
  valIO2 <- eventReactive(input$ioOutputHit,{
    #browser()
    indexCol <- as.numeric(input$ioYear_output)+3
    indexRow <- as.numeric(input$ioComp_output)
    reactData$tableIO2 <- templateIOOutput()
    reactData$tableIO2 <- reactData$tableIO2[1:indexRow,c(1:3,4:(indexCol))]
    reactData$tableIO2
  })
  
  output$editIOOutput <- renderRHandsontable({
    rhandsontable(valIO2(),
                  rowHeaderWidth = 50,
                  fixedColumnsLeft = 2,
                  height = 300,
    )
  })
  
  observeEvent(input$saveIOOutput,{
    removeUI(selector='#textTampil')
    
    editNew<-as.data.frame(hot_to_r(input$editIOOutput))
    editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
    
    datapath <- paste0("data/", input$sut, "/",input$kom, "/")
    fileName <- paste0(datapath,"saveDataTemplate.rds")
    dataDefine <- readRDS(fileName)
    dataDefine$ioOutput <- editNew
    saveRDS(dataDefine,file = fileName)
    
    insertUI(selector='#teksIOOutputSave',
             where = 'afterEnd',
             ui = tags$div(id="textTampil","tabel di atas sudah tersimpan"))
  })
  # Ending IO Output -----------------------------------------------------
  
  
  ################################################################################
  #                                                                              #
  #                                 BUTTON HARGA                                 #
  #                                                                              #
  ################################################################################
  observeEvent(input$modalPriceButton,{
    showModal(
      modalDialog( 
        footer=tagList(
          actionButton(ns("closeModalPrice"), "Tutup")
        ),
        #tabsetPanel(
        argonTabSet(
          #tabPanel(
          id = "tabPrice",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "sm",
          width = 12,
          iconList = lapply(X = 1:2, FUN = argonIcon, name = "atom"),
          argonTab(
            tabName = "Input",
            active = T,
            sidebarLayout(
              sidebarPanel(
                fluidPage(
                  h2("Sunting Harga Input"),
                  fluidRow(
                    column(6,
                           selectInput(ns("priceComp_input"),
                                       "banyaknya komponen yang diinput:",
                                       choices = nrow(readDataTemplate()$ioInput),
                                       selected = nrow(readDataTemplate()$ioInput) )
                           # selectInput(ns("priceYear_input"),
                           #             "Pilih tahun skenario:",
                           #             choices = c(1:30),selected = 30)
                    ),
                    column(6,
                           # selectInput(ns("priceComp_input"),
                           #             "banyaknya komponen yang diinput:",
                           #             choices = c(1:50),selected = 10)
                    )
                  )
                  ),
                tags$br(),
                actionButton(ns("priceInputHit"),"tampilkan tabel"),
                width=12
              ),
              mainPanel(
                tags$div(id = ns('priceInputPlaceholder')),
                width=12)
            )
          ),
          argonTab(
            tabName = "Output",
            #active = T,
            sidebarLayout(
              sidebarPanel(
                fluidPage(
                  h2("Sunting Harga Output"),
                  fluidRow(
                    column(6,
                           selectInput(ns("priceComp_output"),
                                       "banyaknya komponen yang diinput:",
                                       choices = c(1:10),selected = 2)
                           # selectInput(ns("priceYear_output"),
                           #             "Pilih tahun skenario:",
                           #             choices = c(1:30),selected = 30)
                    ),
                    column(6,
                            # selectInput(ns("priceComp_output"),
                            #                     "banyaknya komponen yang diinput:",
                            #                     choices = c(1:10),selected = 2)
                           )
                  )
                ),
                tags$br(),
                actionButton(ns("priceOutputHit"),"tampilkan tabel"),
                width=12
              ),
              mainPanel(
                tags$div(id = ns('priceOutputPlaceholder')),
                width=12)
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    )
  })
  
  observeEvent(input$closeModalPrice,{
    removeModal()
  })
  


  reactData <- reactiveValues(
    tableP1 = NULL, #price input
    tableP2 = NULL, #price output
    tableIO1 = NULL, #io input
    tableIO2 = NULL, #io output
    tableCapP = NULL, #capital privat
    tableCapS = NULL #capital sosial
  )

# Ending template data ----------------------------------------------------

  
# START Price Input ----------------------------------------------------------
  observeEvent(input$priceInputHit, {
    insertUI(selector= paste0("#", ns("priceInputPlaceholder")),
             where='afterEnd',
             ui= uiOutput(ns('priceInputUI'))
    )
  })
  
  output$priceInputUI<- renderUI({
    
    tagList(
      tags$br(),
      tags$br(),
      tags$b('Sunting secara manual'),
      tags$br(),
      tags$b('(hasil salin dari file Ms.Excel pastikan berformat huruf kecil/lowcase)'),
      tags$hr(),
      tags$h5("untuk memilih tipe komponen, keterangan dan unit disediakan menu dropdown pada kolom 1, 2 dan 3"),
      tags$h5("(pastikan komponen, keterangan dan unit sesuai dengan daftar yang disediakan pada menu dropdown)"),
      tags$br(),
      rHandsontableOutput(ns('editPriceInput')),
      tags$br(),
      actionButton(ns('savePriceInput'), 'simpan tabel'), 
      tags$br(), 
      tags$br(),
      tags$div(id='teksPriceInputSave')
    )
  })
  
  valP1 <- eventReactive(input$priceInputHit,{
    #browser()
    indexRow <- as.numeric(input$priceComp_input)
    
    reactData$tableP1 <- readDataTemplate()$ioInput[,1:2]
    #reactData$tableP1 <- reactData$tableP1[1:indexRow,]
    examplePrice2 <- examplePrice[,-1]
    reactData$tableP1 <- merge(reactData$tableP1,unique(examplePrice2), by.x = "keterangan",by.y = "keterangan", all.x = T)
    reactData$tableP1 <- reactData$tableP1[, c(2, 1, 3, 4, 5)]
    
    #sort by nomor krn g alfabet urutannya PR
    reactData$tableP1
  })
  
  output$editPriceInput <- renderRHandsontable({
    rhandsontable(valP1(),
                  rowHeaderWidth = 50,
                  fixedColumnsLeft = 2,
                  height = 500,
    )
  })
  
  observeEvent(input$savePriceInput,{
    browser()
    removeUI(selector='#textTampil')
    
    editNew<-as.data.frame(hot_to_r(input$editPriceInput))
    editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
    
    datapath <- paste0("data/", input$sut, "/",input$kom, "/")
    fileName <- paste0(datapath,"saveDataTemplate.rds")
    dataDefine <- readRDS(fileName)
    dataDefine$priceInput <- editNew
    saveRDS(dataDefine,file = fileName)
    
    insertUI(selector='#teksPriceInputSave',
             where = 'afterEnd',
             ui = tags$div(id="textTampil","tabel di atas sudah tersimpan"))
  })
# ending Price Input -----------------------------------------------------------------


# Start Price Output ------------------------------------------------------
  observeEvent(input$priceOutputHit, {
    insertUI(selector= paste0("#", ns("priceOutputPlaceholder")),
             where='afterEnd',
             ui= uiOutput(ns('priceOutputUI'))
    )
  })
  
  output$priceOutputUI<- renderUI({
    
    tagList(
      tags$br(),
      tags$br(),
      tags$b('Sunting secara manual'),
      tags$br(),
      tags$b('(hasil salin dari file Ms.Excel pastikan berformat huruf kecil/lowcase)'),
      tags$hr(),
      tags$h5("untuk memilih tipe komponen, keterangan dan unit disediakan menu dropdown pada kolom 1, 2 dan 3"),
      tags$h5("(pastikan komponen, keterangan dan unit sesuai dengan daftar yang disediakan pada menu dropdown)"),
      tags$br(),
      rHandsontableOutput(ns('editPriceOutput')),
      tags$br(),
      actionButton(ns('savePriceOutput'), 'simpan tabel'), 
      tags$br(), 
      tags$br(),
      tags$div(id='teksPriceOutputSave')
    )
  })
  
  valP2 <- eventReactive(input$priceOutputHit,{
    #browser()
    #indexCol <- as.numeric(input$priceYear_output)+3
    indexRow <- as.numeric(input$priceComp_output)
    reactData$tableP2 <- examplePrice
    #reactData$tableP2[,1] <- examplePrice[nrow(examplePrice),1]
    #reactData$tableP2[,2] <- examplePrice[nrow(examplePrice),2]
    #reactData$tableP2[,3] <- examplePrice[nrow(examplePrice),3]
    reactData$tableP2 <- reactData$tableP2[1:indexRow,]
    reactData$tableP2
  })
  
  output$editPriceOutput <- renderRHandsontable({
    rhandsontable(valP2(),
                  rowHeaderWidth = 50,
                  fixedColumnsLeft = 2,
                  height = 300,
    )
  })
  
  observeEvent(input$savePriceOutput,{
    removeUI(selector='#textTampilOutput')
    
    editNew<-as.data.frame(hot_to_r(input$editPriceOutput))
    editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
    
    fileName <- c("data/template/profitabilityAnalysis.rds")
    dataDefine <- readRDS(fileName)
    dataDefine$priceOutput <- editNew
    saveRDS(dataDefine,file = fileName)
    
    insertUI(selector='#teksPriceOutputSave',
             where = 'afterEnd',
             ui = tags$div(id="textTampilOutput","tabel di atas sudah tersimpan"))
  })

# Ending Price Output -----------------------------------------------------


  # 
  # output$viewPrice <- renderRHandsontable({
  #   rhandsontable(valP1(),
  #                 rowHeaderWidth = 50,
  #                 fixedColumnsLeft = 3,
  #                 height = 500,
  #   )
  # })
  
  output$viewPrice <- renderDataTable({
    dataView <- valP1()
    dataView
      # rhandsontable(valP1(),
      #               rowHeaderWidth = 50,
      #               fixedColumnsLeft = 3,
      #               height = 500,
      # )
  })

  output$viewIO <- renderDataTable({
    dataView <- exampleIO
    dataView
  })
  
}