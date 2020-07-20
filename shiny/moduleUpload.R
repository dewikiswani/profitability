komoditas <- read.csv("data/template/komoditas.csv", stringsAsFactors = F)




generalUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(5,
           h2("Informasi umum",align = 'center'),
           br(),
           selectInput(ns("sut"),"Sistem Usaha Tani",choices = c("MONOKULTUR","AGROFORESTRI")),
           br(),
           selectInput(ns("kom"),"Komoditas",choices =sort(unique(komoditas$nama_komoditas))),
           #selectInput(ns("kom"),"Komoditas",choices ="" ),
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
             sliderInput(ns("rate.p"), "Discount Rate Private", 7.41 ,min = 0, max = 15, step = 0.01),
             br(),
             sliderInput(ns("rate.s"), "Discount Rate Social", 2.41 ,min = 0, max = 8, step = 0.01),
             br(),
             # sliderInput(ns("labor.p"), "Upah Buruh Privat", 70000 ,min = 40000, max = 200000, step = 1000),
             # br(),
             # sliderInput(ns("labor.s"), "Upah Buruh Sosial", 60000 ,min = 40000, max = 200000, step = 1000),
             # br(),
             sliderInput(ns("nilai.tukar"), "Nilai Tukar Rupiah", 14831 ,min = 10000, max = 20000, step = 10),
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
             actionButton(ns("run_button"),"Jalankan Analisis!",icon("paper-plane"),style="color: white; 
                         background-color: green;")   
      )
    )
}

resultUI <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(4,
             verbatimTextOutput(ns("npv"))
      ),
      column(4,
             verbatimTextOutput(ns("nlc"))
      ),
      column(4,
             verbatimTextOutput(ns("ec"))
      )
    ),
    br(),
    br(),
    fluidRow(
      column(8,
             verbatimTextOutput(ns("hp"))
      ),
      column(4,
             verbatimTextOutput(ns("lr"))
      )
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
  
  # observe({
  #   print(input$sut)
  #   
  #   updateSelectInput(
  #     session,
  #     ns("kom"),
  #     choices = komoditas %>%
  #       filter(sut == input$sut) %>%
  #       select(nama_komoditas) %>%
  #       .[[1]]
  #   )
  #   
  #   a <- komoditas %>%
  #     filter(sut == input$sut) %>%
  #     select(nama_komoditas) %>%
  #     .[[1]]
  #   print(a)
  #   
  #   print(ns("kom"))
  #   
  #   print("__")
  #   
  #   print(input$kom)
  # })
  
  
  # Start template data ketika peneliti memulai skenario ------------------
  # dataTemplate <- reactive({
  #   ioInput <- NULL
  #   ioOutput <- NULL
  #   
  #   priceInput <- NULL
  #   priceOutput <- NULL
  # 
  #   capitalPrivate <- NULL
  #   capitalSocial <- NULL
  # 
  #   combineDef <- list(ioInput=ioInput,ioOutput=ioOutput,
  #                      priceInput=priceInput,priceOutput=priceOutput,
  #                      capitalPrivate=capitalPrivate,
  #                      capitalSocial=capitalSocial)
  #   print("cek")
  # 
  #   combineDef
  #   
  # })
  
  dataTemplate <- reactive({
    datapath <- paste0("data/", input$sut, "/",input$kom, "/")
    
    ioInput <- read.table(paste0(datapath,"io template input.csv"), header = T, sep = ",")
    ioOutput <- read.table(paste0(datapath,"io template output.csv"), header = T, sep = ",")
    
    priceInput <- read.table(paste0(datapath,"price template input.csv"), header = T, sep = ",")
    priceOutput <- read.table(paste0(datapath,"price template output.csv"), header = T, sep = ",")
    
    capitalPrivate <- NULL
    capitalSocial <- NULL
    
    combineDef <- list(ioInput=ioInput,ioOutput=ioOutput,
                       priceInput=priceInput,priceOutput=priceOutput,
                       capitalPrivate=capitalPrivate,
                       capitalSocial=capitalSocial)
    
    datapath <- paste0("data/", input$sut, "/",input$kom, "/")
    fileName <- paste0(datapath,"saveDataTemplate.rds")
    saveRDS(combineDef,file = fileName)
    
    combineDef
    
  })
  
  # observeEvent(input$modalIOButton,{
  #   #browser()
  #   
  #   combineDef <- dataTemplate()
  #   datapath <- paste0("data/", input$sut, "/",input$kom, "/")
  #   fileName <- paste0(datapath,"saveDataTemplate.rds")
  #   print("cc")
  #   #saveRDS(combineDef,file = fileName)
  # 
  # })
  
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
      # tags$br(),
      # tags$b('(hasil salin dari file Ms.Excel pastikan berformat huruf kecil/lowcase)'),
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
      reactData$tableIO1[is.na(reactData$tableIO1)] <- 0
      #reactData$tableIO1 <- reactData$tableIO1[1:indexRow,c(1:3,4:(indexCol))]
      reactData$tableIO1
    }else {
      reactData$tableIO1 <- templateIOInput()
      reactData$tableIO1 <- reactData$tableIO1[1:indexRow,c(1:3,4:(indexCol))]
      reactData$tableIO1[is.na(reactData$tableIO1)] <- 0
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
      # tags$br(),
      # tags$b('(hasil salin dari file Ms.Excel pastikan berformat huruf kecil/lowcase)'),
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
    
    if(indexRow > nrow(templateIOOutput())){
      dataAdd <- data.frame(matrix(data=0,nrow = indexRow - nrow(templateIOOutput()) , ncol = as.numeric(input$ioYear_input)))
      colnames(dataAdd) <- paste0("Y",c(1:input$ioYear_input))
      reactData$tableIO2 <- dplyr::bind_rows(templateIOOutput(),dataAdd)
      reactData$tableIO2[is.na(reactData$tableIO2)] <- 0
      reactData$tableIO2
    }else {
      reactData$tableIO2 <- templateIOOutput()
      reactData$tableIO2 <- reactData$tableIO2[1:indexRow,c(1:3,4:(indexCol))]
      reactData$tableIO2[is.na(reactData$tableIO2)] <- 0
      reactData$tableIO2
    }
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
                  #   tagList(
                  #     tags$br(),
                  #     tags$br(),
                  #     tags$b('Sunting secara manual'),
                  #     tags$br(),
                  #     tags$b('(hasil salin dari file Ms.Excel pastikan berformat huruf kecil/lowcase)'),
                  #     tags$hr(),
                  #     tags$h5("untuk memilih tipe komponen, keterangan dan unit disediakan menu dropdown pada kolom 1, 2 dan 3"),
                  #     tags$h5("(pastikan komponen, keterangan dan unit sesuai dengan daftar yang disediakan pada menu dropdown)"),
                  #     tags$br(),
                  #     rHandsontableOutput(ns('editPriceInput')),
                  #     tags$br(),
                  #     actionButton(ns('savePriceInput'), 'simpan tabel'), 
                  #     tags$br(), 
                  #     tags$br(),
                  #     tags$div(id='teksPriceInputSave')
                  #   )
                  # fluidRow(
                  #   column(6,
                  #          # selectInput(ns("priceComp_input"),
                  #          #             "banyaknya komponen yang diinput:",
                  #          #             choices = nrow(readDataTemplate()$ioInput),
                  #          #             selected = nrow(readDataTemplate()$ioInput) )
                  #          # selectInput(ns("priceYear_input"),
                  #          #             "Pilih tahun skenario:",
                  #          #             choices = c(1:30),selected = 30)
                  #   ),
                  #   column(6,
                  #          # selectInput(ns("priceComp_input"),
                  #          #             "banyaknya komponen yang diinput:",
                  #          #             choices = c(1:50),selected = 10)
                  #   )
                  # )
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
                  # fluidRow(
                  #   column(6,
                  #          # selectInput(ns("priceComp_output"),
                  #          #             "banyaknya komponen yang diinput:",
                  #          #             choices = nrow(readDataTemplate()$ioOutput),
                  #          #             selected = nrow(readDataTemplate()$ioOutput))
                  #          # selectInput(ns("priceYear_output"),
                  #          #             "Pilih tahun skenario:",
                  #          #             choices = c(1:30),selected = 30)
                  #   ),
                  #   column(6,
                  #           # selectInput(ns("priceComp_output"),
                  #           #                     "banyaknya komponen yang diinput:",
                  #           #                     choices = c(1:10),selected = 2)
                  #          )
                  # )
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
      #tags$br(),
      tags$hr(),
      tags$b('Sunting secara manual'),
      tags$h5("kolom yang dapat di sunting hanya kolom harga.privat dan harga.sosial"),
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
    indexRow <- as.numeric(nrow(readDataTemplate()$ioInput))
    
    reactData$tableP1 <- readDataTemplate()$ioInput[,1:2]
    no.id <- as.numeric(rownames(reactData$tableP1))
    reactData$tableP1 <- cbind(no.id,reactData$tableP1)
    
    datapath <- paste0("data/", input$sut, "/",input$kom, "/")
    templatePriceInput <- read.table(paste0(datapath,"price template input.csv"), header = T, sep = ",")
    templatePrice <- (templatePriceInput[,-1])
    reactData$tableP1 <- merge(reactData$tableP1,unique(templatePrice), by.x = "keterangan",by.y = "keterangan", all.x = T)
    reactData$tableP1 <- reactData$tableP1[order(reactData$tableP1$no.id),]     #sort by nomor yang disesuaikan pada tabel i-o
    rownames(reactData$tableP1) <- no.id
    reactData$tableP1 <- reactData$tableP1[, c(3, 1, 4, 5,6)]
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
    #browser()
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
      tags$hr(),
      tags$b('Sunting secara manual'),
      tags$h5("kolom yang dapat di sunting hanya kolom harga.privat dan harga.sosial"),
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
    indexRow <- as.numeric(nrow(readDataTemplate()$ioOutput))
    
    reactData$tableP2 <- readDataTemplate()$ioOutput[,1:2]
    no.id <- as.numeric(rownames(reactData$tableP2))
    reactData$tableP2 <- cbind(no.id,reactData$tableP2)
    
    datapath <- paste0("data/", input$sut, "/",input$kom, "/")
    templatePriceOutput <- read.table(paste0(datapath,"price template output.csv"), header = T, sep = ",")
    templatePrice <- (templatePriceOutput[,-1])
    reactData$tableP2 <- merge(reactData$tableP2,unique(templatePrice), by.x = "keterangan",by.y = "keterangan", all.x = T)
    reactData$tableP2 <- reactData$tableP2[order(reactData$tableP2$no.id),]     #sort by nomor yang disesuaikan pada tabel i-o
    rownames(reactData$tableP2) <- no.id
    reactData$tableP2 <- reactData$tableP2[, c(3, 1, 4, 5,6)]
    reactData$tableP2
    
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
    
    datapath <- paste0("data/", input$sut, "/",input$kom, "/")
    fileName <- paste0(datapath,"saveDataTemplate.rds")
    dataDefine <- readRDS(fileName)
    dataDefine$priceOutput <- editNew
    saveRDS(dataDefine,file = fileName)
    
    insertUI(selector='#teksPriceOutputSave',
             where = 'afterEnd',
             ui = tags$div(id="textTampilOutput","tabel di atas sudah tersimpan"))
  })

# Ending Price Output -----------------------------------------------------
  
  ################################################################################
  #                                                                              #
  #                                RESULT                                        #
  #                                                                              #
  ################################################################################
  data.gab <- eventReactive(input$run_button,{
    # aktifin dataTemplate
    dataTemplate <- dataTemplate()
    
    datapath <- paste0("data/", input$sut, "/",input$kom, "/")
    fileName <- paste0(datapath,"saveDataTemplate.rds")
    dataDefine <- readRDS(fileName)
    
    #### io  ####    
    io.in <-  dataDefine$ioInput
    io.in <- cbind(grup="input",io.in)
    io.out <-  dataDefine$ioOutput
    io.out <- cbind(grup="output",io.out)
    
    io.in[is.na(io.in)] <- 0 #NA replace with zero
    io.out[is.na(io.out)] <- 0
    io.all <- rbind(io.in,io.out) #combine all data input-output
    io.all <- cbind(status="general", io.all) #add variable status
    io.all <- io.all %>% mutate_if(is.factor,as.character) #change factor var to char var
    
    
    yearIO <- ncol(io.in)-4 #banyaknya tahun pada tabel io 
    
    #### price ####
    price.in <-  dataDefine$priceInput
    price.in <- cbind(grup="input",price.in)
    price.out <-  dataDefine$priceOutput
    price.out <- cbind(grup="output",price.out)
    price.in[is.na(price.in)] <- 0
    price.out[is.na(price.out)] <- 0
    price.all <- rbind(price.in, price.out)
    
    p.price<-price.all[-6]
    p.year<-data.frame(replicate(yearIO,p.price$harga.privat)) #replicate nilai private price sebanyak n tahun
    colnames(p.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
    p.price<-cbind(status="harga.privat" ,p.price[c(1:4)],p.year)
    p.price <- p.price %>% mutate_if(is.factor,as.character) #change factor var to char var
    
    s.price<-price.all[-5]
    s.year<-data.frame(replicate(yearIO,s.price$harga.sosial))
    colnames(s.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
    s.price<-cbind(status="harga.sosial",s.price[c(1:4)],s.year)
    s.price <- s.price %>% mutate_if(is.factor,as.character) #change factor var to char
    
    price.all.year <- rbind(p.price, s.price)
    
    #colnames(price.all.year) <- tolower(colnames(price.all.year))
    #colnames(io.all) <- tolower(colnames(io.all))
    #### buat if else untuk modal kapital ####
    
    #### #####
    data.gab <- rbind(io.all, price.all.year) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
    data.gab
  })
  
  hitung.npv<-eventReactive(input$run_button,{
    #observeEvent(input$run_button,{
    #perkalian antara general dan Private Price
    dataGeneral <- filter(data.gab(),status == c("general")) #filter data input output (yg sudah diberi status=general)
    dataPrivat <- filter(data.gab(),status == c("harga.privat")) #filter data private price
    p.budget <- dataGeneral[-(1:5)] * dataPrivat[-c(1:5)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5
    p.budget <- cbind(dataGeneral[1:5],p.budget) #memunculkan kembali variabel 1 sd 5
    p.budget <- p.budget %>%
      mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
    
    #perkalian antara general dengan Social Price
    dataSosial <- filter(data.gab(), status == c("harga.sosial")) #filter data social price
    s.budget <- dataGeneral[-(1:5)] * dataSosial[-c(1:5)]
    s.budget <- cbind(dataGeneral[1:5],s.budget)
    s.budget <- s.budget %>%
      mutate(status = case_when(status == "general" ~ "social budget"))
    
    ################ penghitungan NPV
    p.cost.input <- p.budget %>%
      filter(str_detect(grup,"input"))
    
    s.cost.input <- s.budget %>%
      filter(str_detect(grup,"input"))
    
    p.sum.cost<- p.cost.input[,-(1:5)] %>%
      colSums(na.rm = T)
    s.sum.cost<- s.cost.input[,-(1:5)] %>%
      colSums(na.rm = T)
    
    p.rev.output <- p.budget %>%
      filter(str_detect(grup,"output"))
    s.rev.output <- s.budget %>%
      filter(str_detect(grup,"output"))
    
    p.sum.rev <- p.rev.output[,-(1:5)] %>%
      colSums(na.rm = T)
    s.sum.rev <- s.rev.output[,-(1:5)] %>%
      colSums(na.rm = T)
    
    
    p.profit <- p.sum.rev - p.sum.cost
    s.profit <- s.sum.rev - s.sum.cost
    profit0 <- 0
    p.profit<-c(profit0,p.profit)
    s.profit<-c(profit0,s.profit)
    
    npv.p<-npv(input$rate.p/100,p.profit)
    npv.s<-npv(input$rate.s/100,s.profit)
    
    hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
    
    npv.p.us<-npv.p/input$nilai.tukar
    npv.s.us<-npv.s/input$nilai.tukar
    npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
    hsl.npv<-rbind(hsl.npv,npv.us)
    
    #browser()
    
    rownames(hsl.npv)<-c("NPV (IDR/Ha)", "NPV (US/Ha)")
    hsl.npv
  })
  
  ##npv
  output$npv<- renderPrint({
    hasil<-t(hitung.npv())
    hasil
  })
  
  hitung.nlc<-eventReactive(input$run_button,{
    #perkalian antara general dan Private Price
    dataGeneral <- filter(data.gab(),status == c("general")) #filter data input output (yg sudah diberi status=general)
    dataPrivat <- filter(data.gab(),status == c("harga.privat")) #filter data private price
    p.budget <- dataGeneral[-(1:5)] * dataPrivat[-c(1:5)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5
    p.budget <- cbind(dataGeneral[1:5],p.budget) #memunculkan kembali variabel 1 sd 5
    p.budget <- p.budget %>%
      mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
    
    #perkalian antara general dengan Social Price
    dataSosial <- filter(data.gab(), status == c("harga.sosial")) #filter data social price
    s.budget <- dataGeneral[-(1:5)] * dataSosial[-c(1:5)]
    s.budget <- cbind(dataGeneral[1:5],s.budget)
    s.budget <- s.budget %>%
      mutate(status = case_when(status == "general" ~ "social budget"))
    
    ################ penghitungan NPV
    p.cost.input <- p.budget %>%
      filter(str_detect(grup,"input"))
    
    s.cost.input <- s.budget %>%
      filter(str_detect(grup,"input"))
    
    p.sum.cost<- p.cost.input[,-(1:5)] %>%
      colSums(na.rm = T)
    s.sum.cost<- s.cost.input[,-(1:5)] %>%
      colSums(na.rm = T)
    
    p.tot.cost<- sum(p.sum.cost)
    s.tot.cost<- sum(s.sum.cost)
    
    p.labor.input <- p.budget %>% filter(str_detect(komponen,c("tenaga kerja")))
    s.labor.input <- s.budget %>% filter(str_detect(komponen,c("tenaga kerja")))
    
    p.sum.labor <- p.labor.input[,-(1:5)] %>%
      sum(na.rm = T)
    s.sum.labor <- s.labor.input[,-(1:5)] %>%
      sum(na.rm = T)
    
    nlc.p <- (p.tot.cost - p.sum.labor)/1000000
    nlc.s <- (s.tot.cost - s.sum.labor)/1000000
    nlc<-data.frame(PRIVATE=nlc.p,SOCIAL=nlc.s)
    rownames(nlc)<-c("Non Labor Cost (MRp/Ha)")
    nlc
    
  })
  
  output$nlc<- renderPrint({
    hasil<-t(hitung.nlc())
    hasil
  })
  
  hitung.ec<-eventReactive(input$run_button,{
    #perkalian antara general dan Private Price
    dataGeneral <- filter(data.gab(),status == c("general")) #filter data input output (yg sudah diberi status=general)
    dataPrivat <- filter(data.gab(),status == c("harga.privat")) #filter data private price
    p.budget <- dataGeneral[-(1:5)] * dataPrivat[-c(1:5)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5
    p.budget <- cbind(dataGeneral[1:5],p.budget) #memunculkan kembali variabel 1 sd 5
    p.budget <- p.budget %>%
      mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
    
    #perkalian antara general dengan Social Price
    dataSosial <- filter(data.gab(), status == c("harga.sosial")) #filter data social price
    s.budget <- dataGeneral[-(1:5)] * dataSosial[-c(1:5)]
    s.budget <- cbind(dataGeneral[1:5],s.budget)
    s.budget <- s.budget %>%
      mutate(status = case_when(status == "general" ~ "social budget"))
    
    ################ penghitungan NPV
    p.cost.input <- p.budget %>%
      filter(str_detect(grup,"input"))
    
    s.cost.input <- s.budget %>%
      filter(str_detect(grup,"input"))
    
    p.sum.cost<- p.cost.input[,-(1:5)] %>%
      colSums(na.rm = T)
    s.sum.cost<- s.cost.input[,-(1:5)] %>%
      colSums(na.rm = T)
    
      ############# PERHITUNGAN ESTABLISHMENT COST
      p.ec <- p.sum.cost[[1]]/1000000
      s.ec <- s.sum.cost[[1]]/1000000
      ec <- data.frame(p.ec,s.ec)
      ec<-data.frame(PRIVATE=p.ec,SOCIAL=s.ec)
      rownames(ec)<-c("Establishment cost (1st year only, MRp/ha)")
      ec
  })
  
  output$ec<- renderPrint({
    hasil<-t(hitung.ec())
    hasil
  })
  
  hitung.hp<-eventReactive(input$run_button,{
    
    dataGeneral <- filter(data.gab(),status == c("general")) #filter data input output (yg sudah diberi status=general)
    
    ############# PERHITUNGAN HARVESTING PRODUCT
    fil.prod <- dataGeneral %>%  filter(str_detect(grup,"output")) #filter io untuk grup output (hasil panen)
    sum.prod <- fil.prod[,-(1:5)] %>%
      colSums(na.rm = T)
    tot.prod <- sum(sum.prod)
    
    fil.labor <- dataGeneral %>%  filter(str_detect(komponen, c("tenaga kerja")))
    sum.labor <- fil.labor[,-(1:5)] %>%
      colSums(na.rm = T)
    tot.labor <- sum(sum.labor)
    
    hp <- data.frame(tot.prod/tot.labor)/100
    colnames(hp)<-c("Harvesting Product (ton/HOK) Labor Req for Est (1st year only)")
    rownames(hp) <- c("Value")
    hp
  })
  
  output$hp<- renderPrint({
    hasil<-hitung.hp()
    hasil
  })
  
  hitung.lr<-eventReactive(input$run_button,{
  #observeEvent(input$run_button,{
    dataGeneral <- filter(data.gab(),status == c("general")) #filter data input output (yg sudah diberi status=general)
    
    fil.labor <- dataGeneral %>%  filter(str_detect(komponen, c("tenaga kerja")))
    sum.labor <- fil.labor[,-(1:5)] %>%
      colSums(na.rm = T)
    
    ############# PERHITUNGAN LABOR REQ FOR EST
    lr <- data.frame(sum.labor[[1]]) #pekerja pada tahun 1
    colnames(lr)<-c("Labor Req for Est (1st year only)")
    rownames(lr) <- c("Value")
    lr
    
    #browser()
  })
  
  output$lr<- renderPrint({
    hasil<-hitung.lr()
    hasil
  })
  
  # hitung.all<-eventReactive(input$run_button,{
  # #observeEvent(input$run_button,{
  #   #browser()
  #   
  #   #### read file ####
  #   datapath <- paste0("data/", input$sut, "/",input$kom, "/")
  #   fileName <- paste0(datapath,"saveDataTemplate.rds")
  #   dataDefine <- readRDS(fileName)
  #   
  #   #### io  ####    
  #   io.in <-  dataDefine$ioInput
  #   io.in <- cbind(grup="input",io.in)
  #   io.out <-  dataDefine$ioOutput
  #   io.out <- cbind(grup="output",io.out)
  # 
  #   io.in[is.na(io.in)] <- 0 #NA replace with zero
  #   io.out[is.na(io.out)] <- 0
  #   io.all <- rbind(io.in,io.out) #combine all data input-output
  #   io.all <- cbind(status="general", io.all) #add variable status
  #   io.all <- io.all %>% mutate_if(is.factor,as.character) #change factor var to char var
  #   
  #   
  #   yearIO <- ncol(io.in)-4 #banyaknya tahun pada tabel io 
  #   
  #   #### price ####
  #   price.in <-  dataDefine$priceInput
  #   price.in <- cbind(grup="input",price.in)
  #   price.out <-  dataDefine$priceOutput
  #   price.out <- cbind(grup="output",price.out)
  #   price.in[is.na(price.in)] <- 0
  #   price.out[is.na(price.out)] <- 0
  #   price.all <- rbind(price.in, price.out)
  # 
  #   p.price<-price.all[-6]
  #   p.year<-data.frame(replicate(yearIO,p.price$harga.privat)) #replicate nilai private price sebanyak n tahun
  #   colnames(p.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
  #   p.price<-cbind(status="harga.privat" ,p.price[c(1:4)],p.year)
  #   p.price <- p.price %>% mutate_if(is.factor,as.character) #change factor var to char var
  # 
  #   s.price<-price.all[-5]
  #   s.year<-data.frame(replicate(yearIO,s.price$harga.sosial))
  #   colnames(s.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
  #   s.price<-cbind(status="harga.sosial",s.price[c(1:4)],s.year)
  #   s.price <- s.price %>% mutate_if(is.factor,as.character) #change factor var to char
  # 
  #   price.all.year <- rbind(p.price, s.price)
  #   
  #   #colnames(price.all.year) <- tolower(colnames(price.all.year))
  #   #colnames(io.all) <- tolower(colnames(io.all))
  #   #### buat if else untuk modal kapital ####
  #   
  #   #### #####
  #   data.gab <- rbind(io.all, price.all.year) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
  #   
  #   #perkalian antara general dan Private Price
  #   dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
  #   dataPrivat <- filter(data.gab,status == c("harga.privat")) #filter data private price
  #   p.budget <- dataGeneral[-(1:5)] * dataPrivat[-c(1:5)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5
  #   p.budget <- cbind(dataGeneral[1:5],p.budget) #memunculkan kembali variabel 1 sd 5
  #   p.budget <- p.budget %>%
  #     mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
  # 
  #   #perkalian antara general dengan Social Price
  #   dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
  #   s.budget <- dataGeneral[-(1:5)] * dataSosial[-c(1:5)]
  #   s.budget <- cbind(dataGeneral[1:5],s.budget)
  #   s.budget <- s.budget %>%
  #     mutate(status = case_when(status == "general" ~ "social budget"))
  # 
  #   #penggabungan dengan data capital
  #   # p.cap <- filter(data.gab, status == c("private budget"))
  #   # p.budget <- rbind(p.budget,p.cap)
  #   # 
  #   # s.cap <- filter(data.gab, status == c("social budget"))
  #   # s.budget <- rbind(s.budget,s.cap)
  # 
  #   ################ penghitungan NPV
  #   p.cost.input <- p.budget %>%
  #     filter(str_detect(grup,"input"))
  # 
  #   s.cost.input <- s.budget %>%
  #     filter(str_detect(grup,"input"))
  # 
  #   p.sum.cost<- p.cost.input[,-(1:5)] %>%
  #     colSums(na.rm = T)
  #   s.sum.cost<- s.cost.input[,-(1:5)] %>%
  #     colSums(na.rm = T)
  # 
  #   p.rev.output <- p.budget %>%
  #     filter(str_detect(grup,"output"))
  #   s.rev.output <- s.budget %>%
  #     filter(str_detect(grup,"output"))
  # 
  #   p.sum.rev <- p.rev.output[,-(1:5)] %>%
  #     colSums(na.rm = T)
  #   s.sum.rev <- s.rev.output[,-(1:5)] %>%
  #     colSums(na.rm = T)
  # 
  # 
  #   p.profit <- p.sum.rev - p.sum.cost
  #   s.profit <- s.sum.rev - s.sum.cost
  #   profit0 <- 0
  #   p.profit<-c(profit0,p.profit)
  #   s.profit<-c(profit0,s.profit)
  # 
  #   npv.p<-npv(input$rate.p/100,p.profit)
  #   npv.s<-npv(input$rate.s/100,s.profit)
  # 
  #   hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
  # 
  #   npv.p.us<-npv.p/input$nilai.tukar
  #   npv.s.us<-npv.s/input$nilai.tukar
  #   npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
  #   hsl.npv<-rbind(hsl.npv,npv.us)
  #   rownames(hsl.npv)<-c("NPV (IDR/Ha)", "NPV (US/Ha)")
  #   #hsl.npv
  # 
  #   p.tot.cost<- sum(p.sum.cost)
  #   s.tot.cost<- sum(s.sum.cost)
  # 
  #   p.labor.input <- p.budget %>% filter(str_detect(komponen,c("tenaga kerja")))
  #   s.labor.input <- s.budget %>% filter(str_detect(komponen,c("tenaga kerja")))
  # 
  #   p.sum.labor <- p.labor.input[,-(1:5)] %>%
  #     sum(na.rm = T)
  #   s.sum.labor <- s.labor.input[,-(1:5)] %>%
  #     sum(na.rm = T)
  # 
  #   nlc.p <- (p.tot.cost - p.sum.labor)/1000000
  #   nlc.s <- (s.tot.cost - s.sum.labor)/1000000
  #   nlc<-data.frame(PRIVATE=nlc.p,SOCIAL=nlc.s)
  #   rownames(nlc)<-c("Non Labor Cost (MRp/Ha)")
  #   #nlc
  # 
  # 
  #   ############# PERHITUNGAN ESTABLISHMENT COST
  #   ec <- p.sum.cost[[1]]
  # 
  # 
  #   ############# PERHITUNGAN HARVESTING PRODUCT
  #   fil.prod <- dataGeneral %>%  filter(str_detect(grup,"output")) #filter io untuk grup output (hasil panen)
  #   sum.prod <- fil.prod[,-(1:5)] %>%
  #     colSums(na.rm = T)
  #   tot.prod <- sum(sum.prod)
  # 
  #   fil.labor <- dataGeneral %>%  filter(str_detect(komponen, c("tenaga kerja")))
  #   sum.labor <- fil.labor[,-(1:5)] %>%
  #     colSums(na.rm = T)
  #   tot.labor <- sum(sum.labor)
  # 
  #   hp <- tot.prod/tot.labor
  # 
  # 
  #   ############# PERHITUNGAN LABOR REQ FOR EST
  #   lr <- sum.labor[[1]] #pekerja pada tahun 1
  # 
  #   #### buat dataframe summary
  #   summ.npv <- data.frame(hsl.npv[1,1],hsl.npv[1,2],hsl.npv[2,1],hsl.npv[2,2])
  #   summ.nlc <- data.frame(nlc[1,1],nlc[1,2])
  #   data.summary <- data.frame(summ.npv,
  #                              summ.nlc,
  #                              ec,hp,lr)
  #   colnames(data.summary) <- c("NPV (Rp/Ha) Privat","NPV (Rp/Ha) Sosial",
  #                               "NPV (US/Ha) Privat","NPV (US/Ha) Sosial",
  #                               "Non Labor Cost (Juta Rp/Ha) Privat","Non Labor Cost (Juta Rp/Ha) Sosial",
  #                               "Establishment Cost",
  #                               "Harvesting Product",
  #                               "Labor Req for Est")
  #   rownames(data.summary) <- "value"
  #   data.summary <- t(data.summary)
  #   return(data.summary)
  # 
  # })
  # 
  # output$hasil<- renderPrint({
  #   hitung.all()
  # })
  
  
  output$viewPrice <- renderDataTable({
    if(!is.null(data.gab())){
      dataView <- rbind(dataTemplate()$priceInput, dataTemplate()$priceOutput)
      dataView    
    }else if(!is.null(valP1()) | !is.null(valP2())){
      dataView <- rbind(valP1(),valP2())
      dataView}
  })

  output$viewIO <- renderDataTable({

    
    if(!is.null(data.gab())){
      dataView <- rbind(dataTemplate()$ioInput, dataTemplate()$ioOutput)
      dataView    
    }else if(!is.null(valP1()) | !is.null(valP2())){
      dataView <- rbind(valIO1(),valIO2())
      dataView}
  })
  
}