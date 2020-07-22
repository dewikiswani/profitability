#setwd("C:/dw/ICRAF/profitability/shiny")

library(shiny)
library(argonR)
library(argonDash)
library(magrittr)
library(shiny)

#package function npv
library(FinCal)
library(readxl)
#spy data price yg ada nilainya bukan hanya numeric bisa terbaca yaitu pada kolom 1
library(DT)
library(dplyr)
library(stringr)
library(tidyverse)
library(rhandsontable)
library(shinyWidgets)
library(shinyalert)


# template
source("shiny/sidebar.R")
source("shiny/navbar.R")
source("shiny/header.R")
source("shiny/footer.R")

# input file
komoditas <- read.csv("shiny/data/template/komoditas.csv", stringsAsFactors = F)
indonesia <- read.csv("shiny/data/template/prov sampai desa.csv", stringsAsFactors = F)

# elements
source("shiny/home.R")
source("shiny/upload modal dialog.R")
source("shiny/analisis.R")


# App
app <- shiny::shinyApp(
  ui = argonDashPage(
    title = "Argon Dashboard Demo",
    author = "Dewi Kiswani Bodro",
    description = "ICRAF",
    sidebar = argonSidebar,
    navbar = argonNav, 
    header = argonHeader,
    body = argonDashBody(
      argonTabItems(
        #home,
        upload,
        analisis
      )
    ),
    footer = argonFooter
  )
  ,
  server = function(input, output,session) {
    
    ##kumpulan fungsi
    lowcase <- function(data, index.col){
      for(k in index.col){
        data[,k] <- tolower(data[,k])
      }
      return(data) 
    }
  
    
    # # Section 1 -- Upload File ----------------------------------------------
    #browser()
    #data price
    # data.1 <- eventReactive(input$simulate,{
    #   read.csv("data/template/oilpalm_price_in.csv", header = T, sep = ",")
    #   # inFile <- input$file.1
    #   # if (is.null(inFile)){
    #   #   stop("Harga Input harus dimasukkan") 
    #   # }else{
    #   #   read.csv(inFile$datapath)#ganti read.csv jika delimiter excelnya ,
    #   # }
    # })
    # 
    # data.2 <- eventReactive(input$simulate,{
    #   read.csv("data/template/oilpalm_price_out.csv", header = T, sep = ",")
    #   # inFile <- input$file.2
    #   # if (is.null(inFile)){
    #   #   stop("Harga Output harus dimasukkan") 
    #   # }else{
    #   #   read.csv(inFile$datapath)#ganti read.csv jika delimiter excelnya ,
    #   # }
    # })
    # 
    # #data io
    # data.3 <- eventReactive(input$simulate,{
    #   read.csv("data/template/oilpalm_io_in.csv", header = T, sep = ",")
    #   # inFile <- input$file.3
    #   # if (is.null(inFile)){
    #   #   stop("I-O Input harus dimasukkan") 
    #   # }else{
    #   #   read.csv(inFile$datapath) #ganti read.csv jika delimiter excelnya ,
    #   # }
    # })
    # 
    # data.4 <- eventReactive(input$simulate,{
    #   read.csv("data/template/oilpalm_io_out.csv", header = T, sep = ",")
    #   # inFile <- input$file.4
    #   # if (is.null(inFile)){
    #   #   stop("IO-Output harus dimasukkan") 
    #   # }else{
    #   #   read.csv(inFile$datapath) #ganti read.csv jika delimiter excelnya ,
    #   # }
    # })
    # 
    # #data capital
    # data.5 <- eventReactive(input$simulate,{
    #   read.csv("data/template/oilpalm_capital_p.csv", header = T, sep = ",")
    #   # inFile <- input$file.5
    #   # if (is.null(inFile)) return(NULL)
    #   # read.csv(inFile$datapath)
    # })
    # 
    # data.6 <- eventReactive(input$simulate,{
    #   read.csv("data/template/oilpalm_capital_s.csv", header = T, sep = ",")
    #   # inFile <- input$file.6
    #   # if (is.null(inFile)) return(NULL)
    #   # read.csv(inFile$datapath)
    # })
    
    
    # Section 2 -- variabel input ---------------------------------------------

    
    
    observe({
      updateSelectInput(
        session,
        "kom",
        choices = komoditas %>%
          filter(sut == input$sut) %>%
          select(nama_komoditas) %>%
          .[[1]]
      )
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

    
    dataTemplate <- reactive({
    # observeEvent(input$kom,{
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      
      ioInput <- read.table(paste0(datapath,"io template input.csv"), header = T, sep = ",")
      ioOutput <- read.table(paste0(datapath,"io template output.csv"), header = T, sep = ",")
      
      priceInput <- read.table(paste0(datapath,"price template input.csv"), header = T, sep = ",")
      priceOutput <- read.table(paste0(datapath,"price template output.csv"), header = T, sep = ",")
      
      # case for modal kapital
      if (input$checkKapital == F){
        # capitalPrivate <- NULL
        # capitalSocial <- NULL
        capital <- NULL
      } else if(input$checkKapital == T){
        # capitalPrivate <- read.table(paste0(datapath,"kapital template privat.csv"), header = T, sep = ",")
        # capitalSocial <- read.table(paste0(datapath,"kapital template sosial.csv"), header = T, sep = ",")
        capital <- read.table(paste0(datapath,"kapital template.csv"), header = T, sep = ",")
      }
      
      combineDef <- list(ioInput=ioInput,ioOutput=ioOutput,
                         priceInput=priceInput,priceOutput=priceOutput,
                         # capitalPrivate=capitalPrivate,
                         # capitalSocial=capitalSocial,
                         capital=capital)
      
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveDataTemplate.rds")
      saveRDS(combineDef,file = fileName)
      
      print("pertama kali masuk/login. cek save data default")
      combineDef
      
    })

    
    # readDataTemplate <- eventReactive(input$modalIOButton,{
    #   datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
    #   fileName <- paste0(datapath,"saveDataTemplate.rds")
    #   print("baca data hasil edit")
    #   readRDS(fileName)
    # })
    
    readDataLastEdited <- eventReactive(input$run_button,{
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveDataTemplate.rds")
      # print("data terakhir tersimpan di rds")
      readRDS(fileName)
    })
    
    
    templateIOInput <- reactive({
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      print(datapath)
      readData <- read.table(paste0(datapath,"io template input.csv"), header = T, sep = ",")
      readData
    })
    
    templateIOOutput <- reactive({
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
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
            actionButton(("closeModalIO"), "Tutup")
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
                             selectInput(("ioYear_input"),
                                         "pilih tahun skenario:",
                                         choices = c(1:30),selected = 30)
                      ),
                      column(6,
                             selectInput(("ioComp_input"),
                                         "banyaknya komponen:",
                                         choices = c(1:50),selected = nrow(templateIOInput()))
                      )
                    )
                  ),
                  tags$br(),
                  actionButton(("ioInputHit"),"tampilkan tabel"),
                  width=12
                ),
                mainPanel(
                  tags$div(id = ('ioInputPlaceholder')),
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
                             selectInput(("ioYear_output"),
                                         "pilih tahun skenario:",
                                         choices = c(2:30),selected = 30)
                      ),
                      column(6,
                             selectInput(("ioComp_output"),
                                         "banyaknya komponen:",
                                         choices = c(1:10),selected = nrow(templateIOOutput()))
                      )
                    )
                  ),
                  tags$br(),
                  actionButton(("ioOutputHit"),"tampilkan tabel"),
                  width=12
                ),
                mainPanel(
                  tags$div(id = ('ioOutputPlaceholder')),
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
      insertUI(selector= paste0("#", ("ioInputPlaceholder")),
               where='afterEnd',
               ui= uiOutput(('ioInputUI'))
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
        rHandsontableOutput(('editIOInput')),
        tags$br(),
        actionButton(('saveIOInput'), 'simpan tabel'), 
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
      # browser()
      removeUI(selector='#textTampil')
      
      editNew<-as.data.frame(hot_to_r(input$editIOInput))
      editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
      
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveDataTemplate.rds")
      dataDefine <- readRDS(fileName)
      # dataDefine <- readDataLastEdited()
      
      # replace data IO
      dataDefine$ioInput <- editNew
      
      
      # replace data price
      indexRow <- as.numeric(nrow(dataDefine$ioInput))
      
      reactData$tableP1 <- dataDefine$ioInput[,1:2]
      no.id <- as.numeric(rownames(reactData$tableP1))
      reactData$tableP1 <- cbind(no.id,reactData$tableP1)
      
      templatePriceInput <- read.table(paste0(datapath,"price template input.csv"), header = T, sep = ",")
      templatePrice <- (templatePriceInput[,-1])
      reactData$tableP1 <- merge(reactData$tableP1,unique(templatePrice), by.x = "keterangan",by.y = "keterangan", all.x = T)
      reactData$tableP1 <- reactData$tableP1[order(reactData$tableP1$no.id),]     #sort by nomor yang disesuaikan pada tabel i-o
      rownames(reactData$tableP1) <- no.id
      reactData$tableP1 <- reactData$tableP1[, c(3, 1, 4, 5,6)]
      dataDefine$priceInput <- reactData$tableP1
        
      # save io (baru) dan price (default tanpa ada perubahan)  
      saveRDS(dataDefine,file = fileName)
      
      insertUI(selector='#teksIOInputSave',
               where = 'afterEnd',
               ui = tags$div(id="textTampil","tabel di atas sudah tersimpan"))
    })
    # ending IO Input -----------------------------------------------------------------
    
    
    # Start IO Output ------------------------------------------------------
    observeEvent(input$ioOutputHit, {
      insertUI(selector= paste0("#", ("ioOutputPlaceholder")),
               where='afterEnd',
               ui= uiOutput(('ioOutputUI'))
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
        rHandsontableOutput(('editIOOutput')),
        tags$br(),
        actionButton(('saveIOOutput'), 'simpan tabel'), 
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
      
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveDataTemplate.rds")
      dataDefine <- readRDS(fileName)
      # dataDefine <- readDataLastEdited()
      
      # replace data IO
      dataDefine$ioOutput <- editNew
      
      # replace data price
      indexRow <- as.numeric(nrow(dataDefine$ioOutput))
      
      reactData$tableP2 <- dataDefine$ioOutput[,1:2]
      no.id <- as.numeric(rownames(reactData$tableP2))
      reactData$tableP2 <- cbind(no.id,reactData$tableP2)
      
      templatePriceOutput <- read.table(paste0(datapath,"price template output.csv"), header = T, sep = ",")
      templatePrice <- (templatePriceOutput[,-1])
      reactData$tableP2 <- merge(reactData$tableP2,unique(templatePrice), by.x = "keterangan",by.y = "keterangan", all.x = T)
      reactData$tableP2 <- reactData$tableP2[order(reactData$tableP2$no.id),]     #sort by nomor yang disesuaikan pada tabel i-o
      rownames(reactData$tableP2) <- no.id
      reactData$tableP2 <- reactData$tableP2[, c(3, 1, 4, 5,6)]
      dataDefine$priceOutput <- reactData$tableP2
      
      # save io (baru) dan price (default tanpa ada perubahan) 
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
            actionButton(("closeModalPrice"), "Tutup")
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
                  ),
                  tags$br(),
                  actionButton(("priceInputHit"),"tampilkan tabel"),
                  width=12
                ),
                mainPanel(
                  tags$div(id = ('priceInputPlaceholder')),
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
                  ),
                  tags$br(),
                  actionButton(("priceOutputHit"),"tampilkan tabel"),
                  width=12
                ),
                mainPanel(
                  tags$div(id = ('priceOutputPlaceholder')),
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
      insertUI(selector= paste0("#", ("priceInputPlaceholder")),
               where='afterEnd',
               ui= uiOutput(('priceInputUI'))
      )
    })
    
    output$priceInputUI<- renderUI({
      
      tagList(
        #tags$br(),
        tags$hr(),
        tags$b('Sunting secara manual'),
        tags$h5("kolom yang dapat di sunting hanya kolom harga.privat dan harga.sosial"),
        tags$br(),
        rHandsontableOutput(('editPriceInput')),
        tags$br(),
        actionButton(('savePriceInput'), 'simpan tabel'),
        tags$br(),
        tags$br(),
        tags$div(id='teksPriceInputSave')
      )
    })
    
    valP1 <- eventReactive(input$priceInputHit,{
      #browser()
      indexRow <- as.numeric(nrow(readDataLastEdited()$ioInput))
      
      reactData$tableP1 <- readDataLastEdited()$ioInput[,1:2]
      no.id <- as.numeric(rownames(reactData$tableP1))
      reactData$tableP1 <- cbind(no.id,reactData$tableP1)
      
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
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
      
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveDataTemplate.rds")
      dataDefine <- readRDS(fileName)
      # dataDefine <- readDataLastEdited()
      
      # replace data price
      dataDefine$priceInput <- editNew
      saveRDS(dataDefine,file = fileName)
      
      insertUI(selector='#teksPriceInputSave',
               where = 'afterEnd',
               ui = tags$div(id="textTampil","tabel di atas sudah tersimpan"))
    })
    # ending Price Input -----------------------------------------------------------------
    
    
    # Start Price Output ------------------------------------------------------
    observeEvent(input$priceOutputHit, {
      insertUI(selector= paste0("#", ("priceOutputPlaceholder")),
               where='afterEnd',
               ui= uiOutput(('priceOutputUI'))
      )
    })
    
    output$priceOutputUI<- renderUI({
      
      tagList(
        tags$hr(),
        tags$b('Sunting secara manual'),
        tags$h5("kolom yang dapat di sunting hanya kolom harga.privat dan harga.sosial"),
        tags$br(),
        rHandsontableOutput(('editPriceOutput')),
        tags$br(),
        actionButton(('savePriceOutput'), 'simpan tabel'), 
        tags$br(), 
        tags$br(),
        tags$div(id='teksPriceOutputSave')
      )
    })
    
    valP2 <- eventReactive(input$priceOutputHit,{
      # browser()
      #indexCol <- as.numeric(input$priceYear_output)+3
      indexRow <- as.numeric(nrow(readDataLastEdited()$ioOutput))
      
      reactData$tableP2 <- readDataLastEdited()$ioOutput[,1:2]
      no.id <- as.numeric(rownames(reactData$tableP2))
      reactData$tableP2 <- cbind(no.id,reactData$tableP2)
      
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
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
      
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveDataTemplate.rds")
      dataDefine <- readRDS(fileName)
      # dataDefine <- readDataLastEdited()
      
      # replace data price
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
      # agar ketika run pertama kali yang terbaca tetap data default di excel
      dataTemplate <- dataTemplate()
      
      #setelah dataTemplate(data default) aktif, 
      # lalu read kembali file rds yang tersimpan dr hasil edit jika ada yang diedit
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveDataTemplate.rds")
      dataDefine <- readRDS(fileName)
      # dataDefine <- readDataLastEdited()
      
      # kasih case jika beda rownya walau sbenrnya sudah di tanggulangi saat save tabel io
      if (nrow(dataDefine$ioInput) != nrow(dataDefine$priceInput) | nrow(dataDefine$ioOutput) != nrow(dataDefine$priceOutput)) {
        shinyalert("Error!", "Silahkan tekan modal dialog untuk harga lalu simpan file yang sudah disesuaikan dengan komponen baru yang ditambahkan pada tabel I-O.", type = "error")
      }else if (nrow(dataDefine$ioInput) == nrow(dataDefine$priceInput) & nrow(dataDefine$ioOutput) == nrow(dataDefine$priceOutput)){
        
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
      }
      
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
      # if (nrow(readDataLastEdited()$ioInput) != nrow(readDataLastEdited()$priceInput) | nrow(readDataLastEdited()$ioOutput) != nrow(readDataLastEdited()$priceOutput)) {
      #   print("simpan tabel harga yang baru")
      # }else if (nrow(readDataLastEdited()$ioInput) == nrow(readDataLastEdited()$priceInput) & nrow(readDataLastEdited()$ioOutput) == nrow(readDataLastEdited()$priceOutput)){
      #   hasil<-t(hitung.npv())
      #   hasil
      # }
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
      
      # p.sum.labor <- p.labor.input[,-(1:5)] %>%
      #   colSums(na.rm = T)
      # s.sum.labor <- s.labor.input[,-(1:5)] %>%
      #   colSums(na.rm = T)
      
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
      # if (nrow(readDataLastEdited()$ioInput) != nrow(readDataLastEdited()$priceInput) | nrow(readDataLastEdited()$ioOutput) != nrow(readDataLastEdited()$priceOutput)) {
      #   print("simpan tabel harga yang baru")
      # }else if (nrow(readDataLastEdited()$ioInput) == nrow(readDataLastEdited()$priceInput) & nrow(readDataLastEdited()$ioOutput) == nrow(readDataLastEdited()$priceOutput)){
      #   hasil<-t(hitung.nlc())
      #   hasil
      # }
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
      # if (nrow(readDataLastEdited()$ioInput) != nrow(readDataLastEdited()$priceInput) | nrow(readDataLastEdited()$ioOutput) != nrow(readDataLastEdited()$priceOutput)) {
      #   print("simpan tabel harga yang baru")
      # }else if (nrow(readDataLastEdited()$ioInput) == nrow(readDataLastEdited()$priceInput) & nrow(readDataLastEdited()$ioOutput) == nrow(readDataLastEdited()$priceOutput)){
      #   hasil<-t(hitung.ec())
      #   hasil
      # }
    })
    
    hitung.hp<-eventReactive(input$run_button,{
    # observeEvent(input$run_button,{
      # browser()
      dataGeneral <- filter(data.gab(),status == c("general")) #filter data input output (yg sudah diberi status=general)
      
      ############# PERHITUNGAN HARVESTING PRODUCT
      fil.prod <- dataGeneral %>%  filter(str_detect(grup,"output")) #filter io untuk grup output (hasil panen)
      fil.prod <- fil.prod %>%  filter(str_detect(komponen,"utama"))
      sum.prod <- fil.prod[,-(1:5)] %>%
        colSums(na.rm = T)
      tot.prod <- sum(sum.prod)
      
      fil.labor <- dataGeneral %>%  filter(str_detect(komponen, c("tenaga kerja")))
      sum.labor <- fil.labor[,-(1:5)] %>%
        colSums(na.rm = T)
      tot.labor <- sum(sum.labor)
      
      hp <- data.frame(tot.prod/tot.labor)/1000 # karena ton jadi di bagi 1000
      colnames(hp)<-c("Harvesting Product (ton/HOK) Labor Req for Est (1st year only)")
      rownames(hp) <- c("Value")
      hp
    })
    
    output$hp<- renderPrint({
      hasil<-hitung.hp()
      hasil
      # if (nrow(readDataLastEdited()$ioInput) != nrow(readDataLastEdited()$priceInput) | nrow(readDataLastEdited()$ioOutput) != nrow(readDataLastEdited()$priceOutput)) {
      #   print("simpan tabel harga yang baru")
      # }else if (nrow(readDataLastEdited()$ioInput) == nrow(readDataLastEdited()$priceInput) & nrow(readDataLastEdited()$ioOutput) == nrow(readDataLastEdited()$priceOutput)){
      #   
      #   hasil<-hitung.hp()
      #   hasil
      # }
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
      # if (nrow(readDataLastEdited()$ioInput) != nrow(readDataLastEdited()$priceInput) | nrow(readDataLastEdited()$ioOutput) != nrow(readDataLastEdited()$priceOutput)) {
      #   print("validasi dan simpan tabel harga yang baru")
      # }else if (nrow(readDataLastEdited()$ioInput) == nrow(readDataLastEdited()$priceInput) & nrow(readDataLastEdited()$ioOutput) == nrow(readDataLastEdited()$priceOutput)){
      #   hasil<-hitung.lr()
      #   hasil
      # }
    })
    
    output$viewPrice <- renderDataTable({
      if(!is.null(data.gab())){
        dataView <- rbind(readDataLastEdited()$priceInput, readDataLastEdited()$priceOutput)
        dataView    
      }else if(!is.null(valP1()) | !is.null(valP2())){
        dataView <- rbind(valP1(),valP2())
        dataView
      }
      # else if (nrow(readDataLastEdited()$ioInput) != nrow(readDataLastEdited()$priceInput) | nrow(readDataLastEdited()$ioOutput) != nrow(readDataLastEdited()$priceOutput)) {
      #   print("validasi dan simpan tabel harga yang baru (jumlah baris komponen i-o tidak sama dengan komponen harga)")
      # }
    })
    
    output$viewIO <- renderDataTable({
      if(!is.null(data.gab())){
        dataView <- rbind(readDataLastEdited()$ioInput, readDataLastEdited()$ioOutput)
        dataView    
      }else if(!is.null(valP1()) | !is.null(valP2())){
        dataView <- rbind(valIO1(),valIO2())
        dataView
      }
      # else if (nrow(readDataLastEdited()$ioInput) != nrow(readDataLastEdited()$priceInput) | nrow(readDataLastEdited()$ioOutput) != nrow(readDataLastEdited()$priceOutput)) {
      #   print("validasi dan simpan tabel harga yang baru (jumlah baris komponen i-o tidak sama dengan komponen harga)")
      # }
    })
    
  }
)

runApp(app)
