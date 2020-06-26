examplePrice <- read.table("data/template/price template.csv", header = T, sep = ",")
exampleIO <- read.table("data/template/io template.csv", header = T, sep = ",")


buttonUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("modalPriceButton"),'Harga'),
    actionButton(ns("modalIOButton"),'Input-Output'),
    br(),
    br(),
    br(),
    checkboxInput(ns("checkKapital"),'modal kapital dimasukkan kedalam perhitungan'),
    actionButton(ns("modalCapitalButton"),'Modal Kapital'),
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
      tabName = "Table Price",
      active = T,
      dataTableOutput(ns("viewPrice")),
      style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
    ),
    argonTab(
      tabName = "Table Input-Output",
      active = F,
      dataTableOutput(ns("viewIO")),
      style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
    )
  )
}

buttonModule <- function(input, output, session) {
  
  # load the namespace
  ns <- session$ns
  
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
                                       choices = c(1:50),selected = 10)
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
  

# Start template data ketika peneliti memulai skenario ------------------

  # ({
  #   exampleData <- read.table("data/template/contoh.csv", header = T, sep = ",")
  # })
  
  # dataTemplate <- reactive({
  #   #exampleData <- read.table("data/template/contoh.csv", header = T, sep = ",")
  #   priceInput <- examplePrice
  #   priceOutput <- examplePrice
  #   ioInput <- exampleIO
  #   ioOutput <- exampleIO
  # 
  #   capitalPrivate <- NULL
  #   capitalSocial <- NULL
  # 
  #   combineDef <- list(priceInput=priceInput,priceOutput=priceOutput,ioInput=ioInput,
  #                      capitalPrivate=capitalPrivate,
  #                      capitalSocial=capitalSocial)
  # 
  #   combineDef
  #   fileName <- c("data/template/profitabilityAnalysis.rds")
  #   saveRDS(combineDef,file = fileName)
  # })
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
    #indexCol <- as.numeric(input$priceYear_input)+3
    indexRow <- as.numeric(input$priceComp_input)
    reactData$tableP1 <- examplePrice
    reactData$tableP1[,1] <- examplePrice[1,1]
    reactData$tableP1[,2] <- examplePrice[1,2]
    reactData$tableP1[,3] <- examplePrice[1,3]
    reactData$tableP1 <- reactData$tableP1[1:indexRow,]
    reactData$tableP1
  })
  
  output$editPriceInput <- renderRHandsontable({
    rhandsontable(valP1(),
                  rowHeaderWidth = 50,
                  fixedColumnsLeft = 3,
                  height = 500,
    )
  })
  
  observeEvent(input$savePriceInput,{
    removeUI(selector='#textTampil')
    
    editNew<-as.data.frame(hot_to_r(input$editPriceInput))
    editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
    
    fileName <- c("data/template/profitabilityAnalysis.rds")
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
    reactData$tableP2[,1] <- examplePrice[nrow(examplePrice),1]
    reactData$tableP2[,2] <- examplePrice[nrow(examplePrice),2]
    reactData$tableP2[,3] <- examplePrice[nrow(examplePrice),3]
    reactData$tableP2 <- reactData$tableP2[1:indexRow,]
    reactData$tableP2
  })
  
  output$editPriceOutput <- renderRHandsontable({
    rhandsontable(valP2(),
                  rowHeaderWidth = 50,
                  fixedColumnsLeft = 3,
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
                                       choices = c(1:50),selected = 10)
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
                                       choices = c(1:10),selected = 2)
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
    reactData$tableIO1 <- exampleIO
    reactData$tableIO1[,1] <- exampleIO[1,1]
    reactData$tableIO1[,2] <- exampleIO[1,2]
    reactData$tableIO1[,3] <- exampleIO[1,3]
    reactData$tableIO1 <- reactData$tableIO1[1:indexRow,c(1:3,4:(indexCol))]
    reactData$tableIO1
  })
  
  output$editIOInput <- renderRHandsontable({
    rhandsontable(valIO1(),
                  rowHeaderWidth = 50,
                  fixedColumnsLeft = 3,
                  height = 500,
    )
  })
  
  observeEvent(input$saveIOInput,{
    removeUI(selector='#textTampil')
    
    editNew<-as.data.frame(hot_to_r(input$editIOInput))
    editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
    
    fileName <- c("data/template/profitabilityAnalysis.rds")
    dataDefine <- readRDS(fileName)
    dataDefine$priceInput <- editNew
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
    reactData$tableIO2 <- exampleIO
    reactData$tableIO2[,1] <- exampleIO[nrow(exampleIO),1]
    reactData$tableIO2[,2] <- exampleIO[nrow(exampleIO),2]
    reactData$tableIO2[,3] <- exampleIO[nrow(exampleIO),3]
    reactData$tableIO2 <- reactData$tableIO2[1:indexRow,c(1:3,4:(indexCol))]
    reactData$tableIO2
  })
  
  output$editIOOutput <- renderRHandsontable({
    rhandsontable(valIO2(),
                  rowHeaderWidth = 50,
                  fixedColumnsLeft = 3,
                  height = 300,
    )
  })
  
  observeEvent(input$saveIOOutput,{
    removeUI(selector='#textTampil')
    
    editNew<-as.data.frame(hot_to_r(input$editIOOutput))
    editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
    
    fileName <- c("data/template/profitabilityAnalysis.rds")
    dataDefine <- readRDS(fileName)
    dataDefine$priceInput <- editNew
    saveRDS(dataDefine,file = fileName)
    
    insertUI(selector='#teksIOOutputSave',
             where = 'afterEnd',
             ui = tags$div(id="textTampil","tabel di atas sudah tersimpan"))
  })
  # Ending IO Output -----------------------------------------------------
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