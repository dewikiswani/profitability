
#setwd("C:/dw/ICRAF/profitability/shiny")
# C:/dw/ICRAF/project R/theme 2/profitability/shiny/data/template

library(shiny)
library(argonR)
library(argonDash)
library(magrittr)

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

# untuk tab analisis
library(shinydashboard)
library(data.table)
library(DT)

library(ggplot2)
library(cowplot) #ggplot2 white theme 
library(plotly)


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
source("shiny/pambaru_modul2.R")
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
        # verifikasi,
        pamBaru
        ,
        deskriptifPlot
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
    
    # Start template data ---------------------------------------------------- 
    dataTemplate <- reactive({
      removeUI(selector='#statusCapital')
      
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      
      ioInput <- read.table(paste0(datapath,"io template input.csv"), header = T, sep = ",")
      ioOutput <- read.table(paste0(datapath,"io template output.csv"), header = T, sep = ",")
      
      priceInput <- read.table(paste0(datapath,"price template input.csv"), header = T, sep = ",")
      priceOutput <- read.table(paste0(datapath,"price template output.csv"), header = T, sep = ",")
      
      # case for modal kapital
      cekCapital <- file.exists(paste0(datapath,"kapital template.csv")) #cek keberadaan file ini ada atau engga
      
      if (cekCapital == T & input$checkKapital == F ){
        capital <- NULL
        insertUI(selector='#teksStatusCapital',
                 where = 'afterEnd',
                 ui = tags$div(id="statusCapital","Terdapat tabel modal kapital (tidak dimasukkan ke dalam perhitungan)"))
      } else if(cekCapital == T & input$checkKapital == T){
        capital <- read.table(paste0(datapath,"kapital template.csv"), header = T, sep = ",")
        insertUI(selector='#teksStatusCapital',
                 where = 'afterEnd',
                 ui = tags$div(id="statusCapital","Terdapat tabel modal kapital (dimasukkan kedalam perhitungan)"))
      } else if (cekCapital == F & input$checkKapital == F){
        capital <- NULL
        insertUI(selector='#teksStatusCapital',
                 where = 'afterEnd',
                 ui = tags$div(id="statusCapital","Tidak terdapat tabel modal kapital"))
      } else if(cekCapital == F & input$checkKapital == T){
        capital <- NULL
        insertUI(selector='#teksStatusCapital',
                 where = 'afterEnd',
                 ui = tags$div(id="statusCapital","Tidak terdapat tabel modal kapital"))
      }
      
      npv <- NULL
      nlc <- NULL
      ec <- NULL
      hp <- NULL
      lr <- NULL
      
      rate.p <- input$rate.p
      rate.s <- input$rate.s
      nilai.tukar <- input$nilai.tukar
      sut <- input$sut
      kom <- input$kom
      th <- input$th
      
      
      combineDef <- list(ioInput=ioInput,ioOutput=ioOutput,
                         priceInput=priceInput,priceOutput=priceOutput,
                         capital=capital,
                         npv=npv,
                         nlc=nlc,
                         ec=ec,
                         hp=hp,
                         lr=lr,
                         rate.p=rate.p,
                         rate.s=rate.s,
                         nilai.tukar=nilai.tukar,
                         sut=sut,
                         kom=kom,
                         th=th
      )
      
      # save data untuk setiap perubahan
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,".rds")
      saveRDS(combineDef,file = fileName)
      
      
      
      print("pertama kali masuk/login. cek save data default")
      combineDef
      
    })
    
    
    ################################################################################
    #                                                                              #
    #                          RESULT DATA DEFAULT                                 #
    #                                                                              #
    ################################################################################
    resultTemplate <- reactive({
      
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      
      ioInput <- read.table(paste0(datapath,"io template input.csv"), header = T, sep = ",")
      ioOutput <- read.table(paste0(datapath,"io template output.csv"), header = T, sep = ",")
      
      priceInput <- read.table(paste0(datapath,"price template input.csv"), header = T, sep = ",")
      priceOutput <- read.table(paste0(datapath,"price template output.csv"), header = T, sep = ",")
      
      # case for modal kapital
      cekCapital <- file.exists(paste0(datapath,"kapital template.csv")) #cek keberadaan file ini ada atau engga
      
      if(cekCapital == T){
        capital <- read.table(paste0(datapath,"kapital template.csv"), header = T, sep = ",")
      } else if (cekCapital == F){
        capital <- NULL
      }
      
      
      
      
      dataDefine <- list(ioInput=ioInput,ioOutput=ioOutput,
                         priceInput=priceInput,priceOutput=priceOutput,
                         capital=capital
      )
      
      
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
      
      #### buat if else untuk modal kapital ####
      if (is.null(dataDefine$capital)){
        # capital = NULL
        data.gab <- rbind(io.all,
                          price.all.year) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
        data.gab
        
      }else{
        capital <- cbind(grup="input",dataDefine$capital)
        
        # menambahkan pada tabel io matrix bernilai 1
        # nrow nya = dibagi 2 asumsi io modal kapital privat = io modal kapital sosial
        # modal kapital sosialnya diwakili oleh momdal kapital privat
        ioKapital <- data.frame(matrix(data=1,nrow = nrow(capital)/2 , ncol = ncol(dataDefine$ioInput)-3))
        colnames(ioKapital)<-paste0(c(rep("Y", yearIO)),1:yearIO)
        ioKapital<-cbind(status="general" ,capital[nrow(capital)/2,c(1:4)],ioKapital)
        ioKapital <- ioKapital %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        
        kapitalPrivat <- filter(capital,komponen == c("modal kapital privat"))
        kapitalPrivat <- cbind(status ="harga.privat",kapitalPrivat )
        kapitalSosial <- filter(capital,komponen == c("modal kapital sosial"))
        kapitalSosial <- cbind(status ="harga.sosial",kapitalSosial )
        data.gab <- rbind(io.all, ioKapital,
                          price.all.year, 
                          kapitalPrivat, kapitalSosial) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
        data.gab
      }
      
      # hitung npv --------------------------------------------------------------
      dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
      dataPrivat <- filter(data.gab,status == c("harga.privat")) #filter data private price
      p.budget <- dataGeneral[-(1:5)] * dataPrivat[-c(1:5)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5
      p.budget <- cbind(dataGeneral[1:5],p.budget) #memunculkan kembali variabel 1 sd 5
      p.budget <- p.budget %>%
        mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
      
      #perkalian antara general dengan Social Price
      dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
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
      # ending  npv --------------------------------------------------------------
      
      
      
      # hitung nlc --------------------------------------------------------------
      
      ################ penghitungan NLC
      
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
      # ending  nlc ------------------------------------------------------- 
      
      # hitung EC --------------------------------------------------------------
      ############# PERHITUNGAN ESTABLISHMENT COST
      p.ec <- p.sum.cost[[1]]/1000000
      s.ec <- s.sum.cost[[1]]/1000000
      ec <- data.frame(p.ec,s.ec)
      ec<-data.frame(PRIVATE=p.ec,SOCIAL=s.ec)
      rownames(ec)<-c("Establishment cost (1st year only, MRp/ha)")
      ec
      
      # ending  EC ------------------------------------------------------- 
      
      # hitung hp --------------------------------------------------------------
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
      # ending  hp ------------------------------------------------------- 
      
      # hitung lr --------------------------------------------------------------
      ############# PERHITUNGAN LABOR REQ FOR EST
      lr <- data.frame(sum.labor[[1]]) #pekerja pada tahun 1
      colnames(lr)<-c("Labor Req for Est (1st year only)")
      rownames(lr) <- c("Value")
      lr
      
      # ending  lr ------------------------------------------------------- 
      
      ##### save data dafault
      
      # replace data price
      dataDefine$npv <- hsl.npv
      dataDefine$nlc <- nlc
      dataDefine$ec <- ec
      dataDefine$hp <- hp
      dataDefine$lr <- lr
      
      dataDefine$rate.p <- input$rate.p
      dataDefine$rate.s <- input$rate.s
      dataDefine$nilai.tukar <- input$nilai.tukar
      
      
      dataDefine$sut <- input$sut
      dataDefine$kom <- input$kom
      dataDefine$th <- input$th
      
      print("save result default untuk klik pertama run_button")
      
      fileName <- paste0(datapath,"resultDefault","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,".rds")
      saveRDS(dataDefine,file = fileName)
      
      
      ##### ending save data default
      
      
    })
    
    
    readDataLastEdited <- eventReactive(input$run_button,{
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,".rds")
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
    
    templateCapital <- reactive({
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      print(datapath)
      readData <- read.table(paste0(datapath,"kapital template.csv"), header = T, sep = ",")
      readData
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
                                         choices = c(30),selected = 30) # pilihannnya masih 30 tahun sesuai default
                      ),
                      column(6,
                             selectInput(("ioComp_input"),
                                         "banyaknya komponen:",
                                         choices = c(nrow(templateIOInput()):50),selected = nrow(templateIOInput())) #tidak bisa kurang dari default, jika mau kurang 0 kan sluaruh io nya
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
                                         choices = c(30),selected = 30) # pilihannnya masih 30 tahun sesuai default
                      ),
                      column(6,
                             selectInput(("ioComp_output"),
                                         "banyaknya komponen:",
                                         choices = c(nrow(templateIOOutput()):10),selected = nrow(templateIOOutput()))#tidak bisa kurang dari default, jika mau kurang 0 kan sluaruh io nya
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
      fileName <- paste0(datapath,"saveData","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,".rds")
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
      fileName <- paste0(datapath,"saveData","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,".rds")
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
                    tags$b('Sunting secara manual'),
                    tags$h5("kolom yang dapat di sunting hanya kolom harga.privat dan harga.sosial"),
                  ),
                  tags$br(),
                  width=12
                ),
                mainPanel(
                  rHandsontableOutput(('editPriceInput')),
                  tags$br(),
                  actionButton(('savePriceInput'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksPriceInputSave'),
                  width=12)
              )
            ),
            argonTab(
              tabName = "Output",
              sidebarLayout(
                sidebarPanel(
                  fluidPage(
                    h2("Sunting Harga Output"),
                    tags$b('Sunting secara manual'),
                    tags$h5("kolom yang dapat di sunting hanya kolom harga.privat dan harga.sosial"),
                    
                  ),
                  tags$br(),
                  width=12
                ),
                mainPanel(
                  rHandsontableOutput(('editPriceOutput')),
                  tags$br(),
                  actionButton(('savePriceOutput'), 'simpan tabel'), 
                  tags$br(), 
                  tags$br(),
                  tags$div(id='teksPriceOutputSave'),
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
    
    
    
    
    # START Price Input ----------------------------------------------------------
    valP1 <-eventReactive(input$modalPriceButton,{
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,".rds")
      dataDefine <- readRDS(fileName)
      
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
      fileName <- paste0(datapath,"saveData","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,".rds")
      dataDefine <- readRDS(fileName)
      
      # replace data price
      dataDefine$priceInput <- editNew
      saveRDS(dataDefine,file = fileName)
      
      insertUI(selector='#teksPriceInputSave',
               where = 'afterEnd',
               ui = tags$div(id="textTampil","tabel di atas sudah tersimpan"))
    })
    # ending Price Input -----------------------------------------------------------------
    
    
    # Start Price Output ------------------------------------------------------
    valP2 <- eventReactive(input$modalPriceButton,{
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,".rds")
      dataDefine <- readRDS(fileName)
      
      indexRow <- as.numeric(nrow(dataDefine$ioOutput))
      
      reactData$tableP2 <- dataDefine$ioOutput[,1:2]
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
      fileName <- paste0(datapath,"saveData","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,".rds")
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
    #                          BUTTON MODAL KAPITAL                                #
    #                                                                              #
    ################################################################################
    observeEvent(input$modalCapitalButton,{
      showModal(
        modalDialog( 
          footer=tagList(
            actionButton(("closeCapitalButton"), "Tutup")
          ),
          argonTabSet(
            id = "tabCapital",
            card_wrapper = TRUE,
            horizontal = TRUE,
            circle = FALSE,
            size = "sm",
            width = 12,
            iconList = lapply(X = 1:1, FUN = argonIcon, name = "atom"),
            argonTab(
              tabName = "Sunting Harga pada Tabel Modal Kapital",
              active = T,
              sidebarLayout(
                sidebarPanel(
                  fluidPage(
                    h5("Jika komponen tidak digunakan pada perhitungan, sunting harga menjadi bernilai 0"),
                    tags$br(),
                  ),
                  width=12
                ),
                mainPanel(
                  rHandsontableOutput(('editCapital')),
                  tags$br(),
                  actionButton(('saveCapital'), 'simpan tabel'), 
                  tags$br(), 
                  tags$br(),
                  tags$div(id='teksCapitalSave'),
                  width=12
                )
              )
            ))
          ,
          size="l",
          easyClose = FALSE)
      )
    })
    
    observeEvent(input$closeCapitalButton,{
      removeModal()
    })
    
    valCap <- eventReactive(input$modalCapitalButton,{
      # case for modal kapital
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      cekCapital <- file.exists(paste0(datapath,"kapital template.csv")) #cek keberadaan file ini ada atau engga
      #  case ketika datanya ada dan tidak di ceklis hrusnya di awal munculin html utk div bahwa ada modal kapital dan tdknya
      
      if (cekCapital == T){ # klo kapital templatenya ada mau di ceklis atau engga ttp dimunculin
        reactData$tableCap <- templateCapital()
      } else if (cekCapital == F & input$checkKapital == F){
        reactData$tableCap <- data.frame(matrix("tidak terdapat tabel modal kapital",nrow=1,ncol=1))
        colnames(reactData$tableCap) <- "Keterangan"
        reactData$tableCap
      } else if(cekCapital == F & input$checkKapital == T){
        reactData$tableCap <- data.frame(matrix("tidak terdapat tabel modal kapital",nrow=1,ncol=1))
        colnames(reactData$tableCap) <- "Keterangan"
        reactData$tableCap
      }
    })
    
    output$editCapital <- renderRHandsontable({
      rhandsontable(valCap(),
                    rowHeaderWidth = 50,
                    fixedColumnsLeft = 2,
                    height = 500,
      )
    })
    
    observeEvent(input$saveCapital,{
      # browser()
      removeUI(selector='#textTampil')
      
      editNew<-as.data.frame(hot_to_r(input$editCapital))
      editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
      
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,".rds")
      dataDefine <- readRDS(fileName)
      # dataDefine <- readDataLastEdited()
      
      if(!is.null(dataDefine$capital)){
        # replace data capital
        dataDefine$capital <- editNew
        
        # save CAPITAL BARU
        saveRDS(dataDefine,file = fileName)
        
        insertUI(selector='#teksCapitalSave',
                 where = 'afterEnd',
                 ui = tags$div(id="textTampil","tabel di atas sudah tersimpan"))
      }else if(is.null(dataDefine$capital)){
        insertUI(selector='#teksCapitalSave',
                 where = 'afterEnd',
                 ui = tags$div(id="textTampil","komoditas ini tidak memiliki tabel default untuk modal kapital"))
      }
    })
    
    ################################################################################
    #                                                                              #
    #                                RESULT                                        #
    #                                                                              #
    ################################################################################
    data.gab <- eventReactive(input$run_button,{
      # observeEvent(input$run_button,{
      #   browser()
      
      # aktifin dataTemplate
      # agar ketika run pertama kali yang terbaca tetap data default di excel
      
      resultTemplate()
      dataTemplate()
      
      #setelah dataTemplate(data default) aktif, 
      # lalu read kembali file rds yang tersimpan dr hasil edit jika ada yang diedit
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,".rds")
      dataDefine <- readRDS(fileName)
      # dataDefine <- readDataLastEdited()
      
      
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
      
      #### buat if else untuk modal kapital ####
      if (is.null(dataDefine$capital)){
        # capital = NULL
        data.gab <- rbind(io.all,
                          price.all.year) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
        data.gab
        
      }else{
        capital <- cbind(grup="input",dataDefine$capital)
        
        # menambahkan pada tabel io matrix bernilai 1
        # nrow nya = dibagi 2 asumsi io modal kapital privat = io modal kapital sosial
        # modal kapital sosialnya diwakili oleh momdal kapital privat
        ioKapital <- data.frame(matrix(data=1,nrow = nrow(capital)/2 , ncol = ncol(dataDefine$ioInput)-3))
        colnames(ioKapital)<-paste0(c(rep("Y", yearIO)),1:yearIO)
        ioKapital<-cbind(status="general" ,capital[nrow(capital)/2,c(1:4)],ioKapital)
        ioKapital <- ioKapital %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        
        kapitalPrivat <- filter(capital,komponen == c("modal kapital privat"))
        kapitalPrivat <- cbind(status ="harga.privat",kapitalPrivat )
        kapitalSosial <- filter(capital,komponen == c("modal kapital sosial"))
        kapitalSosial <- cbind(status ="harga.sosial",kapitalSosial )
        data.gab <- rbind(io.all, ioKapital,
                          price.all.year, 
                          kapitalPrivat, kapitalSosial) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
        data.gab
      }
    })
    
    hitung.npv<-eventReactive(input$run_button,{
      # observeEvent(input$run_button,{
      #   browser()
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
    
    
    
    # Start uiShowResult ------------------------------------------------------
    observeEvent(input$run_button, {
      # browser()
      insertUI(selector='#uiShowResult',
               where='afterEnd',
               ui= uiOutput('showResult'))
      
      
    }) 
    
    output$showResult <- renderUI({
      fluidPage(
        fluidRow(
          column(12,
                 h1("Hasil Analisis", align = "center"),
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
        fluidRow(
          column(9,
                 h6(" ")
          ),
          column(3,
                 actionButton(("saveNewPAM"),"Simpan PAM baru",icon("paper-plane"),style="color: white;background-color: green;")
          )
        ),
        fluidRow(
          column(9,
                 h6(" ")
          ),
          column(3,
                 br(),
                 tags$div(id='teksNewPamSave')
          )
        )
        
        
      )
      
    })
    
    
    # ending uiShowResult ------------------------------------------------------
    
    output$viewPrice <- renderDataTable({
      if(!is.null(data.gab())){
        dataView <- rbind(readDataLastEdited()$priceInput, readDataLastEdited()$priceOutput)
        dataView    
      }else if(!is.null(valP1()) | !is.null(valP2())){
        dataView <- rbind(valP1(),valP2())
        dataView
      }
    })
    
    output$viewIO <- renderDataTable({
      if(!is.null(data.gab())){
        dataView <- rbind(readDataLastEdited()$ioInput, readDataLastEdited()$ioOutput)
        dataView    
      }else if(!is.null(valP1()) | !is.null(valP2())){
        dataView <- rbind(valIO1(),valIO2())
        dataView
      }
    })
    
    output$viewKapital <- renderDataTable({
      # case for modal kapital
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      cekCapital <- file.exists(paste0(datapath,"kapital template.csv")) #cek keberadaan file ini ada atau engga
      
      if(cekCapital == T & input$checkKapital == T){
        # dataView <- data.frame(matrix(1,nrow=3,ncol=3))
        dataView <- readDataLastEdited()$capital
        dataView    
      }else if(cekCapital == T & input$checkKapital == F){
        dataView <- data.frame(matrix("terdapat default tabel modal kapital, tapi tidak dimasukkan kedalam perhitungan",nrow=1,ncol=1))
        colnames(dataView) <- "Keterangan"
        dataView
      }
      else if(cekCapital == F){
        # dataView <- data.frame(matrix(0,nrow=3,ncol=3))
        dataView <- data.frame(matrix("tidak terdapat tabel modal kapital",nrow=1,ncol=1))
        colnames(dataView) <- "Keterangan"
        dataView
      }
    })
    
    
    observeEvent(input$saveNewPAM,{
      # browser()
      removeUI(selector='#textTampilNewPamSave')
      
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,".rds")
      dataDefine <- readRDS(fileName)
      
      # replace data 
      dataDefine$npv <- t(hitung.npv())
      dataDefine$nlc <- t(hitung.nlc())
      dataDefine$ec <- t(hitung.ec())
      dataDefine$hp <- hitung.hp()
      dataDefine$lr <- hitung.lr()
      
      dataDefine$rate.p <- input$rate.p
      dataDefine$rate.s <- input$rate.s
      dataDefine$nilai.tukar <- input$nilai.tukar
      
      # id save data
      waktuDefine<-Sys.time()
      simpanDefine<-gsub(" ","_",waktuDefine,fixed = TRUE)
      simpanDefine<-gsub(":","-",simpanDefine,fixed = TRUE)
      namaSken <- paste0(input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,"_",input$petani)
      namafileDefine<-paste0("PAMBARU","_",namaSken,"_",simpanDefine)
      print("save result sesuai inputan terakhir, yg membedakan ketika ada modal kapital tp tdk di ceklis, jk defaulnya ada maka ttp dihitung untuk resultTemplate nya")
      
      dataDefine$waktu.save <- simpanDefine
      saveRDS(dataDefine, file = paste0(datapath,"/",namafileDefine))
      
      insertUI(selector='#teksNewPamSave',
               where = 'afterEnd',
               ui = tags$div(id="textTampilNewPamSave","Tabel PAM sudah berhasil disimpan"))
    })
    
    
    ################################################################################
    #                                                                              #
    #                          deskriptifPlot                                      #
    #                                                                              #
    ################################################################################
    
    observeEvent(input$provShowDeskriptifHit, {
      removeUI(selector='#showPlotAll') #spy plotnya terrefresh
      valsNewPam$Data$Waktu.PAM.disimpan <- NULL
      valsNewPam$Data$Sistem.Usaha.Tani <-  NULL
      valsNewPam$Data$Komoditas <- NULL
      valsNewPam$Data$Tahun  <-  NULL
      valsNewPam$Data$NPV.Privat.RP  <-  NULL
      valsNewPam$Data$NPV.Sosial.RP  <-  NULL
      valsNewPam$Data$Discount.Rate.Private  <-   NULL
      valsNewPam$Data$Discount.Rate.Social  <-   NULL
      valsNewPam$Data$Nilai.Tukar  <-   NULL
      valsNewPam$Data$ceklis <-  NULL
      valsNewPam$Data$del <- NULL
      
      valsNewPam$Data <- NULL
      insertUI(selector='#uiListPamDefault',
               where='afterEnd',
               ui= uiOutput('showTable')
      )
    }) 
    
    observeEvent(c(input$provDeskriptif,input$provTahunDeskriptif), {
      removeUI(selector='#showTable')
    }) 
    
    
    output$showTable<- renderUI({
      fluidPage(
        box(width=12,
            h3(strong("Daftar PAM Default"),align="center"),
            h5(strong("daftar telah diurutkan berdasarkan nilai NPV Privat"),align="center"),
            hr(),
            column(6,offset = 6,
                   HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
                   
                   HTML('</div>')
            ),
            br(),
            
            column(12,dataTableOutput("Main_tableDefault")),
            
            h3(strong("Daftar PAM Baru"),align="center"),
            h5(strong("daftar telah diurutkan berdasarkan waktu tabel PAM baru disimpan"),align="center"),
            hr(),
            column(12,dataTableOutput("Main_tableNewPam")),
            tags$script(HTML('$(document).on("click", "input", function () {
  var checkboxes = document.getElementsByName("row_selected");
  var checkboxesChecked = [];
  for (var i=0; i<checkboxes.length; i++) {
     if (checkboxes[i].checked) {
        checkboxesChecked.push(checkboxes[i].value);
     }
  }
  Shiny.onInputChange("checked_rows",checkboxesChecked);
      })')),
            tags$script(HTML('$(document).on("click", "input", function () {
  var checkboxes = document.getElementsByName("row_selected_newPam");
  var checkboxesChecked = [];
  for (var i=0; i<checkboxes.length; i++) {
     if (checkboxes[i].checked) {
        checkboxesChecked.push(checkboxes[i].value);
     }
  }
  Shiny.onInputChange("checked_rows_newPam",checkboxesChecked);
      })')),
            tags$script("$(document).on('click', '#Main_tableNewPam button', function () {
                    Shiny.onInputChange('lastClickId',this.id);
                    Shiny.onInputChange('lastClick', Math.random())
  });"),
            
            br(),
            fluidRow(
              column(10,
                     h6(" ")
              ),
              column(2,
                     actionButton(inputId = "buttonPlot",label = "Tampilkan Plot",style="color: white; 
                         background-color: green;")
              )
            ),
            br(),
            br(),
            tags$div(id = 'showPlot'),
        ))
    })
    
    
    # START SHOW TABLE DEFAULT ------------------------------------------------------
    loadRDSAll <- eventReactive(input$provShowDeskriptifHit,{
      # loadRDSAll <- reactive({
      ##### step 1 filter yang ada pattern resultDefault
      folderSut <- sort(unique(komoditas$sut))
      folderKom <- sort(unique(komoditas$nama_komoditas))
      
      kombinasiFolder <- as.vector(outer(folderSut, folderKom, paste, sep="/"))
      dirFile <- paste0("shiny/data/",kombinasiFolder)
      
      nameFiles <- list.files(path = paste0(dirFile,"/"),pattern = paste0("resultDefault"))
      kombinasiFile <- as.vector(outer(dirFile, nameFiles, paste, sep="/"))
      cekFile <- file.exists(kombinasiFile) #cek keberadaan file ini ada atau engga
      
      # remove index yang cekFilenya == F, munculin yang cekFilenya == T aja
      indexFileTrue <- which(cekFile == T)
      kombinasiFile <- kombinasiFile[which(cekFile == T)]
      
      funcFile <- function(x){
        a <- readRDS(x)
        b <- c(x,a)
        b}
      
      
      ##### step 2 filter yang ada pattern input$provDeskriptif ex: (_ACEH)
      # cek dari vector kombinasiFile yang sudah di cek T or F nya
      provFile <- kombinasiFile %>% 
        str_subset(pattern = paste0("_",input$provDeskriptif))
      
      
      ##### step 3 filter yang ada pattern input$provTahunDeskriptif ex: (_2020)
      tahunFile <- provFile %>% 
        str_subset(pattern = paste0("_",input$provTahunDeskriptif))
      tahunFile
      
      patternAll <- lapply(tahunFile, funcFile)
      patternAll
    })
    
    
    vals<-reactiveValues()
    
    vals$Data<-data.table(
      Sistem.Usaha.Tani =  NULL,
      Komoditas = NULL,
      Tahun = NULL,
      NPV.Privat.RP = NULL,
      NPV.Sosial.RP = NULL,
      Discount.Rate.Private =  NULL,
      Discount.Rate.Social =  NULL,
      Nilai.Tukar =  NULL,
      ceklis = NULL
    )
    
    
    
    
    output$Main_tableDefault<-renderDataTable({
      if(length(loadRDSAll())==0){
        vals$Data$Sistem.Usaha.Tani <-  "file tidak tersedia"
        vals$Data$Komoditas <- "file tidak tersedia"
        vals$Data$Tahun  <-  "file tidak tersedia"
        vals$Data$NPV.Privat.RP  <-  "file tidak tersedia"
        vals$Data$NPV.Sosial.RP  <-  "file tidak tersedia"
        vals$Data$Discount.Rate.Private  <-   "file tidak tersedia"
        vals$Data$Discount.Rate.Social  <-   "file tidak tersedia"
        vals$Data$Nilai.Tukar  <-   "file tidak tersedia"
        vals$Data$ceklis <-  "file tidak tersedia"
      } else {
        vals$Data$Sistem.Usaha.Tani  <-   unlist(lapply(loadRDSAll(), function(x)x[[15]]))
        vals$Data$Komoditas  <-  unlist(lapply(loadRDSAll(), function(x)x[[16]]))
        vals$Data$Tahun  <-  unlist(lapply(loadRDSAll(), function(x)x[[17]]))
        vals$Data$NPV.Privat.RP  <-  unlist(lapply(loadRDSAll(), function(x)x[[7]][1,1]))
        vals$Data$NPV.Sosial.RP  <-  unlist(lapply(loadRDSAll(), function(x)x[[7]][1,2]))#nama file dr listValDef ada di index terakhir = 6
        vals$Data$Discount.Rate.Private  <-   unlist(lapply(loadRDSAll(), function(x)x[[12]]))
        vals$Data$Discount.Rate.Social  <-   unlist(lapply(loadRDSAll(), function(x)x[[13]]))
        vals$Data$Nilai.Tukar  <-   unlist(lapply(loadRDSAll(), function(x)x[[14]]))
        vals$Data$ceklis<-paste0('<input type="checkbox" name="row_selected" value="Row',1:length(vals$Data$Komoditas),'"><br>')
      }
      
      dataView <-  data.frame(
        Sistem.Usaha.Tani =  vals$Data$Sistem.Usaha.Tani,
        Komoditas = vals$Data$Komoditas,
        Tahun = vals$Data$Tahun,
        NPV.Privat.RP = vals$Data$NPV.Privat.RP,
        NPV.Sosial.RP = vals$Data$NPV.Sosial.RP ,
        Discount.Rate.P =  vals$Data$Discount.Rate.Private,
        Discount.Rate.S =  vals$Data$Discount.Rate.Social,
        Nilai.Tukar =  vals$Data$Nilai.Tukar ,
        Pilih.File = vals$Data$ceklis
      )
      
      No <- as.numeric(rownames(dataView))
      dataView <- dataView[with(dataView,order(dataView$NPV.Privat.RP,decreasing = T)),]
      rownames(dataView) <- No
      dataView <- datatable(dataView,escape = F)
      dataView  %>% 
        formatCurrency('NPV.Privat.RP',currency = "", interval = 3, mark = ",",digits = 1) %>%  
        formatCurrency('NPV.Sosial.RP',currency = "", interval = 3, mark = ",",digits = 1)%>%  
        formatCurrency('Nilai.Tukar',currency = "", interval = 3, mark = ",",digits = 0)
    }
    )
    
    # ENDING SHOW TABLE DEFAULT -----------------------------------------------
    
    
    # START SHOW TABLE NEW PAM -----------------------------------------------
    loadRDSAllNewPam <- eventReactive(input$provShowDeskriptifHit,{
      ##### step 1 filter yang ada pattern resultDefault
      folderSut <- sort(unique(komoditas$sut))
      folderKom <- sort(unique(komoditas$nama_komoditas))
      
      kombinasiFolder <- as.vector(outer(folderSut, folderKom, paste, sep="/"))
      dirFile <- paste0("shiny/data/",kombinasiFolder)
      
      nameFiles <- list.files(path = paste0(dirFile,"/"),pattern = paste0("PAMBARU"))
      kombinasiFile <- as.vector(outer(dirFile, nameFiles, paste, sep="/"))
      cekFile <- file.exists(kombinasiFile) #cek keberadaan file ini ada atau engga
      
      # remove index yang cekFilenya == F, munculin yang cekFilenya == T aja
      indexFileTrue <- which(cekFile == T)
      kombinasiFile <- kombinasiFile[which(cekFile == T)]
      
      funcFile <- function(x){
        a <- readRDS(x)
        b <- c(x,a)
        b}
      
      
      ##### step 2 filter yang ada pattern input$provDeskriptif ex: (_ACEH)
      # cek dari vector kombinasiFile yang sudah di cek T or F nya
      provFile <- kombinasiFile %>% 
        str_subset(pattern = paste0("_",input$provDeskriptif))
      
      
      ##### step 3 filter yang ada pattern input$provTahunDeskriptif ex: (_2020)
      tahunFile <- provFile %>% 
        str_subset(pattern = paste0("_",input$provTahunDeskriptif))
      tahunFile
      
      patternAll <- lapply(tahunFile, funcFile)
      patternAll
    })
    
    
    valsNewPam<-reactiveValues()
    
    valsNewPam$Data<-data.table(
      Waktu.PAM.disimpan = NULL,
      Sistem.Usaha.Tani =  NULL,
      Komoditas = NULL,
      Tahun = NULL,
      NPV.Privat.RP = NULL,
      NPV.Sosial.RP = NULL,
      Discount.Rate.Private =  NULL,
      Discount.Rate.Social =  NULL,
      Nilai.Tukar =  NULL,
      ceklis = NULL,
      del = NULL
    )
    
    
    output$Main_tableNewPam<-renderDataTable({
      if(length(loadRDSAllNewPam())==0){
        valsNewPam$Data$Waktu.PAM.disimpan <- "file tidak tersedia"
        valsNewPam$Data$Sistem.Usaha.Tani <-  "file tidak tersedia"
        valsNewPam$Data$Komoditas <- "file tidak tersedia"
        valsNewPam$Data$Tahun  <-  "file tidak tersedia"
        valsNewPam$Data$NPV.Privat.RP  <-  "file tidak tersedia"
        valsNewPam$Data$NPV.Sosial.RP  <-  "file tidak tersedia"
        valsNewPam$Data$Discount.Rate.Private  <-   "file tidak tersedia"
        valsNewPam$Data$Discount.Rate.Social  <-   "file tidak tersedia"
        valsNewPam$Data$Nilai.Tukar  <-   "file tidak tersedia"
        valsNewPam$Data$ceklis <-  "file tidak tersedia"
        valsNewPam$Data$del <- "file tidak tersedia"
      } else {
        valsNewPam$Data$Waktu.PAM.disimpan <- unlist(lapply(loadRDSAllNewPam(), function(x)x[[18]]))
        valsNewPam$Data$Sistem.Usaha.Tani  <-   unlist(lapply(loadRDSAllNewPam(), function(x)x[[15]]))
        valsNewPam$Data$Komoditas  <-  unlist(lapply(loadRDSAllNewPam(), function(x)x[[16]]))
        valsNewPam$Data$Tahun  <-  unlist(lapply(loadRDSAllNewPam(), function(x)x[[17]]))
        valsNewPam$Data$NPV.Privat.RP  <-  unlist(lapply(loadRDSAllNewPam(), function(x)x[[7]][1,1]))
        valsNewPam$Data$NPV.Sosial.RP  <-  unlist(lapply(loadRDSAllNewPam(), function(x)x[[7]][1,2]))#nama file dr listValDef ada di index terakhir = 6
        valsNewPam$Data$Discount.Rate.Private  <-   unlist(lapply(loadRDSAllNewPam(), function(x)x[[12]]))
        valsNewPam$Data$Discount.Rate.Social  <-   unlist(lapply(loadRDSAllNewPam(), function(x)x[[13]]))
        valsNewPam$Data$Nilai.Tukar  <-   unlist(lapply(loadRDSAllNewPam(), function(x)x[[14]]))
        valsNewPam$Data$ceklis<-paste0('<input type="checkbox" name="row_selected_newPam" value="Row',1:length(valsNewPam$Data$Komoditas),'"><br>')
        valsNewPam$Data$del<-
          paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
                <button type="button" class="btn btn-secondary delete" id=delete_',1:length(valsNewPam$Data$Komoditas),'>Delete</button>
             </div>
             ')
      }
      
      
      
      dataView <-  data.frame(
        Waktu.PAM.disimpan = valsNewPam$Data$Waktu.PAM.disimpan,
        Sistem.Usaha.Tani =  valsNewPam$Data$Sistem.Usaha.Tani,
        Komoditas = valsNewPam$Data$Komoditas,
        Tahun = valsNewPam$Data$Tahun,
        NPV.Privat.RP = valsNewPam$Data$NPV.Privat.RP,
        NPV.Sosial.RP = valsNewPam$Data$NPV.Sosial.RP ,
        Discount.Rate.P =  valsNewPam$Data$Discount.Rate.Private,
        Discount.Rate.S =  valsNewPam$Data$Discount.Rate.Social,
        Nilai.Tukar =  valsNewPam$Data$Nilai.Tukar ,
        Pilih.File = valsNewPam$Data$ceklis,
        Hapus.File = valsNewPam$Data$del
      )
      
      No <- as.numeric(rownames(dataView))
      dataView <- dataView[with(dataView,order(dataView$Waktu.PAM.disimpan,decreasing = T)),]
      rownames(dataView) <- No
      dataView <- datatable(dataView,escape = F)
      dataView  %>% 
        formatCurrency('NPV.Privat.RP',currency = "", interval = 3, mark = ",",digits = 1) %>%  
        formatCurrency('NPV.Sosial.RP',currency = "", interval = 3, mark = ",",digits = 1)%>%  
        formatCurrency('Nilai.Tukar',currency = "", interval = 3, mark = ",",digits = 0)
    }
    )
    
    
    observeEvent(input$lastClick,{
      # browser()
      if (input$lastClickId%like%"delete")
      {
        row_to_del=as.numeric(gsub("delete_","",input$lastClickId))
        valsNewPam$Data=valsNewPam$Data[-row_to_del]
        listFileName <- unlist(lapply(loadRDSAllNewPam(), function(x)x[[1]]))
        fileNameSelected <- listFileName[row_to_del]
        file.remove(fileNameSelected)
        
        valsNewPam$Data$Waktu.PAM.disimpan <- NULL
        valsNewPam$Data$Sistem.Usaha.Tani <-  NULL
        valsNewPam$Data$Komoditas <- NULL
        valsNewPam$Data$Tahun  <-  NULL
        valsNewPam$Data$NPV.Privat.RP  <-  NULL
        valsNewPam$Data$NPV.Sosial.RP  <-  NULL
        valsNewPam$Data$Discount.Rate.Private  <-   NULL
        valsNewPam$Data$Discount.Rate.Social  <-   NULL
        valsNewPam$Data$Nilai.Tukar  <-   NULL
        valsNewPam$Data$ceklis <-  NULL
        valsNewPam$Data$del <- NULL
        
        valsNewPam$Data <- NULL
      }
      
      removeUI(selector='#showPlotAll') 
      removeUI(selector='#showTable')
      
    })
    
    # ENDING SHOW TABLE NEW PAM  -----------------------------------------------
    
    
    observeEvent(input$buttonPlot, {
      # browser()
      
      removeUI(selector='#showPlotAll')
      
      insertUI(selector='#showPlot',
               where='afterEnd',
               ui= uiOutput('showPlotAll'))
      
      
    }) 
    
    output$showPlotAll <- renderUI({
      row_to_select=as.numeric(gsub("Row","",input$checked_rows))
      row_to_select_newPam = as.numeric(gsub("Row","",input$checked_rows_newPam))
      
      
      if(identical(row_to_select,numeric(0)) & identical(row_to_select_newPam,numeric(0))){
        box(width=12,
            hr(),
            br(),
            br(),
            h3("Belum ada data PAM yang terpilih",align="center"),
            br(),
            br(),
            hr())
        
      }else {
        hr()
        plotlyOutput(('allPlot'))
      }
      
    })
    
    
    
    preparePlot <- eventReactive(input$buttonPlot,{
      row_to_select=as.numeric(gsub("Row","",input$checked_rows))
      row_to_select_newPam = as.numeric(gsub("Row","",input$checked_rows_newPam))
      
      # data default
      dataCheck <- loadRDSAll()
      dataCheck <- dataCheck[row_to_select]
      sut <- unlist(lapply(dataCheck, function(x)x[[15]]))
      komoditas <- unlist(lapply(dataCheck, function(x)x[[16]]))
      NPV.Privat.RP <- unlist(lapply(dataCheck, function(x)x[[7]][1,1]))
      
      dataPlotDefault <- data.frame(sut=sut,
                                    komoditas=komoditas,
                                    NPV.Privat.RP=NPV.Privat.RP)
      
      # data Pam baru
      dataCheckNewPam <- loadRDSAllNewPam()
      dataCheckNewPam <- dataCheckNewPam[row_to_select_newPam]
      sut <- unlist(lapply(dataCheckNewPam, function(x)x[[15]]))
      sut<-paste0("PAM BARU ",sut)
      komoditas <- unlist(lapply(dataCheckNewPam, function(x)x[[16]]))
      komoditas<-paste0("PAM BARU ",komoditas) # supaya barchartnya terpisah dari setiap komoditas
      NPV.Privat.RP <- unlist(lapply(dataCheckNewPam, function(x)x[[7]][1,1]))
      
      dataPlotNewPam <- data.frame(sut=sut,
                                   komoditas=komoditas,
                                   NPV.Privat.RP=NPV.Privat.RP)
      dataPlot <- rbind(dataPlotDefault,dataPlotNewPam)
      
      
      dataPlot %>%
        group_by(sut) %>%
        plot_ly(x = ~komoditas, y = ~NPV.Privat.RP, type = "bar", color = ~sut)
    })
    
    output$allPlot <- renderPlotly({
      preparePlot()
    })
    
    
  }
)

runApp(app)