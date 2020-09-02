
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
source("shiny/2_sidebar.R")
source("shiny/navbar.R")
source("shiny/header.R")
source("shiny/footer.R")

# input file
komoditas <- read.csv("shiny/data/template/komoditas.csv", stringsAsFactors = F)
dataPupuk <- read.csv("shiny/data/template/jenis pupuk.csv", stringsAsFactors = F)

# elements
source("shiny/2_verifikasi.R")
source("shiny/2_pambaru_modul2.R")
# source("shiny/analisis.R")


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
        verifikasi,
        pamBaru
        # ,
        # deskriptifPlot
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
    
    # Section informasi umum ---------------------------------------------
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
      updateSelectInput(
        session,
        "selected_provinsi",
        choices = komoditas %>%
          filter(nama_komoditas == input$kom) %>%
          select(provinsi) %>%
          .[[1]]
      )
    })
    
    observe({
      updateSelectInput(
        session,
        "th",
        choices = komoditas %>%
          filter(provinsi == input$selected_provinsi) %>%
          select(tahun_analisis) %>%
          .[[1]]
      )
    })
    
    observe({
      updateSelectInput(
        session,
        "tipeLahan",
        choices = komoditas %>%
          filter(tahun_analisis == input$th) %>%
          select(tipe_lahan) %>%
          .[[1]]
      )
    })
    
    # End - Section informasi umum ---------------------------------------------
    
    
    
    # Section preparation data ---------
    dataTemplate <- reactive({
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      
      readDataTemplate <- read.table(paste0(datapath,input$selected_provinsi,"_",input$th,".csv"), header = T, sep = ",")
      yearIO <- 30 #tahun daur tanam
      
      inputData <- filter(readDataTemplate,faktor == c("input"))
      ioInput <- inputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
      priceInput <- inputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
      
      outputData <- filter(readDataTemplate,faktor == c("output"))
      ioOutput <- outputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
      priceOutput <- outputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
      
      
      
      # case for modal kapital
      cekCapital <- file.exists(paste0(datapath,"kapital template.csv")) #cek keberadaan file ini ada atau engga
      capital <- NULL
      
      
      # if (cekCapital == T & input$checkKapital == F ){
      #   capital <- NULL
      #   insertUI(selector='#teksStatusCapital',
      #            where = 'afterEnd',
      #            ui = tags$div(id="statusCapital","Terdapat tabel modal kapital (tidak dimasukkan ke dalam perhitungan)"))
      # } else if(cekCapital == T & input$checkKapital == T){
      #   capital <- read.table(paste0(datapath,"kapital template.csv"), header = T, sep = ",")
      #   insertUI(selector='#teksStatusCapital',
      #            where = 'afterEnd',
      #            ui = tags$div(id="statusCapital","Terdapat tabel modal kapital (dimasukkan kedalam perhitungan)"))
      # } else if (cekCapital == F & input$checkKapital == F){
      #   capital <- NULL
      #   insertUI(selector='#teksStatusCapital',
      #            where = 'afterEnd',
      #            ui = tags$div(id="statusCapital","Tidak terdapat tabel modal kapital"))
      # } else if(cekCapital == F & input$checkKapital == T){
      #   capital <- NULL
      #   insertUI(selector='#teksStatusCapital',
      #            where = 'afterEnd',
      #            ui = tags$div(id="statusCapital","Tidak terdapat tabel modal kapital"))
      # }
      
      # informasi umum
      sut <- input$sut
      kom <- input$kom
      provinsi <- input$selected_provinsi
      th <- input$th
      tipeLahan <- input$tipeLahan
      tipeKebun <- readDataTemplate$tipe
      
      
      
      
      combineDef <- list(ioInput=ioInput,ioOutput=ioOutput,
                         priceInput=priceInput,priceOutput=priceOutput,
                         capital=capital,
                         sut=sut,
                         kom=kom,
                         provinsi = provinsi,
                         th=th,
                         tipeLahan = tipeLahan,
                         tipeKebun = tipeKebun)
      
      # save data untuk setiap perubahan
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      saveRDS(combineDef,file = fileName)
      
      
      
      print("pertama kali masuk/login. cek save data default")
      combineDef
      
    })
    
    resultTemplate <- reactive({
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      
      readDataTemplate <- read.table(paste0(datapath,input$selected_provinsi,"_",input$th,".csv"), header = T, sep = ",")
      yearIO <- 30 #tahun daur tanam
      
      inputData <- filter(readDataTemplate,faktor == c("input"))
      ioInput <- inputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
      priceInput <- inputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
      
      outputData <- filter(readDataTemplate,faktor == c("output"))
      ioOutput <- outputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
      priceOutput <- outputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
      
      
      
      # case for modal kapital
      cekCapital <- file.exists(paste0(datapath,"kapital template.csv")) #cek keberadaan file ini ada atau engga
      
      if(cekCapital == T){
        capital <- read.table(paste0(datapath,"kapital template.csv"), header = T, sep = ",")
      } else if (cekCapital == F){
        capital <- NULL
      }
      
      # Asumsi makro
      readDataAsumsiMakro <- read.table(paste0("shiny/data/template/asumsi makro",".csv"), header = T, sep = ",")
      asumsiMakroTemplate <- filter(readDataAsumsiMakro,tahun == input$th)
      
      rate.p <- asumsiMakroTemplate$rate.p
      rate.s <- asumsiMakroTemplate$rate.s
      nilai.tukar <- asumsiMakroTemplate$nilai.tukar
      
      # memmbuat list gabungan dataDefine
      dataDefine <- list(ioInput=ioInput,ioOutput=ioOutput,
                         priceInput=priceInput,priceOutput=priceOutput,
                         capital=capital,
                         rate.p = rate.p,
                         rate.s = rate.s,
                         nilai.tukar=nilai.tukar)
      
      #informasi umum
      dataDefine$sut <- input$sut
      dataDefine$kom <- input$kom
      dataDefine$provinsi <- input$selected_provinsi
      dataDefine$th <- input$th
      dataDefine$tipeLahan <- input$tipeLahan
      dataDefine$tipeKebun <- readDataTemplate$tipe
      dataDefine$tipeData <- c("BAU")
      
      
      
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
        data.gab <- bind_rows(io.all,
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
      p.budget <- dataGeneral[-(c(1:5,36))] * dataPrivat[-c(1:5,36)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
      p.budget <- cbind(dataGeneral[c(1:4)],dataPrivat[36],p.budget) #memunculkan kembali variabel 1 sd 5
      p.budget <- p.budget %>%
        mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
      
      #perkalian antara general dengan Social Price
      dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
      s.budget <- dataGeneral[-c(1:5,36)] * dataSosial[-c(1:5,36)]
      s.budget <- cbind(dataGeneral[c(1:4)],dataSosial[36],s.budget)
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
      
      npv.p<-npv(dataDefine$rate.p/100,p.profit)
      npv.s<-npv(dataDefine$rate.s/100,s.profit)
      
      hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
      
      npv.p.us<-npv.p/dataDefine$nilai.tukar
      npv.s.us<-npv.s/dataDefine$nilai.tukar
      npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
      hsl.npv<-rbind(hsl.npv,npv.us)
      
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
      sum.prod <- fil.prod[,-c(1:5,36)] %>%
        colSums(na.rm = T)
      tot.prod <- sum(sum.prod)
      
      fil.labor <- dataGeneral %>%  filter(str_detect(komponen, c("tenaga kerja")))
      sum.labor <- fil.labor[,-c(1:5,36)] %>%
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
      
      ##### save data template
      
      # RESULT 
      dataDefine$npv <- hsl.npv
      dataDefine$nlc <- nlc
      dataDefine$ec <- ec
      dataDefine$hp <- hp
      dataDefine$lr <- lr
      
      
      print("save result template untuk klik pertama asumsiMakro_button")
      
      fileName <- paste0(datapath,"resultTemplate","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      saveRDS(dataDefine,file = fileName)
      
      
      ##### ending save data template
      
      
    })
    
    # end - Section preparation data --------
    
    
    
    # Section asumsi makro ---------------------------------------------
    observeEvent(input$asumsiMakro_button, {
      # removeUI(selector='#showTable')
      # removeUI(selector='#showMakro')
      # browser()
      dataTemplate()
      resultTemplate()
      
      insertUI(selector='#uiShowMakro',
               where='afterEnd',
               ui= uiOutput('showMakro'))
      
      
    }) 
    
    output$showMakro <- renderUI({
      argonRow(
      argonColumn(
        width = 12,
        argonH1("Asumsi Makro", display = 4),
        h5("Langkah 2: menentukan asumsi makro untuk data PAM yang dibangun"),
        br(),
        fluidRow(
        column(3,
               sliderInput(("rate.p"), "Discount Rate Private", 7.4 ,min = 0, max = 15, step = 0.01)
        ),
        column(3,
               sliderInput(("rate.s"), "Discount Rate Social", 2.4 ,min = 0, max = 8, step = 0.01)
        ),
        column(4,
               sliderInput(("nilai.tukar"), "Nilai Tukar Rupiah", 14831 ,min = 10000, max = 20000, step = 10)
        ),
        column(2,
               br(),
               actionButton(("tampilkanTabel_button"),"Tampilkan Tabel PAM",icon("paper-plane"),style="color: white; 
                         background-color: green;")
        )
      )
      )
      )
    })
    # End - Section asumsi makro ---------------------------------------------

    # Section tampilkan tabel---------------------------------------------
    observeEvent(input$tampilkanTabel_button, {
      dataTemplate()
      resultTemplate()
      insertUI(selector='#uiShowTable',
               where='afterEnd',
               ui= uiOutput('showTable'))
      
      insertUI(selector='#uiShowButton',
               where='afterEnd',
               ui= uiOutput('showButton'))
      }) 
    
    output$showTable <- renderUI({
      argonRow(
      argonColumn(
        width = 12,
        argonH1("Tabel", display = 4),
        h5("Langkah 3: menampilkan Tabel PAM yang terpilih"),
        
        # jika tdk bisa jadi input buttton maka coba ubah nama action  buttonnya sepertinya conflict dengan script lain
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
            # tableOutput("cekTable"),
            dataTableOutput("showTablePrice"),
            style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
          ),
          argonTab(
            tabName = "Tabel Kuantitas",
            active = F,
            dataTableOutput(("showTableKuantitas")),
            style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
          ),
          argonTab(
            tabName = "Tabel Modal Kapital",
            active = F,
            dataTableOutput(("showTableKapital")),
            style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
          )
        ),
      )
      )
    })
    
    output$showTablePrice <- renderDataTable({
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      readDataLastEdited <- readRDS(fileName)
      dataView <- rbind(readDataLastEdited$priceInput, readDataLastEdited$priceOutput)
      dataView[is.na(dataView)] <- 0 #NA replace with zero
      dataView
      
      # if(!is.null(data.gab())){
      #   dataView <- rbind(readDataLastEdited$priceInput, readDataLastEdited$priceOutput) #karena output nya saja yg kmgknan berubah
      #   dataView[is.na(dataView)] <- 0 #NA replace with zero
      #   dataView
      # }else 
      #   if(!is.null(valP2Template())){
      #   dataView <- rbind(readDataLastEdited$priceInput, readDataLastEdited$priceOutput)
      #   dataView[is.na(dataView)] <- 0 #NA replace with zero
      #   dataView
      # }
      
      # CONDITION IF ELSE NYA DI UBAH
      # if(!is.null(valP2Template())){
      #   dataView <- rbind(readDataLastEdited$priceInput, readDataLastEdited$priceOutput) #karena output nya saja yg kmgknan berubah
      #   dataView[is.na(dataView)] <- 0 #NA replace with zero
      #   dataView
      # }else{
      #   dataView <- rbind(readDataLastEdited$priceInput, readDataLastEdited$priceOutput)
      #   dataView[is.na(dataView)] <- 0 #NA replace with zero
      #   dataView
      # }
      
      # if(!is.null(data.gab())){
      #   dataView <- rbind(readDataLastEdited()$priceInput, readDataLastEdited()$priceOutput)
      #   dataView
      # }else if(!is.null(valP1()) | !is.null(valP2Template())){
      #   dataView <- rbind(valP1(),valP2Template())
      #   dataView
      # }
    })
    
    output$showTableKuantitas <- renderDataTable({
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      # print("data terakhir tersimpan di rds")
      readDataLastEdited <- readRDS(fileName)
      dataView <- rbind(readDataLastEdited$ioInput, readDataLastEdited$ioOutput)
      dataView[is.na(dataView)] <- 0 #NA replace with zero
      dataView
      
    })
    
    output$showTableKapital <- renderDataTable({
      # case for modal kapital
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      cekCapital <- file.exists(paste0(datapath,"kapital template.csv")) #cek keberadaan file ini ada atau engga
      
      if(cekCapital == T){
        datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
        fileName <- paste0(datapath,"saveData","_",
                           # input$sut,"_",input$kom,"_",
                           input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
        # print("data terakhir tersimpan di rds")
        readDataLastEdited <- readRDS(fileName)
        dataView <- readDataLastEdited$capital
        dataView[is.na(dataView)] <- 0 #NA replace with zero
        dataView    
      }
      else if(cekCapital == F){
        dataView <- data.frame(matrix("tidak terdapat tabel modal kapital",nrow=1,ncol=1))
        colnames(dataView) <- "Keterangan"
        dataView
      }
    })
    
    output$showButton <- renderUI({
      argonRow(
        argonColumn(
          width = 12,
          br(),
          h5("Langkah 4: Membangun tabel PAM dengan menyunting atau menggunakan data template", align = "center"),
          fluidRow(
            column(5,
                   
            ),
            column(7,
                   actionButton("buatPAM_button","Membangun PAM",icon("paper-plane"),style="color: white;
                           background-color: green;")
            )
          )
        )
      )
      
    })
    
    # End - Section tampilkan tabel ---------------------------------------------
    
    # Section Popup Modal Dialog---------------------------------------------
    dataModalCreatePam <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("closeModalCreatePam"), "Batal")
        ),
        argonTabSet(
          id = "tabTemplate",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Langkah 5: Membangun Tabel",
            active = T,
            fluidRow(
              column(8,
                     actionButton(("sunting_button"),"Sunting Tabel Kuantitas, Harga, dan Modal Kapital",style="color: white;
                         background-color: green;"),
              ),
              column(4,
                     actionButton(("template_button"),"Gunakan Data Template",style="color: white;
                         background-color: green;")
              )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    
    
    observeEvent(input$buatPAM_button,{
      # browser()
      showModal(dataModalCreatePam())
      
    })
    
    observeEvent(input$closeModalCreatePam,{
      removeModal()
    })

    observeEvent(input$template_button,{
      showModal(
        modalDialog( 
          footer=tagList(
            actionButton(("backModalCreatePam"), "Kembali"),
            actionButton(("running_button"), "Jalankan Analisis",style="color: white;background-color: green;")
          ),
          argonTabSet(
            id = "tabPrice",
            card_wrapper = TRUE,
            horizontal = TRUE,
            circle = FALSE,
            size = "sm",
            width = 12,
            argonTab(
              tabName = "Sunting Harga Output",
              active = T,
              sidebarLayout(
                sidebarPanel(
                  fluidPage(
                    h3("Sunting secara manual"),
                    tags$h5("kolom yang dapat di sunting hanya kolom harga.privat dan harga.sosial"),
                    
                  ),
                  tags$br(),
                  width=12
                ),
                mainPanel(
                  rHandsontableOutput(('editPriceOutputTemplate')),
                  tags$br(),
                  actionButton(('savePriceOutputTemplate'), 'simpan tabel'), 
                  tags$br(), 
                  tags$br(),
                  tags$div(id='teksPriceOutputSaveTemplate'),
                  width=12)
              )
            ))
          ,
          size="l",
          easyClose = FALSE)
      )
    })
    
    observeEvent(input$backModalCreatePam,{
      showModal(dataModalCreatePam())
    })
    
    observeEvent(input$running_button,{
      removeModal()
        insertUI(selector='#uiShowResult',
                 where='afterEnd',
                 ui= uiOutput('showResult'))
    })
    
    # Start Price Output ------------------------------------------------------
    reactData <- reactiveValues(
      tableP1 = NULL, #price input
      tableP2 = NULL, #price output
      tableIO1 = NULL, #io input
      tableIO2 = NULL, #io output
      tableCapP = NULL, #capital privat
      tableCapS = NULL #capital sosial
    )
    
    valP2Template <- eventReactive(input$template_button,{
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      reactData$tableP2 <- dataDefine$priceOutput
      reactData$tableP2
      
      
      # indexRow <- as.numeric(nrow(dataDefine$ioOutput))
      # reactData$tableP2 <- dataDefine$ioOutput[,1:2]
      # no.id <- as.numeric(rownames(reactData$tableP2))
      # reactData$tableP2 <- cbind(no.id,reactData$tableP2)
      # 
      # readDataTemplate <- read.table(paste0(datapath,input$selected_provinsi,"_",input$th,".csv"), header = T, sep = ",")
      # yearIO <- 30 #tahun daur tanam
      # outputData <- filter(readDataTemplate,faktor == c("output"))
      # templatePriceOutput <- outputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
      # 
      # # templatePriceOutput <- read.table(paste0(datapath,"price template output.csv"), header = T, sep = ",")
      # templatePrice <- (templatePriceOutput[,-1])
      # reactData$tableP2 <- merge(reactData$tableP2,unique(templatePrice), by.x = "jenis",by.y = "jenis", all.x = T)
      # reactData$tableP2 <- reactData$tableP2[order(reactData$tableP2$no.id),]     #sort by nomor yang disesuaikan pada tabel i-o
      # rownames(reactData$tableP2) <- no.id
      # reactData$tableP2 <- reactData$tableP2[, c(3, 1, 4, 5,6)]
      # reactData$tableP2
      # 
      # reactData$tableP2 <- reactData$tableP2[1:indexRow,]
      # reactData$tableP2
    })
    
    output$editPriceOutputTemplate <- renderRHandsontable({
      rhandsontable(valP2Template(),
                    rowHeaderWidth = 50,
                    fixedColumnsLeft = 2,
                    height = 300,
      )
    })
    
    observeEvent(input$savePriceOutputTemplate,{
      removeUI(selector='#textTampilOutputTemplate')
      # browser()
      editNew<-as.data.frame(hot_to_r(input$editPriceOutputTemplate))
      editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
      
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      # replace data price
      dataDefine$priceOutput <- editNew
      dataDefine$rate.p <- input$rate.p
      dataDefine$rate.s <- input$rate.s
      dataDefine$nilai.tukar <- input$nilai.tukar
      dataDefine$tipeData <- c("SIMULASI")
      
      saveRDS(dataDefine,file = fileName)
      
      insertUI(selector='#teksPriceOutputSaveTemplate',
               where = 'afterEnd',
               ui = tags$div(id="textTampilOutputTemplate","tabel di atas sudah tersimpan"))
      # browser()
    })
    
    # Ending Price Output -----------------------------------------------------
    
    
    
    output$showResult <- renderUI({
      fluidPage(
        fluidRow(
          column(11,
                 br(),
                 br(),
                 h1("Hasil Analisis", align = "center"),
                 br(),
          )
        ),
        br(),
        fluidRow(
          column(6,
                 h3("Business As Usual (BAU)", align = "center")
          ),
          column(6,
                 h3("Simulasi", align = "center")
          )
        ),
        fluidRow(
          column(4,
                 dataTableOutput("tableResultBAU1"),
          ),
          column(2,
                 dataTableOutput("tableResultBAU2")
                 
          ),
          column(4,
                 dataTableOutput("tableResultSimulasi1"),
                 
          ),
          column(2,
                 dataTableOutput("tableResultSimulasi2")
                 
          ),
        ),
        fluidRow(
          column(9,
                 plotlyOutput('plotComparing')
          ),
          column(3,
                 actionButton(("saveNewPAM"),"Simpan PAM baru",icon("paper-plane"),style="color: white;background-color: green;"),
                 br(),
                 tags$div(id='teksNewPamSave')
          )
        )
        
        
      )
    })
    
    ################################################################################
    #                                                                              #
    #                                RESULT                                        #
    #                                                                              #
    ################################################################################
    data.gab <- eventReactive(input$running_button,{
      # observeEvent(input$running_button,{
      # browser()
      # 
      # aktifin dataTemplate
      # agar ketika run pertama kali yang terbaca tetap data default di excel
      
      # resultTemplate()
      # dataTemplate()
      
      #setelah dataTemplate(data default) aktif, 
      # lalu read kembali file rds yang tersimpan dr hasil edit jika ada yang diedit
      # datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      # fileName <- paste0(datapath,"saveData","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,".rds")
      
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"resultTemplate","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
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
        data.gab <- bind_rows(io.all,
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
      p.budget <- dataGeneral[-(c(1:5,36))] * dataPrivat[-c(1:5,36)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
      p.budget <- cbind(dataGeneral[c(1:4)],dataPrivat[36],p.budget) #memunculkan kembali variabel 1 sd 5
      p.budget <- p.budget %>%
        mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
      
      #perkalian antara general dengan Social Price
      dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
      s.budget <- dataGeneral[-c(1:5,36)] * dataSosial[-c(1:5,36)]
      s.budget <- cbind(dataGeneral[c(1:4)],dataSosial[36],s.budget)
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
      
      npv.p<-npv(dataDefine$rate.p/100,p.profit)
      npv.s<-npv(dataDefine$rate.s/100,s.profit)
      
      hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
      
      npv.p.us<-npv.p/dataDefine$nilai.tukar
      npv.s.us<-npv.s/dataDefine$nilai.tukar
      npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
      hsl.npv<-rbind(hsl.npv,npv.us)
      
      #browser()
      
      rownames(hsl.npv)<-c("NPV (Rp/Ha)", "NPV (US/Ha)")
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
      rownames(nlc)<-c("Non Labor Cost (Juta Rp/Ha)")
      nlc
      # ending  nlc ------------------------------------------------------- 
      
      # hitung EC --------------------------------------------------------------
      ############# PERHITUNGAN ESTABLISHMENT COST
      p.ec <- p.sum.cost[[1]]/1000000
      s.ec <- s.sum.cost[[1]]/1000000
      ec <- data.frame(p.ec,s.ec)
      ec<-data.frame(PRIVATE=p.ec,SOCIAL=s.ec)
      rownames(ec)<-c("Establishment cost (1 tahun pertama, Juta Rp/Ha)")
      ec
      
      # ending  EC ------------------------------------------------------- 
      
      # hitung hp --------------------------------------------------------------
      ############# PERHITUNGAN HARVESTING PRODUCT
      fil.prod <- dataGeneral %>%  filter(str_detect(grup,"output")) #filter io untuk grup output (hasil panen)
      fil.prod <- fil.prod %>%  filter(str_detect(komponen,"utama"))
      sum.prod <- fil.prod[,-c(1:5,36)] %>%
        colSums(na.rm = T)
      tot.prod <- sum(sum.prod)
      
      fil.labor <- dataGeneral %>%  filter(str_detect(komponen, c("tenaga kerja")))
      sum.labor <- fil.labor[,-c(1:5,36)] %>%
        colSums(na.rm = T)
      tot.labor <- sum(sum.labor)
      
      hp <- data.frame(tot.prod/tot.labor)/1000 # karena ton jadi di bagi 1000
      colnames(hp)<-c("Harvesting Product (ton/HOK) Labor Req for Est (1 tahun pertama)")
      rownames(hp) <- c("Nilai")
      hp <- data.frame(t(hp))
      hp
      # ending  hp ------------------------------------------------------- 
      
      # hitung lr --------------------------------------------------------------
      ############# PERHITUNGAN LABOR REQ FOR EST
      lr <- data.frame(sum.labor[[1]]) #pekerja pada tahun 1
      colnames(lr)<-c("Labor Req for Est (1 tahun pertama)")
      rownames(lr) <- c("Nilai")
      lr <- data.frame(t(lr))
      lr
      
      # ending  lr ------------------------------------------------------- 
      
      tabel1 <- rbind(hsl.npv,nlc,ec)
      tabel1[] <- lapply(tabel1, function(i) sprintf('%.6g', i))
      tabel1
      
      tabel2 <- rbind(hp,lr)
      tabel2[] <- lapply(tabel2, function(i) sprintf('%.6g', i))
      tabel2
      
      tabelGab <- list(tabel1=tabel1,tabel2=tabel2)
      tabelGab
      
      
      
    })
    
    data.gab.new <- eventReactive(input$running_button,{
      # observeEvent(input$running_button,{
      # browser()
      # 
      # aktifin dataTemplate
      # agar ketika run pertama kali yang terbaca tetap data default di excel
      
      # resultTemplate()
      # dataTemplate()
      
      #setelah dataTemplate(data default) aktif, 
      # lalu read kembali file rds yang tersimpan dr hasil edit jika ada yang diedit
      # datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      # fileName <- paste0(datapath,"saveData","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_provinsi,".rds")
      
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
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
        data.gab <- bind_rows(io.all,
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
      p.budget <- dataGeneral[-(c(1:5,36))] * dataPrivat[-c(1:5,36)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
      p.budget <- cbind(dataGeneral[c(1:4)],dataPrivat[36],p.budget) #memunculkan kembali variabel 1 sd 5
      p.budget <- p.budget %>%
        mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
      
      #perkalian antara general dengan Social Price
      dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
      s.budget <- dataGeneral[-c(1:5,36)] * dataSosial[-c(1:5,36)]
      s.budget <- cbind(dataGeneral[c(1:4)],dataSosial[36],s.budget)
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
      
      rownames(hsl.npv)<-c("NPV (Rp/Ha)", "NPV (US/Ha)")
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
      rownames(nlc)<-c("Non Labor Cost (Juta Rp/Ha)")
      nlc
      # ending  nlc ------------------------------------------------------- 
      
      # hitung EC --------------------------------------------------------------
      ############# PERHITUNGAN ESTABLISHMENT COST
      p.ec <- p.sum.cost[[1]]/1000000
      s.ec <- s.sum.cost[[1]]/1000000
      ec <- data.frame(p.ec,s.ec)
      ec<-data.frame(PRIVATE=p.ec,SOCIAL=s.ec)
      rownames(ec)<-c("Establishment cost (1 tahun pertama, Juta Rp/Ha)")
      ec
      
      # ending  EC ------------------------------------------------------- 
      
      # hitung hp --------------------------------------------------------------
      ############# PERHITUNGAN HARVESTING PRODUCT
      fil.prod <- dataGeneral %>%  filter(str_detect(grup,"output")) #filter io untuk grup output (hasil panen)
      fil.prod <- fil.prod %>%  filter(str_detect(komponen,"utama"))
      sum.prod <- fil.prod[,-c(1:5,36)] %>%
        colSums(na.rm = T)
      tot.prod <- sum(sum.prod)
      
      fil.labor <- dataGeneral %>%  filter(str_detect(komponen, c("tenaga kerja")))
      sum.labor <- fil.labor[,-c(1:5,36)] %>%
        colSums(na.rm = T)
      tot.labor <- sum(sum.labor)
      
      hp <- data.frame(tot.prod/tot.labor)/1000 # karena ton jadi di bagi 1000
      colnames(hp)<-c("Harvesting Product (ton/HOK) Labor Req for Est (1 tahun pertama)")
      rownames(hp) <- c("Nilai")
      hp <- data.frame(t(hp))
      hp
      # ending  hp ------------------------------------------------------- 
      
      # hitung lr --------------------------------------------------------------
      ############# PERHITUNGAN LABOR REQ FOR EST
      lr <- data.frame(sum.labor[[1]]) #pekerja pada tahun 1
      colnames(lr)<-c("Labor Req for Est (1 tahun pertama)")
      rownames(lr) <- c("Nilai")
      lr <- data.frame(t(lr))
      lr
      
      # ending  lr ------------------------------------------------------- 
      
      # RESULT 
      dataDefine$npv <- hsl.npv
      dataDefine$nlc <- nlc
      dataDefine$ec <- ec
      dataDefine$hp <- hp
      dataDefine$lr <- lr
      saveRDS(dataDefine,file = fileName)
      
      
      tabel1 <- rbind(hsl.npv,nlc,ec)
      tabel1[] <- lapply(tabel1, function(i) sprintf('%.6g', i))
      tabel1
      
      tabel2 <- rbind(hp,lr)
      tabel2[] <- lapply(tabel2, function(i) sprintf('%.6g', i))
      tabel2
      
      tabelGab <- list(tabel1=tabel1,tabel2=tabel2)
      tabelGab
      
      
      
    })
    
    output$tableResultBAU1 <- renderDataTable({
      datatable(data.gab()$tabel1, option=list(dom = "t"))
    })
    
    output$tableResultBAU2 <- renderDataTable({
      datatable(data.gab()$tabel2, option=list(dom = "t"))
      
    })
    
    output$tableResultSimulasi1 <- renderDataTable({
      datatable(data.gab.new()$tabel1, option=list(dom = "t"))
    })
    
    output$tableResultSimulasi2 <- renderDataTable({
      datatable(data.gab.new()$tabel2, option=list(dom = "t"))
    })
    
    
    preparePlot <- eventReactive(input$running_button,{
      # row_to_select=as.numeric(gsub("Row","",input$checked_rows))
      # row_to_select_newPam = as.numeric(gsub("Row","",input$checked_rows_newPam))
      
      # data template BAU
      # dataCheck <- loadRDSAll()
      # dataCheck <- dataCheck[row_to_select]
      # sut <- unlist(lapply(dataCheck, function(x)x[[15]]))
      # komoditas <- unlist(lapply(dataCheck, function(x)x[[16]]))
      # NPV.Privat.RP <- unlist(lapply(dataCheck, function(x)x[[7]][1,1]))
      # browser()
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"resultTemplate","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      
      dataPlotBAU <- data.frame(tipe.data=dataDefine$tipeData,
                                    komoditas=dataDefine$kom,
                                    NPV.Privat.RP=dataDefine$npv[1,1])
      
      # data simulasi Pam baru
      # dataCheckNewPam <- loadRDSAllNewPam()
      # dataCheckNewPam <- dataCheckNewPam[row_to_select_newPam]
      # sut <- unlist(lapply(dataCheckNewPam, function(x)x[[15]]))
      # sut<-paste0("PAM BARU ",sut)
      # komoditas <- unlist(lapply(dataCheckNewPam, function(x)x[[16]]))
      # komoditas<-paste0("PAM BARU ",komoditas) # supaya barchartnya terpisah dari setiap komoditas
      # NPV.Privat.RP <- unlist(lapply(dataCheckNewPam, function(x)x[[7]][1,1]))
      
      # datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      dataPlotSimulasi <- data.frame(tipe.data=dataDefine$tipeData,
                                   komoditas=dataDefine$kom,
                                   NPV.Privat.RP=dataDefine$npv[1,1])
      
      
      dataPlot <- rbind(dataPlotBAU,dataPlotSimulasi)
      
      
      dataPlot %>%
        group_by(tipe.data) %>%
        plot_ly(x = ~komoditas, y = ~NPV.Privat.RP, type = "bar", color = ~tipe.data)
    })
    
    output$plotComparing <- renderPlotly({
      preparePlot()
    })
    
   
    
    # End - Section Popup Modal Dialog ---------------------------------------------
    
    # Start - Section sunting_button---------------------------------------------
    ################################################################################
    #                                                                              #
    #                                 BUTTON IO                                    #
    #                                                                              #
    ################################################################################
    observeEvent(input$sunting_button,{
      showModal(
        modalDialog( 
          footer=tagList(
            # actionButton(("backModalCreatePam"), "Kembali"),
            actionButton(("sunting_button_1"), "Lanjut",style="color: white;background-color: green;")
          ),
          argonTabSet(
            id = "tabSunting1",
            card_wrapper = TRUE,
            horizontal = TRUE,
            circle = FALSE,
            size = "l",
            width = 12,
            argonTab(
              tabName = "Pilih Tahun Daur Tanam",
              active = T,
              selectInput(("ioYear_input"),
                          "pilih tahun skenario:",
                          choices = c(30:60),selected = 30) # pilihannnya masih 30 tahun sesuai default
            ))
          ,
          size="l",
          easyClose = FALSE)
      )
    })
    
    modalPilihBaris <- function(failed = FALSE) {
      modalDialog( 
        footer=tagList(
          # actionButton(("backModalCreatePam"), "Kembali"),
          actionButton(("sunting_button_2"), "Lanjut",style="color: white;background-color: green;")
        ),
        argonTabSet(
          id = "tabSunting2",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Menentukan Komponen (Baris) pada Input Tabel Kuantitas",
            active = T,
            h3("Apakah user akan menambahkan komponen (baris) pada bagian Input Tabel Kuantitas?"),
            radioButtons(("ioKomponen_input"),
                         " ",
                         choices = c("Tidak","Ya"),selected = "Tidak"), # pilihannnya masih 30 tahun sesuai default
            # tags$div(id='tambahBaris')
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    
    observeEvent(input$sunting_button_1,{
      showModal(modalPilihBaris())
    })
    
    # observeEvent(input$ioKomponen_input,{
    #   if (input$ioKomponen_input == "Ya"){
    #     insertUI(selector='#tambahBaris',
    #              where = 'afterEnd',
    #              ui = uiOutput("pilihTambahBaris")) 
    #   }else if(input$ioKomponen_input == "Tidak"){
    #     removeUI(selector='#pilihTambahBaris')
    #   }
    # })
    # 
    # 
    # output$pilihTambahBaris<- renderUI({
    #   selectInput(("pilihTambahBaris_input"),
    #               "Berapa komponen (baris) yang akan ditambahkan pada Input Tabel Kuantitas?",
    #               choices = c(1:10),selected = 1,width = "600px")
    #   
    # })
    
    modalTambahBaris <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("sunting_button_3"),"Simpan dan Lanjut",style="color: white;
                         background-color: green;")
        ),
        argonTabSet(
          id = "tabSunting3",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Sunting Tabel Kuantitas (Input)",
            active = T,
            fluidRow(
              column(6,
                     selectInput('pilihKomponenInput',"Pilih Komponen", choices = c("pupuk", "bibit", "peralatan","tenaga kerja")),
                     
              ),
              column(6,
                     br(),
                     actionButton("showTabelJenis","Tampilkan Tabel")
                     ),
              column(12,
                     tags$div(id = 'uiShowTablePilihJenis')
                     )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    
    observeEvent(input$showTabelJenis,{
      insertUI(selector='#uiShowTablePilihJenis',
               where='afterEnd',
               ui= uiOutput('showTablePilihJenis'))
      
      
    }) 
    
    output$showTablePilihJenis <- renderUI({
        if (input$pilihKomponenInput == "pupuk"){
          fluidRow(
            column(12,
                   selectInput("tambahBarisPupuk",
                                    "Berapa baris yang akan ditambahkan untuk komponen pupuk?",
                                    choices = c(0:10),selected = 5,width = "600px")
            ),
            column(12,
                   rHandsontableOutput('tabelJenis')
            )
          )
          # selectInput("tambahBarisPupuk",
          #                  "Berapa baris yang akan ditambahkan untuk komponen pupuk?",
          #                  choices = c(0:10),selected = 0)
          # br()
          # rHandsontableOutput('tabelJenis')
        }
    })
    
    
    valJenis <- eventReactive(input$showTabelJenis,{
      # datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      # fileName <- paste0(datapath,"saveData","_",
      #                    # input$sut,"_",input$kom,"_",
      #                    input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      # dataDefine <- readRDS(fileName)
      # 
      # reactData$tableIO1 <- dataDefine$ioInput
      # reactData$tableIO1
      dataPupuk
    })
    
    output$tabelJenis <- renderRHandsontable({
      rhandsontable(valJenis(),
                    rowHeaderWidth = 50,
                    fixedColumnsLeft = 2,
                    height = 300,
      )
    })
    
    
    observeEvent(input$sunting_button_3,{
      # browser()
      showModal(suntingTabelKuantitas_input())
    })
    
    
    suntingTabelKuantitas_input <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("batalSunting_button"), "Batal", style="color: white;background-color: red;"),
          actionButton("backtoPilihBaris_button","Kembali"),
          actionButton(("sunting_button_4"),"Simpan dan Lanjut",style="color: white;
                         background-color: green;")
        ),
        argonTabSet(
          id = "tabSunting4",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Langkah 6: Sunting Tabel Kuantitas (Input)",
            active = T,
            fluidRow(
              column(12,
                     h1("Tabel kuantitas",align = "center"),
                     rHandsontableOutput(('suntingKuantitasInput'))
                     )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$batalSunting_button,{
      removeModal()
    })
    
    observeEvent(input$backtoPilihBaris_button,{
      showModal(modalPilihBaris())
    })
    
    
   observeEvent(input$sunting_button_2,{
     if (input$ioKomponen_input == "Ya"){
       showModal(modalTambahBaris()) 
     }else if(input$ioKomponen_input == "Tidak"){
       showModal(suntingTabelKuantitas_input())
     }
   })
   
   valIO1 <- eventReactive(input$sunting_button_2,{
     datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
     fileName <- paste0(datapath,"saveData","_",
                        # input$sut,"_",input$kom,"_",
                        input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
     dataDefine <- readRDS(fileName)
     
     reactData$tableIO1 <- dataDefine$ioInput
     reactData$tableIO1
     
   })
   
   output$suntingKuantitasInput <- renderRHandsontable({
     rhandsontable(valIO1(),
                   rowHeaderWidth = 50,
                   fixedColumnsLeft = 2,
                   height = 300,
     )
   })
    
    
    
    # End - Section sunting_button---------------------------------------------
    
   # Section informasi umum new---------------------------------------------
   observe({
     updateSelectInput(
       session,
       "kom_new",
       choices = komoditas %>%
         filter(sut == input$sut_new) %>%
         select(nama_komoditas) %>%
         .[[1]]
     )
   })
   
   observe({
     updateSelectInput(
       session,
       "selected_provinsi_new",
       choices = komoditas %>%
         filter(nama_komoditas == input$kom_new) %>%
         select(provinsi) %>%
         .[[1]]
     )
   })
   
   observe({
     updateSelectInput(
       session,
       "th_new",
       choices = komoditas %>%
         filter(provinsi == input$selected_provinsi_new) %>%
         select(tahun_analisis) %>%
         .[[1]]
     )
   })
   
   observe({
     updateSelectInput(
       session,
       "tipeLahan_new",
       choices = komoditas %>%
         filter(tahun_analisis == input$th_new) %>%
         select(tipe_lahan) %>%
         .[[1]]
     )
   })
   
   # End - Section informasi umum new---------------------------------------------
   
   
   
   # Section asumsi makro NEW---------------------------------------------
   reactData_new <- reactiveValues(
     tableP1 = NULL, #price input
     tableP2 = NULL, #price output
     tableIO1 = NULL, #io input
     tableIO2 = NULL, #io output
     tableCapP = NULL, #capital privat
     tableCapS = NULL #capital sosial
   )
   
   data_new <- reactive({
     # informasi umum
     sut <- input$sut_new
     kom <- input$kom_new
     provinsi <- input$selected_provinsi_new
     th <- input$th_new
     tipeLahan <- input$tipeLahan_new
     
     
     
     
     combineDef <- list(
                        sut=sut,
                        kom=kom,
                        provinsi = provinsi,
                        th=th,
                        tipeLahan = tipeLahan)
     
     # save data untuk setiap perubahan
     datapath <- paste0("shiny/data/", input$sut_new, "/")
     fileName <- paste0(datapath,"saveData_new","_",
                        # input$sut,"_",input$kom,"_",
                        input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
     saveRDS(combineDef,file = fileName)
     combineDef
     
   })
   
   
   observeEvent(input$asumsiMakro_button_new, {
     data_new()
     
     insertUI(selector='#uiShowMakro_new',
              where='afterEnd',
              ui= uiOutput('showMakro_new'))
     
     
   }) 
   
   output$showMakro_new <- renderUI({
     argonRow(
       argonColumn(
         width = 12,
         argonH1("Asumsi Makro", display = 4),
         h5("Langkah 2: menentukan asumsi makro untuk data PAM yang dibangun"),
         br(),
         fluidRow(
           column(3,
                  sliderInput(("rate.p_new"), "Discount Rate Private", 7.4 ,min = 0, max = 15, step = 0.01)
           ),
           column(3,
                  sliderInput(("rate.s_new"), "Discount Rate Social", 2.4 ,min = 0, max = 8, step = 0.01)
           ),
           column(4,
                  sliderInput(("nilai.tukar_new"), "Nilai Tukar Rupiah", 14831 ,min = 10000, max = 20000, step = 10)
           ),
           column(2,
                  br(),
                  actionButton(("pilihBarisInput_new"),"Membangun Tabel PAM",icon("paper-plane"),style="color: white; 
                         background-color: green;")
           )
         )
       )
     )
   })
   # End - Section asumsi makro NEW---------------------------------------------
  
   modalPilihBarisInput_new <- function(failed = FALSE) {
     modalDialog( 
       footer=tagList(
         actionButton(("bangunKuantitas_new"), "Lanjut",style="color: white;background-color: green;")
       ),
       argonTabSet(
         id = "tabNew",
         card_wrapper = TRUE,
         horizontal = TRUE,
         circle = FALSE,
         size = "l",
         width = 12,
         argonTab(
           tabName = "Menentukan Jumlah Komponen (Baris) Pada Tabel Kuantitas bagian Input",
           active = T,
           h3("Berapa jumlah komponen (baris) yang akan user bangun pada Input-Tabel Kuantitas?"),
           selectInput(("pilihTambahBaris_input_new"),
                       " ",
                       choices = c(5:40),selected = 5,width = "800px")
         ))
       ,
       size="l",
       easyClose = FALSE)
   }
   
   
   observeEvent(input$pilihBarisInput_new,{
     showModal(modalPilihBarisInput_new())
   })
    
   modalTabelKuantitas_new <- function(failed = FALSE) {
     modalDialog( 
       footer=tagList(
         actionButton(("pilihBaris_output_new"), "Simpan Tabel dan Lanjutkan Membangun Tabel Harga",style="color: white;background-color: green;")
       ),
       argonTabSet(
         id = "tabNew",
         card_wrapper = TRUE,
         horizontal = TRUE,
         circle = FALSE,
         size = "l",
         width = 12,
         argonTab(
           tabName = "Langkah 3: Mengisi Tabel Kuantitas Bagian Input",
           active = T,
           h3("Tabel Kuantitas", align = "center"),
           rHandsontableOutput('kuantitasInput_new')
         ))
       ,
       size="l",
       easyClose = FALSE)
   }
   
   valIO1_new <- eventReactive(input$bangunKuantitas_new,{
     readDataTemplate <- read.table(paste0("shiny/data/template/tabel pam kosong",".csv"), header = T, sep = ",")
     yearIO <- 30 #tahun daur tanam
     
     inputData <- readDataTemplate[1:input$pilihTambahBaris_input_new,]
     ioInput <- inputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
     ioInput$komponen <- as.character(ioInput$komponen)
     ioInput$jenis<- as.character(ioInput$jenis)
     ioInput$unit<- as.character(ioInput$unit)
     ioInput[,c(4:33)] <- as.numeric(as.character(ioInput[,c(4:33)]))
     
     reactData_new$tableIO1 <- ioInput
     reactData_new$tableIO1
     
   })
   
   output$kuantitasInput_new <- renderRHandsontable({
     rhandsontable(valIO1_new(),
                   rowHeaderWidth = 50,
                   fixedColumnsLeft = 2,
                   height = 300,
     )
   })
   
   
   observeEvent(input$bangunKuantitas_new,{
     # browser()
     showModal(modalTabelKuantitas_new())
   })
    
  }
)

runApp(app)