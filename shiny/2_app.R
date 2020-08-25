
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
# indonesia <- read.csv("shiny/data/template/prov sampai desa.csv", stringsAsFactors = F)

# elements
source("shiny/2_verifikasi.R")
source("shiny/pambaru_modul2.R")
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
      inputData <- filter(readDataTemplate,faktor == c("input"))
      ioInput <- inputData[,c(2:4,8:37)] #menghilangkan kolom harga
      priceInput <- inputData[,c(2,3,5,6,7)] #mengambil kolom harga
      
      outputData <- filter(readDataTemplate,faktor == c("output"))
      ioOutput <- outputData[,c(2:4,8:37)] #menghilangkan kolom harga
      priceOutput <- outputData[,c(2,3,5,6,7)] #mengambil kolom harga
      
      
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
      
      
      combineDef <- list(ioInput=ioInput,ioOutput=ioOutput,
                         priceInput=priceInput,priceOutput=priceOutput,
                         capital=capital,
                         sut=sut,
                         kom=kom,
                         provinsi = provinsi,
                         th=th,
                         tipeLahan = tipeLahan)
      
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
      inputData <- filter(readDataTemplate,faktor == c("input"))
      ioInput <- inputData[,c(2:4,8:37)] #menghilangkan kolom harga
      priceInput <- inputData[,c(2,3,5,6,7)] #mengambil kolom harga
      
      outputData <- filter(readDataTemplate,faktor == c("output"))
      ioOutput <- outputData[,c(2:4,8:37)] #menghilangkan kolom harga
      priceOutput <- outputData[,c(2,3,5,6,7)] #mengambil kolom harga
      
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
      p.budget <- cbind(dataGeneral[c(1:4,36)],p.budget) #memunculkan kembali variabel 1 sd 5
      p.budget <- p.budget %>%
        mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
      
      #perkalian antara general dengan Social Price
      dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
      s.budget <- dataGeneral[-c(1:5,36)] * dataSosial[-c(1:5,36)]
      s.budget <- cbind(dataGeneral[c(1:4,36)],s.budget)
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
      }) 
    
    output$showTable <- renderUI({
      argonRow(
      argonColumn(
        width = 12,
        argonH1("Tabel", display = 4),
        h5("Langkah 3: menampilkan Tabel PAM yang terpilih"),
        
        # ga bisa pakai dataTableOutput krn action button dibawah tabset ini tdk bisa jadi input buttton utk showmodal berikutnya
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
    
    observeEvent(input$tampilkanTabel_button, {
      insertUI(selector='#uiShowButton',
               where='afterEnd',
               ui= uiOutput('showButton'))


    })

    output$showButton <- renderUI({
      argonRow(
        argonColumn(
        width = 12,
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



    
    output$showTablePrice <- renderDataTable({
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      # print("data terakhir tersimpan di rds")
      readDataLastEdited <- readRDS(fileName)
      dataView <- rbind(readDataLastEdited$priceInput, readDataLastEdited$priceOutput)
      dataView[is.na(dataView)] <- 0 #NA replace with zero
      dataView
      
      # CONDITION IF ELSE NYA DI UBAH
      # if(!is.null(valP1()) | !is.null(valP2())){
      #   dataView <- rbind(valP1(),valP2())
      #   dataView
      # }else{
      #   datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      #   fileName <- paste0(datapath,"saveData","_",
      #                      # input$sut,"_",input$kom,"_",
      #                      input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      #   # print("data terakhir tersimpan di rds")
      #   readDataLastEdited <- readRDS(fileName)
      #   dataView <- rbind(readDataLastEdited$priceInput, readDataLastEdited$priceOutput)
      #   dataView
      # }
      
      # if(!is.null(data.gab())){
      #   dataView <- rbind(readDataLastEdited()$priceInput, readDataLastEdited()$priceOutput)
      #   dataView
      # }else if(!is.null(valP1()) | !is.null(valP2())){
      #   dataView <- rbind(valP1(),valP2())
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
    
    # End - Section tampilkan tabel ---------------------------------------------
    
    # Section Popup Modal Dialog---------------------------------------------
    observeEvent(input$buatPAM_button,{
      # browser()
      showModal(
        modalDialog(
          footer=tagList(
            actionButton(("closeModalBuatPam"), "Batal")
          ),
          h1("Langkah 5: Membangun Tabel", align="center"),
          br(),
          br(),
          br(),
          actionButton(("sunting_button"),"Sunting Tabel Kuantitas, Harga, dan Modal Kapital",style="color: white;
                         background-color: green;"),
          
          actionButton(("template_button"),"Gunakan Data Template",style="color: white;
                         background-color: green;")
          ,
          size="l",
          easyClose = FALSE)
      )
      
    })
    
    observeEvent(input$closeModalBuatPam,{
      removeModal()
    })

    observeEvent(input$template_button,{
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
    
    # End - Section Popup Modal Dialog ---------------------------------------------
    

    
    
    
    
    
  }
)

runApp(app)