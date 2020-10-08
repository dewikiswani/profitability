
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
library(shinyalert)

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
kumpulanDataJenisInputOutput <- read.csv("shiny/data/template/kumpulan jenis input output.csv")
indonesia <- read.csv("shiny/data/template/prov sampai desa.csv", stringsAsFactors = F)


# elements
source("shiny/2_pamTemplate_modul1.R")
source("shiny/2_pamBaru_modul2.R")
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
        pamTemplate,
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
    
    # # Section informasi umum ---------------------------------------------
    source("shiny/server/informasi umum.R", local = TRUE)
    # # End - Section informasi umum ---------------------------------------
    
    
    
    # Section preparation data ---------
    dataTemplate <- reactive({
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      
      readDataTemplate <- read.table(paste0(datapath,input$sut,"_",input$kom,"_",input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".csv"), header = T, sep = ",")
      readDataTemplate[is.na(readDataTemplate)] <- 0
      readDataTemplate <- lowcase(readDataTemplate, c(7:11))
        
      yearIO <- 30 #tahun daur tanam
      
      inputData <- filter(readDataTemplate,faktor == c("input"))
      ioInput <- inputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
      priceInput <- inputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
      
      outputData <- filter(readDataTemplate,faktor == c("output"))
      ioOutput <- outputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
      priceOutput <- outputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
      
      
      capital <- filter(readDataTemplate,faktor == c("modal kapital"))
      
      # kondisi if else nya dari cek jumlah row di variable capital
      if(dim(capital)[1] == 0){
        capital <- NULL
        capitalPrivat <- NULL
        capitalSosial <- NULL
      } else if(dim(capital)[1] > 0){
        capital <- capital[c(8,9,11,14:43)]
        capitalPrivat <- filter(capital,str_detect(komponen,"privat"))
        capitalSosial <- filter(capital,str_detect(komponen,"sosial"))
      }

      # informasi umum
      sut <- input$sut
      kom <- input$kom
      provinsi <- input$selected_provinsi
      th <- input$th
      tipeLahan <- input$tipeLahan
      tipeKebun <- readDataTemplate$tipe.kebun
      
      # asumsi makro
      rate.p <- input$rate.p
      rate.s <- input$rate.s
      nilai.tukar <- input$nilai.tukar
      
      combineDef <- list(ioInput=ioInput,ioOutput=ioOutput,
                         priceInput=priceInput,priceOutput=priceOutput,
                         capital=capital, capitalPrivat = capitalPrivat, capitalSosial = capitalSosial,
                         sut=sut,
                         kom=kom,
                         provinsi = provinsi,
                         th=th,
                         tipeLahan = tipeLahan,
                         tipeKebun = tipeKebun,
                         rate.p = rate.p,
                         rate.s = rate.s,
                         nilai.tukar = nilai.tukar)
      
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
      
      readDataTemplate <- read.table(paste0(datapath,input$sut,"_",input$kom,"_",input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".csv"), header = T, sep = ",")
      readDataTemplate[is.na(readDataTemplate)] <- 0
      readDataTemplate <- lowcase(readDataTemplate, c(7:11))
      yearIO <- 30 #tahun daur tanam
      
      inputData <- filter(readDataTemplate,faktor == c("input"))
      ioInput <- inputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
      priceInput <- inputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
      
      outputData <- filter(readDataTemplate,faktor == c("output"))
      ioOutput <- outputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
      priceOutput <- outputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
      
      
      
      # case for modal kapital
      capital <- filter(readDataTemplate,faktor == c("modal kapital"))
      
      # kondisi if else nya dari cek jumlah row di variable capital
      if(dim(capital)[1] == 0){
        capital <- NULL
        capitalPrivat <- NULL
        capitalSosial <- NULL
      } else if(dim(capital)[1] > 0){
        capital <- capital[c(8,9,11,14:43)]
        capitalPrivat <- filter(capital,str_detect(komponen,"privat"))
        capitalSosial <- filter(capital,str_detect(komponen,"sosial"))
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
                         capital=capital, capitalPrivat = capitalPrivat, capitalSosial = capitalSosial,
                         rate.p = rate.p,
                         rate.s = rate.s,
                         nilai.tukar=nilai.tukar)
      
      #informasi umum
      dataDefine$sut <- input$sut
      dataDefine$kom <- input$kom
      dataDefine$provinsi <- input$selected_provinsi
      dataDefine$th <- input$th
      dataDefine$tipeLahan <- input$tipeLahan
      dataDefine$tipeKebun <- readDataTemplate$tipe.kebun
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
        
      }else if (!is.null(dataDefine$capital)){
        capital <- cbind(grup="input",dataDefine$capital)
        
        # menambahkan pada tabel io matrix bernilai 1
        ioKapital <- data.frame(matrix(data=1,nrow = nrow(capital) , ncol = ncol(dataDefine$ioInput)-3))
        colnames(ioKapital)<-paste0(c(rep("Y", yearIO)),1:yearIO)
        ioKapital<-cbind(status="modal kapital" ,capital[c(1:4)],ioKapital)
        ioKapital <- ioKapital %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        
        kapitalPrivat <- filter(capital,komponen == c("modal kapital privat"))
        kapitalPrivat <- cbind(status ="harga.privat",kapitalPrivat )
        kapitalPrivat <- kapitalPrivat %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        kapitalSosial <- filter(capital,komponen == c("modal kapital sosial"))
        kapitalSosial <- cbind(status ="harga.sosial",kapitalSosial )
        kapitalSosial <- kapitalSosial %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        data.gab <- bind_rows(io.all, ioKapital,
                              price.all.year, 
                              kapitalPrivat, kapitalSosial) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
        # hitung npv --------------------------------------------------------------
        dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
        dataCapitalAll <- filter(data.gab,status == c("modal kapital"))
        
        
        dataGeneralPrivat <- filter(dataCapitalAll,komponen == c("modal kapital privat"))
        dataGeneralPrivat <- rbind(dataGeneral,dataGeneralPrivat)
        dataPrivat <- filter(data.gab,status == c("harga.privat"))
        p.budget <- dataGeneralPrivat[-(c(1:5,36))] * dataPrivat[-c(1:5,36)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
        p.budget <- cbind(dataGeneralPrivat[c(1:4)],dataPrivat[36],p.budget) #memunculkan kembali variabel 1 sd 5
        p.budget <- p.budget[-1] #menghilangkan label status yang awal
        p.budget <- cbind(status = "privat budget", p.budget) #merename keseluruhan tabel status
        
      
        
        
        #perkalian antara general dengan Social Price
        dataGeneralSosial <- filter(dataCapitalAll,komponen == c("modal kapital sosial"))
        dataGeneralSosial <- rbind(dataGeneral,dataGeneralSosial)
        dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
        s.budget <- dataGeneralSosial[-c(1:5,36)] * dataSosial[-c(1:5,36)]
        s.budget <- cbind(dataGeneralSosial[c(1:4)],dataSosial[36],s.budget)
        s.budget <- s.budget[-1] #menghilangkan label status yang awal
        s.budget <- cbind(status = "sosial budget", s.budget) #merename keseluruhan tabel status
        
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
        
      }
      
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
      fil.labor <- filter(fil.labor, str_detect(unit, c("hok")))
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
    
    # Section input informasi umum  dan asumsi makcro---------------------------------------------
    observeEvent(c(input$sut,input$kom,input$selected_provinsi,input$th,input$tipeLahan), {
      
      removeUI(selector='#showMakro')
      removeUI(selector='#showTable')
      removeUI(selector='#showButton')
      removeUI(selector='#showResult')
    })
    
    observeEvent(c(input$rate.p,input$rate.s,input$nilai.tukar), {
      removeUI(selector='#showTable')
      removeUI(selector='#showButton')
      removeUI(selector='#showResult')
    })
    
    
    # End - Section input informasi umum  dan asumsi makcro---------------------------------------------
    
    # Section asumsi makro ---------------------------------------------
    observeEvent(input$asumsiMakro_button, {
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
      dataDefine <- readRDS(fileName)
      dataView <- rbind(dataDefine$priceInput, dataDefine$priceOutput)
      dataView[is.na(dataView)] <- 0 #NA replace with zero
      dataView
      
    })
    
    output$showTableKuantitas <- renderDataTable({
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      # print("data terakhir tersimpan di rds")
      dataDefine <- readRDS(fileName)
      dataView <- rbind(dataDefine$ioInput, dataDefine$ioOutput)
      dataView[is.na(dataView)] <- 0 #NA replace with zero
      dataView
      
    })
    
    output$showTableKapital <- renderDataTable({
      # case for modal kapital
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      if (!is.null(dataDefine$capital)){
        dataView <- dataDefine$capital
        dataView[is.na(dataView)] <- 0 #NA replace with zero
        dataView    
      }
      else if (is.null(dataDefine$capital)){
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
    observeEvent(input$buatPAM_button,{
      # browser()
      dataTemplate()
      resultTemplate()
      showModal(dataModalCreatePam())
      
    })
    
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
    
    observeEvent(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital),{
      removeModal()
      insertUI(selector='#uiShowResult',
               where='afterEnd',
               ui= uiOutput('showResult'))
    })
    
    # Start Price Output ------------------------------------------------------
    reactData <- reactiveValues(
      timeInput = NULL,
      tableP1 = NULL, #price input
      tableP2 = NULL, #price output
      tableIO1 = NULL, #io input
      tableIO2 = NULL, #io output
      tableCapP = NULL, #capital privat
      tableCapS = NULL, #capital sosial
      tableAddPupuk = NULL,
      tableAddBibit = NULL,
      tableAddPeralatan = NULL,
      tableAddTK = NULL,
      tableAddUtama =NULL,
      tableAddSampingan = NULL,
      tableAddCapPrivat = NULL,
      tableAddCapSosial = NULL,
      tableCapitalAll = NULL
    )
    
    valP2Template <- eventReactive(input$template_button,{
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      reactData$tableP2 <- dataDefine$priceOutput
      reactData$tableP2
      
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
                 id = 'bau',
                 tags$style('#bau {
                            background-color: #00cca3;
                            }'),
                 # tags$head(tags$style('h3 {color:white;}')),
                 h3("Business As Usual (BAU)", align = "center")
                 
          ),
          column(6,
                 id = 'sim',
                 tags$style('#sim {
                            background-color: #b3b3ff;
                            }'),
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
    data.gab <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital),{
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
      
      if (is.null(dataDefine$capital)){
        # capital = NULL
        data.gab <- bind_rows(io.all,
                              price.all.year) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
        
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
        
      }else if (!is.null(dataDefine$capital)){
        capital <- cbind(grup="input",dataDefine$capital)
        
        # menambahkan pada tabel io matrix bernilai 1
        ioKapital <- data.frame(matrix(data=1,nrow = nrow(capital) , ncol = ncol(dataDefine$ioInput)-3))
        colnames(ioKapital)<-paste0(c(rep("Y", yearIO)),1:yearIO)
        ioKapital<-cbind(status="modal kapital" ,capital[c(1:4)],ioKapital)
        ioKapital <- ioKapital %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        
        kapitalPrivat <- filter(capital,komponen == c("modal kapital privat"))
        kapitalPrivat <- cbind(status ="harga.privat",kapitalPrivat )
        kapitalPrivat <- kapitalPrivat %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        kapitalSosial <- filter(capital,komponen == c("modal kapital sosial"))
        kapitalSosial <- cbind(status ="harga.sosial",kapitalSosial )
        kapitalSosial <- kapitalSosial %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        data.gab <- bind_rows(io.all, ioKapital,
                              price.all.year, 
                              kapitalPrivat, kapitalSosial) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
        # hitung npv --------------------------------------------------------------
        dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
        dataCapitalAll <- filter(data.gab,status == c("modal kapital"))
        
        
        dataGeneralPrivat <- filter(dataCapitalAll,komponen == c("modal kapital privat"))
        dataGeneralPrivat <- rbind(dataGeneral,dataGeneralPrivat)
        dataPrivat <- filter(data.gab,status == c("harga.privat"))
        p.budget <- dataGeneralPrivat[-(c(1:5,36))] * dataPrivat[-c(1:5,36)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
        p.budget <- cbind(dataGeneralPrivat[c(1:4)],dataPrivat[36],p.budget) #memunculkan kembali variabel 1 sd 5
        p.budget <- p.budget[-1] #menghilangkan label status yang awal
        p.budget <- cbind(status = "privat budget", p.budget) #merename keseluruhan tabel status
        
        
        
        
        #perkalian antara general dengan Social Price
        dataGeneralSosial <- filter(dataCapitalAll,komponen == c("modal kapital sosial"))
        dataGeneralSosial <- rbind(dataGeneral,dataGeneralSosial)
        dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
        s.budget <- dataGeneralSosial[-c(1:5,36)] * dataSosial[-c(1:5,36)]
        s.budget <- cbind(dataGeneralSosial[c(1:4)],dataSosial[36],s.budget)
        s.budget <- s.budget[-1] #menghilangkan label status yang awal
        s.budget <- cbind(status = "sosial budget", s.budget) #merename keseluruhan tabel status
        
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
        
      }

      
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
      fil.labor <- filter(fil.labor, str_detect(unit, c("hok")))
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
    
    data.gab.new <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital),{
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
      
      
      if (is.null(dataDefine$capital)){
        # capital = NULL
        data.gab <- bind_rows(io.all,
                              price.all.year) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
        
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
        
      }else if (!is.null(dataDefine$capital)){
        capital <- cbind(grup="input",dataDefine$capital)
        
        # menambahkan pada tabel io matrix bernilai 1
        ioKapital <- data.frame(matrix(data=1,nrow = nrow(capital) , ncol = ncol(dataDefine$ioInput)-3))
        colnames(ioKapital)<-paste0(c(rep("Y", yearIO)),1:yearIO)
        ioKapital<-cbind(status="modal kapital" ,capital[c(1:4)],ioKapital)
        ioKapital <- ioKapital %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        
        kapitalPrivat <- filter(capital,komponen == c("modal kapital privat"))
        kapitalPrivat <- cbind(status ="harga.privat",kapitalPrivat )
        kapitalPrivat <- kapitalPrivat %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        kapitalSosial <- filter(capital,komponen == c("modal kapital sosial"))
        kapitalSosial <- cbind(status ="harga.sosial",kapitalSosial )
        kapitalSosial <- kapitalSosial %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        data.gab <- bind_rows(io.all, ioKapital,
                              price.all.year, 
                              kapitalPrivat, kapitalSosial) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
        # hitung npv --------------------------------------------------------------
        dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
        dataCapitalAll <- filter(data.gab,status == c("modal kapital"))
        
        
        dataGeneralPrivat <- filter(dataCapitalAll,komponen == c("modal kapital privat"))
        dataGeneralPrivat <- rbind(dataGeneral,dataGeneralPrivat)
        dataPrivat <- filter(data.gab,status == c("harga.privat"))
        p.budget <- dataGeneralPrivat[-(c(1:5,ncol(dataGeneralPrivat)))] * dataPrivat[-c(1:5,ncol(dataPrivat))] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
        p.budget <- cbind(dataGeneralPrivat[c(1:4)],dataPrivat[36],p.budget) #memunculkan kembali variabel 1 sd 5
        p.budget <- p.budget[-1] #menghilangkan label status yang awal
        p.budget <- cbind(status = "privat budget", p.budget) #merename keseluruhan tabel status
        
        
        
        
        #perkalian antara general dengan Social Price
        dataGeneralSosial <- filter(dataCapitalAll,komponen == c("modal kapital sosial"))
        dataGeneralSosial <- rbind(dataGeneral,dataGeneralSosial)
        dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
        s.budget <- dataGeneralSosial[-c(1:5,ncol(dataGeneralSosial))] * dataSosial[-c(1:5,ncol(dataSosial))]
        s.budget <- cbind(dataGeneralSosial[c(1:4)],dataSosial[36],s.budget)
        s.budget <- s.budget[-1] #menghilangkan label status yang awal
        s.budget <- cbind(status = "sosial budget", s.budget) #merename keseluruhan tabel status
        
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
        
      }
      
      
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
      fil.labor <- filter(fil.labor, str_detect(unit, c("hok")))
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
      # %>%
      # formatStyle("PRIVATE",
      #   backgroundColor = "blue")
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
    
    
    preparePlot <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital),{
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
    
    observeEvent(input$saveNewPAM, {
      browser()
      datapath <- paste0("shiny/data/", input$sut, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      #replace informasi umum -- untuk lbh yakin yang tersave adalah pilihan terakhir user
      dataDefine$sut <- input$sut
      dataDefine$kom <- input$kom
      dataDefine$provinsi <- input$selected_provinsi
      dataDefine$th <- input$th
      dataDefine$tipeLahan <- input$tipeLahan
      # dataDefine$tipeKebun <- readDataTemplate$tipe.kebun
      # dataDefine$tipeData <- c("BAU")
      
      #replace asumsi macro-- untuk lbh yakin yang tersave adalah pilihan terakhir user
      dataDefine$rate.p <- input$rate.p
      dataDefine$rate.s <- input$rate.s
      dataDefine$nilai.tukar <- input$nilai.tukar
      # dataDefine$tipeData <- c("SIMULASI")
      
      saveRDS(dataDefine,file = fileName)
      
    }) 
    
    # End - Section Popup Modal Dialog ---------------------------------------------
    
    # Start - Section sunting_button---------------------------------------------
    ################################################################################
    #                                                                              #
    #                                 BUTTON IO KUANTITAS                          #
    #                                                                              #
    ################################################################################
    observeEvent(input$sunting_button,{
      showModal(
        modalDialog( 
          footer=tagList(
            actionButton(("sunting_button_1"), "Simpan dan Lanjut",style="color: white;background-color: green;")
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
                          choices = c(30:60),selected = 30, width = "600px"),# pilihannnya masih 30 tahun sesuai default
              tags$div(id = 'uiTahunDaurTanam')
            ))
          ,
          size="l",
          easyClose = FALSE)
      )
    })
    
    observeEvent(input$sunting_button_1,{
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      # replace data price
      dataDefine$ioYear_input <- input$ioYear_input
      saveRDS(dataDefine,file = fileName)
      
      
      showModal(modalPilihBarisOutput())
    })
    
    observeEvent(input$ioYear_input,{
      if (input$ioYear_input > 30){
        insertUI(selector='#uiTahunDaurTanam',
                 where='afterEnd',
                 ui= uiOutput('tahunDaurTanam'))
      } else if(input$ioYear_input == 30){
        removeUI(selector = '#tahunDaurTanam')
      } else if(input$ioYear_input == 30 & input$ioTipeDaurTanam == "tabel berisi data template diambil dari tahun ke-1" ){
        removeUI(selector = '#tahunDaurTanam')
      } else if(input$ioYear_input == 30 & input$ioTipeDaurTanam == "tabel berisi nilai 0" ){
        removeUI(selector = '#tahunDaurTanam')
      }
    })
    
    output$tahunDaurTanam <- renderUI({
      radioButtons("ioTipeDaurTanam",
                   " ",
                   width = "600px",
                   choices = c("tabel berisi data template diambil dari tahun ke-1",
                               "tabel berisi nilai 0"),
                   selected = "tabel berisi data template diambil dari tahun ke-1")
    })
    
    
    modalPilihBarisOutput <- function(failed = FALSE) {
      modalDialog( 
        footer=tagList(
          actionButton(("sunting_button_2_output"), "Lanjut",style="color: white;background-color: green;")
        ),
        argonTabSet(
          id = "tabSunting2",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Menentukan Komponen (Baris) pada Output Tabel Kuantitas",
            active = T,
            h3("Apakah user akan menambahkan komponen (baris) pada bagian Output Tabel Kuantitas?"),
            radioButtons(("ioKomponen_output"),
                         " ",
                         choices = c("Tidak","Ya"),selected = "Tidak"), 
            # tags$div(id='tambahBaris')
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$sunting_button_2_output,{
      if (input$ioKomponen_output == "Ya"){
        showModal(modalTambahBarisOutput()) 
      }else if(input$ioKomponen_output == "Tidak"){
        showModal(suntingTabelKuantitas_output())
      }
    })
    
    modalTambahBarisOutput <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("sunting_button_3_output"),"Lanjut",style="color: white;
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
            tabName = "Sunting Tabel Kuantitas (Output)",
            active = T,
            fluidRow(
              column(9,
                     selectInput('pilihKomponenOutput',"Pilih Komponen", width = "600px", 
                                 choices = c(" ","utama", "sampingan")),
                     #dibuat manipulasi option nilai kosong krn utk mengaktifkan nilai input$pilihKomponenInput saat user kedua kalinya masuk 
                     
              ),
              column(3,
                     br(),
                     actionButton("showTabelJenisOutput","pilih jumlah baris")
              ),
              column(12,
                     tags$div(id = 'uiShowTablePilihJenisOutput')
              )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$showTabelJenisOutput,{
      insertUI(selector='#uiShowTablePilihJenisOutput',
               where='afterEnd',
               ui= uiOutput('showTablePilihJenisOutput'))
    }) 
    
    # Start Modal Pilih Komponen Output ----------------------------------------
    ################################################################################
    #                                                                              #
    #                      MODAL DIALOG PILIH KOMPONEN OUTPUT                       #
    #                                                                              #
    ################################################################################
    
    source("shiny/server/showTablePilihJenisOutput.R", local = TRUE)
    
    observeEvent(input$sunting_button_3_output,{
      showModal(suntingTabelKuantitas_output())
    })
    
    
    suntingTabelKuantitas_output <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("batalSunting_button_kuantitasOutput"), "Batal", style="color: white;background-color: red;"),
          actionButton("backtoPilihBaris_output","Kembali"),
          actionButton(("sunting_button_4_output"),"Simpan dan Lanjut",style="color: white;
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
            tabName = "Langkah 6: Sunting Tabel Kuantitas (Output)",
            active = T,
            fluidRow(
              column(12,
                     h1("Tabel kuantitas (Output)",align = "center"),
                     rHandsontableOutput('suntingKuantitasOutput')
              )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$batalSunting_button_kuantitasOutput,{
      removeModal()
    })

    observeEvent(input$backtoPilihBaris_output,{
      showModal(modalTambahBarisOutput())
    })
    
    output$suntingKuantitasOutput <- renderRHandsontable({
      rhandsontable(valIO2(),
                    rowHeaderWidth = 50,
                    fixedColumnsLeft = 2,
                    height = 300
      )%>%
        hot_col(1, readOnly = TRUE)
    })
    
    valIO2 <- eventReactive(c(input$sunting_button_2_output,input$sunting_button_3_output),{
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      if((is.null(dataDefine$addUtama) | is.null(dataDefine$addSampingan)) & input$ioYear_input == 30 & input$ioKomponen_output == "Tidak"){
        reactData$tableIO2 <- dataDefine$ioOutput
        reactData$tableIO2
      } else if  ((!is.null(dataDefine$addUtama) | !is.null(dataDefine$addSampingan) )  & input$ioYear_input == 30 & input$ioKomponen_output == "Tidak"){
        reactData$tableIO2 <- dataDefine$ioOutput
        reactData$tableIO2
      } else if ((!is.null(dataDefine$addUtama) | !is.null(dataDefine$addSampingan) )  & input$ioYear_input == 30 & input$ioKomponen_output == "Ya"){
        reactData$tableIO2 <- bind_rows(dataDefine$ioOutput,dataDefine$addUtama,dataDefine$addSampingan) 
        reactData$tableIO2
      } else if((is.null(dataDefine$addUtama) | is.null(dataDefine$addSampingan) ) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi nilai 0" ){
        reactData$tableIO2 <- dataDefine$ioOutput
        yearIOadd <- as.numeric(input$ioYear_input)
        addCol <- data.frame(matrix(0, nrow = nrow(dataDefine$ioOutput), ncol = yearIOadd - 30))
        colnames(addCol)<-paste0(c(rep("Y", yearIOadd)),1:yearIOadd)[31:yearIOadd] # start dari thun 31 utk nama kolom baru
        addCol <- cbind(dataDefine$ioOutput,addCol)
        reactData$tableIO2 <- bind_rows(addCol,dataDefine$addUtama,dataDefine$addSampingan) 
        reactData$tableIO2
      } else if((is.null(dataDefine$addUtama) | is.null(dataDefine$addSampingan)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi data template diambil dari tahun ke-1" ){
        reactData$tableIO2 <- dataDefine$ioOutput
        yearIOaddMin30 <- as.numeric(input$ioYear_input) - 30
        filterIO <-  dataDefine$ioOutput[,-(1:3)]
        addCol <- filterIO[,1:yearIOaddMin30] 
        
        yearIOadd <- as.numeric(input$ioYear_input)
        colnames(addCol)<-paste0(c(rep("Y", ncol(addCol))),31:yearIOadd)
        addCol <- cbind(dataDefine$ioOutput,addCol)
        reactData$tableIO2 <- bind_rows(addCol,dataDefine$addUtama,dataDefine$addSampingan) 
        reactData$tableIO2
      }else if((!is.null(dataDefine$addUtama) | !is.null(dataDefine$addSampingan)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi nilai 0" ){
        reactData$tableIO2 <- dataDefine$ioOutput
        yearIOadd <- as.numeric(input$ioYear_input)
        addCol <- data.frame(matrix(0, nrow = nrow(dataDefine$ioOutput), ncol = yearIOadd - 30))
        colnames(addCol)<-paste0(c(rep("Y", yearIOadd)),1:yearIOadd)[31:yearIOadd] # start dari thun 31 utk nama kolom baru
        addCol <- cbind(dataDefine$ioOutput,addCol)
        
        reactData$tableIO2 <- bind_rows(addCol,dataDefine$addUtama,dataDefine$addSampingan) 
        reactData$tableIO2
        
      } else if((!is.null(dataDefine$addUtama) | !is.null(dataDefine$addSampingan)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi data template diambil dari tahun ke-1" ){
        reactData$tableIO2 <- dataDefine$ioOutput
        yearIOaddMin30 <- as.numeric(input$ioYear_input) - 30
        filterIO <-  dataDefine$ioOutput[,-(1:3)]
        addCol <- filterIO[,1:yearIOaddMin30] 
        
        yearIOadd <- as.numeric(input$ioYear_input)
        colnames(addCol)<-paste0(c(rep("Y", ncol(addCol))),31:yearIOadd)
        addCol <- cbind(dataDefine$ioOutput,addCol)
        
        reactData$tableIO2 <- bind_rows(addCol,dataDefine$addUtama,dataDefine$addSampingan) 
        reactData$tableIO2
        
      }
      
    })
    
    
    # End -  Modal Pilih Komponen Output ----------------------------------------
    
    observeEvent(input$sunting_button_4_output,{
      # save data untuk setiap perubahan
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      editNew<-as.data.frame(hot_to_r(input$suntingKuantitasOutput))
      editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
      
      dataDefine$ioOutput <- editNew
      
      saveRDS(dataDefine,file = fileName)

      showModal(modalPilihBarisInput())
    })
    
    modalPilihBarisInput <- function(failed = FALSE) {
      modalDialog( 
        footer=tagList(
          actionButton(("sunting_button_2_input"), "Lanjut",style="color: white;background-color: green;")
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
                         choices = c("Tidak","Ya"),selected = "Tidak"), 
            # tags$div(id='tambahBaris')
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    
    observeEvent(input$sunting_button_2_input,{
      if (input$ioKomponen_input == "Ya"){
        showModal(modalTambahBarisInput()) 
      }else if(input$ioKomponen_input == "Tidak"){
        showModal(suntingTabelKuantitas_input())
      }
    })
    
    
    modalTambahBarisInput <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("sunting_button_3_input"),"Lanjut",style="color: white;
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
              column(9,
                     selectInput('pilihKomponenInput',"Pilih Komponen", width = "600px", 
                                 choices = c(" ","pupuk", "bibit", "peralatan","tenaga kerja")),
                     #dibuat manipulasi option nilai kosong krn utk mengaktifkan nilai input$pilihKomponenInput saat user kedua kalinya masuk 
                     
              ),
              column(3,
                     br(),
                     actionButton("showTabelJenis","pilih jumlah baris")
              ),
              column(12,
                     tags$div(id = 'uiShowTablePilihJenisInput')
              )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$pilihKomponenInput,{
      removeUI(selector='#showTablePilihJenisInput')
    }) 
    
    observeEvent(input$showTabelJenis,{
      insertUI(selector='#uiShowTablePilihJenisInput',
               where='afterEnd',
               ui= uiOutput('showTablePilihJenisInput'))
    }) 
    
    
    # Start Modal Pilih Komponen Input ----------------------------------------
    ################################################################################
    #                                                                              #
    #                      MODAL DIALOG PILIH KOMPONEN INPUT                       #
    #                                                                              #
    ################################################################################
    
    source("shiny/server/showTablePilihJenisInput.R", local = TRUE)
    
    # End -  Modal Pilih Komponen Input ----------------------------------------
    
    observeEvent(input$sunting_button_3_input,{
      showModal(suntingTabelKuantitas_input())
    })
    
    
    suntingTabelKuantitas_input <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("batalSunting_button_kuantitasInput"), "Batal", style="color: white;background-color: red;"),
          actionButton("backtoPilihBaris_input","Kembali"),
          actionButton(("sunting_button_4"),"Simpan dan Lanjut Membangun Tabel Harga",style="color: white;
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
            tabName = "Langkah 7: Sunting Tabel Kuantitas (Input)",
            active = T,
            fluidRow(
              column(12,
                     h1("Tabel kuantitas (Input)",align = "center"),
                     rHandsontableOutput('suntingKuantitasInput')
              )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$batalSunting_button_kuantitasInput,{
      removeModal()
    })
    
    observeEvent(input$backtoPilihBaris_input,{
      showModal(modalPilihBarisInput())
    })
    
    
    
    output$suntingKuantitasInput <- renderRHandsontable({
      rhandsontable(valIO1(),
                    rowHeaderWidth = 50,
                    fixedColumnsLeft = 2,
                    height = 300,
      )%>%
        hot_col(1, readOnly = TRUE)
    })
    
    valIO1 <- eventReactive(c(input$sunting_button_2_input,input$sunting_button_3_input),{
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      if((is.null(dataDefine$addPupuk) | is.null(dataDefine$addBibit) | is.null(dataDefine$addPeralatan) |is.null(dataDefine$addTK)) & input$ioYear_input == 30 & input$ioKomponen_input == "Tidak"){
        reactData$tableIO1 <- dataDefine$ioInput
        reactData$tableIO1
      } else if  ((!is.null(dataDefine$addPupuk) | !is.null(dataDefine$addBibit) | !is.null(dataDefine$addPeralatan) |!is.null(dataDefine$addTK))  & input$ioYear_input == 30 & input$ioKomponen_input == "Tidak"){
        reactData$tableIO1 <- dataDefine$ioInput
        reactData$tableIO1
      } else if ((!is.null(dataDefine$addPupuk) | !is.null(dataDefine$addBibit) | !is.null(dataDefine$addPeralatan) |!is.null(dataDefine$addTK))  & input$ioYear_input == 30 & input$ioKomponen_input == "Ya"){
        reactData$tableIO1 <- bind_rows(dataDefine$ioInput,dataDefine$addPupuk,dataDefine$addBibit,dataDefine$addPeralatan,dataDefine$addTK) 
        reactData$tableIO1
      } else if((is.null(dataDefine$addPupuk) | is.null(dataDefine$addBibit) | is.null(dataDefine$addPeralatan) |is.null(dataDefine$addTK)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi nilai 0" ){
        reactData$tableIO1 <- dataDefine$ioInput
        yearIOadd <- as.numeric(input$ioYear_input)
        addCol <- data.frame(matrix(0, nrow = nrow(dataDefine$ioInput), ncol = yearIOadd - 30))
        colnames(addCol)<-paste0(c(rep("Y", yearIOadd)),1:yearIOadd)[31:yearIOadd] # start dari thun 31 utk nama kolom baru
        addCol <- cbind(dataDefine$ioInput,addCol)
        reactData$tableIO1 <- bind_rows(addCol,dataDefine$addPupuk,dataDefine$addBibit,dataDefine$addPeralatan,dataDefine$addTK) 
        reactData$tableIO1
      } else if((is.null(dataDefine$addPupuk) | is.null(dataDefine$addBibit) | is.null(dataDefine$addPeralatan) |is.null(dataDefine$addTK)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi data template diambil dari tahun ke-1" ){
        reactData$tableIO1 <- dataDefine$ioInput
        yearIOaddMin30 <- as.numeric(input$ioYear_input) - 30
        filterIO <-  dataDefine$ioInput[,-(1:3)]
        addCol <- filterIO[,1:yearIOaddMin30] 
        
        yearIOadd <- as.numeric(input$ioYear_input)
        colnames(addCol)<-paste0(c(rep("Y", ncol(addCol))),31:yearIOadd)
        addCol <- cbind(dataDefine$ioInput,addCol)
        reactData$tableIO1 <- bind_rows(addCol,dataDefine$addPupuk,dataDefine$addBibit,dataDefine$addPeralatan,dataDefine$addTK)
        reactData$tableIO1
      }else if((!is.null(dataDefine$addPupuk) | !is.null(dataDefine$addBibit) | !is.null(dataDefine$addPeralatan) |!is.null(dataDefine$addTK)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi nilai 0" ){
        reactData$tableIO1 <- dataDefine$ioInput
        yearIOadd <- as.numeric(input$ioYear_input)
        addCol <- data.frame(matrix(0, nrow = nrow(dataDefine$ioInput), ncol = yearIOadd - 30))
        colnames(addCol)<-paste0(c(rep("Y", yearIOadd)),1:yearIOadd)[31:yearIOadd] # start dari thun 31 utk nama kolom baru
        addCol <- cbind(dataDefine$ioInput,addCol)
        reactData$tableIO1 <- bind_rows(addCol,dataDefine$addPupuk,dataDefine$addBibit,dataDefine$addPeralatan,dataDefine$addTK) 
        reactData$tableIO1
        
      } else if((!is.null(dataDefine$addPupuk) | !is.null(dataDefine$addBibit) | !is.null(dataDefine$addPeralatan) |!is.null(dataDefine$addTK)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi data template diambil dari tahun ke-1" ){
        reactData$tableIO1 <- dataDefine$ioInput
        yearIOaddMin30 <- as.numeric(input$ioYear_input) - 30
        filterIO <-  dataDefine$ioInput[,-(1:3)]
        addCol <- filterIO[,1:yearIOaddMin30] 
        
        yearIOadd <- as.numeric(input$ioYear_input)
        colnames(addCol)<-paste0(c(rep("Y", ncol(addCol))),31:yearIOadd)
        addCol <- cbind(dataDefine$ioInput,addCol)
        reactData$tableIO1 <- bind_rows(addCol,dataDefine$addPupuk,dataDefine$addBibit,dataDefine$addPeralatan,dataDefine$addTK) 
        reactData$tableIO1
        
      }
      
    })

    
    observeEvent(input$sunting_button_4,{
      # browser()
      # save data untuk setiap perubahan
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      editNew<-as.data.frame(hot_to_r(input$suntingKuantitasInput))
      editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
      
      dataDefine$ioInput <- editNew
      
      saveRDS(dataDefine,file = fileName)
      showModal(modalTabelHarga())
    })
    
    ################################################################################
    #                                                                              #
    #                                 MODAL  HARGA                                 #
    #                                                                              #
    ################################################################################
    modalTabelHarga <- function(failed = FALSE) {
      modalDialog( 
        footer=tagList(
          actionButton(("batalButtonHarga"), "Batal", style="color: white;background-color: red;"),
          actionButton(("capitalButton"), "Simpan Tabel dan Lanjutkan Membangun Tabel Modal Kapital",style="color: white;background-color: green;")
        ),
        argonTabSet(
          id = "tabHarga",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Langkah 8: Menyunting Tabel Harga",
            active = T,
            h3("Tabel Harga Output", align = "center"),
            rHandsontableOutput('hargaOutput'),
            br(),
            h3("Tabel Harga Input", align = "center"),
            rHandsontableOutput('hargaInput')
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$batalButtonHarga,{
      # browser()
      removeModal()
    })
    
    output$hargaOutput <- renderRHandsontable({
      rhandsontable(valP2(),
                    rowHeaderWidth = 50,
                    fixedColumnsLeft = 2,
                    height = 100,
      )
    })
    
    
    valP2 <- eventReactive(input$sunting_button_4,{
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      
      readDataTemplate <- read.table(paste0(datapath,input$sut,"_",input$kom,"_",input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".csv"), header = T, sep = ",")
      readDataTemplate[is.na(readDataTemplate)] <- 0
      readDataTemplate <- lowcase(readDataTemplate, c(7:11))
      
      outputData <- filter(readDataTemplate,faktor == c("output"))
      priceOutput <- outputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
      priceOutput <- (priceOutput[,-1])
      
      
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      reactData$tableP2 <- dataDefine$ioOutput[,1:2]
      no.id <- as.numeric(rownames(reactData$tableP2))
      reactData$tableP2 <- cbind(no.id,reactData$tableP2)
      
      reactData$tableP2 <- merge(reactData$tableP2,unique(priceOutput), by.x = "jenis",by.y = "jenis", all.x = T)
      reactData$tableP2 <- reactData$tableP2[order(reactData$tableP2$no.id),]     #sort by nomor yang disesuaikan pada tabel i-o
      rownames(reactData$tableP2) <- no.id
      reactData$tableP2 <- reactData$tableP2[, c(3, 1, 4, 5,6)]
      reactData$tableP2
      
      indexRow <- as.numeric(nrow(dataDefine$ioOutput))
      reactData$tableP2 <- reactData$tableP2[1:indexRow,]
      reactData$tableP2
    })
    
    output$hargaInput <- renderRHandsontable({
      rhandsontable(valP1(),
                    rowHeaderWidth = 50,
                    fixedColumnsLeft = 2,
                    height = 600,
      )
    })
    
    valP1 <- eventReactive(input$sunting_button_4,{
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      readDataTemplate <- read.table(paste0(datapath,input$sut,"_",input$kom,"_",input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".csv"), header = T, sep = ",")
      readDataTemplate[is.na(readDataTemplate)] <- 0
      readDataTemplate <- lowcase(readDataTemplate, c(7:11))
      
      inputData <- filter(readDataTemplate,faktor == c("input"))
      priceInput <- inputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
      priceInput <- (priceInput[,-1])
      
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      reactData$tableP1 <- dataDefine$ioInput[,1:2]
      no.id <- as.numeric(rownames(reactData$tableP1))
      reactData$tableP1 <- cbind(no.id,reactData$tableP1)
      
      reactData$tableP1 <- merge(reactData$tableP1,unique(priceInput), by.x = "jenis",by.y = "jenis", all.x = T)
      reactData$tableP1 <- reactData$tableP1[order(reactData$tableP1$no.id),]     #sort by nomor yang disesuaikan pada tabel i-o
      rownames(reactData$tableP1) <- no.id
      reactData$tableP1 <- reactData$tableP1[, c(3, 1, 4, 5,6)]
      reactData$tableP1
      
      
      indexRow <- as.numeric(nrow(dataDefine$ioInput))
      reactData$tableP1 <- reactData$tableP1[1:indexRow,]
      reactData$tableP1
    })
    

    observeEvent(input$capitalButton,{
      # save data untuk setiap perubahan
      datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         # input$sut,"_",input$kom,"_",
                         input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      editNewP1<-as.data.frame(hot_to_r(input$hargaInput))
      editNewP1[is.na(editNewP1)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
      
      editNewP2<-as.data.frame(hot_to_r(input$hargaOutput))
      editNewP2[is.na(editNewP2)] <- 0 
      
      dataDefine$priceInput <- editNewP1
      dataDefine$priceOutput <- editNewP2
      
      saveRDS(dataDefine,file = fileName)
      
      # browser()
      
      if(is.null(dataDefine$capital)){
        showModal(modalTanpaCapital())
      }else if(!is.null(dataDefine$capital)){
        showModal(modalTabelCapital())
      }
      
      
    })
    
    modalTanpaCapital <- function(failed = FALSE) {
      modalDialog( 
        footer=tagList(
          actionButton(("running_button_tanpaCapital"), "Jalankan Analisis",style="color: white;background-color: green;")
        ),
        argonTabSet(
          id = "tabNew",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Status Tabel Modal Kapital",
            active = T,
            h3("Komoditas ini Tidak Memiliki Tabel Modal Kapital", align = "center")
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    # observeEvent(input$running_button_tanpaCapital,{
    #  browser()
    # })
    
    
    
    modalTabelCapital <- function(failed = FALSE) {
      modalDialog( 
        footer=tagList(
          # actionButton(("running_button"), "Jalankan Analisis",style="color: white;background-color: green;")
        ),
        argonTabSet(
          id = "tabNew",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Tabel Modal Kapital",
            active = T,
            h3("Apakah user akan menambah komponen untuk tabel modal kapital?"),
            radioButtons("tipeTabelCapital",
                         " ",
                         choices = c("Tidak","Ya"),selected = "Tidak"),
            tags$div(id='uiTipeModalKapital')
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$tipeTabelCapital,{
      if (input$tipeTabelCapital == "Tidak"){
        removeUI(selector = '#tipeModalKapital')
        insertUI(selector='#uiTipeModalKapital',
                 where='afterEnd',
                 ui= uiOutput('tipeModalKapital_No'))
      } else if(input$tipeTabelCapital == "Ya"){
        removeUI(selector = '#tipeModalKapital_No')
        insertUI(selector='#uiTipeModalKapital',
                 where='afterEnd',
                 ui= uiOutput('tipeModalKapital'))
      } 
    })
    
    output$tipeModalKapital_No <- renderUI({
      fluidRow(
        column(9,
               
        ),
        column(3,
               actionButton(("running_button_noEditCapital"), "Jalankan Analisis",style="color: white;background-color: green;")
        )
      )
      
    })
    
    
    
    output$tipeModalKapital <- renderUI({
      fluidRow(
        column(6,
               
        ),
        column(6,
               actionButton(("EksisTabelCapital"), "Lanjut Membangun Tabel Modal Kapital")
        )
      )
      
    })
    
    
    observeEvent(input$EksisTabelCapital,{
      if (input$tipeTabelCapital == "Ya"){
        showModal(modalPilihBarisCapital())
      } 
    })
    
    
    modalPilihBarisCapital <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("suntingModalKapital"),"Lanjut",style="color: white;
                         background-color: green;")
        ),
        argonTabSet(
          id = "tabSunting",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Sunting Tabel Modal Kapital",
            active = T,
            fluidRow(
              column(9,
                     selectInput('pilihKomponenCapital',"Pilih Komponen", width = "600px", 
                                 choices = c(" ","privat", "sosial")),
                     #dibuat manipulasi option nilai kosong krn utk mengaktifkan nilai input$pilihKomponenInput saat user kedua kalinya masuk 
                     
              ),
              column(3,
                     br(),
                     actionButton("showTabelJenisCapital","pilih jumlah baris")
              ),
              column(12,
                     tags$div(id = 'uiShowTablePilihJenisCapital')
              )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$showTabelJenisCapital,{
      insertUI(selector='#uiShowTablePilihJenisCapital',
               where='afterEnd',
               ui= uiOutput('showTablePilihJenisCapital'))
    }) 
    
    # Start Modal Pilih Komponen Modal Kapital ----------------------------------------
    ################################################################################
    #                                                                              #
    #                      MODAL DIALOG PILIH KOMPONEN MODAL KAPITAL               #
    #                                                                              #
    ################################################################################
    
    source("shiny/server/showTablePilihJenisCapital.R", local = TRUE)
    
    
    
    # End -  Modal Pilih Komponen Modal Kapital ----------------------------------------
    
    
    ################################################################################
    #                                                                              #
    #                               PROSES 2 PAM BARU                             #
    #                                                                              #
    ################################################################################
    source("shiny/server/proses_2_PAM_baru.R", local = TRUE)
    
  }
)

runApp(app)