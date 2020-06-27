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


# template
source("sidebar.R")
source("navbar.R")
source("header.R")
source("footer.R")



# elements
source("home.R")
source("module.R")
source("upload modal dialog.R")
source("moduleAnalisis.R")
source("analisis.R")

# App
shiny::shinyApp(
  ui = argonDashPage(
    title = "Argon Dashboard Demo",
    author = "Dewi Kiswani Bodro",
    description = "ICRAF",
    sidebar = argonSidebar,
    navbar = argonNav, 
    header = argonHeader,
    body = argonDashBody(
      argonTabItems(
        home,
        upload
        ,
        analisis
      )
    ),
    footer = argonFooter
  ),
  server = function(input, output,session) {
    callModule(buttonModule,"profit")
    callModule(analisisModule,"lokasi")
    callModule(analisisModule,"komoditas")
    
    ##kumpulan fungsi
    lowcase <- function(data, index.col){
      for(k in index.col){
        data[,k] <- tolower(data[,k])
      }
      return(data) 
    }
    
    n=30
    profit0=0
    
    # # Section 1 -- Upload File ----------------------------------------------
    #browser()
    #data price
    data.1 <- eventReactive(input$simulate,{
      read.csv("data/template/oilpalm_price_in.csv", header = T, sep = ",")
      # inFile <- input$file.1
      # if (is.null(inFile)){
      #   stop("Harga Input harus dimasukkan") 
      # }else{
      #   read.csv(inFile$datapath)#ganti read.csv jika delimiter excelnya ,
      # }
    })
    
    data.2 <- eventReactive(input$simulate,{
      read.csv("data/template/oilpalm_price_out.csv", header = T, sep = ",")
      # inFile <- input$file.2
      # if (is.null(inFile)){
      #   stop("Harga Output harus dimasukkan") 
      # }else{
      #   read.csv(inFile$datapath)#ganti read.csv jika delimiter excelnya ,
      # }
    })
    
    #data io
    data.3 <- eventReactive(input$simulate,{
      read.csv("data/template/oilpalm_io_in.csv", header = T, sep = ",")
      # inFile <- input$file.3
      # if (is.null(inFile)){
      #   stop("I-O Input harus dimasukkan") 
      # }else{
      #   read.csv(inFile$datapath) #ganti read.csv jika delimiter excelnya ,
      # }
    })
    
    data.4 <- eventReactive(input$simulate,{
      read.csv("data/template/oilpalm_io_out.csv", header = T, sep = ",")
      # inFile <- input$file.4
      # if (is.null(inFile)){
      #   stop("IO-Output harus dimasukkan") 
      # }else{
      #   read.csv(inFile$datapath) #ganti read.csv jika delimiter excelnya ,
      # }
    })
    
    #data capital
    data.5 <- eventReactive(input$simulate,{
      read.csv("data/template/oilpalm_capital_p.csv", header = T, sep = ",")
      # inFile <- input$file.5
      # if (is.null(inFile)) return(NULL)
      # read.csv(inFile$datapath)
    })
    
    data.6 <- eventReactive(input$simulate,{
      read.csv("data/template/oilpalm_capital_s.csv", header = T, sep = ",")
      # inFile <- input$file.6
      # if (is.null(inFile)) return(NULL)
      # read.csv(inFile$datapath)
    })
    
    
    # Section 2 -- variabel input ---------------------------------------------
    
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
    
    ### tabel price
    price<-eventReactive(input$simulate,{
      #browser()
      data.1=data.1()
      data.2=data.2()
      
      data.1[is.na(data.1)] <- 0
      data.2[is.na(data.2)] <- 0
      data3 <- rbind(data.1, data.2)
      
      colnames(data3) <- tolower(colnames(data3))
      data3 <- lowcase(data3,c(1:4))
      p.price<-data3[-6]
      colnames(p.price)[5] <- c("private.price")
      p.year<-data.frame(replicate(n,p.price$private.price)) #replicate nilai private price sebanyak n tahun
      colnames(p.year)<-paste0(c(rep("y", n)),1:n)
      p.price<-cbind(status="private price" ,p.price[c(1:4)],p.year)
      p.price <- p.price %>% mutate_if(is.factor,as.character) #change factor var to char var
      
      s.price<-data3[-5]
      colnames(s.price)[5] <- c("social.price")
      s.year<-data.frame(replicate(n,s.price$social.price))
      colnames(s.year)<-paste0(c(rep("y", n)),1:n)
      s.price<-cbind(status="social price",s.price[c(1:4)],s.year)
      s.price <- s.price %>% mutate_if(is.factor,as.character) #change factor var to char 
      #browser()
      data3 <- rbind(p.price, s.price)
      return(data3)
      
    })
    
    
    
    
    #### tabel io
    io<-eventReactive(input$simulate,{
      io.in=data.3()
      io.out=data.4()
      
      colnames(io.in) <- tolower(colnames(io.in))
      colnames(io.out) <- tolower(colnames(io.out))
      
      io.in[is.na(io.in)] <- 0 #NA replace with zero
      io.out[is.na(io.out)] <- 0
      data1 <- rbind(io.in,io.out) #combine all data input-output
      data1 <- cbind(status="general", data1) #add variable status
      data1 <- data1 %>% mutate_if(is.factor,as.character) #change factor var to char var
      data1 <- lowcase(data1,c(1:5))
      
      return(data1)
      
    })
    
    
    ### tabel capital 
    capital<-eventReactive(input$simulate,{
      
      if (is.null(data.5()) & is.null(data.6()) ) {
        data2 <- NULL
        return(data2)
      } else {
        
        pcap=data.5()
        scap=data.6()
        
        
        pcap[is.na(pcap)] <- 0 #NA replace with zero
        pcap <- cbind(status="private budget", pcap)
        pcap <- pcap %>% mutate_if(is.factor,as.character)
        
        scap[is.na(scap)] <- 0 #NA replace with zero
        scap <- cbind(status="social budget", scap)
        scap <- scap %>% mutate_if(is.factor,as.character)
        
        colnames(pcap) <- tolower(colnames(pcap))
        colnames(scap) <- tolower(colnames(scap))
        
        pcap <- lowcase(pcap,c(1:4))
        scap <- lowcase(scap,c(1,2,3,4))
        
        data2 <- rbind(pcap,scap)
        return(data2)
      }
    })
    
    #hitung.all<-eventReactive(
    observeEvent(input$simulate,{
      #browser()
      
      data.gab<-rbind(price(),io(),capital())
      #perkalian antara general dan Private Price
      a <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
      b <- filter(data.gab,status == c("private price")) #filter data private price
      p.budget <- a[-(1:5)] * b[-c(1:5)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5
      p.budget <- cbind(a[1:5],p.budget) #memunculkan kembali variabel 1 sd 5 
      p.budget <- p.budget %>% 
        mutate(status = case_when(status == "general" ~ "private budget")) #mengubah status yg General mjd Private Budget
      
      #perkalian antara general dengan Social Price
      c <- filter(data.gab, status == c("social price")) #filter data social price
      s.budget <- a[-(1:5)] * c[-c(1:5)]
      s.budget <- cbind(a[1:5],s.budget)
      s.budget <- s.budget %>% 
        mutate(status = case_when(status == "general" ~ "social budget"))
      
      #penggabungan dengan data capital
      p.cap <- filter(data.gab, status == c("private budget"))
      p.budget <- rbind(p.budget,p.cap)
      
      s.cap <- filter(data.gab, status == c("social budget"))
      s.budget <- rbind(s.budget,s.cap)
      
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
      #profit0 <- 0
      p.profit<-c(profit0,p.profit)
      s.profit<-c(profit0,s.profit)
      
      npv.p<-npv(input$rate.p/100,p.profit)
      npv.s<-npv(input$rate.s/100,s.profit)
      
      hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
      
      npv.p.us<-npv.p/input$nilai.tukar
      npv.s.us<-npv.s/input$nilai.tukar
      npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
      hsl.npv<-rbind(hsl.npv,npv.us)
      rownames(hsl.npv)<-c("NPV (IDR/Ha)", "NPV (US/Ha)")
      #hsl.npv
      
      p.tot.cost<- sum(p.sum.cost)
      s.tot.cost<- sum(s.sum.cost)
      
      p.budget$var1 <- tolower(p.budget$var1)
      p.labor.input <- p.budget %>% filter(str_detect(var1,c("tenaga","tk","kerja")))
      s.budget$var1 <- tolower(s.budget$var1)
      s.labor.input <- s.budget %>% filter(str_detect(var1,c("tenaga","tk","kerja")))
      
      p.sum.labor <- p.labor.input[,-(1:5)] %>% 
        sum(na.rm = T)
      s.sum.labor <- s.labor.input[,-(1:5)] %>% 
        sum(na.rm = T)
      
      nlc.p <- (p.tot.cost - p.sum.labor)/1000000
      nlc.s <- (s.tot.cost - s.sum.labor)/1000000
      nlc<-data.frame(PRIVATE=nlc.p,SOCIAL=nlc.s)
      rownames(nlc)<-c("Non Labor Cost (MRp/Ha)")
      #nlc
      
      
      ############# PERHITUNGAN ESTABLISHMENT COST
      ec <- p.sum.cost[[1]]
      
      
      ############# PERHITUNGAN HARVESTING PRODUCT
      e <- a
      fil.prod <- e %>%  filter(str_detect(grup,"output")) #filter io untuk grup output (hasil panen)
      sum.prod <- fil.prod[,-(1:5)] %>% 
        colSums(na.rm = T)
      tot.prod <- sum(sum.prod)
      
      fil.labor <- e %>%  filter(str_detect(var1, c("tenaga","tk","kerja"))) 
      sum.labor <- fil.labor[,-(1:5)] %>% 
        colSums(na.rm = T)
      tot.labor <- sum(sum.labor)
      
      hp <- tot.prod/tot.labor
      
      
      ############# PERHITUNGAN LABOR REQ FOR EST
      lr <- sum.labor[[1]] #pekerja pada tahun 1
      
      #### buat dataframe summary
      summ.npv <- data.frame(hsl.npv[1,1],hsl.npv[1,2],hsl.npv[2,1],hsl.npv[2,2])
      summ.nlc <- data.frame(nlc[1,1],nlc[1,2])
      data.summary <- data.frame(summ.npv,
                                 summ.nlc,
                                 ec,hp,lr)
      colnames(data.summary) <- c("NPV (Rp/Ha) Privat","NPV (Rp/Ha) Sosial",
                                  "NPV (US/Ha) Privat","NPV (US/Ha) Sosial",
                                  "Non Labor Cost (Juta Rp/Ha) Privat","Non Labor Cost (Juta Rp/Ha) Sosial",
                                  "Establishment Cost",
                                  "Harvesting Product",
                                  "Labor Req for Est")
      rownames(data.summary) <- "value"
      return(data.summary)
      
    })
    
    
    hitung.npv<-eventReactive(input$simulate,{
      data.gab<-rbind(price(),io(),capital())
      #perkalian antara general dan Private Price
      a <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
      b <- filter(data.gab,status == c("private price")) #filter data private price
      p.budget <- a[-(1:5)] * b[-c(1:5)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5
      p.budget <- cbind(a[1:5],p.budget) #memunculkan kembali variabel 1 sd 5 
      p.budget <- p.budget %>% 
        mutate(status = case_when(status == "general" ~ "private budget")) #mengubah status yg General mjd Private Budget
      
      #perkalian antara general dengan Social Price
      c <- filter(data.gab, status == c("social price")) #filter data social price
      s.budget <- a[-(1:5)] * c[-c(1:5)]
      s.budget <- cbind(a[1:5],s.budget)
      s.budget <- s.budget %>% 
        mutate(status = case_when(status == "general" ~ "social budget"))
      
      #penggabungan dengan data capital
      p.cap <- filter(data.gab, status == c("private budget"))
      p.budget <- rbind(p.budget,p.cap)
      
      s.cap <- filter(data.gab, status == c("social budget"))
      s.budget <- rbind(s.budget,s.cap)
      
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
      #profit0 <- 0
      p.profit<-c(profit0,p.profit)
      s.profit<-c(profit0,s.profit)
      
      npv.p<-npv(input$rate.p/100,p.profit)
      npv.s<-npv(input$rate.s/100,s.profit)
      
      hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
      
      npv.p.us<-npv.p/input$nilai.tukar
      npv.s.us<-npv.s/input$nilai.tukar
      npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
      hsl.npv<-rbind(hsl.npv,npv.us)
      rownames(hsl.npv)<-c("NPV (IDR/Ha)", "NPV (US/Ha)")
      hsl.npv
    })
    
    ##npv 
    output$npv<- renderPrint({
      hasil<-t(hitung.npv())
      hasil
    })
    
    hitung.nlc<-eventReactive(input$simulate,{
      data.gab<-rbind(price(),io(),capital())
      #perkalian antara general dan Private Price
      a <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
      b <- filter(data.gab,status == c("private price")) #filter data private price
      p.budget <- a[-(1:5)] * b[-c(1:5)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5
      p.budget <- cbind(a[1:5],p.budget) #memunculkan kembali variabel 1 sd 5 
      p.budget <- p.budget %>% 
        mutate(status = case_when(status == "general" ~ "private budget")) #mengubah status yg General mjd Private Budget
      
      #perkalian antara general dengan Social Price
      c <- filter(data.gab, status == c("social price")) #filter data social price
      s.budget <- a[-(1:5)] * c[-c(1:5)]
      s.budget <- cbind(a[1:5],s.budget)
      s.budget <- s.budget %>% 
        mutate(status = case_when(status == "general" ~ "social budget"))
      
      #penggabungan dengan data capital
      p.cap <- filter(data.gab, status == c("private budget"))
      p.budget <- rbind(p.budget,p.cap)
      
      s.cap <- filter(data.gab, status == c("social budget"))
      s.budget <- rbind(s.budget,s.cap)
      
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
      
      
      p.tot.cost<- sum(p.sum.cost)
      s.tot.cost<- sum(s.sum.cost)
      
      p.budget$var1 <- tolower(p.budget$var1)
      p.labor.input <- p.budget %>% filter(str_detect(var1,c("tenaga","tk","kerja")))
      s.budget$var1 <- tolower(s.budget$var1)
      s.labor.input <- s.budget %>% filter(str_detect(var1,c("tenaga","tk","kerja")))
      
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
    
    hitung.ec<-eventReactive(input$simulate,{
      data.gab<-rbind(price(),io(),capital())
      #perkalian antara general dan Private Price
      a <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
      b <- filter(data.gab,status == c("private price")) #filter data private price
      p.budget <- a[-(1:5)] * b[-c(1:5)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5
      p.budget <- cbind(a[1:5],p.budget) #memunculkan kembali variabel 1 sd 5 
      p.budget <- p.budget %>% 
        mutate(status = case_when(status == "general" ~ "private budget")) #mengubah status yg General mjd Private Budget
      
      #perkalian antara general dengan Social Price
      c <- filter(data.gab, status == c("social price")) #filter data social price
      s.budget <- a[-(1:5)] * c[-c(1:5)]
      s.budget <- cbind(a[1:5],s.budget)
      s.budget <- s.budget %>% 
        mutate(status = case_when(status == "general" ~ "social budget"))
      
      #penggabungan dengan data capital
      p.cap <- filter(data.gab, status == c("private budget"))
      p.budget <- rbind(p.budget,p.cap)
      
      s.cap <- filter(data.gab, status == c("social budget"))
      s.budget <- rbind(s.budget,s.cap)
      
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
      p.ec <- p.sum.cost[[1]]
      s.ec <- s.sum.cost[[1]]
      ec <- data.frame(p.ec,s.ec)
      ec<-data.frame(PRIVATE=p.ec,SOCIAL=s.ec)
      rownames(ec)<-c("Establishment cost (1st year only, MRp/ha)")
      ec
      
      
    })
    
    output$ec<- renderPrint({
      hasil<-t(hitung.ec())
      hasil
    })
    
    hitung.hp<-eventReactive(input$simulate,{
      data.gab<-rbind(price(),io(),capital())
      #perkalian antara general dan Private Price
      a <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
      
      ############# PERHITUNGAN HARVESTING PRODUCT
      e <- a
      fil.prod <- e %>%  filter(str_detect(grup,"output")) #filter io untuk grup output (hasil panen)
      sum.prod <- fil.prod[,-(1:5)] %>% 
        colSums(na.rm = T)
      tot.prod <- sum(sum.prod)
      
      fil.labor <- e %>%  filter(str_detect(var1, c("tenaga","tk","kerja"))) 
      sum.labor <- fil.labor[,-(1:5)] %>% 
        colSums(na.rm = T)
      tot.labor <- sum(sum.labor)
      
      hp <- data.frame(tot.prod/tot.labor)
      colnames(hp)<-c("Harvesting Product (ton/HOK) Labor Req for Est (1st year only)")
      rownames(hp) <- c("Value")
      hp
      
    })
    
    output$hp<- renderPrint({
      hasil<-hitung.hp()
      hasil
    })
    
    hitung.lr<-eventReactive(input$simulate,{
      data.gab<-rbind(price(),io(),capital())
      #perkalian antara general dan Private Price
      a <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
      
      ############# PERHITUNGAN HARVESTING PRODUCT
      e <- a
      
      fil.labor <- e %>%  filter(str_detect(var1, c("tenaga","tk","kerja"))) 
      sum.labor <- fil.labor[,-(1:5)] %>% 
        colSums(na.rm = T)
      tot.labor <- sum(sum.labor)
      
      ############# PERHITUNGAN LABOR REQ FOR EST
      lr <- data.frame(sum.labor[[1]]) #pekerja pada tahun 1
      colnames(lr)<-c("Labor Req for Est (1st year only)")
      rownames(lr) <- c("Value")
      lr
    })
    
    output$lr<- renderPrint({
      hasil<-hitung.lr()
      hasil
    })
    
    # output$viewPrice <- renderDataTable({
    #   dataView <- data.frame(price()[,2:7])
    #   dataView
    # })
    # 
    # output$viewIO <- renderDataTable({
    #   dataView <- data.frame(io())
    #   dataView
    # })
  }
)


