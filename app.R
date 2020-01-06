setwd("C:/dw/ICRAF/profitability")


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

# template
source("sidebar.R")
source("navbar.R")
source("header.R")
source("footer.R")

# elements
source("cards/cards_tab.R")
source("alerts/alerts_tab.R")
source("css_effects/effects_tab.R")


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
        cards_tab,
        alerts_tab,
        effects_tab
      )
    ),
    footer = argonFooter
  ),
  server = function(input, output) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
    
    output$plot <- renderPlot({
      dist <- switch(
        input$dist,
        norm = rnorm,
        unif = runif,
        lnorm = rlnorm,
        exp = rexp,
        rnorm
      )
      
      hist(dist(500))
    })
    
    # argonTable
    output$argonTable <- renderUI({
      
      wrap <- if (input$cardWrap == "Enable") TRUE else FALSE
      
      argonTable(
        cardWrap = wrap,
        headTitles = c(
          "PROJECT",
          "BUDGET",
          "STATUS",
          "USERS",
          "COMPLETION",
          ""
        ),
        argonTableItems(
          argonTableItem("Argon Design System"),
          argonTableItem(dataCell = TRUE, "$2,500 USD"),
          argonTableItem(
            dataCell = TRUE, 
            argonBadge(
              text = "Pending",
              status = "danger"
            )
          ),
          argonTableItem(
            argonAvatar(
              size = "sm",
              src = "https://image.flaticon.com/icons/svg/219/219976.svg"
            )
          ),
          argonTableItem(
            dataCell = TRUE, 
            argonProgress(value = 60, status = "danger")
          ),
          argonTableItem(
            argonButton(
              name = "Click me!",
              status = "warning",
              icon = "atom",
              size = "sm"
            )
          )
        )
      )
    })
    
    
    #A <- read.csv("C:/dw/ICRAF/profitability/data/upload data/tprice_1_trad.csv")
    #B <- read.csv("C:/dw/ICRAF/profitability/data/upload data/tprice_2_nontrad.csv")
    #C <- read.csv("C:/dw/ICRAF/profitability/data/upload data/tprice_3_labor.csv")
    
    data<- reactive({
      get(input$dataset)
    })
    
    ######## TAB SUMMARY METADATA
    #tabel price
    #sdata.1<-read.csv("https://raw.githubusercontent.com/dewikiswani/shiny/master/p-trad%20nontrad.csv", header=T)
    #sdata.2<-read.csv("https://raw.githubusercontent.com/dewikiswani/shiny/master/p-labor.csv", header=T)
    #sdata.3<-read.csv("https://raw.githubusercontent.com/dewikiswani/shiny/master/p-output.csv", header=T)
    
    sdata.1<-eventReactive(input$s.simulate,{
      read.csv("https://raw.githubusercontent.com/dewikiswani/shiny/master/p-trad%20nontrad.csv", header=T)
    })
    
    sdata.2<-eventReactive(input$s.simulate,{
      read.csv("https://raw.githubusercontent.com/dewikiswani/shiny/master/p-labor.csv", header=T)
    })
    
    sdata.3<-eventReactive(input$s.simulate,{
      read.csv("https://raw.githubusercontent.com/dewikiswani/shiny/master/p-output.csv", header=T)
    })
    
    #Tabel I-O
    #sdata.4<-read.csv("https://raw.githubusercontent.com/dewikiswani/shiny/master/io-trad%20nontrad.csv", header=T)
    #sdata.5<-read.csv("https://raw.githubusercontent.com/dewikiswani/shiny/master/io-labor.csv", header=T)
    #sdata.6<-read.csv("https://raw.githubusercontent.com/dewikiswani/shiny/master/io-output.csv", header=T)
    
    sdata.4<-eventReactive(input$s.simulate,{
      read.csv("https://raw.githubusercontent.com/dewikiswani/shiny/master/io-trad%20nontrad.csv", header=T)
    })
    
    sdata.5<-eventReactive(input$s.simulate,{
      read.csv("https://raw.githubusercontent.com/dewikiswani/shiny/master/io-labor.csv", header=T)
    })
    
    sdata.6<-eventReactive(input$s.simulate,{
      read.csv("https://raw.githubusercontent.com/dewikiswani/shiny/master/io-output.csv", header=T)
    })
    
    
    #tabel price summary metadata
    sprice.i<-eventReactive(input$s.simulate,{
      #browser()
      data.1=sdata.1()
      data.2=sdata.2()
      
      #replace value labor dengan nilai pada input slider
      price.labor.p<-replace(x=data.2[1,],values = input$slabor.p)
      price.labor.s<-replace(x=data.2[2,],values = input$slabor.s)
      data.3<-rbind(price.labor.p,price.labor.s)
      
      nama_data=paste0("data.",1:2)
      check_data=sapply(nama_data, function(nama)
        !is.null(get(nama))
      )
      if(!any(check_data)){
        stop("Price-Input data must be uploaded")
      }
      
      dim1=dim(get(nama_data[check_data]))
      
      for(i in seq_along(check_data) ) {
        
        if(!check_data[i]){
          assign(nama_data[i],data.frame(matrix(,nrow =dim1[1] ,ncol=dim1[2])))
        }}
      
      df = cbind(data.1,data.2)
      check_df<-apply(apply(df,2,is.na),2,all)
      df2=df[,!check_df]
      return(df2)
      
    })
    
    sprice.o<-eventReactive(input$s.simulate,{
      df<-as.data.frame(cbind(sdata.3()))
      df
    })
    
    
    output$stabel.1 <- renderDataTable({
      df<-cbind(sprice.i(),sprice.o())
      rownames(df)<-c("Private","Social")
      df<-t(df)
      df
      DT::datatable(df, editable = "column",options = list(pageLength = 50))
    }) 

    #tabel i-o summary metadata
    sio.i<-eventReactive(input$s.simulate,{
      #browser()
      data.1=sdata.4()
      data.2=sdata.5()
      nama_data=paste0("data.",1:2)
      check_data=sapply(nama_data, function(nama)
        !is.null(get(nama))
      )
      if(!any(check_data)){
        stop("IO-Output data must be uploaded")
      }
      
      dim1=dim(get(nama_data[check_data]))
      
      for(i in seq_along(check_data) ) {
        
        if(!check_data[i]){
          assign(nama_data[i],data.frame(matrix(,nrow =dim1[1] ,ncol=dim1[2])))
        }}
      #browser()
      df = cbind(data.1,data.2)
      check_df<-apply(apply(df,2,is.na),2,all)
      df2<-as.data.frame(df[,!check_df])
      return(df2)
      
    })
    
    sio.o<-eventReactive(input$s.simulate,{
      df<-as.data.frame(cbind(sdata.6()))
      #req(df)
      df
    })
    
    output$stabel.2 <- renderDataTable({
      #browser()
      df<-cbind(sio.i(),sio.o())
      rownames(df)<-paste(rep("Year", nrow(df)), seq(1,nrow(df),1), sep = "")
      df<-t(df)
      df
      DT::datatable(df, editable = "row",options = list(pageLength = 50))
    })
    
    
    hitung.snpv<-eventReactive(input$s.simulate,{
      
      #browser()
      price<-cbind(sprice.i(),sprice.o())
      i.o<-cbind(sio.i(),sio.o())
      
      #untuk menghasilkan dataset p.budget dan s.budget
      a<-t(as.matrix(i.o))
      b<-t(as.matrix(price))
      
      #dataset p.budget
      p.budget<-t(a*b[,1])
      
      #menghitung total cost input
      total.cost.p<-rowSums(p.budget[,1:ncol(sprice.i())], na.rm = T)
      
      
      #menghitung profit
      if (ncol(sprice.o())==1){
        profit.p<-p.budget[,ncol(price)-ncol(sprice.o())+1] - total.cost.p
      } else {
        profit.p<-rowSums(p.budget[,ncol(price)-ncol(sprice.o())+1:ncol(sprice.o())], na.rm = T) - total.cost.p
      }
      
      #manipulasi vector profit sehingga ada values ketika time=0
      profit0<-0
      profit.p<-c(profit0,profit.p)
      
      #dataset p.budget
      s.budget<-t(a*b[,2])
      
      #menghitung total cost input
      total.cost.s<-rowSums(s.budget[,1:ncol(sprice.i())], na.rm = T)
      
      #menghitung profit
      if (ncol(sprice.o())==1){
        profit.s<-s.budget[,ncol(price)-ncol(sprice.o())+1] - total.cost.s
      } else {
        profit.s<-rowSums(s.budget[,ncol(price)-ncol(sprice.o())+1:ncol(sprice.o())],na.rm = T) - total.cost.s
      }
      
      profit.s<-c(profit0,profit.s)
      npv.pri<-npv(input$srate.p/100,profit.p)
      npv.so<-npv(input$srate.s/100,profit.s)
      hasil<-data.frame(PRIVATE=npv.pri,SOCIAL=npv.so)
      npv.pri.us<-npv.pri/input$snilai.tukar
      npv.so.us<-npv.so/input$snilai.tukar
      hasil.us<-data.frame(PRIVATE=npv.pri.us,SOCIAL=npv.so.us)
      hasil<-rbind(hasil,hasil.us)
      rownames(hasil)<-c("NPV (IDR/Ha)", "NPV (US/Ha)")
      hasil
      
    })
    
    ##npv 
    output$snpv<- renderPrint({
      hasil<-t(hitung.snpv())
      hasil
    })
    
    
    hitung.snlc<-eventReactive(input$s.simulate,{
      #browser()
      price<-sprice.i()
      i.o<-sio.i()
      price.labor<-sdata.2()
      io.labor<-sdata.5()
      
      #replace value labor dengan nilai pada input slider
      price.labor.p<-replace(x=price.labor[1,],values = input$slabor.p)
      price.labor.s<-replace(x=price.labor[2,],values = input$slabor.s)
      price.labor<-rbind(price.labor.p, price.labor.s)
      
      
      #untuk menghasilkan dataset p.budget dan s.budget
      a<-t(as.matrix(i.o))
      b<-t(as.matrix(price))
      c<-t(as.matrix(io.labor))
      d<-t(as.matrix(price.labor))
      
      
      #dataset p.input 
      p.input<-t(a*b[,1])
      p.labor<-t(c*d[,1])
      
      
      #menghitung total cost input dan labor cost private
      total.cost.p<-sum(rowSums(p.input[,1:ncol(price)], na.rm = T))
      
      
      yearly.lc.p<-rowSums(p.labor, na.rm = T)
      labor.cost.p<-sum(yearly.lc.p)
      nlc.p<-(total.cost.p-labor.cost.p)/1000000
      
      #dataset p.budget
      s.input<-t(a*b[,2])
      s.labor<-t(c*d[,2])
      
      
      #menghitung total cost input dan labor cost social
      total.cost.s<-sum(rowSums(s.input[,1:ncol(price)], na.rm = T))
      
      yearly.lc.s<-rowSums(s.labor, na.rm = T)
      labor.cost.s<-sum(yearly.lc.s)
      nlc.s<-(total.cost.s-labor.cost.s)/1000000
      
      hasil<-data.frame(PRIVATE=nlc.p,SOCIAL=nlc.s)
      rownames(hasil)<-c("Non Labor Cost (MRp/Ha)")
      hasil
    })
    
    
    
    output$snonlabor.cost<- renderPrint({
      hasil<-t(hitung.snlc())
      hasil
    })
    
    hitung.sestcost<-eventReactive(input$s.simulate,{
      #browser()
      price<-sprice.i()
      i.o<-sio.i()
      
      #untuk menghasilkan dataset p.budget dan s.budget
      a<-t(as.matrix(i.o))
      b<-t(as.matrix(price))
      
      #dataset p.budget
      p.budget<-t(a*b[,1])
      
      #menghitung total cost input
      total.cost.p<-rowSums(p.budget[,1:ncol(sprice.i())], na.rm = T)
      total.cost.p<-total.cost.p[1]/1000000
      
      #dataset s.budget
      s.budget<-t(a*b[,2])
      
      #menghitung total cost input
      total.cost.s<-rowSums(s.budget[,1:ncol(sprice.i())], na.rm = T)
      total.cost.s<-total.cost.s[1]/1000000
      
      hasil<-data.frame(PRIVATE=total.cost.p,SOCIAL=total.cost.s)
      rownames(hasil)<-c("Establishment Cost (1st year only, MRp/Ha)")
      hasil
    })
    
    
    output$sestcost.print<- renderPrint({
      hasil<-t(hitung.sestcost())
      hasil
    })
    
    hitung.ssummary<-eventReactive(input$s.simulate,{
      #hitung labor req for est
      i.o<-sdata.5()
      i.o<-rowSums(i.o)
      i.o<-i.o[1]
      
      hasil<-data.frame(i.o)
      rownames(hasil)<-c(" ")
      colnames(hasil)<-c("Labor Req for Est (1st year only) = ")
      hasil<-t(hasil)
      hasil
    })
    
    output$ssummary<- renderPrint({
      signif(hitung.ssummary(),digits = 2)
    })
    
    hitung.ssummary.2<-eventReactive(input$s.simulate,{
      
      #browser()
      #hitung  harvesting product
      total.labor<-sdata.5()
      total.labor<-sum(rowSums(total.labor),na.rm = T)
      total.prod<-sdata.6()
      total.prod<-sum(rowSums(total.prod),na.rm = T)/1000
      harvest.prod<-total.prod/total.labor
      format(harvest.prod,digits = 2, nsmall = 2)
      
      hasil<-data.frame(harvest.prod)
      rownames(hasil)<-c(" ")
      colnames(hasil)<-c("Harvesting Product (ton/HOK) = ")
      hasil<-t(hasil)
      hasil
    })
    
    output$ssummary.2<- renderPrint({
      signif(hitung.ssummary.2(),digits = 2)
    })
    
    sasumsi.1<-eventReactive(input$s.simulate,{
      df<-data.frame(input$srate.p,input$srate.s)
      
    })
    
    sasumsi.2<-eventReactive(input$s.simulate,{
      df<-data.frame(input$slabor.p,input$slabor.s)
      
    })
    
    
    output$svalue <- renderPrint({ 
      hasil<-sasumsi.1()
      colnames(hasil)<-c("PRIVATE","SOCIAL")
      rownames(hasil)<-c("Discount Rate")
      
      hasil.2<-sasumsi.2()
      colnames(hasil.2)<-c("PRIVATE","SOCIAL")
      rownames(hasil.2)<-c("Cost of Labor")
      hasil<-rbind(hasil,hasil.2)
      hasil<-t(hasil)
      hasil
    })
    
    sasumsi.3<-eventReactive(input$s.simulate,{
      df<-data.frame(input$snilai.tukar)
      
    })
    
    output$svalue.2 <- renderPrint({ 
      hasil<-sasumsi.3()
      colnames(hasil)<-c("Rupiah Exchange Rate = ")
      rownames(hasil)<-c(" ")
      hasil<-t(hasil)
      hasil
    })
    
    sasumsi<-eventReactive(input$s.simulate,{
      read.csv("C:/dw/ICRAF/profitability/data/PAM/asumsi_rubber.csv", header=T)
    })
      
      
    output$svalue.3 <- renderPrint({ 
      hasil<-sasumsi()
      hasil
    })
    
    
    #### tabel 1
    data.1<-eventReactive(input$simulate,{
      inFile <- input$file.1
      if (is.null(inFile)) return(NULL)
      #if (is.null(inFile)){
      # stop("Cek") 
      #}else{
      read.csv(inFile$datapath)#ganti read.csv jika delimiter excelnya ,
      #}
    })
    
    data.2 <- eventReactive(input$simulate,{
      inFile <- input$file.2
      if (is.null(inFile)) return(NULL)
      read.csv(inFile$datapath)
    })
    
    data.3 <- eventReactive(input$simulate,{
      inFile <- input$file.3
      if (is.null(inFile)) return(NULL)
      read.csv(inFile$datapath)
    })
    
    data.4 <- eventReactive(input$simulate,{
      inFile <- input$file.4
      if (is.null(inFile)) return(NULL)
      read.csv(inFile$datapath)
    })
    
    data.5 <- eventReactive(input$simulate,{
      #browser()
      inFile <- input$file.5
      if (is.null(inFile)){
        stop("Price-Output data must be uploaded") 
      }else{
        read.csv(inFile$datapath)#ganti read.csv jika delimiter excelnya ,
      }
    })
    
    
    price.i<-eventReactive(input$simulate,{
      #browser()
      data.1=data.1()
      data.2=data.2()
      data.3=data.3()
      data.4=data.4()
      
      #replace value labor dengan nilai pada input slider
      price.labor.p<-replace(x=data.3[1,],values = input$labor.p)
      price.labor.s<-replace(x=data.3[2,],values = input$labor.s)
      data.3<-rbind(price.labor.p,price.labor.s)
      
      nama_data=paste0("data.",1:4)
      check_data=sapply(nama_data, function(nama)
        !is.null(get(nama))
      )
      if(!any(check_data)){
        stop("Price-Input data must be uploaded")
      }
      
      dim1=dim(get(nama_data[check_data]))
      
      for(i in seq_along(check_data) ) {
        
        if(!check_data[i]){
          assign(nama_data[i],data.frame(matrix(,nrow =dim1[1] ,ncol=dim1[2])))
        }}
      
      df = cbind(data.1,data.2,data.3,data.4)
      check_df<-apply(apply(df,2,is.na),2,all)
      df2=df[,!check_df]
      return(df2)
      
    })
    
    price.o<-eventReactive(input$simulate,{
      df<-as.data.frame(cbind(data.5()))
      df
    })
    
    
    output$tabel.1 <- renderDataTable({
      df<-cbind(price.i(),price.o())
      rownames(df)<-c("Private","Social")
      df<-t(df)
      df
      DT::datatable(df, editable = "column",options = list(pageLength = 20))
    }) 
    
    
    #### tabel 2
    data.6<-eventReactive(input$simulate,{
      inFile <- input$file.6
      if (is.null(inFile)) return(NULL)
      read.csv(inFile$datapath)#ganti read.csv jika delimiter excelnya ,
      #}
    })
    
    data.7 <- eventReactive(input$simulate,{
      inFile <- input$file.7
      if (is.null(inFile)) return(NULL)
      read.csv(inFile$datapath)
    })
    
    data.8 <- eventReactive(input$simulate,{
      inFile <- input$file.8
      if (is.null(inFile)) return(NULL)
      read.csv(inFile$datapath)
    })
    
    data.9 <- eventReactive(input$simulate,{
      inFile <- input$file.9
      if (is.null(inFile)) return(NULL)
      read.csv(inFile$datapath)
    })
    
    data.10 <- eventReactive(input$simulate,{
      #browser()
      inFile <- input$file.10
      if (is.null(inFile)){
        stop("IO-Input data must be uploaded") 
      }else{
        read.csv(inFile$datapath)#ganti read.csv jika delimiter excelnya ,
      }
      
    })
    
    io.i<-eventReactive(input$simulate,{
      data.1=data.6()
      data.2=data.7()
      data.3=data.8()
      data.4=data.9()
      nama_data=paste0("data.",1:4)
      check_data=sapply(nama_data, function(nama)
        !is.null(get(nama))
      )
      if(!any(check_data)){
        stop("IO-Output data must be uploaded")
      }
      
      dim1=dim(get(nama_data[check_data]))
      
      for(i in seq_along(check_data) ) {
        
        if(!check_data[i]){
          assign(nama_data[i],data.frame(matrix(,nrow =dim1[1] ,ncol=dim1[2])))
        }}
      #browser()
      df = cbind(data.1,data.2,data.3,data.4)
      check_df<-apply(apply(df,2,is.na),2,all)
      df2<-as.data.frame(df[,!check_df])
      return(df2)
      
    })
    
    io.o<-eventReactive(input$simulate,{
      df<-as.data.frame(cbind(data.10()))
      #req(df)
      df
    })
    
    output$tabel.2 <- renderDataTable({
      #browser()
      df<-cbind(io.i(),io.o())
      rownames(df)<-paste(rep("Year", nrow(df)), seq(1,nrow(df),1), sep = "")
      df<-t(df)
      df
      DT::datatable(df, editable = "row",options = list(pageLength = 20))
    })
    
    
    hitung.npv<-eventReactive(input$simulate,{
      #browser()
      price<-cbind(price.i(),price.o())
      i.o<-cbind(io.i(),io.o())
      
      #untuk menghasilkan dataset p.budget dan s.budget
      a<-t(as.matrix(i.o))
      b<-t(as.matrix(price))
      
      #dataset p.budget
      p.budget<-t(a*b[,1])
      
      #menghitung total cost input
      total.cost.p<-rowSums(p.budget[,1:ncol(price.i())], na.rm = T)
      
      
      #menghitung profit
      if (ncol(price.o())==1){
        profit.p<-p.budget[,ncol(price)-ncol(price.o())+1] - total.cost.p
      } else {
        profit.p<-rowSums(p.budget[,ncol(price)-ncol(price.o())+1:ncol(price.o())], na.rm = T) - total.cost.p
      }
      
      #manipulasi vector profit sehingga ada values ketika time=0
      profit0<-0
      profit.p<-c(profit0,profit.p)
      
      #dataset p.budget
      s.budget<-t(a*b[,2])
      
      #menghitung total cost input
      total.cost.s<-rowSums(s.budget[,1:ncol(price.i())], na.rm = T)
      
      #menghitung profit
      if (ncol(price.o())==1){
        profit.s<-s.budget[,ncol(price)-ncol(price.o())+1] - total.cost.s
      } else {
        profit.s<-rowSums(s.budget[,ncol(price)-ncol(price.o())+1:ncol(price.o())],na.rm = T) - total.cost.s
      }
      
      profit.s<-c(profit0,profit.s)
      npv.pri<-npv(input$rate.p/100,profit.p)
      npv.so<-npv(input$rate.s/100,profit.s)
      hasil<-data.frame(PRIVATE=npv.pri,SOCIAL=npv.so)
      npv.pri.us<-npv.pri/input$nilai.tukar
      npv.so.us<-npv.so/input$nilai.tukar
      hasil.us<-data.frame(PRIVATE=npv.pri.us,SOCIAL=npv.so.us)
      hasil<-rbind(hasil,hasil.us)
      rownames(hasil)<-c("NPV (IDR/Ha)", "NPV (US/Ha)")
      hasil
      
    })
    
    ##npv 
    output$npv<- renderPrint({
      hasil<-t(hitung.npv())
      hasil
    })
    
    
    
    ####diubah seberapa pun hasilnya tetap 69.1 untuk kasus jagung.. bgtupun pada kasus yang lainnya tdk akan berubah
    #### nilai labor.p dan labor.s berubah akan mempengaruhi nilai npv dan establishment cost
    ####diubah seberapa pun hasilnya tetap 69.1 untuk kasus jagung.. bgtupun pada kasus yang lainnya tdk akan berubah
    #### nilai labor.p dan labor.s berubah akan mempengaruhi nilai npv dan establishment cost
    hitung.nlc<-eventReactive(input$simulate,{
      #browser()
      price<-price.i()
      i.o<-io.i()
      price.labor<-data.3()
      io.labor<-data.8()
      
      #replace value labor dengan nilai pada input slider
      price.labor.p<-replace(x=price.labor[1,],values = input$labor.p)
      price.labor.s<-replace(x=price.labor[2,],values = input$labor.s)
      price.labor<-rbind(price.labor.p, price.labor.s)
      
      
      #untuk menghasilkan dataset p.budget dan s.budget
      a<-t(as.matrix(i.o))
      b<-t(as.matrix(price))
      c<-t(as.matrix(io.labor))
      d<-t(as.matrix(price.labor))
      
      
      #dataset p.input 
      p.input<-t(a*b[,1])
      p.labor<-t(c*d[,1])
      
      
      #menghitung total cost input dan labor cost private
      total.cost.p<-sum(rowSums(p.input[,1:ncol(price)], na.rm = T))
      
      
      yearly.lc.p<-rowSums(p.labor, na.rm = T)
      labor.cost.p<-sum(yearly.lc.p)
      nlc.p<-(total.cost.p-labor.cost.p)/1000000
      
      #dataset p.budget
      s.input<-t(a*b[,2])
      s.labor<-t(c*d[,2])
      
      
      #menghitung total cost input dan labor cost social
      total.cost.s<-sum(rowSums(s.input[,1:ncol(price)], na.rm = T))
      
      yearly.lc.s<-rowSums(s.labor, na.rm = T)
      labor.cost.s<-sum(yearly.lc.s)
      nlc.s<-(total.cost.s-labor.cost.s)/1000000
      
      hasil<-data.frame(PRIVATE=nlc.p,SOCIAL=nlc.s)
      rownames(hasil)<-c("Non Labor Cost (MRp/Ha)")
      hasil
    })
    
  
    
    output$nonlabor.cost<- renderPrint({
      hasil<-t(hitung.nlc())
      hasil
    })
    
    hitung.estcost<-eventReactive(input$simulate,{
      #browser()
      price<-price.i()
      i.o<-io.i()
      
      #untuk menghasilkan dataset p.budget dan s.budget
      a<-t(as.matrix(i.o))
      b<-t(as.matrix(price))
      
      #dataset p.budget
      p.budget<-t(a*b[,1])
      
      #menghitung total cost input
      total.cost.p<-rowSums(p.budget[,1:ncol(price.i())], na.rm = T)
      total.cost.p<-total.cost.p[1]/1000000
      
      #dataset s.budget
      s.budget<-t(a*b[,2])
      
      #menghitung total cost input
      total.cost.s<-rowSums(s.budget[,1:ncol(price.i())], na.rm = T)
      total.cost.s<-total.cost.s[1]/1000000
      
      hasil<-data.frame(PRIVATE=total.cost.p,SOCIAL=total.cost.s)
      rownames(hasil)<-c("Establishment Cost (1st year only, MRp/Ha)")
      hasil
    })
    
    
    output$estcost.print<- renderPrint({
      hasil<-t(hitung.estcost())
      hasil
    })
    
    hitung.summary<-eventReactive(input$simulate,{
      #hitung labor req for est
      i.o<-data.8()
      i.o<-rowSums(i.o)
      i.o<-i.o[1]
      
      hasil<-data.frame(i.o)
      rownames(hasil)<-c(" ")
      colnames(hasil)<-c("Labor Req for Est (1st year only) = ")
      hasil<-t(hasil)
      hasil
    })
    
    output$summary<- renderPrint({
      signif(hitung.summary(),digits = 2)
    })
    
    hitung.summary.2<-eventReactive(input$simulate,{
      
      
      #hitung  harvesting product
      total.labor<-data.8()
      total.labor<-sum(rowSums(total.labor))
      total.prod<-data.10()
      total.prod<-sum(rowSums(total.prod))/1000
      harvest.prod<-total.prod/total.labor
      format(harvest.prod,digits = 2, nsmall = 2)
      
      hasil<-data.frame(harvest.prod)
      rownames(hasil)<-c(" ")
      colnames(hasil)<-c("Harvesting Product (ton/HOK) = ")
      hasil<-t(hasil)
      hasil
    })
    
    output$summary.2<- renderPrint({
      signif(hitung.summary.2(),digits = 2)
    })
    
    asumsi.1<-eventReactive(input$simulate,{
        df<-data.frame(input$rate.p,input$rate.s)
        
    })
    
    asumsi.2<-eventReactive(input$simulate,{
      df<-data.frame(input$labor.p,input$labor.s)
      
    })
    
    
    output$value <- renderPrint({ 
      hasil<-asumsi.1()
      colnames(hasil)<-c("PRIVATE","SOCIAL")
      rownames(hasil)<-c("Discount Rate")
      
      hasil.2<-asumsi.2()
      colnames(hasil.2)<-c("PRIVATE","SOCIAL")
      rownames(hasil.2)<-c("Cost of Labor")
      hasil<-rbind(hasil,hasil.2)
      hasil<-t(hasil)
      hasil
    })
    
    asumsi.3<-eventReactive(input$simulate,{
      df<-data.frame(input$nilai.tukar)
      
    })
    
    output$value.2 <- renderPrint({ 
      hasil<-asumsi.3()
      colnames(hasil)<-c("Rupiah Exchange Rate = ")
      rownames(hasil)<-c(" ")
      hasil<-t(hasil)
      hasil
    })
    
    
    
  }
)


