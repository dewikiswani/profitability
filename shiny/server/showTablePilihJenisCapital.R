output$showTablePilihJenisCapital <- renderUI({
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  
  if (input$pilihKomponenCapital == "privat"){
    fluidRow(
      column(9,
             selectInput("tambahBarisPrivat",
                         "Berapa baris yang akan ditambahkan untuk komponen modal kapital privat?",
                         choices = c(1:5),
                         selected = if (is.null(dataDefine$addCapPrivat)){
                           1
                         } else {nrow(dataDefine$addCapPrivat)}
                         ,width = "500px")
      ),
      column(3,
             br(),
             actionButton("showTabelAddCapPrivat","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddCapPrivat')
      )
    )
  } else if (input$pilihKomponenCapital == "sosial"){
    fluidRow(
      column(9,
             selectInput("tambahBarisSosial",
                         "Berapa baris yang akan ditambahkan untuk komponen modal kapital sosial?",
                         choices = c(1:5),
                         selected = if (is.null(dataDefine$addCapSosial)){
                           1
                         } else {nrow(dataDefine$addCapSosial)}
                         ,width = "500px")
      ),
      column(3,
             br(),
             br(),
             actionButton("showTabelAddCapSosial","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddCapSosial')
      )
    )
  }
})

################################################################################
#                                                                              #
#                                    PRIVAT                                     #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddCapPrivat,{
  removeUI(selector='#textTampilSaveTambahPrivat')
  insertUI(selector='#rhandsAddCapPrivat',
           where='afterEnd',
           ui= uiOutput('showRhandsAddCapPrivat'))
}) 

output$showRhandsAddCapPrivat <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahPrivat'),
                  tags$br(),
                  actionButton(('saveTambahBarisPrivat'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahPrivat')
  ))
})



output$tabelTambahPrivat <- renderRHandsontable({
  rhandsontable(valJenisPrivat(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300
  )%>%
    hot_col(2, readOnly = TRUE)
})

valJenisPrivat<- eventReactive(input$showTabelAddCapPrivat,{
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addCapPrivat)){
    reactData$tableAddCapPrivat <- as.data.frame(dataDefine$addCapPrivat[,-1]) #kolom komponen yg sudah di save pd file rds di hide
    reactData$tableAddCapPrivat <- as.data.frame(reactData$tableAddCapPrivat[1:as.numeric(input$tambahBarisPrivat),])
    rownames(reactData$tableAddCapPrivat) <- c(1:nrow(reactData$tableAddCapPrivat))
    reactData$tableAddCapPrivat[] <- lapply(reactData$tableAddCapPrivat, as.character) #ubah dr faktor jd char
    reactData$tableAddCapPrivat
  } else if (is.null(dataDefine$addCapPrivat)){
    dataKomponen <- filter(kumpulanDataJenisInputOutput,komponen == c("modal kapital privat"))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain privat ga masuk level faktor nya
    reactData$tableAddCapPrivat <- as.data.frame(dataKomponen[,c(3)])
    reactData$tableAddCapPrivat <- as.data.frame(reactData$tableAddCapPrivat[1:as.numeric(input$tambahBarisPrivat),])
    colnames(reactData$tableAddCapPrivat) <- c("jenis")
    rownames(reactData$tableAddCapPrivat) <- c(1:input$tambahBarisPrivat)
    reactData$tableAddCapPrivat <- cbind(reactData$tableAddCapPrivat, unit.harga="rp")
    reactData$tableAddCapPrivat
  } 
  
})


observeEvent(input$saveTambahBarisPrivat,{
  # browser()
  removeUI(selector='#textTampilSaveTambahPrivat')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahPrivat))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  editNew <- cbind(komponen = "modal kapital privat", editNew)
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data price
  dataDefine$addCapPrivat <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahPrivat',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahPrivat","tabel di atas sudah tersimpan"))
})



################################################################################
#                                                                              #
#                                SOSIAL                                        #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddCapSosial,{
  removeUI(selector='#textTampilSaveTambahSosial')
  insertUI(selector='#rhandsAddCapSosial',
           where='afterEnd',
           ui= uiOutput('showRhandsAddCapSosial'))
}) 

output$showRhandsAddCapSosial <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahSosial'),
                  tags$br(),
                  actionButton(('saveTambahBarisSosial'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahSosial')
  ))
  
})

output$tabelTambahSosial <- renderRHandsontable({
  rhandsontable(valJenisSosial(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300
  )%>%
    hot_col(2, readOnly = TRUE)
})


valJenisSosial<- eventReactive(input$showTabelAddCapSosial,{
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addCapSosial)){
    reactData$tableAddCapSosial <- as.data.frame(dataDefine$addCapSosial[,-1]) #kolom komponen yg sudah di save pd file rds di hide
    reactData$tableAddCapSosial <- as.data.frame(reactData$tableAddCapSosial[1:as.numeric(input$tambahBarisSosial),])
    rownames(reactData$tableAddCapSosial) <- c(1:nrow(reactData$tableAddCapSosial))
    reactData$tableAddCapSosial[] <- lapply(reactData$tableAddCapSosial, as.character) #ubah dr faktor jd char
    reactData$tableAddCapSosial
  } else if (is.null(dataDefine$addCapSosial)){
    dataKomponen <- filter(kumpulanDataJenisInputOutput,komponen == c("modal kapital sosial"))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain sosial ga masuk level faktor nya
    reactData$tableAddCapSosial <- as.data.frame(dataKomponen[,c(3)])
    reactData$tableAddCapSosial <- as.data.frame(reactData$tableAddCapSosial[1:as.numeric(input$tambahBarisSosial),])
    colnames(reactData$tableAddCapSosial) <- c("jenis")
    rownames(reactData$tableAddCapSosial) <- c(1:input$tambahBarisSosial)
    reactData$tableAddCapSosial <- cbind(reactData$tableAddCapSosial, unit.harga="rp")
    reactData$tableAddCapSosial
  } 
  
})

observeEvent(input$saveTambahBarisSosial,{
  # browser()
  removeUI(selector='#textTampilSaveTambahSosial')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahSosial))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  editNew <- cbind(komponen = "modal kapital sosial", editNew)
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data price
  dataDefine$addCapSosial <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahSosial',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahSosial","tabel di atas sudah tersimpan"))
})





observeEvent(input$suntingModalKapital,{
  # browser()
  showModal(suntingTabelModalKapital())
})


suntingTabelModalKapital <- function(failed = FALSE) {
  modalDialog(
    footer=tagList(
      actionButton(("batalSunting"), "Batal", style="color: white;background-color: red;"),
      actionButton("backtoPilihBaris_capital","Kembali"),
      actionButton(("runningButton_capital"),"Jalankan Analisis",style="color: white;
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
        tabName = "Langkah 9: Sunting Tabel Modal Kapital",
        active = T,
        fluidRow(
          column(12,
                 h1("Tabel Modal Kapital",align = "center"),
                 rHandsontableOutput('rhandsSuntingModalKapital')
          )
        )
      ))
    ,
    size="l",
    easyClose = FALSE)
}

observeEvent(input$batalSunting,{
  removeModal()
})

observeEvent(input$backtoPilihBaris_capital,{
  showModal(modalPilihBarisCapital())
})

output$rhandsSuntingModalKapital <- renderRHandsontable({
  rhandsontable(valCapital(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300
  )%>%
    hot_col(1, readOnly = TRUE)
})



valCapital <- eventReactive(input$suntingModalKapital,{
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if((is.null(dataDefine$addCapPrivat) | is.null(dataDefine$addCapSosial)) & input$ioYear_input == 30 & input$tipeTabelCapital == "Tidak"){
    reactData$tableCapitalAll <- dataDefine$capital
    reactData$tableCapitalAll
  } else if  ((!is.null(dataDefine$addCapPrivat) | !is.null(dataDefine$addCapSosial) )  & input$ioYear_input == 30 & input$tipeTabelCapital == "Tidak"){
    reactData$tableCapitalAll <- dataDefine$capital
    reactData$tableCapitalAll
  } else if ((!is.null(dataDefine$addCapPrivat) | !is.null(dataDefine$addCapSosial) )  & input$ioYear_input == 30 & input$tipeTabelCapital == "Ya"){
    reactData$tableCapitalAll <- bind_rows(dataDefine$capital,dataDefine$addCapPrivat,dataDefine$addCapSosial) 
    reactData$tableCapitalAll
  } else if((is.null(dataDefine$addCapPrivat) | is.null(dataDefine$addCapSosial) ) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi nilai 0" ){
    reactData$tableCapitalAll <- dataDefine$capital
    yearIOadd <- as.numeric(input$ioYear_input)
    addCol <- data.frame(matrix(0, nrow = nrow(dataDefine$capital), ncol = yearIOadd - 30))
    colnames(addCol)<-paste0(c(rep("Y", yearIOadd)),1:yearIOadd)[31:yearIOadd] # start dari thun 31 utk nama kolom baru
    addCol <- cbind(dataDefine$capital,addCol)
    reactData$tableCapitalAll <- bind_rows(addCol,dataDefine$addCapPrivat,dataDefine$addCapSosial) 
    reactData$tableCapitalAll
  } else if((is.null(dataDefine$addCapPrivat) | is.null(dataDefine$addCapSosial)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi data template diambil dari tahun ke-1" ){
    reactData$tableCapitalAll <- dataDefine$capital
    yearIOaddMin30 <- as.numeric(input$ioYear_input) - 30
    filterIO <-  dataDefine$capital[,-(1:3)]
    addCol <- filterIO[,1:yearIOaddMin30] 
    
    yearIOadd <- as.numeric(input$ioYear_input)
    colnames(addCol)<-paste0(c(rep("Y", ncol(addCol))),31:yearIOadd)
    addCol <- cbind(dataDefine$capital,addCol)
    reactData$tableCapitalAll <- bind_rows(addCol,dataDefine$addCapPrivat,dataDefine$addCapSosial) 
    reactData$tableCapitalAll
  }else if((!is.null(dataDefine$addCapPrivat) | !is.null(dataDefine$addCapSosial)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi nilai 0" ){
    reactData$tableCapitalAll <- dataDefine$capital
    yearIOadd <- as.numeric(input$ioYear_input)
    addCol <- data.frame(matrix(0, nrow = nrow(dataDefine$capital), ncol = yearIOadd - 30))
    colnames(addCol)<-paste0(c(rep("Y", yearIOadd)),1:yearIOadd)[31:yearIOadd] # start dari thun 31 utk nama kolom baru
    addCol <- cbind(dataDefine$capital,addCol)
    
    reactData$tableCapitalAll <- bind_rows(addCol,dataDefine$addCapPrivat,dataDefine$addCapSosial) 
    reactData$tableCapitalAll
    
  } else if((!is.null(dataDefine$addCapPrivat) | !is.null(dataDefine$addCapSosial)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi data template diambil dari tahun ke-1" ){
    reactData$tableCapitalAll <- dataDefine$capital
    yearIOaddMin30 <- as.numeric(input$ioYear_input) - 30
    filterIO <-  dataDefine$capital[,-(1:3)]
    addCol <- filterIO[,1:yearIOaddMin30] 
    
    yearIOadd <- as.numeric(input$ioYear_input)
    colnames(addCol)<-paste0(c(rep("Y", ncol(addCol))),31:yearIOadd)
    addCol <- cbind(dataDefine$capital,addCol)
    
    reactData$tableCapitalAll <- bind_rows(addCol,dataDefine$addCapPrivat,dataDefine$addCapSosial) 
    reactData$tableCapitalAll
    
  }
  
})
