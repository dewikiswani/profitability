output$showTablePilihJenisOutput <- renderUI({
  datapath <- paste0("data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  
  if (input$pilihKomponenOutput == "utama"){
    fluidRow(
      column(9,
             selectInput("tambahBarisUtama",
                         "Berapa baris yang akan ditambahkan untuk komponen output utama?",
                         choices = c(1:5),
                         selected = if (is.null(dataDefine$addUtama)){
                           1
                         } else {nrow(dataDefine$addUtama)}
                         ,width = "500px")
      ),
      column(3,
             br(),
             actionButton("showTabelAddUtama","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddUtama')
      )
    )
  } else if (input$pilihKomponenOutput == "sampingan"){
    fluidRow(
      column(9,
             selectInput("tambahBarisSampingan",
                         "Berapa baris yang akan ditambahkan untuk komponen output sampingan?",
                         choices = c(1:5),
                         selected = if (is.null(dataDefine$addSampingan)){
                           1
                         } else {nrow(dataDefine$addSampingan)}
                         ,width = "500px")
      ),
      column(3,
             br(),
             br(),
             actionButton("showTabelAddSampingan","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddSampingan')
      )
    )
  }
})

################################################################################
#                                                                              #
#                                    UTAMA                                     #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddUtama,{
  removeUI(selector='#textTampilSaveTambahUtama')
  insertUI(selector='#rhandsAddUtama',
           where='afterEnd',
           ui= uiOutput('showRhandsAddUtama'))
}) 

output$showRhandsAddUtama <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahUtama'),
                  tags$br(),
                  actionButton(('saveTambahBarisUtama'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahUtama')
  ))
})



output$tabelTambahUtama <- renderRHandsontable({
  rhandsontable(valJenisUtama(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300
  )
})

valJenisUtama<- eventReactive(input$showTabelAddUtama,{
  
  datapath <- paste0("data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addUtama)){
    reactData$tableAddUtama <- as.data.frame(dataDefine$addUtama[,c("jenis","unit")]) #kolom komponen yg sudah di save pd file rds di hide
    reactData$tableAddUtama <- as.data.frame(reactData$tableAddUtama[1:as.numeric(input$tambahBarisUtama),])
    rownames(reactData$tableAddUtama) <- c(1:nrow(reactData$tableAddUtama))
    reactData$tableAddUtama[] <- lapply(reactData$tableAddUtama, as.character) #ubah dr faktor jd char
    reactData$tableAddUtama
  } else if (is.null(dataDefine$addUtama)){
    dataKomponen <- filter(kumpulanDataJenisInputOutput,tipe.kebun == c("UMUM"))
    # dataKomponen <- filter(kumpulanDataJenisInputOutput,komoditas == input$kom)
    dataKomponen <- filter(dataKomponen,komponen == c("utama"))
    dataKomponen <- lowcase(dataKomponen,c(2:ncol(dataKomponen)))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain utama ga masuk level faktor nya
    # dataKomponen[] <- lapply(dataKomponen, as.factor) #ubah char jd faktor, spy bs di drop down yg hanya komponen utama aja
    reactData$tableAddUtama <- as.data.frame(dataKomponen[,c("jenis","unit")])
    reactData$tableAddUtama <- as.data.frame(reactData$tableAddUtama[1:as.numeric(input$tambahBarisUtama),])
    rownames(reactData$tableAddUtama) <- c(1:input$tambahBarisUtama)
    reactData$tableAddUtama
  } 
  
})


observeEvent(input$saveTambahBarisUtama,{
  # browser()
  removeUI(selector='#textTampilSaveTambahUtama')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahUtama))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  
  datapath <- paste0("data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data 
  if(dataDefine$tipeKebun == "LARGE SCALE"){
    editNew <- cbind(bagian = "output",komponen = "utama", editNew)
  }else{
    editNew <- cbind(komponen = "utama", editNew)
  }
  
  dataDefine$addUtama <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahUtama',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahUtama","tabel di atas sudah tersimpan"))
})



################################################################################
#                                                                              #
#                                SAMPINGAN                                     #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddSampingan,{
  removeUI(selector='#textTampilSaveTambahSampingan')
  insertUI(selector='#rhandsAddSampingan',
           where='afterEnd',
           ui= uiOutput('showRhandsAddSampingan'))
}) 

output$showRhandsAddSampingan <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahSampingan'),
                  tags$br(),
                  actionButton(('saveTambahBarisSampingan'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahSampingan')
  ))
  
})

output$tabelTambahSampingan <- renderRHandsontable({
  rhandsontable(valJenisSampingan(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300
  )
})


valJenisSampingan<- eventReactive(input$showTabelAddSampingan,{
  
  datapath <- paste0("data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addSampingan)){
    reactData$tableAddSampingan <- as.data.frame(dataDefine$addSampingan[,c("jenis","unit")]) #kolom komponen yg sudah di save pd file rds di hide
    reactData$tableAddSampingan <- as.data.frame(reactData$tableAddSampingan[1:as.numeric(input$tambahBarisSampingan),])
    rownames(reactData$tableAddSampingan) <- c(1:nrow(reactData$tableAddSampingan))
    reactData$tableAddSampingan[] <- lapply(reactData$tableAddSampingan, as.character) #ubah dr faktor jd char
    reactData$tableAddSampingan
  } else if (is.null(dataDefine$addSampingan)){
    # dataKomponen <- filter(kumpulanDataJenisInputOutput,komoditas == input$kom)
    dataKomponen <- filter(kumpulanDataJenisInputOutput,tipe.kebun == c("UMUM"))
    dataKomponen <- filter(dataKomponen,komponen == c("sampingan"))
    dataKomponen <- lowcase(dataKomponen,c(2:ncol(dataKomponen)))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain Sampingan ga masuk level faktor nya
    # dataKomponen[] <- lapply(dataKomponen, as.factor) #ubah char jd faktor, spy bs di drop down yg hanya komponen Sampingan aja
    reactData$tableAddSampingan <- as.data.frame(dataKomponen[,c("jenis","unit")])
    reactData$tableAddSampingan <- as.data.frame(reactData$tableAddSampingan[1:as.numeric(input$tambahBarisSampingan),])
    rownames(reactData$tableAddSampingan) <- c(1:input$tambahBarisSampingan)
    reactData$tableAddSampingan
  } 
  
})

observeEvent(input$saveTambahBarisSampingan,{
  # browser()
  removeUI(selector='#textTampilSaveTambahSampingan')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahSampingan))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  # editNew <- cbind(komponen = "sampingan", editNew)
  
  datapath <- paste0("data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data 
  if(dataDefine$tipeKebun == "LARGE SCALE"){
    editNew <- cbind(bagian = "output",komponen = "sampingan", editNew)
  }else{
    editNew <- cbind(komponen = "sampingan", editNew)
  }
  
  dataDefine$addSampingan <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahSampingan',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahSampingan","tabel di atas sudah tersimpan"))
})





