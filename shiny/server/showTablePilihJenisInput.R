output$showTablePilihJenisInput <- renderUI({
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  
  if (input$pilihKomponenInput == "pupuk"){
    fluidRow(
      column(9,
             selectInput("tambahBarisPupuk",
                         "Berapa baris yang akan ditambahkan untuk komponen pupuk?",
                         choices = c(1:10),
                         selected = if (is.null(dataDefine$addPupuk)){
                           1
                         } else {nrow(dataDefine$addPupuk)}
                         ,width = "500px")
      ),
      column(3,
             br(),
             actionButton("showTabelAddPupuk","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddPupuk')
      )
    )
  } else if (input$pilihKomponenInput == "bibit"){
    fluidRow(
      column(9,
             selectInput("tambahBarisBibit",
                         "Berapa baris yang akan ditambahkan untuk komponen bibit?",
                         choices = c(1:10),
                         selected = if (is.null(dataDefine$addBibit)){
                           1
                         } else {nrow(dataDefine$addBibit)}
                         ,width = "500px")
      ),
      column(3,
             br(),
             actionButton("showTabelAddBibit","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddBibit')
      )
    )
  }else if (input$pilihKomponenInput == "peralatan"){
    fluidRow(
      column(9,
             selectInput("tambahBarisPeralatan",
                         "Berapa baris yang akan ditambahkan untuk komponen peralatan?",
                         choices = c(1:10),
                         selected = if (is.null(dataDefine$addPeralatan)){
                           1
                         } else {nrow(dataDefine$addPeralatan)},width = "500px")
      ),
      column(3,
             br(),
             actionButton("showTabelAddPeralatan","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddPeralatan')
      )
    )
  } else if (input$pilihKomponenInput == "tenaga kerja"){
    fluidRow(
      column(9,
             selectInput("tambahBarisTK",
                         "Berapa baris yang akan ditambahkan untuk komponen tenaga kerja?",
                         choices = c(1:10),
                         selected = if (is.null(dataDefine$addTK)){
                           1
                         } else {nrow(dataDefine$addTK)}
                         ,width = "500px")
      ),
      column(3,
             br(),
             actionButton("showTabelAddTK","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddTK')
      )
    )
  }
})

################################################################################
#                                                                              #
#                                    PUPUK                                     #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddPupuk,{
  insertUI(selector='#rhandsAddPupuk',
           where='afterEnd',
           ui= uiOutput('showRhandsAddPupuk'))
}) 

output$showRhandsAddPupuk <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahPupuk'),
                  tags$br(),
                  actionButton(('saveTambahBarisPupuk'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahPupuk')
  ))
  
})

observeEvent(input$saveTambahBarisPupuk,{
  # browser()
  removeUI(selector='#textTampilSaveTambahPupuk')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahPupuk))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  editNew <- cbind(komponen = "pupuk", editNew)
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data price
  dataDefine$addPupuk <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahPupuk',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahPupuk","tabel di atas sudah tersimpan"))
})


valJenisPupuk <- eventReactive(input$showTabelAddPupuk,{
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addPupuk)){
    reactData$tableAddPupuk <- as.data.frame(dataDefine$addPupuk[,-1]) #kolom komponen yg sudah di save pd file rds di hide
    reactData$tableAddPupuk <- as.data.frame(reactData$tableAddPupuk[1:as.numeric(input$tambahBarisPupuk),])
    reactData$tableAddPupuk
  } else if (is.null(dataDefine$addPupuk)){
    dataKomponen <- filter(kumpulanDataJenisInputOutput,komponen == c("pupuk"))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain pupuk ga masuk level faktor nya
    dataKomponen[] <- lapply(dataKomponen, as.factor) #ubah char jd faktor, spy bs di drop down yg hanya komponen pupuk aja
    reactData$tableAddPupuk <- as.data.frame(dataKomponen[,c(3:4)])
    reactData$tableAddPupuk <- as.data.frame(reactData$tableAddPupuk[1:as.numeric(input$tambahBarisPupuk),])
    rownames(reactData$tableAddPupuk) <- c(1:input$tambahBarisPupuk)
    reactData$tableAddPupuk
  } 

})

output$tabelTambahPupuk <- renderRHandsontable({
  rhandsontable(valJenisPupuk(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300,
  )
})

################################################################################
#                                                                              #
#                                    BIBIT                                     #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddBibit,{
  insertUI(selector='#rhandsAddBibit',
           where='afterEnd',
           ui= uiOutput('showRhandsAddBibit'))
}) 

output$showRhandsAddBibit <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahBibit'),
                  tags$br(),
                  actionButton(('saveTambahBarisBibit'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahBibit')
  ))
  
})

observeEvent(input$saveTambahBarisBibit,{
  # browser()
  removeUI(selector='#textTampilSaveTambahBibit')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahBibit))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  editNew <- cbind(komponen = "bibit", editNew)
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data price
  dataDefine$addBibit <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahBibit',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahBibit","tabel di atas sudah tersimpan"))
})


valJenisBibit <- eventReactive(input$showTabelAddBibit,{
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addBibit)){
    reactData$tableAddBibit <- as.data.frame(dataDefine$addBibit[,-1])
    reactData$tableAddBibit <- as.data.frame(reactData$tableAddBibit[1:as.numeric(input$tambahBarisBibit),])
    reactData$tableAddBibit
  } else if (is.null(dataDefine$addBibit)){
    # dataKomponen <- filter(kumpulanDataJenisInputOutput,komoditas == input$kom)
    dataKomponen <- filter(kumpulanDataJenisInputOutput,komponen == c("bibit"))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain bibit ga masuk level faktor nya
    # dataKomponen[] <- lapply(dataKomponen, as.factor) #ubah char jd faktor, spy bs di drop down yg hanya komponen bibit aja
    reactData$tableAddBibit <- as.data.frame(dataKomponen[,c(3:4)])
    reactData$tableAddBibit <- as.data.frame(reactData$tableAddBibit[1:as.numeric(input$tambahBarisBibit),])
    rownames(reactData$tableAddBibit) <- c(1:input$tambahBarisBibit)
    reactData$tableAddBibit
  } 
  
})

output$tabelTambahBibit <- renderRHandsontable({
  rhandsontable(valJenisBibit(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300,
  )
})

################################################################################
#                                                                              #
#                                  PERALATAN                                   #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddPeralatan,{
  insertUI(selector='#rhandsAddPeralatan',
           where='afterEnd',
           ui= uiOutput('showRhandsAddPeralatan'))
}) 

output$showRhandsAddPeralatan <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahPeralatan'),
                  tags$br(),
                  actionButton(('saveTambahBarisPeralatan'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahPeralatan')
  ))
  
})

observeEvent(input$saveTambahBarisPeralatan,{
  # browser()
  removeUI(selector='#textTampilSaveTambahPeralatan')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahPeralatan))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  editNew <- cbind(komponen = "Peralatan", editNew)
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data price
  dataDefine$addPeralatan <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahPeralatan',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahPeralatan","tabel di atas sudah tersimpan"))
})


valJenisPeralatan <- eventReactive(input$showTabelAddPeralatan,{
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addPeralatan)){
    reactData$tableAddPeralatan <- as.data.frame(dataDefine$addPeralatan[,-1])
    reactData$tableAddPeralatan <- as.data.frame(reactData$tableAddPeralatan[1:as.numeric(input$tambahBarisPeralatan),])
    reactData$tableAddPeralatant
  } else if (is.null(dataDefine$addPeralatan)){
    # dataKomponen <- filter(kumpulanDataJenisInputOutput,komoditas == input$kom)
    dataKomponen <- filter(kumpulanDataJenisInputOutput,komponen == c("peralatan"))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain peralatan ga masuk level faktor nya
    dataKomponen[] <- lapply(dataKomponen, as.factor) #ubah char jd faktor, spy bs di drop down yg hanya komponen peralatan aja
    reactData$tableAddPeralatan <- as.data.frame(dataKomponen[,c(3:4)])
    reactData$tableAddPeralatan <- as.data.frame(reactData$tableAddPeralatan[1:as.numeric(input$tambahBarisPeralatan),])
    rownames(reactData$tableAddPeralatan) <- c(1:input$tambahBarisPeralatan)
    reactData$tableAddPeralatan
  } 
  
})

output$tabelTambahPeralatan <- renderRHandsontable({
  rhandsontable(valJenisPeralatan(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300,
  )
})

################################################################################
#                                                                              #
#                                 TENAGA KERJA                                 #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddTK,{
  insertUI(selector='#rhandsAddTK',
           where='afterEnd',
           ui= uiOutput('showRhandsAddTK'))
}) 

output$showRhandsAddTK <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahTK'),
                  tags$br(),
                  actionButton(('saveTambahBarisTK'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahTK')
  ))
  
})

observeEvent(input$saveTambahBarisTK,{
  # browser()
  removeUI(selector='#textTampilSaveTambahTK')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahTK))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  editNew <- cbind(komponen = "tenaga kerja", editNew)
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data price
  dataDefine$addTK <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahTK',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahTK","tabel di atas sudah tersimpan"))
})


valJenisTK <- eventReactive(input$showTabelAddTK,{
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     # input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addTK)){
    reactData$tableAddTK <- as.data.frame(dataDefine$addTK[,-1])
    reactData$tableAddTK <- as.data.frame(reactData$tableAddTK[1:as.numeric(input$tambahBarisTK),])
    reactData$tableAddTK
  } else if (is.null(dataDefine$addTK)){
    # dataKomponen <- filter(kumpulanDataJenisInputOutput,komoditas == input$kom)
    dataKomponen <- filter(kumpulanDataJenisInputOutput,komponen == c("tenaga kerja"))
    
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain TK ga masuk level faktor nya
    dataKomponen[] <- lapply(dataKomponen, as.factor) #ubah char jd faktor, spy bs di drop down yg hanya komponen TK aja
    reactData$tableAddTK <- as.data.frame(dataKomponen[,c(3:4)])
    reactData$tableAddTK <- as.data.frame(reactData$tableAddTK[1:as.numeric(input$tambahBarisTK),])
    rownames(reactData$tableAddTK) <- c(1:input$tambahBarisTK)
    reactData$tableAddTK
  } 
  
})

output$tabelTambahTK <- renderRHandsontable({
  rhandsontable(valJenisTK(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300,
  )
})



