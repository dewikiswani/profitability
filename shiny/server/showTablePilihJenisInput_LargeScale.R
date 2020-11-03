output$showTablePilihJenisInput_LargeScale <- renderUI({
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  
  if (input$pilihKomponenInput_LargeScale == "pupuk"){
    fluidRow(
      column(9,
             selectInput("tambahBarisPupuk_LS",
                         "Berapa baris yang akan ditambahkan untuk komponen pupuk?",
                         choices = c(1:10),
                         selected = if (is.null(dataDefine$addPupuk)){
                           1
                         } else {nrow(dataDefine$addPupuk)}
                         ,width = "500px")
      ),
      column(3,
             br(),
             actionButton("showTabelAddPupuk_LS","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddPupuk_LS')
      )
    )
  } else if (input$pilihKomponenInput_LargeScale == "bibit"){
    fluidRow(
      column(9,
             selectInput("tambahBarisBibit_LS",
                         "Berapa baris yang akan ditambahkan untuk komponen bibit?",
                         choices = c(1:10),
                         selected = if (is.null(dataDefine$addBibit)){
                           1
                         } else {nrow(dataDefine$addBibit)}
                         ,width = "500px")
      ),
      column(3,
             br(),
             actionButton("showTabelAddBibit_LS","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddBibit_LS')
      )
    )
  }else if (input$pilihKomponenInput_LargeScale == "peralatan"){
    fluidRow(
      column(9,
             selectInput("tambahBarisPeralatan_LS",
                         "Berapa baris yang akan ditambahkan untuk komponen peralatan?",
                         choices = c(1:10),
                         selected = if (is.null(dataDefine$addPeralatan)){
                           1
                         } else {nrow(dataDefine$addPeralatan)},width = "500px")
      ),
      column(3,
             br(),
             actionButton("showTabelAddPeralatan_LS","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddPeralatan_LS')
      )
    )
  } else if (input$pilihKomponenInput_LargeScale == "bahan kimia"){
    fluidRow(
      column(9,
             selectInput("tambahBarisBahanKimia_LS",
                         "Berapa baris yang akan ditambahkan untuk komponen bahan kimia?",
                         choices = c(1:10),
                         selected = if (is.null(dataDefine$addBahanKimia)){
                           1
                         } else {nrow(dataDefine$addBahanKimia)},width = "500px")
      ),
      column(3,
             br(),
             actionButton("showTabelAddBahanKimia_LS","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddBahanKimia_LS')
      )
    )
  } else if (input$pilihKomponenInput_LargeScale == "tradable capital"){
    fluidRow(
      column(9,
             selectInput("tambahBarisTradCapital_LS",
                         "Berapa baris yang akan ditambahkan untuk komponen tradable capital?",
                         choices = c(1:10),
                         selected = if (is.null(dataDefine$addTradCapital)){
                           1
                         } else {nrow(dataDefine$addTradCapital)},width = "500px")
      ),
      column(3,
             br(),
             actionButton("showTabelAddTradCapital_LS","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddTradCapital_LS')
      )
    )
  } else if (input$pilihKomponenInput_LargeScale == "tenaga kerja ahli"){
    fluidRow(
      column(9,
             selectInput("tambahBarisTK_LS",
                         "Berapa baris yang akan ditambahkan untuk komponen tenaga kerja ahli?",
                         choices = c(1:10),
                         selected = if (is.null(dataDefine$addTK)){
                           1
                         } else {nrow(dataDefine$addTK)}
                         ,width = "500px")
      ),
      column(3,
             br(),
             actionButton("showTabelAddTK_LS","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddTK_LS')
      )
    )
  } else if (input$pilihKomponenInput_LargeScale == "tenaga kerja unskilled"){
    fluidRow(
      column(9,
             selectInput("tambahBarisTKUnskilled_LS",
                         "Berapa baris yang akan ditambahkan untuk komponen tenaga kerja unskilled?",
                         choices = c(1:10),
                         selected = if (is.null(dataDefine$addTKUnskilled)){
                           1
                         } else {nrow(dataDefine$addTKUnskilled)}
                         ,width = "500px")
      ),
      column(3,
             br(),
             actionButton("showTabelAddTKUnskilled_LS","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddTKUnskilled_LS')
      )
    )
  }else if (input$pilihKomponenInput_LargeScale == "factor capital"){
    fluidRow(
      column(9,
             selectInput("tambahBarisFactorCapital_LS",
                         "Berapa baris yang akan ditambahkan untuk komponen factor capital?",
                         choices = c(1:10),
                         selected = if (is.null(dataDefine$addFactorCapital)){
                           1
                         } else {nrow(dataDefine$addFactorCapital)}
                         ,width = "500px")
      ),
      column(3,
             br(),
             actionButton("showTabelAddFactorCapital_LS","tampilkan tabel")
      ),
      column(12,
             tags$div(id='rhandsAddFactorCapital_LS')
      )
    )
  }
})

################################################################################
#                                                                              #
#                                    1.PUPUK                                     #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddPupuk_LS,{
  removeUI(selector='#textTampilSaveTambahPupuk_LS')
  insertUI(selector='#rhandsAddPupuk_LS',
           where='afterEnd',
           ui= uiOutput('showRhandsAddPupuk_LS'))
}) 

output$showRhandsAddPupuk_LS <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahPupuk_LS'),
                  tags$br(),
                  actionButton(('saveTambahBarisPupuk_LS'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahPupuk_LS')
  ))
  
})

output$tabelTambahPupuk_LS <- renderRHandsontable({
  rhandsontable(valJenisPupuk_LS(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300
  )
})


valJenisPupuk_LS <- eventReactive(input$showTabelAddPupuk_LS,{
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addPupuk)){
    reactData$tableAddPupuk <- as.data.frame(dataDefine$addPupuk[,-1]) #kolom komponen yg sudah di save pd file rds di hide
    reactData$tableAddPupuk <- as.data.frame(reactData$tableAddPupuk[1:as.numeric(input$tambahBarisPupuk_LS),])
    rownames(reactData$tableAddPupuk) <- c(1:nrow(reactData$tableAddPupuk))
    reactData$tableAddPupuk
  } else if (is.null(dataDefine$addPupuk)){
    dataKomponen <- filter(kumpulanDataJenisInputOutput,tipe.kebun == c("LARGE SCALE"))
    dataKomponen <- filter(dataKomponen,komponen == c("pupuk"))
    dataKomponen <- lowcase(dataKomponen,c(2:ncol(dataKomponen)))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain pupuk ga masuk level faktor nya
    dataKomponen[] <- lapply(dataKomponen, as.factor) #ubah char jd faktor, spy bs di drop down yg hanya komponen pupuk aja
    reactData$tableAddPupuk <- as.data.frame(dataKomponen[,c("jenis","unit")])
    reactData$tableAddPupuk <- as.data.frame(reactData$tableAddPupuk[1:as.numeric(input$tambahBarisPupuk_LS),])
    rownames(reactData$tableAddPupuk) <- c(1:nrow(reactData$tableAddPupuk))
    reactData$tableAddPupuk
  } 
  
})


observeEvent(input$saveTambahBarisPupuk_LS,{
  # browser()
  removeUI(selector='#textTampilSaveTambahPupuk_LS')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahPupuk_LS))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  # editNew <- cbind(komponen = "pupuk", editNew)
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data
  editNew <- cbind(bagian = "tradable input",komponen = "pupuk", editNew)
  dataDefine$addPupuk <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahPupuk_LS',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahPupuk_LS","tabel di atas sudah tersimpan"))
})




################################################################################
#                                                                              #
#                                    2.BIBIT                                     #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddBibit_LS,{
  removeUI(selector='#textTampilSaveTambahBibit_LS')
  insertUI(selector='#rhandsAddBibit_LS',
           where='afterEnd',
           ui= uiOutput('showRhandsAddBibit_LS'))
}) 

output$showRhandsAddBibit_LS <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahBibit_LS'),
                  tags$br(),
                  actionButton(('saveTambahBarisBibit_LS'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahBibit_LS')
  ))
  
})
valJenisBibit_LS <- eventReactive(input$showTabelAddBibit_LS,{
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addBibit)){
    reactData$tableAddBibit <- as.data.frame(dataDefine$addBibit[,-1])
    reactData$tableAddBibit <- as.data.frame(reactData$tableAddBibit[1:as.numeric(input$tambahBarisBibit_LS),])
    rownames(reactData$tableAddBibit) <- c(1:nrow(reactData$tableAddBibit))
    reactData$tableAddBibit[] <- lapply(reactData$tableAddBibit, as.character) #ubah dr faktor jd char
    reactData$tableAddBibit
  } else if (is.null(dataDefine$addBibit)){
    dataKomponen <- filter(kumpulanDataJenisInputOutput,tipe.kebun == c("LARGE SCALE"))
    dataKomponen <- filter(dataKomponen,komponen == c("bibit"))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain bibit ga masuk level faktor nya
    # dataKomponen[] <- lapply(dataKomponen, as.factor) #ubah char jd faktor, spy bs di drop down yg hanya komponen bibit aja
    reactData$tableAddBibit <- as.data.frame(dataKomponen[,c("jenis","unit")])
    reactData$tableAddBibit <- as.data.frame(reactData$tableAddBibit[1:as.numeric(input$tambahBarisBibit_LS),])
    rownames(reactData$tableAddBibit) <- c(1:nrow(reactData$tableAddBibit))
    reactData$tableAddBibit
  } 
  
})

output$tabelTambahBibit_LS <- renderRHandsontable({
  rhandsontable(valJenisBibit_LS(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300
  )
})


observeEvent(input$saveTambahBarisBibit_LS,{
  # browser()
  removeUI(selector='#textTampilSaveTambahBibit_LS')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahBibit_LS))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  # editNew <- cbind(komponen = "bibit", editNew)
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data
  editNew <- cbind(bagian = "tradable input",komponen = "bibit", editNew)
  dataDefine$addBibit <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahBibit_LS',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahBibit_LS","tabel di atas sudah tersimpan"))
})



################################################################################
#                                                                              #
#                                  3.PERALATAN                                   #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddPeralatan_LS,{
  removeUI(selector='#textTampilSaveTambahPeralatan_LS')
  insertUI(selector='#rhandsAddPeralatan_LS',
           where='afterEnd',
           ui= uiOutput('showRhandsAddPeralatan_LS'))
}) 

output$showRhandsAddPeralatan_LS <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahPeralatan_LS'),
                  tags$br(),
                  actionButton(('saveTambahBarisPeralatan_LS'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahPeralatan_LS')
  ))
  
})

valJenisPeralatan_LS <- eventReactive(input$showTabelAddPeralatan_LS,{
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addPeralatan)){
    reactData$tableAddPeralatan <- as.data.frame(dataDefine$addPeralatan[,-1])
    reactData$tableAddPeralatan <- as.data.frame(reactData$tableAddPeralatan[1:as.numeric(input$tambahBarisPeralatan_LS),])
    rownames(reactData$tableAddPeralatan) <- c(1:nrow(reactData$tableAddPeralatan))
    reactData$tableAddPeralatant[] <- lapply(reactData$tableAddPeralatant, as.character) #ubah dr faktor jd char
    reactData$tableAddPeralatant
  } else if (is.null(dataDefine$addPeralatan)){
    dataKomponen <- filter(kumpulanDataJenisInputOutput,tipe.kebun == c("LARGE SCALE"))
    dataKomponen <- filter(dataKomponen,komponen == c("peralatan"))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain peralatan ga masuk level faktor nya
    # dataKomponen[] <- lapply(dataKomponen, as.factor) #ubah char jd faktor, spy bs di drop down yg hanya komponen peralatan aja
    reactData$tableAddPeralatan <- as.data.frame(dataKomponen[,c("jenis","unit")])
    reactData$tableAddPeralatan <- as.data.frame(reactData$tableAddPeralatan[1:as.numeric(input$tambahBarisPeralatan_LS),])
    rownames(reactData$tableAddPeralatan) <- c(1:nrow(reactData$tableAddPeralatan))
    reactData$tableAddPeralatan
  } 
  
})

output$tabelTambahPeralatan_LS <- renderRHandsontable({
  rhandsontable(valJenisPeralatan_LS(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300
  )
})


observeEvent(input$saveTambahBarisPeralatan_LS,{
  # browser()
  removeUI(selector='#textTampilSaveTambahPeralatan_LS')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahPeralatan_LS))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  # editNew <- cbind(komponen = "Peralatan", editNew)
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data 
  editNew <- cbind(bagian = "tradable input",komponen = "peralatan", editNew)
  dataDefine$addPeralatan <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahPeralatan_LS',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahPeralatan_LS","tabel di atas sudah tersimpan"))
})

################################################################################
#                                                                              #
#                                  4.BAHAN KIMIA                                 #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddBahanKimia_LS,{
  removeUI(selector='#textTampilSaveTambahBahanKimia_LS')
  insertUI(selector='#rhandsAddBahanKimia_LS',
           where='afterEnd',
           ui= uiOutput('showRhandsAddBahanKimia_LS'))
}) 

output$showRhandsAddBahanKimia_LS <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahBahanKimia_LS'),
                  tags$br(),
                  actionButton(('saveTambahBarisBahanKimia_LS'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahBahanKimia_LS')
  ))
  
})

valJenisBahanKimia_LS <- eventReactive(input$showTabelAddBahanKimia_LS,{
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$BahanKimia)){
    reactData$tableBahanKimia <- as.data.frame(dataDefine$BahanKimia[,-1])
    reactData$tableBahanKimia <- as.data.frame(reactData$tableBahanKimia[1:as.numeric(input$tambahBarisBahanKimia_LS),])
    rownames(reactData$tableBahanKimia) <- c(1:nrow(reactData$tableBahanKimia))
    reactData$tableBahanKimia[] <- lapply(reactData$tableBahanKimia, as.character) #ubah dr faktor jd char
    reactData$tableBahanKimia
  } else if (is.null(dataDefine$BahanKimia)){
    dataKomponen <- filter(kumpulanDataJenisInputOutput,tipe.kebun == c("LARGE SCALE"))
    dataKomponen <- filter(dataKomponen,komponen == c("bahan kimia"))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain bahan kimia ga masuk level faktor nya
    # dataKomponen[] <- lapply(dataKomponen, as.factor) #ubah char jd faktor, spy bs di drop down yg hanya komponen bahan kimia aja
    reactData$tableBahanKimia <- as.data.frame(dataKomponen[,c("jenis","unit")])
    reactData$tableBahanKimia <- as.data.frame(reactData$tableBahanKimia[1:as.numeric(input$tambahBarisBahanKimia_LS),])
    rownames(reactData$tableBahanKimia) <- c(1:nrow(reactData$tableBahanKimia))
    reactData$tableBahanKimia
  } 
  
})

output$tabelTambahBahanKimia_LS <- renderRHandsontable({
  rhandsontable(valJenisBahanKimia_LS(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300
  )
})


observeEvent(input$saveTambahBarisBahanKimia_LS,{
  # browser()
  removeUI(selector='#textTampilSaveTambahBahanKimia_LS')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahBahanKimia_LS))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  # editNew <- cbind(komponen = "bahan kimia", editNew)
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data 
  editNew <- cbind(bagian = "tradable input",komponen = "bahan kimia", editNew)
  dataDefine$addBahanKimia <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahBahanKimia_LS',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahBahanKimia_LS","tabel di atas sudah tersimpan"))
})


################################################################################
#                                                                              #
#                         5.TRADABLE CAPITAL                                     #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddTradCapital_LS,{
  removeUI(selector='#textTampilSaveTambahTradCapital_LS')
  insertUI(selector='#rhandsAddTradCapital_LS',
           where='afterEnd',
           ui= uiOutput('showRhandsAddTradCapital_LS'))
}) 

output$showRhandsAddTradCapital_LS <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahTradCapital_LS'),
                  tags$br(),
                  actionButton(('saveTambahBarisTradCapital_LS'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahTradCapital_LS')
  ))
  
})

output$tabelTambahTradCapital_LS <- renderRHandsontable({
  rhandsontable(valJenisTradCapital_LS(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300
  )
})


valJenisTradCapital_LS <- eventReactive(input$showTabelAddTradCapital_LS,{
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addTradCapital)){
    reactData$tableAddTradCapital <- as.data.frame(dataDefine$addTradCapital[,-1]) #kolom komponen yg sudah di save pd file rds di hide
    reactData$tableAddTradCapital <- as.data.frame(reactData$tableAddTradCapital[1:as.numeric(input$tambahBarisTradCapital_LS),])
    rownames(reactData$tableAddTradCapital) <- c(1:nrow(reactData$tableAddTradCapital))
    reactData$tableAddTradCapital[] <- lapply(reactData$tableAddTradCapital, as.character) #ubah dr faktor jd char
    reactData$tableAddTradCapital
  } else if (is.null(dataDefine$addTradCapital)){
    dataKomponen <- filter(kumpulanDataJenisInputOutput,tipe.kebun == c("LARGE SCALE"))
    dataKomponen <- filter(dataKomponen,komponen == c("tradable capital"))
    dataKomponen <- lowcase(dataKomponen,c(2:ncol(dataKomponen)))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain TradCapital ga masuk level faktor nya
    # dataKomponen[] <- lapply(dataKomponen, as.factor) #ubah char jd faktor, spy bs di drop down yg hanya komponen TradCapital aja
    reactData$tableAddTradCapital <- as.data.frame(dataKomponen[,c("jenis","unit")])
    reactData$tableAddTradCapital <- as.data.frame(reactData$tableAddTradCapital[1:as.numeric(input$tambahBarisTradCapital_LS),])
    rownames(reactData$tableAddTradCapital) <- c(1:nrow(reactData$tableAddTradCapital))
    reactData$tableAddTradCapital
  } 
  
})


observeEvent(input$saveTambahBarisTradCapital_LS,{
  # browser()
  removeUI(selector='#textTampilSaveTambahTradCapital_LS')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahTradCapital_LS))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  # editNew <- cbind(komponen = "tradable capital", editNew)
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data
  editNew <- cbind(bagian = "tradable capital",komponen = "tradable capital", editNew)
  dataDefine$addTradCapital <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahTradCapital_LS',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahTradCapital_LS","tabel di atas sudah tersimpan"))
})
################################################################################
#                                                                              #
#                                 6.TENAGA KERJA AHLI                            #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddTK_LS,{
  removeUI(selector='#textTampilSaveTambahTK_LS')
  insertUI(selector='#rhandsAddTK_LS',
           where='afterEnd',
           ui= uiOutput('showRhandsAddTK_LS'))
}) 

output$showRhandsAddTK_LS <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahTK_LS'),
                  tags$br(),
                  actionButton(('saveTambahBarisTK_LS'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahTK_LS')
  ))
  
})

valJenisTK_LS <- eventReactive(input$showTabelAddTK_LS,{
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addTK)){
    reactData$tableAddTK <- as.data.frame(dataDefine$addTK[,-1])
    reactData$tableAddTK <- as.data.frame(reactData$tableAddTK[1:as.numeric(input$tambahBarisTK_LS),])
    rownames(reactData$tableAddTK) <- c(1:nrow(reactData$tableAddTK))
    reactData$tableAddTK[] <- lapply(reactData$tableAddTK, as.character) #ubah dr faktor jd char
    reactData$tableAddTK
  } else if (is.null(dataDefine$addTK)){
    # dataKomponen <- filter(kumpulanDataJenisInputOutput,komoditas == input$kom)
    dataKomponen <- filter(kumpulanDataJenisInputOutput,tipe.kebun == c("LARGE SCALE"))
    dataKomponen <- filter(dataKomponen,komponen == c("tenaga kerja ahli"))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain TK ga masuk level faktor nya
    # dataKomponen[] <- lapply(dataKomponen, as.factor) #ubah char jd faktor, spy bs di drop down yg hanya komponen TK aja
    reactData$tableAddTK <- as.data.frame(dataKomponen[,c("jenis","unit")])
    reactData$tableAddTK <- as.data.frame(reactData$tableAddTK[1:as.numeric(input$tambahBarisTK_LS),])
    rownames(reactData$tableAddTK) <- c(1:nrow(reactData$tableAddTK))
    reactData$tableAddTK
  } 
  
})

output$tabelTambahTK_LS <- renderRHandsontable({
  rhandsontable(valJenisTK_LS(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300
  )
})


observeEvent(input$saveTambahBarisTK_LS,{
  # browser()
  removeUI(selector='#textTampilSaveTambahTK_LS')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahTK_LS))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  # editNew <- cbind(komponen = "tenaga kerja", editNew)
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data 
  editNew <- cbind(bagian = "factor labor",komponen = "tenaga kerja ahli", editNew)
  dataDefine$addTK <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahTK_LS',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahTK_LS","tabel di atas sudah tersimpan"))
})


################################################################################
#                                                                              #
#                      7.TENAGA KERJA UNSKILLED                                #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddTKUnskilled_LS,{
  removeUI(selector='#textTampilSaveTambahTKUnskilled_LS')
  insertUI(selector='#rhandsAddTKUnskilled_LS',
           where='afterEnd',
           ui= uiOutput('showRhandsAddTKUnskilled_LS'))
}) 

output$showRhandsAddTKUnskilled_LS <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahTKUnskilled_LS'),
                  tags$br(),
                  actionButton(('saveTambahBarisTKUnskilled_LS'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahTKUnskilled_LS')
  ))
  
})

valJenisTKUnskilled_LS <- eventReactive(input$showTabelAddTKUnskilled_LS,{
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addTKUnskilled)){
    reactData$tableAddTKUnskilled <- as.data.frame(dataDefine$addTKUnskilled[,-1])
    reactData$tableAddTKUnskilled <- as.data.frame(reactData$tableAddTK[1:as.numeric(input$tambahBarisTKUnskilled_LS),])
    rownames(reactData$tableAddTKUnskilled) <- c(1:nrow(reactData$tableAddTKUnskilled))
    reactData$tableAddTKUnskilled[] <- lapply(reactData$tableAddTKUnskilled, as.character) #ubah dr faktor jd char
    reactData$tableAddTKUnskilled
  } else if (is.null(dataDefine$addTKUnskilled)){
    dataKomponen <- filter(kumpulanDataJenisInputOutput,tipe.kebun == c("LARGE SCALE"))
    dataKomponen <- filter(dataKomponen,komponen == c("tenaga kerja unskilled"))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain TKUnskilled ga masuk level faktor nya
    # dataKomponen[] <- lapply(dataKomponen, as.factor) #ubah char jd faktor, spy bs di drop down yg hanya komponen TKUnskilledaja
    reactData$tableAddTKUnskilled <- as.data.frame(dataKomponen[,c("jenis","unit")])
    reactData$tableAddTKUnskilled <- as.data.frame(reactData$tableAddTKUnskilled[1:as.numeric(input$tambahBarisTKUnskilled_LS),])
    rownames(reactData$tableAddTKUnskilled) <- c(1:nrow(reactData$tableAddTKUnskilled))
    reactData$tableAddTKUnskilled
  } 
  
})

output$tabelTambahTKUnskilled_LS <- renderRHandsontable({
  rhandsontable(valJenisTKUnskilled_LS(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300
  )
})


observeEvent(input$saveTambahBarisTKUnskilled_LS,{
  # browser()
  removeUI(selector='#textTampilSaveTambahTKUnskilled_LS')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahTKUnskilled_LS))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  # editNew <- cbind(komponen = "tenaga kerja unskilled", editNew)
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data 
  editNew <- cbind(bagian = "factor labor",komponen = "tenaga kerja unskilled", editNew)
  dataDefine$addTKUnskilled <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahTKUnskilled_LS',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahTKUnskilled_LS","tabel di atas sudah tersimpan"))
})



################################################################################
#                                                                              #
#                                   8. FAKTOR CAPITAL                          #
#                                                                              #
################################################################################
observeEvent(input$showTabelAddFactorCapital_LS,{
  removeUI(selector='#textTampilSaveTambahFactorCapital_LS')
  insertUI(selector='#rhandsAddFactorCapital_LS',
           where='afterEnd',
           ui= uiOutput('showRhandsAddFactorCapital_LS'))
}) 

output$showRhandsAddFactorCapital_LS <- renderUI({
  fluidRow(column(12,
                  rHandsontableOutput('tabelTambahFactorCapital_LS'),
                  tags$br(),
                  actionButton(('saveTambahBarisFactorCapital_LS'), 'simpan tabel'),
                  tags$br(),
                  tags$br(),
                  tags$div(id='teksSaveTambahFactorCapital_LS')
  ))
  
})

output$tabelTambahFactorCapital_LS <- renderRHandsontable({
  rhandsontable(valJenisFactorCapital_LS(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300
  )
})


valJenisFactorCapital_LS <- eventReactive(input$showTabelAddFactorCapital_LS,{
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$addFactorCapital)){
    reactData$tableAddFactorCapital <- as.data.frame(dataDefine$addFactorCapital[,-1]) #kolom komponen yg sudah di save pd file rds di hide
    reactData$tableAddFactorCapital <- as.data.frame(reactData$tableAddFactorCapital[1:as.numeric(input$tambahBarisFactorCapital_LS),])
    rownames(reactData$tableAddFactorCapital) <- c(1:nrow(reactData$tableAddFactorCapital))
    reactData$tableAddFactorCapital[] <- lapply(reactData$tableAddFactorCapital, as.character) #ubah dr faktor jd char
    reactData$tableAddFactorCapital
  } else if (is.null(dataDefine$addFactorCapital)){
    dataKomponen <- filter(kumpulanDataJenisInputOutput,tipe.kebun == c("LARGE SCALE"))
    dataKomponen <- filter(dataKomponen,komponen == c("factor capital"))
    dataKomponen <- lowcase(dataKomponen,c(2:ncol(dataKomponen)))
    dataKomponen[] <- lapply(dataKomponen, as.character) #ubah dr faktor jd char, spy faktor selain FactorCapital ga masuk level faktor nya
    # dataKomponen[] <- lapply(dataKomponen, as.factor) #ubah char jd faktor, spy bs di drop down yg hanya komponen FactorCapital aja
    reactData$tableAddFactorCapital <- as.data.frame(dataKomponen[,c("jenis","unit")])
    reactData$tableAddFactorCapital <- as.data.frame(reactData$tableAddFactorCapital[1:as.numeric(input$tambahBarisFactorCapital_LS),])
    rownames(reactData$tableAddFactorCapital) <- c(1:nrow(reactData$tableAddFactorCapital))
    reactData$tableAddFactorCapital
  } 
  
})


observeEvent(input$saveTambahBarisFactorCapital_LS,{
  # browser()
  removeUI(selector='#textTampilSaveTambahFactorCapital_LS')
  
  editNew<-as.data.frame(hot_to_r(input$tabelTambahFactorCapital_LS))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  # editNew <- cbind(komponen = "factor capital", editNew)
  
  datapath <- paste0("shiny/data/", input$sut, "/",input$kom, "/")
  fileName <- paste0(datapath,"saveData","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi,"_",input$th,"_",input$tipeLahan,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data
  editNew <- cbind(bagian = "factor capital",komponen = "factor capital", editNew)
  dataDefine$addFactorCapital <- editNew
  saveRDS(dataDefine,file = fileName)
  
  insertUI(selector='#teksSaveTambahFactorCapital_LS',
           where = 'afterEnd',
           ui = tags$div(id="textTampilSaveTambahFactorCapital_LS","tabel di atas sudah tersimpan"))
})
