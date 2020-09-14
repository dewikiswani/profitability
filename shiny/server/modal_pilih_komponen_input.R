output$showTablePilihJenis <- renderUI({
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
      column(12,
             selectInput("tambahBarisBibit",
                         "Berapa baris yang akan ditambahkan untuk komponen bibit?",
                         choices = c(0:10),selected = 5,width = "500px")
      ),
      column(12,
             rHandsontableOutput('tabelTambahBibit'),
             tags$br(), 
             actionButton(('saveTambahBarisBibit'), 'simpan tabel'), 
             tags$br(), 
             tags$br(),
             tags$div(id='teksSaveTambahBibit')
      )
    )
  }else if (input$pilihKomponenInput == "peralatan"){
    fluidRow(
      column(12,
             selectInput("tambahBarisPeralatan",
                         "Berapa baris yang akan ditambahkan untuk komponen peralatan?",
                         choices = c(0:10),selected = 5,width = "500px")
      ),
      column(12,
             rHandsontableOutput('tabelTambahPeralatan'),
             tags$br(), 
             actionButton(('saveTambahBarisPeralatan'), 'simpan tabel'), 
             tags$br(), 
             tags$br(),
             tags$div(id='teksSaveTambahPeralatan')
      )
    )
  } else if (input$pilihKomponenInput == "tenaga kerja"){
    fluidRow(
      column(12,
             selectInput("tambahBarisTK",
                         "Berapa baris yang akan ditambahkan untuk komponen tenaga kerja?",
                         choices = c(0:10),selected = 5,width = "600px")
      ),
      column(12,
             rHandsontableOutput('tabelTambahTK'),
             tags$br(), 
             actionButton(('saveTambahBarisTK'), 'simpan tabel'), 
             tags$br(), 
             tags$br(),
             tags$div(id='teksSaveTambahTK')
      )
    )
  }
})


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
    reactData$tableAddPupuk <- as.data.frame(dataDefine$addPupuk[,-1])
    reactData$tableAddPupuk <- as.data.frame(reactData$tableAddPupuk[1:as.numeric(input$tambahBarisPupuk),])
    colnames(reactData$tableAddPupuk) <- "jenis"
    reactData$tableAddPupuk
  } else if (is.null(dataDefine$addPupuk)){
    reactData$tableAddPupuk <- as.data.frame(dataPupuk[,-1])
    reactData$tableAddPupuk <- as.data.frame(reactData$tableAddPupuk[1:as.numeric(input$tambahBarisPupuk),])
    colnames(reactData$tableAddPupuk) <- "jenis"
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