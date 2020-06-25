buttonUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("modalPriceButton"),'Harga'),
    actionButton(ns("modalIOButton"),'Input-Output'),
    br(),
    br(),
    br(),
    checkboxInput(ns("checkKapital"),'modal kapital dimasukkan kedalam perhitungan'),
    actionButton(ns("modalCapitalButton"),'Modal Kapital'),
  )
}


buttonModule <- function(input, output, session) {
  
  # load the namespace
  ns <- session$ns
  
  ################################################################################
  #                                                                              #
  #                                 BUTTON HARGA                                 #
  #                                                                              #
  ################################################################################
  observeEvent(input$modalPriceButton,{
    showModal(
      modalDialog( 
        footer=tagList(
          actionButton(ns("closeModalPrice"), "Tutup")
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
                  fluidRow(
                    column(6,
                           selectInput(ns("priceYear_input"),
                                       "Pilih tahun skenario:",
                                       choices = c(1:30),selected = 30)
                    ),
                    column(6,
                           selectInput(ns("priceComp_input"),
                                       "banyaknya komponen yang diinput:",
                                       choices = c(1:50),selected = 10)
                    )
                  )
                  ),
                tags$br(),
                actionButton(ns("priceInputHit"),"tampilkan tabel"),
                width=12
              ),
              mainPanel(
                tags$div(id = ns('priceInputPlaceholder')),
                width=12)
            )
          ),
          argonTab(
            tabName = "Output",
            #active = T,
            sidebarLayout(
              sidebarPanel(
                fluidPage(
                  h2("Sunting Harga Output"),
                  fluidRow(
                    column(6,
                           selectInput(ns("priceYear_output"),
                                       "Pilih tahun skenario:",
                                       choices = c(1:30),selected = 30)
                    ),
                    column(6,
                                    selectInput(ns("priceComp_output"),
                                                "banyaknya komponen yang diinput:",
                                                choices = c(1:10),selected = 2)
                           )
                  )
                ),
                tags$br(),
                actionButton(ns("priceOutputHit"),"tampilkan tabel"),
                width=12
              ),
              mainPanel(
                tags$div(id = ns('priceOutputPlaceholder')),
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
  

  
  # START Price Input ----------------------------------------------------------
  satAkun <- reactiveValues(
    table1 = NULL
  )
  
  observeEvent(input$priceInputHit, {
    insertUI(selector= paste0("#", ns("priceInputPlaceholder")),
             where='afterEnd',
             ui= uiOutput(ns('priceInputUI'))
    )
  })
  
  output$priceInputUI<- renderUI({
    
    tagList(
      tags$br(),
      tags$br(),
      tags$b('Sunting secara manual'),
      tags$br(),
      tags$h4("untuk memilih tipe komponen dan unit disediakan menu dropdown pada kolom 1 dan 3"),
      tags$h6("pastikan komponen dan unit sesuai dengan daftar yang disediakan pada menu dropdown"),
      tags$br(),
      rHandsontableOutput(ns('editPriceInput')),
      tags$br(),
      actionButton(ns('savePriceInput'), 'simpan tabel'), 
      tags$br(), 
      tags$br(),
      tags$div(id='teksPriceInputSave')
    )
  })
  
  valSatLand <- eventReactive(input$priceInputHit,{
    # selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    # fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
    # dataDefine <-  readRDS(fileName)
    
    # if (is.null(dataDefine$satSelisih)) {
    #   satAkun$table1 = data$listConsumZero
    #   satAkun$table1
    # }else{
    #   selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    #   fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
    #   dataDefine <-  readRDS(fileName)
    #   satAkun$table1 <- data$listConsumZero
    #   tupla <- ordered(factor(sort(colnames(LDMProp_his)[landCover_his!=0])))
    #   satAkun$table1[,1] <- tupla[1]
    #   satAkun$table1[,2] <- tupla[1]
    #   satAkun$table1 = dplyr::bind_rows(dataDefine$satSelisih,satAkun$table1)
    # }
    # 
    # indexCol <- c(colnames(satAkun$table1)[1],colnames(satAkun$table1)[2],
    #               paste0("y",input$pilihtahunSatLand))
    # indexRow <- input$banyakTupla
    # indexRow <- as.numeric(indexRow)
    # satAkun$table1 <- satAkun$table1[1:indexRow,c(indexCol)]
    # satAkun$table1
    satAkun$table1 <- read.table("data/contoh.csv", header = T, sep = ",")
    satAkun$table1
  })
  
  output$editPriceInput <- renderRHandsontable({
    rhandsontable(valSatLand(),
                  rowHeaderWidth = 50,
                  fixedColumnsLeft = 2,
                  height = 200,
    ) 
    # %>% 
    #   hot_table(contextMenu = TRUE) %>% 
    #   hot_context_menu(customOpts = list(
    #     search = list(name = "Search",
    #                   callback = htmlwidgets::JS(
    #                     "function (key, options) {
    #                      var srch = prompt('Search criteria');
    #                      this.search.query(srch);
    #                      this.render();
    #                    }")))) %>% 
    #   hot_col(col = c(colnames(satAkun$table1)[1],colnames(satAkun$table1)[2]),
    #           type = "dropdown", source = sort(colnames(LDMProp_his)[landCover_his!=0])) 
    
  })
  
  
  ##### simpan tabel Sat baru ke dalam folder ####
  observeEvent(input$savePriceInput,{
    removeUI(selector='#textInvalid')
    removeUI(selector='#textTampil')
    
    satEditNew<-as.data.frame(hot_to_r(input$editPriceInput))
    satInvalid <- satEditNew[-2,]
    tupla <- ordered(factor(sort(colnames(LDMProp_his)[landCover_his!=0])))
    satZero = data$listConsumZero
    satZero[,1] <- tupla[1]
    satZero[,2] <- tupla[1]
    satSelisih <- dplyr::bind_rows(satZero,satEditNew)
    satSelisih <- satSelisih[-(1:nrow(data$listConsumZero)),]
    rownames(satSelisih) <- 1:nrow(satSelisih)
    satSelisih[is.na(satSelisih)] <- 0
    
    if (nlevels(satSelisih[,1])==length(colnames(LDMProp_his)[landCover_his!=0]) & nlevels(satSelisih[,2])==length(colnames(LDMProp_his)[landCover_his!=0]) ) {
      selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
      fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
      
      dataDefine <-  readRDS(fileName)
      dataDefine[5] <- list(dataDefine$fdSelisih)
      dataDefine$satSelisih <- satSelisih
      
      saveRDS(dataDefine, file = fileName)
      
      insertUI(selector='#teksPriceInputSave',
               where = 'afterEnd',
               ui = tags$div(id="textTampil","tabel di atas sudah tersimpan"))
    }else {
      
      insertUI(selector='#teksPriceInputSave',
               where = 'afterEnd',
               ui =tags$h4(id='textInvalid', 
                           "masih ada tutupan lahan yang belum dipilih, tabel tidak dapat disimpan"))
    }
  })
  
}