
# Section informasi umum new---------------------------------------------

# observe({
#   updateSelectInput(
#     session,
#     "kom_new",
#     choices = komoditas %>%
#       filter(sut == input$sut_new) %>%
#       select(nama_komoditas) %>%
#       .[[1]]
#   )
# })


observe({
  updateSelectInput(session,
                    "selected_provinsi_new",
                    choices =sort(unique(indonesia$provinsi)))
})

# observe({
#   updateSelectInput(
#     session,
#     "selected_provinsi_new",
#     choices = komoditas %>%
#       filter(nama_komoditas == input$kom_new) %>%
#       select(provinsi) %>%
#       .[[1]]
#   )
# })

# observe({
#   updateSelectInput(
#     session,
#     "th_new",
#     choices = komoditas %>%
#       filter(provinsi == input$selected_provinsi_new) %>%
#       select(tahun_analisis) %>%
#       .[[1]]
#   )
# })

# observe({
#   updateSelectInput(
#     session,
#     "tipeLahan_new",
#     choices = komoditas %>%
#       filter(tahun_analisis == input$th_new) %>%
#       select(tipe_lahan) %>%
#       .[[1]]
#   )
# })

# End - Section informasi umum new---------------------------------------------



# Section asumsi makro NEW---------------------------------------------
reactData_new <- reactiveValues(
  tableP1 = NULL, #price input
  tableP2 = NULL, #price output
  tableIO1 = NULL, #io input
  tableIO2 = NULL, #io output
  tableCapP = NULL, #capital privat
  tableCapS = NULL #capital sosial
)

data_new <- reactive({
  # informasi umum
  sut <- input$sut_new
  kom <- toupper(input$kom_new)
  provinsi <- input$selected_provinsi_new
  th <- input$th_new
  tipeLahan <- input$tipeLahan_new
  tipeKebun <- input$tipeKebun_new
  waktuInput<-Sys.time()
  waktuInput<-gsub(" ","_",waktuInput,fixed = TRUE)
  waktuInput<-gsub(":","-",waktuInput,fixed = TRUE)
  reactData$timeInput <- waktuInput
  jamInput <- strsplit(waktuInput,"_")[[1]][2]
  tanggalInput <- strsplit(waktuInput,"_")[[1]][1]
  
  
  
  combineDef <- list(
    sut=sut,
    kom=kom,
    provinsi = provinsi,
    th=th,
    tipeLahan = tipeLahan,
    tipeKebun = tipeKebun,
    waktuInput = waktuInput,
    jamInput = jamInput,
    tanggalInput = tanggalInput)
  
  # save data untuk setiap perubahan
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  saveRDS(combineDef,file = fileName)
  combineDef
  
})

observeEvent(c(input$sut_new,input$kom_new,input$selected_provinsi_new,input$th_new,input$tipeLahan_new,input$tipeKebun_new), {
  
  removeUI(selector='#showMakro_new')
  removeUI(selector='#showTable_new')
  removeUI(selector='#showButton_new')
  removeUI(selector='#showResult_new')
})




observeEvent(input$asumsiMakro_button_new, {
  # browser()
  if(input$kom_new == ""){
    shinyalert("Gagal!", "User harus mendefinisikan komoditas", type = "error")
    # modalKomNull()
  }else{
    data_new()
    
    insertUI(selector='#uiShowMakro_new',
             where='afterEnd',
             ui= uiOutput('showMakro_new'))
  }
}) 

# modalKomNull <- function(failed = FALSE) {
#   modalDialog( 
#     footer=tagList(
#       actionButton(("bangunKuantitasOut_new"), "Lanjut",style="color: white;background-color: green;")
#     ),
#     argonTabSet(
#       id = "tabNew",
#       card_wrapper = TRUE,
#       horizontal = TRUE,
#       circle = FALSE,
#       size = "l",
#       width = 12,
#       argonTab(
#         tabName = "Menentukan Jumlah Komponen (Baris) Pada Tabel Kuantitas bagian Output",
#         active = T,
#         h3("Berapa jumlah komponen (baris) yang akan user bangun pada Output-Tabel Kuantitas?"),
#         selectInput(("pilihTambahBaris_output_new"),
#                     " ",
#                     choices = c(1:10),selected = 1,width = "800px")
#       ))
#     ,
#     size="l",
#     easyClose = FALSE)
# }




output$showMakro_new <- renderUI({
  argonRow(
    argonColumn(
      width = 12,
      argonH1("Asumsi Makro", display = 4),
      h5("Langkah 2: menentukan asumsi makro untuk data PAM yang dibangun"),
      br(),
      fluidRow(
        column(3,
               sliderInput(("rate.p_new"), "Discount Rate Private", 7.4 ,min = 0, max = 15, step = 0.01)
        ),
        column(3,
               sliderInput(("rate.s_new"), "Discount Rate Social", 2.4 ,min = 0, max = 8, step = 0.01)
        ),
        column(4,
               sliderInput(("nilai.tukar_new"), "Nilai Tukar Rupiah", 14831 ,min = 10000, max = 20000, step = 10)
        ),
        column(2,
               br(),
               actionButton(("pilihBarisOutput_new"),"Membangun Tabel PAM",icon("paper-plane"),style="color: white; 
                         background-color: green;")
        )
      )
    )
  )
})

observeEvent(c(input$rate.p_new,input$rate.s_new,input$nilai.tukar_new), {
  removeUI(selector='#showTable_new')
  removeUI(selector='#showButton_new')
  removeUI(selector='#showResult_new')
})

# End - Section asumsi makro NEW---------------------------------------------
################################################################################
#                                                                              #
#                    TABEL KUANTITAS OUTPUT                                     #
#                                                                              #
################################################################################

observeEvent(input$pilihBarisOutput_new,{
  # show modal
  showModal(modalPilihBarisOutput_new())
  
})

modalPilihBarisOutput_new <- function(failed = FALSE) {
  modalDialog( 
    footer=tagList(
      actionButton(("bangunKuantitasOut_new"), "Lanjut",style="color: white;background-color: green;")
    ),
    argonTabSet(
      id = "tabNew",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "l",
      width = 12,
      argonTab(
        tabName = "Menentukan Jumlah Komponen (Baris) Pada Tabel Kuantitas bagian Output",
        active = T,
        h3("Berapa jumlah komponen (baris) yang akan user bangun pada Output-Tabel Kuantitas?"),
        selectInput(("pilihTambahBaris_output_new"),
                    " ",
                    choices = c(1:10),selected = if (is.null(reactData_new$tableIO2)){
                      1
                    } else {nrow(reactData_new$tableIO2)}
                    ,width = "800px")
      ))
    ,
    size="l",
    easyClose = FALSE)
}

observeEvent(input$bangunKuantitasOut_new,{
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data price
  dataDefine$pilihTambahBaris_output_new <- input$pilihTambahBaris_output_new
  saveRDS(dataDefine,file = fileName)
  
  showModal(modalTabelKuantitasOut_new())
})

valIO2_new <- eventReactive(input$bangunKuantitasOut_new,{
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  if(is.null(dataDefine$ioOutput)){
  readDataTemplate <- read.table(paste0("shiny/data/template/tabel pam kosong",".csv"), header = T, sep = ",")
  yearIO <- 30 #tahun daur tanam
  
  inputData <- readDataTemplate[1:input$pilihTambahBaris_output_new,]
  ioInput <- inputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
  ioInput$komponen <- as.character(ioInput$komponen)
  ioInput$jenis<- as.character(ioInput$jenis)
  ioInput$unit<- as.character(ioInput$unit)
  ioInput[,c(4:33)] <- as.numeric(as.character(ioInput[,c(4:33)]))
  
  reactData_new$tableIO2 <- ioInput
  reactData_new$tableIO2
  }else{
    reactData$tableIO2 <- dataDefine$ioOutput
    reactData$tableIO2
  }
 
})

output$kuantitasOutput_new <- renderRHandsontable({
  rhandsontable(valIO2_new(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300,
  )
})


modalTabelKuantitasOut_new <- function(failed = FALSE) {
  modalDialog( 
    footer=tagList(
      actionButton("backtoRowOutput","Kembali"),
      actionButton(("pilihBarisInput_new"), "Simpan Tabel dan Lanjutkan Membangun Input pada Tabel Kuantitas",style="color: white;background-color: green;")
    ),
    argonTabSet(
      id = "tabNew2",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "l",
      width = 12,
      argonTab(
        tabName = "Langkah 3: Mengisi Tabel Kuantitas Bagian Output",
        active = T,
        h3("Tabel Kuantitas (OUTPUT)", align = "center"),
        rHandsontableOutput('kuantitasOutput_new')
      ))
    ,
    size="l",
    easyClose = FALSE)
}

observeEvent(input$backtoRowOutput,{
  showModal(modalPilihBarisOutput_new())
})

################################################################################
#                                                                              #
#                    TABEL KUANTITAS INPUT                                     #
#                                                                              #
################################################################################

observeEvent(input$pilihBarisInput_new,{
  # save data untuk setiap perubahan
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  editNew<-as.data.frame(hot_to_r(input$kuantitasOutput_new))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  
  dataDefine$ioOutput <- editNew
  saveRDS(dataDefine,file = fileName)
  
  showModal(modalPilihBarisInput_new())
})

modalPilihBarisInput_new <- function(failed = FALSE) {
  modalDialog( 
    footer=tagList(
      actionButton(("bangunKuantitas_new"), "Lanjut",style="color: white;background-color: green;")
    ),
    argonTabSet(
      id = "tabNew",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "l",
      width = 12,
      argonTab(
        tabName = "Menentukan Jumlah Komponen (Baris) Pada Tabel Kuantitas bagian Input",
        active = T,
        h3("Berapa jumlah komponen (baris) yang akan user bangun pada Input-Tabel Kuantitas?"),
        selectInput(("pilihTambahBaris_input_new"),
                    " ",
                    choices = c(5:40),selected = if (is.null(reactData_new$tableIO1)){
                      20
                    } else {nrow(reactData_new$tableIO1)}
                    ,width = "800px")
      ))
    ,
    size="l",
    easyClose = FALSE)
}

observeEvent(input$bangunKuantitas_new,{
  # browser()
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  # replace data price
  dataDefine$pilihTambahBaris_input_new <- input$pilihTambahBaris_input_new
  saveRDS(dataDefine,file = fileName)
  
  showModal(modalTabelKuantitasIn_new())
})


modalTabelKuantitasIn_new <- function(failed = FALSE) {
  modalDialog( 
    footer=tagList(
      actionButton("backtoRowInput","Kembali"),
      actionButton(("bangunTabelHarga_new"), "Simpan Tabel dan Lanjutkan Membangun Tabel Harga",style="color: white;background-color: green;")
    ),
    argonTabSet(
      id = "tabNew",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "l",
      width = 12,
      argonTab(
        tabName = "Langkah 4: Mengisi Tabel Kuantitas Bagian Input",
        active = T,
        h3("Tabel Kuantitas (INPUT)", align = "center"),
        rHandsontableOutput('kuantitasInput_new')
      ))
    ,
    size="l",
    easyClose = FALSE)
}

observeEvent(input$backtoRowInput,{
  browser()
  showModal(modalPilihBarisInput_new())
})


valIO1_new <- eventReactive(input$bangunKuantitas_new,{
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  if(is.null(dataDefine$ioInput)){
    readDataTemplate <- read.table(paste0("shiny/data/template/tabel pam kosong",".csv"), header = T, sep = ",")
    yearIO <- 30 #tahun daur tanam
    
    inputData <- readDataTemplate[1:input$pilihTambahBaris_input_new,]
    ioInput <- inputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
    ioInput$komponen <- as.character(ioInput$komponen)
    ioInput$jenis<- as.character(ioInput$jenis)
    ioInput$unit<- as.character(ioInput$unit)
    ioInput[,c(4:33)] <- as.numeric(as.character(ioInput[,c(4:33)]))
    
    reactData_new$tableIO1 <- ioInput
    reactData_new$tableIO1
  }else{
    reactData$tableIO1 <- dataDefine$ioInput
    reactData$tableIO1
  }
})

output$kuantitasInput_new <- renderRHandsontable({
  rhandsontable(valIO1_new(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 600,
  )
})

observeEvent(input$bangunTabelHarga_new,{
  # save data untuk setiap perubahan
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  editNew<-as.data.frame(hot_to_r(input$kuantitasInput_new))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  
  
  dataDefine$ioInput <- editNew
  saveRDS(dataDefine,file = fileName)
  
  # show modal
  showModal(modalTabelHarga_new())
  
})




################################################################################
#                                                                              #
#                                 BUTTON HARGA                                 #
#                                                                              #
################################################################################
modalTabelHarga_new <- function(failed = FALSE) {
  modalDialog( 
    footer=tagList(
      actionButton(("batalButton"), "Batal", style="color: white;background-color: red;"),
      actionButton(("running_button_new"), "Simpan Tabel dan Jalankan Analisis",style="color: white;background-color: green;")
    ),
    argonTabSet(
      id = "tabNew3",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "l",
      width = 12,
      argonTab(
        tabName = "Langkah 5: Mengisi Tabel Harga",
        active = T,
        h3("Tabel Harga", align = "center"),
        rHandsontableOutput('hargaOutput_new'),
        br(),
        rHandsontableOutput('hargaInput_new')
      ))
    ,
    size="l",
    easyClose = FALSE)
}

observeEvent(input$batalButton,{
  browser()
  removeModal()
})

output$hargaOutput_new <- renderRHandsontable({
  rhandsontable(valP2_new(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 100,
  )
})


valP2_new <- eventReactive(input$bangunTabelHarga_new,{
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  if(is.null(dataDefine$priceInput) & is.null(dataDefine$priceOutput)){
    reactData$tableP2 <- dataDefine$ioOutput[,1:2]
    
    unit.harga <- data.frame(matrix("rp",ncol = 1,nrow = nrow(reactData$tableP2)))
    colnames(unit.harga) <- "unit.harga"
    harga.privat <- data.frame(matrix(0,ncol = 1,nrow = nrow(reactData$tableP2)))
    colnames(harga.privat) <- "harga.privat"
    harga.sosial <- data.frame(matrix(0,ncol = 1,nrow = nrow(reactData$tableP2)))
    colnames(harga.sosial) <- "harga.sosial"
    
    reactData$tableP2 <- cbind(reactData$tableP2,unit.harga,harga.privat,harga.sosial)
    reactData$tableP2
  }else{
    reactData$tableP2 <- dataDefine$priceOutput
    reactData$tableP2
  }
})

output$hargaInput_new <- renderRHandsontable({
  rhandsontable(valP1_new(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 600,
  )
})


valP1_new <- eventReactive(input$bangunTabelHarga_new,{
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  if(is.null(dataDefine$priceInput) & is.null(dataDefine$priceOutput)){
    reactData$tableP1 <- dataDefine$ioInput[,1:2]
    
    unit.harga <- data.frame(matrix("rp",ncol = 1,nrow = nrow(reactData$tableP1)))
    colnames(unit.harga) <- "unit.harga"
    harga.privat <- data.frame(matrix(0,ncol = 1,nrow = nrow(reactData$tableP1)))
    colnames(harga.privat) <- "harga.privat"
    harga.sosial <- data.frame(matrix(0,ncol = 1,nrow = nrow(reactData$tableP1)))
    colnames(harga.sosial) <- "harga.sosial"
    
    reactData$tableP1 <- cbind(reactData$tableP1,unit.harga,harga.privat,harga.sosial)
    reactData$tableP1
  }else{
    reactData$tableP1 <- dataDefine$priceInput
    reactData$tableP1
  }
})

observeEvent(input$running_button_new,{
  # save data untuk setiap perubahan
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  editNewP1<-as.data.frame(hot_to_r(input$hargaInput_new))
  editNewP1[is.na(editNewP1)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  
  editNewP2<-as.data.frame(hot_to_r(input$hargaOutput_new))
  editNewP2[is.na(editNewP2)] <- 0 
  
  dataDefine$priceInput <- editNewP1
  dataDefine$priceOutput <- editNewP2
  
  saveRDS(dataDefine,file = fileName)
})

# Section tampilkan tabel---------------------------------------------
observeEvent(input$running_button_new, {
  removeModal()
  insertUI(selector='#uiShowTable_new',
           where='afterEnd',
           ui= uiOutput('showTable_new'))
  
  insertUI(selector='#uiShowResult_new',
           where='afterEnd',
           ui= uiOutput('showResult_new'))
}) 

output$showTable_new <- renderUI({
  argonRow(
    argonColumn(
      width = 12,
      argonH1("Tabel", display = 4),
      h5("Menampilkan Tabel PAM"),
      
      # jika tdk bisa jadi input buttton maka coba ubah nama action  buttonnya sepertinya conflict dengan script lain
      argonTabSet(
        id = "tab1_new",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "sm",
        width = 12,
        iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Tabel Harga",
          active = T,
          dataTableOutput("showTablePrice_new"),
          style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
        ),
        argonTab(
          tabName = "Tabel Kuantitas",
          active = F,
          dataTableOutput(("showTableKuantitas_new")),
          style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
        ),
        argonTab(
          tabName = "Tabel Modal Kapital",
          active = F,
          dataTableOutput(("showTableKapital_new")),
          style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
        )
      ),
    )
  )
})

output$showTablePrice_new <- renderDataTable({
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  readDataLastEdited <- readRDS(fileName)
  dataView <- rbind(readDataLastEdited$priceInput, readDataLastEdited$priceOutput)
  dataView[is.na(dataView)] <- 0 #NA replace with zero
  dataView
})

output$showTableKuantitas_new <- renderDataTable({
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  # print("data terakhir tersimpan di rds")
  readDataLastEdited <- readRDS(fileName)
  dataView <- rbind(readDataLastEdited$ioInput, readDataLastEdited$ioOutput)
  dataView[is.na(dataView)] <- 0 #NA replace with zero
  no.id <- as.numeric(1:nrow(dataView))
  rownames(dataView) <- no.id
  dataView
  
})

output$showTableKapital_new <- renderDataTable({
  # case for modal kapital
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  cekCapital <- file.exists(paste0(datapath,"kapital template.csv")) #cek keberadaan file ini ada atau engga
  
  if(cekCapital == T){
    fileName <- paste0(datapath,"saveData_new","_",
                       input$sut,"_",input$kom,"_",
                       input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
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

################################################################################
#                                                                              #
#                                RESULT                                        #
#                                                                              #
################################################################################

output$showResult_new <- renderUI({
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
             dataTableOutput("tableResultBAU1_new"),
      ),
      column(6,
             dataTableOutput("tableResultBAU2_new")
             
      )
    ),
    fluidRow(
      column(4,
             plotlyOutput('plot_new')
      ),
      column(6,
             
      ),
      column(2,
             actionButton(("saveNewPAM_new"),"Simpan PAM",icon("paper-plane"),style="color: white;background-color: green;"),
             br(),
             tags$div(id='teksNewPamSave')
      )
    )
    
    
  )
})

output$tableResultBAU1_new <- renderDataTable({
  datatable(data.gab_new()$tabel1, option=list(dom = "t"))
})

output$tableResultBAU2_new <- renderDataTable({
  datatable(data.gab_new()$tabel2, option=list(dom = "t"))
  
})

data.gab_new <- eventReactive(input$running_button_new,{
  
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  dataDefine$rate.p <- input$rate.p_new
  dataDefine$rate.s <- input$rate.s_new
  dataDefine$nilai.tukar <- input$nilai.tukar_new
  # dataDefine <- readDataLastEdited()
  
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



preparePlot_new <- eventReactive(input$running_button_new,{
  
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  
  dataPlot <- data.frame(
    # tipe.data=dataDefine$tipeData,
    komoditas=dataDefine$kom,
    NPV.Privat.RP=dataDefine$npv[1,1])
  
  
  dataPlot %>%
    plot_ly(x = ~komoditas, y = ~NPV.Privat.RP, type = "bar", color = ~komoditas)
})

output$plot_new <- renderPlotly({
  preparePlot_new()
})

observeEvent(input$saveNewPAM_new, {
  browser()
  datapath <- paste0("shiny/data/", input$sut_new, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_new","_",
                     input$sut,"_",input$kom,"_",
                     input$selected_provinsi_new,"_",input$th_new,"_",input$tipeLahan_new,"_",input$tipeKebun_new,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  #replace informasi umum -- untuk lbh yakin yang tersave adalah pilihan terakhir user
  dataDefine$sut <- input$sut
  
  #replace asumsi macro-- untuk lbh yakin yang tersave adalah pilihan terakhir user
  dataDefine$rate.p <- input$rate.p
  
  
  
})