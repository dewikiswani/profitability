buttonUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("modalDefineButton"),'Deskripsi Skenario')
  )
}


buttonModule <- function(input, output, session) {
  
  # load the namespace
  ns <- session$ns
  
  ################################################################################
  #                                                                              #
  #                        BUTTON DEFINE                                         #
  #                                                                              #
  ################################################################################
  observeEvent(input$modalDefineButton,{
    showModal(modalDialog(sidebarLayout(sidebarPanel(
      fluidPage(
        textInput(ns("intervensiDef"),
                  label="Nama Peneliti"),
        selectInput(ns("sut"),
                    "Sistem Usaha Tani",choices = ""),
        selectInput(ns("kom"),
                    "Komoditas",choices =""),
        selectInput(("tahunSkenario"),
                    label="Tahun",
                    choices = c(2000:2020)),
        selectInput("selected_provinsi",
                    "Pilih Provinsi:",
                    choices = ""),
        selectInput("selected_kota",
                    "Pilih Kota/Kabupaten:",
                    choices = ""),
        selectInput("selected_kec",
                    "Pilih Kecamatan:",
                    choices = ""),
        selectInput("selected_desa",
                    "Pilih Desa:",
                    choices = "")
      ),
      tags$br(),
      actionButton(("defHit"),"tampilkan"),
      width=5
    ),
    mainPanel(
      tags$div(id = 'defPlaceholder'),
      width=7
    )),
    title="Deskripsi Skenario",
    footer= tagList(
      actionButton(("saveModalDef"), "simpan skenario"),
      actionButton(("cancelModalDef"), "batal")
    ),
    size="l",
    easyClose = FALSE
    ))
  })
  
}