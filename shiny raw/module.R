#setwd("C:/dw/ICRAF/profitability/shiny")
buttonUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("modalDefineButton"),'Deskripsi Skenario')
  )
}

buttonModule <- function(input,output,session){
  # load the namespace
  ns <- session$ns
  
  # selected_provinsi <- eventReactive(selected_provinsi(),{
  #   filter(indonesia, provinsi == input$selected_provinsi)
  # })
  # 
  # observeEvent(selected_provinsi(), {
  #   choices <- unique(selected_provinsi()$kabkot)
  #   updateSelectInput(session, "selected_kota", choices = NULL)
  # })
  
  ################################################################################
  #                                                                              #
  #                        BUTTON DEFINE                                         #
  #                                                                              #
  ################################################################################
  observeEvent(input$modalDefineButton,{
    showModal(modalDialog(sidebarLayout(sidebarPanel(
      fluidRow(
        textInput(ns("intervensiDef"),
                  label="Nama Peneliti"),
        selectInput("sut",
                    "Sistem Usaha Tani",choices = c("Monokultur","Agroforestry")),
        selectInput("kom",
                    "Komoditas",choices =sort(unique(komoditas$nama_komoditas))),
        selectInput(("tahunSkenario"),
                    label="Tahun",
                    choices = c(2000:2020)),
        selectInput("selected_provinsi",
                    "Pilih Provinsi:",
                    choices = sort(unique(indonesia$provinsi))),
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
