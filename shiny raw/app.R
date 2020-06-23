source("global.R")
source("module.R")


ui <- fluidPage( 
  titlePanel("Analisis"),
  tabsetPanel(
    tabPanel(
      h5("Profitability"),
      tags$br(),
      tags$br(),
      buttonUI("profit"),
    )
  )
)



server <- function(input,output,session){
  callModule(buttonModule, "profit")
  # 
  # observe({
  #   updateSelectInput(session,
  #                     "kom",
  #                     choices =sort(unique(komoditas$nama_komoditas)))
  # })
  # 
  # observe({
  #   updateSelectInput(session,
  #                     "user",
  #                     choices =sort(unique(peneliti$nama_peneliti)))
  # })
  # 
  # observe({
  #   updateSelectInput(session,
  #                     "selected_provinsi",
  #                     choices =sort(unique(indonesia$provinsi)))
  # })
  # 
  # 
  # observe({
  #   updateSelectInput(
  #     session,
  #     "selected_kota",
  #     choices = indonesia %>%
  #       filter(provinsi == input$selected_provinsi) %>%
  #       select(kabkot) %>%
  #       .[[1]]
  #   )
  # })
  # 
  # observe({
  #   updateSelectInput(
  #     session,
  #     "selected_kec",
  #     choices = indonesia %>%
  #       filter(kabkot == input$selected_kota) %>%
  #       select(kecamatan) %>%
  #       .[[1]]
  #   )
  # })
  # 
  # observe({
  #   updateSelectInput(
  #     session,
  #     "selected_desa",
  #     choices = indonesia %>%
  #       filter(kecamatan == input$selected_kec) %>%
  #       select(desa) %>%
  #       .[[1]]
  #   )
  # })
}


shinyApp(ui,server)

