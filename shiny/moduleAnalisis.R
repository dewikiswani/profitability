contoh <- read.table("data/template/price template.csv", header = T, sep = ",")


analisisUI <- function(id){
  ns <- NS(id)
  argonTabSet(
    id = "tab-1",
    card_wrapper = TRUE,
    horizontal = TRUE,
    circle = FALSE,
    size = "sm",
    width = 12,
    iconList = lapply(X = 1:2, FUN = argonIcon, name = "atom"),
    argonTab(
      tabName = "Berdasarkan Lokasi",
      active = T,
      dataTableOutput(ns("tab1")),
      style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
    ),
    argonTab(
      tabName = "Berdasarkan Komoditas",
      active = F,
      dataTableOutput(ns("tab2")),
      style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
    )
  )
}

analisisModule <- function(input, output, session) {
  
  # load the namespace
  ns <- session$ns
  
  output$tab1 <- renderDataTable({
    dataView <- contoh
    dataView
  })
  
  output$tab2 <- renderDataTable({
    dataView <- contoh
    dataView
  })
  
}