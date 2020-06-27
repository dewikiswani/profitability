contoh <- read.table("data/template/price template.csv", header = T, sep = ",")


analisisUI <- function(id){
  ns <- NS(id)
  tagList(
  argonRow(
    argonCard(
      width = 12,
      title = "Daftar Skenario",
      src = NULL,
      hover_lift = TRUE,
      shadow = TRUE,
      shadow_size = NULL,
      hover_shadow = FALSE,
      border_level = 0,
      icon = argonIcon("atom"),
      status = "primary",
      background_color = NULL,
      gradient = FALSE,
      floating = FALSE,
      argonRow(
        argonColumn(
          width = 12,
          fluidRow(
            column(10,
                   h1("area list skenario"),
                   br(),
                   h4("area list skenario")
            )
          )
        )
      )
    )
  ),
  argonRow(
    argonCard(
      width = 12,
      title = "PLOT",
      src = NULL,
      hover_lift = TRUE,
      shadow = TRUE,
      shadow_size = NULL,
      hover_shadow = FALSE,
      border_level = 0,
      icon = argonIcon("atom"),
      status = "primary",
      background_color = NULL,
      gradient = FALSE,
      floating = FALSE,
      argonRow(
        argonColumn(
          width = 12,
          fluidRow(
            column(10,
                   h1("area PLOT"),
                   br(),
                   h4("area plot")
            )
          )
        )
      )
    )
  ),
  dataTableOutput(ns("tab1")),
  #style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
)}

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