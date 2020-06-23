# OxShef Shiny: tutorial-apps/controls-dependent-on-data

library("tidyverse")
library("gapminder")
library("rnaturalearthhires")
library("sf")
library("shiny")
library(indonesia)

# OxShef Shiny: tutorial-apps/controls-dependent-on-data

library("leaflet")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # uiOutput("selected_provinsi_UI"),
      selectInput("selected_provinsi",
                  "Pilih Provinsi:",
                  choices = ""),
      hr(),
      # uiOutput("selected_kota_UI"),
      selectInput("selected_kota",
                  "Pilih Kota/Kabupaten:",
                  choices = ""),
      hr(),
      # uiOutput("selected_kec_UI"),
      selectInput("selected_kec",
                  "Pilih Kecamatan:",
                  choices = ""),
      hr(),
      # uiOutput("selected_desa_UI"),
      selectInput("selected_desa",
                  "Pilih Desa:",
                  choices = "")
      ),
    mainPanel(
      
    )
  )
)


server <-function(input, output, session) {
  
  indonesia <- read.csv("C:/dw/ICRAF/profitability/data/lusita 2.0/prov sampai desa.csv")
  observe({
    updateSelectInput(session,
                      "selected_provinsi",
                      choices =sort(unique(indonesia$provinsi))) 
  })
  
  
  observe({
    updateSelectInput(
      session,
      "selected_kota",
      choices = indonesia %>%
        filter(provinsi == input$selected_provinsi) %>%
        select(kabkot) %>%
        .[[1]]
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "selected_kec",
      choices = indonesia %>%
        filter(kabkot == input$selected_kota) %>%
        select(kecamatan) %>%
        .[[1]]
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "selected_desa",
      choices = indonesia %>%
        filter(kecamatan == input$selected_kec) %>%
        select(desa) %>%
        .[[1]]
    )
  })
}

shinyApp(ui, server)

