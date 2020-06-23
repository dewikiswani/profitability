# OxShef Shiny: tutorial-apps/controls-dependent-on-data

library("tidyverse")
library("gapminder")
library("rnaturalearthhires")
library("sf")
library("shiny")

# OxShef Shiny: tutorial-apps/controls-dependent-on-data

library("leaflet")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      "The pulldown menu below contains only those continents included in the gapminder dataset.",
      # uiOutput("selected_continent_UI"),
      selectInput("selected_continent",
                  "Selected continent:",
                  choices = ""),
      hr(),
      "The pulldown menu below is populated with countries included in the gapminder dataset which are members of the continent chosen above.",
      # uiOutput("selected_country_UI"),
      selectInput("selected_country",
                  "Selected country:",
                  choices = ""),
      hr(),
      "Hover and click on the displayed country shape for additional information about the country.",
      "Country shapes are from the rnaturalearthhires library.",
      hr(),
      HTML("This app is part of the <a href='https://oxshef.github.io/oxshef_shiny/tutorials_controls-dependent-on-data.html' target='_blank'>Oxshef \"Controls dependent on data\" tutorial</a>")
    ),
    mainPanel(
      #leafletOutput("selected_country_map")
    )
  )
)

#country_shps <- countries10 %>%
 # st_as_sf() %>%
  #setNames(. , tolower(colnames(.)))

#gapminder_countries <- country_shps %>%
#  inner_join(gapminder %>%
#               filter(year == max(year)),
#             by = c("name" = "country", "continent" = "continent"))

server <-function(input, output, session) {
  observe({
    updateSelectInput(session,
                      "selected_continent",
                        choices = unique(indonesia_kota$nama_provinsi))
  })


  observe({
    updateSelectInput(
      session,
      "selected_country",
      choices = indonesia_kota %>%
        filter(nama_provinsi == input$selected_continent) %>%
        select(nama_kota) %>%
        .[[1]]
    )


  })


}

shinyApp(ui, server)

