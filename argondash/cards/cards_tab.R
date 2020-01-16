cards_tab <- argonTabItem(
  tabName = "cards",
  argonDashHeader(
    gradient = FALSE,
    color = "white",
    top_padding = 8,
    bottom_padding = 8,
    argonRow(
      argonColumn(
        width = 12,
        h1("Land Use Profitability"),
        #p('My first paragraph, with some ', strong('bold'),' text.'),
        p("The World Agroforestry Centre is the region's leading agroforestry research-in-development organization. 
           Regionally headquartered in Indonesia since 1993, we maintain offices and programs in Indonesia, 
           the Philippines, Thailand and Viet Nam. Since 1993, the Indonesia Country Programs of ICRAF 
           has been researching issues critical to sustainable national development, such as landscape restoration, 
           enhanced agroforestry systems for improved livelihoods, value chains and markets, environmental services' 
           schemes and co-investment, land-use planning for low-emission development, biodiversity conservation, 
           peatland restoration and resolution of conflict over land tenure. ICRAF Indonesia Country Program currently 
           implementing several projects to develop provincial level green economic growth plan and low carbon 
           development strategy. As part of our ongoing research on Green Growth Plan in ICRAF Indonesia, 
           we are going to develop a new calculation tools for land use profitability modelling.")
      )
    ),
    

    
    argonH1("", display = 4),
    argonRow(
      argonColumn(
        width = 3
      ),
      argonColumn(
        width = 3
      ),
      argonColumn(
        width = 3
      ),
      argonColumn(
        width = 3
      )
    ),
    br(), br(),
    
    # profile cards
    
    argonH1(" ", display = 4),
    argonRow(
      argonColumn(
        width = 12,
        argonProfile(
          title = "ICRAF",
          subtitle = "Indonesia",
          src = "https://pbs.twimg.com/profile_images/1191981430689144834/Em5N9JbQ_400x400.jpg",
          #url = "https://www.google.com",
          #url_1 = "https://www.google.com",
          #url_2 = "https://www.google.com",
          stats = argonProfileStats(
            
          )
          
          ,
          "Leveraging the world's largest repository of agroforestry science and information, we develop knowledge practices, from farmers' fields to the global sphere, to ensure food security and environmental sustainability. Our vision is an equitable world where all people have viable livelihoods supported by healthy and productive landscapes."
        )
      )
    )

  )
  )
  
