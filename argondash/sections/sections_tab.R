sections_tab <- argonTabItem(
  tabName = "sections",
  argonDashHeader(
    gradient = FALSE,
    color = "secondary",
    top_padding = 8,
    bottom_padding = 8,
    mask = TRUE,
      background_img = "https://imgs.mongabay.com/wp-content/uploads/sites/20/2016/11/03130006/oilpalm_replaces_rubber_agroforests-feature.jpg",
    opacity = 6,
    argonH1("Header with mask", display = 1) %>% argonTextColor(color = "white"),
    argonLead("This is the content.") %>% argonTextColor(color = "white")
  )
)
