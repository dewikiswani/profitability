argonSidebar <- argonDashSidebar(
  vertical = TRUE,
  skin = "light",
  background = "white",
  size = "lg",
  side = "left",
  id = "my_sidebar",
  brand_url = "http://www.google.com",
  brand_logo = "https://pbs.twimg.com/profile_images/1191981430689144834/Em5N9JbQ_400x400.jpg",
  #title = "ICRAF",
  argonSidebarHeader(title = "Main Menu"),
  argonSidebarMenu(
    argonSidebarItem(
      tabName = "cards",
      icon = argonIcon(name = "tv-2", color = "info"),
      "Home"
    ),
    argonSidebarItem(
      tabName = "effects",
      icon = argonIcon(name = "atom", color = "black"),
      "Summary Metadata"
    ),
    argonSidebarItem(
      tabName = "alerts",
      icon = argonIcon(name = "bullet-list-67", color = "danger"),
      "Profitability - Upload file"
    )
  ),
  argonSidebarDivider()
  #argonSidebarHeader(title = "Other Items")
)