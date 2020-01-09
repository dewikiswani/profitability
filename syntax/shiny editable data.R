library(shiny)
library(DT)

callback <- c(
  "var tbl = $(table.table().node());",
  "var id = tbl.closest('.datatables').attr('id');",
  "table.on('autoFill', function(e, datatable, cells){",
  "  var out = [];",
  "  for(var i = 0; i < cells.length; ++i){",
  "    var cells_i = cells[i];",
  "    for(var j = 0; j < cells_i.length; ++j){",
  "      var c = cells_i[j];",
  "      var value = c.set === null ? '' : c.set;", # null => problem in R
  "      out.push({",
  "        row: c.index.row + 1,",
  "        col: c.index.column,",
  "        value: value",
  "      });",
  # to color the autofilled cells, uncomment the two lines below  
  #  "      $(table.cell(c.index.row, c.index.column).node())",
  #  "        .css('background-color', 'yellow');",
  "    }",
  "  }",
  "  Shiny.setInputValue(id + '_cells_filled:DT.cellInfo', out);",
  "  table.rows().invalidate();", # this updates the column type
  "});"
)

ui <- fluidPage(
  #br(),
  #DTOutput("dt1"),
  #br(),
  #DTOutput("dt2"),
  #br(),
  #DTOutput("dt3"),
  #br(),
  #verbatimTextOutput("hitung"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6,
               fileInput("file.1", "TABLE INPUT-OUTPUT"),
               fileInput("file.2", "TABLE CAPITAL"),
               fileInput("file.3", "TABLE PRICE"),
               actionButton("simulate","INPUT!",icon("paper-plane"),style="color: white; 
                         background-color: green;")
        ),
        column(6,
               sliderInput("rate.p", "Discount Rate Private", 7.8 ,min = 0, max = 15, step = 0.01),
               sliderInput("rate.s", "Discount Rate Social", 2.8 ,min = 0, max = 8, step = 0.01),
               sliderInput("labor.p", "Cost of Labor Private", 50000 ,min = 40000, max = 200000, step = 1000),
               sliderInput("labor.s", "Cost of Labor Social", 50000 ,min = 40000, max = 200000, step = 1000),
               sliderInput("nilai.tukar", "Rupiah Exchange Rate", 10000 ,min = 9000, max = 20000, step = 100)
        ),
        column(6,
               argonH1("Assumption", display = 4),
               #h4("Assumption & Summary"),
               #fluidRow(column(12, verbatimTextOutput("value"))),
               #fluidRow(column(12, verbatimTextOutput("value.2"))),
               br(),
               argonH1("Summary", display = 4),
               fluidRow(column(12, verbatimTextOutput("hitung")))
        )
        
        
      )
    ),
    mainPanel(
      fluidRow(
        column(6,
               DTOutput("dt1"),
               br(),
               DTOutput("dt2"),
               br(),
               DTOutput("dt3"),
               br(),
          
        )
        
        
        
      )
      
    )
    
  )
)

server <- function(input, output){
  
  load("1.rdata", dat <- new.env())
  dat <- dat$x
  dat1 <- filter(dat, Status == c("General"))
  dat1 <- select(dat1,-Status)
  dat2 <- dat %>% filter(str_detect(Status,"Private Budget"))
  dat2 <- select(dat2,-Status)
  dat3.p <- dat %>% filter(str_detect(Status,"Private Price"))
  dat3.p <- select(dat3.p,-Status)
  dat3.s <- dat %>% filter(str_detect(Status,"Social Price"))
  #dat3.s <- select(dat3.s,-Status)
  dat3 <- cbind(dat3.p[(1:5)],dat3.s[7])
  
  dat3 <- dat3 %>% 
    rename(
      "Private Price" = Y1,
      "Social Price" = Y2
    )
  
  #as.character ke variabel yg string
  # by specific columns:
  dat1<-dat1 %>% 
    mutate_at(vars(Grup,Var1,Var2,Unit), ~as.character(.))
  dat2<-dat2 %>% 
    mutate_at(vars(Grup,Var1,Var2,Unit), ~as.character(.))
  dat3<-dat3 %>% 
    mutate_at(vars(Grup,Var1,Var2,Unit), ~as.character(.))
  
  output[["dt1"]] <- renderDT({
    
    datatable(dat1, 
              editable = list(target = "cell"),
              selection = "none",
              extensions = "AutoFill",
              callback = JS(callback), 
              options = list(
                autoFill = TRUE
              )
    )
  }, server = TRUE)
  
  ed.dat1 <- reactive({
    info <- rbind(input[["dt1_cells_filled"]], input[["dt1_cell_edit"]])
    if(!is.null(info)){
      info <- unique(info)
      #info$value[info$value==""] <- NA
      dat1 <<- editData(dat1, info, proxy = "dt1")
    }
    dat1
    
  })
  
  
  ##### data 2
  output[["dt2"]] <- renderDT({
    
    datatable(dat2, 
              editable = list(target = "cell"),
              selection = "none",
              extensions = "AutoFill",
              callback = JS(callback), 
              options = list(
                autoFill = TRUE
              )
    )
  }, server = TRUE)
  
  ed.dat2 <- reactive({
    info <- rbind(input[["dt2_cells_filled"]], input[["dt2_cell_edit"]])
    if(!is.null(info)){
      info <- unique(info)
      #info$value[info$value==""] <- NA
      dat2 <<- editData(dat2, info, proxy = "dt2")
    }
    dat2
  })
  
  ##### data 3
  output[["dt3"]] <- renderDT({
    
    datatable(dat3, 
              editable = list(target = "cell"),
              selection = "none",
              extensions = "AutoFill",
              callback = JS(callback), 
              options = list(
                autoFill = TRUE
              )
    )
  }, server = TRUE)
  
  ed.dat3 <- reactive({
    info <- rbind(input[["dt3_cells_filled"]], input[["dt3_cell_edit"]])
    if(!is.null(info)){
      info <- unique(info)
      #info$value[info$value==""] <- NA
      dat3 <<- editData(dat3, info, proxy = "dt3")
    }
    dat3
  })
  
  output$hitung <- renderPrint({
    #browser()
    #manipulasi data1
    data1 <- cbind(Status="General", ed.dat1())
    
    #manipulasi data2
    ## asumsi data capital untuk private = social
    pcap <- cbind(Status="Private Budget", ed.dat2())
    scap <- cbind(Status="Social Budget", ed.dat2())
    
    # manipulasi data3 = data price
    p.price<-ed.dat3()[-6]
    n=30
    p.year<-data.frame(replicate(n,p.price$"Private Price"))
    colnames(p.year)<-paste0(c(rep("Y", n)),1:n)
    p.price<-cbind(Status="Private Price",p.price[c(1:4)],p.year)
    
    s.price<-ed.dat3()[-5]
    n=30
    s.year<-data.frame(replicate(n,s.price$"Social Price"))
    colnames(s.year)<-paste0(c(rep("Y", n)),1:n)
    s.price<-cbind(Status="Social Price",s.price[c(1:4)],s.year)
    
    gab<-rbind(data1,p.price,s.price,pcap,scap)
    
    #perkalian antara general dan Private Price
    a <- filter(gab, Status == c("General"))
    b <- filter(gab, Status == c("Private Price"))
    p.budget <- a[-(1:5)] * b[-c(1:5)]
    p.budget <- cbind(a[1:5],p.budget)
    p.budget <- p.budget %>% 
      mutate(Status = case_when(Status == "General" ~ "Private Budget", TRUE ~ as.character(Status)))
    
    #perkalian antara general dengan Social Price
    c <- filter(gab, Status == c("Social Price"))
    s.budget <- a[-(1:5)] * c[-c(1:5)]
    s.budget <- cbind(a[1:5],s.budget)
    s.budget <- s.budget %>% 
      mutate(Status = case_when(Status == "General" ~ "Social Budget", TRUE ~ as.character(Status)))
    
    #penggabungan dengan capital
    p.cap <- filter(gab, Status == c("Private Budget"))
    p.budget <- rbind(p.budget,p.cap)
    
    s.cap <- filter(gab, Status == c("Social Budget"))
    s.budget <- rbind(s.budget,s.cap)
    
    ################ penghitungan NPV
    p.cost.input <- p.budget %>% 
      filter(str_detect(Grup,"Input"))  
    s.cost.input <- s.budget %>% 
      filter(str_detect(Grup,"Input")) 
    
    p.sum.cost<- p.cost.input[,-(1:5)] %>% 
      colSums(na.rm = T)
    s.sum.cost<- s.cost.input[,-(1:5)] %>% 
      colSums(na.rm = T)
    
    p.rev.output <- p.budget %>% 
      filter(str_detect(Grup,"Output"))
    s.rev.output <- s.budget %>% 
      filter(str_detect(Grup,"Output"))
    
    p.sum.rev <- p.rev.output[,-(1:5)] %>% 
      colSums(na.rm = T)
    s.sum.rev <- s.rev.output[,-(1:5)] %>% 
      colSums(na.rm = T)           
    
    
    p.profit <- p.sum.rev - p.sum.cost        
    s.profit <- s.sum.rev - s.sum.cost 
    profit0 <- 0
    p.profit<-c(profit0,p.profit)
    s.profit<-c(profit0,s.profit)
    npv.p<-npv(7/100,p.profit)
    npv.s<-npv(2/100,s.profit)
    
    hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
    
    nilai.tukar <- 10000
    npv.p.us<-npv.p/nilai.tukar
    npv.s.us<-npv.s/nilai.tukar
    npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
    hsl.npv<-rbind(hsl.npv,npv.us)
    rownames(hsl.npv)<-c("(IDR/Ha)", "(US/Ha)")
    hsl.npv
    
    
    ############### PENGHITUNGAN NON-LABOR COST
    p.tot.cost<- sum(p.sum.cost)
    s.tot.cost<- sum(s.sum.cost)
    
    p.labor.input <- p.budget %>% filter(str_detect(Var1,"Tenaga Kerja"))
    s.labor.input <- s.budget %>% filter(str_detect(Var1,"Tenaga Kerja"))
    
    p.sum.labor <- p.labor.input[,-(1:5)] %>% 
      sum(na.rm = T)
    s.sum.labor <- s.labor.input[,-(1:5)] %>% 
      sum(na.rm = T)
    
    nlc.p <- (p.tot.cost - p.sum.labor)/1000000
    nlc.s <- (s.tot.cost - s.sum.labor)/1000000
    nlc<-data.frame(PRIVATE=nlc.p,SOCIAL=nlc.s)
    rownames(nlc)<-c("(MRp/Ha)")
    nlc
    
    ############# PERHITUNGAN ESTABLISHMENT COST
    ec <- p.sum.cost[[1]]
    
    ############# PERHITUNGAN HARVESTING PRODUCT
    e <- filter(gab, Status == c("General"))
    fil.prod <- e %>%  filter(str_detect(Grup,"Output"))
    sum.prod <- fil.prod[,-(1:5)] %>% 
      colSums(na.rm = T)
    tot.prod <- sum(sum.prod)
    
    fil.labor <- e %>%  filter(str_detect(Var1,"Tenaga Kerja"))
    sum.labor <- fil.labor[,-(1:5)] %>% 
      colSums(na.rm = T)
    tot.labor <- sum(sum.labor)
    
    hp <- tot.prod/tot.labor
    
    ############# PERHITUNGAN LABOR REQ FOR EST
    lr <- sum.labor[[1]]
    
    gab <- list(hsl.npv,nlc,ec,hp,lr)
    names(gab) <- c("NPV","NON LABOR COST", "ESTABLISHMENT COST","HARVESTING PRODUCT", "LABOR REQ for est.")
    gab
    
  })
  

  

  
}

shinyApp(ui, server)












