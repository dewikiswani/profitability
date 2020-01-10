library(shiny)
library(DT)
library(rio)


ui <- fluidPage(
  titlePanel("Land Use Profitability"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(4,
               fileInput("file.1", "Table Input-Output"),
               fileInput("file.2", "Table Capital"),
               fileInput("file.3", "Table Price"),
               actionButton("simulate","Simulate!")
        ),
        column(5,
               sliderInput("rate.p", "Discount Rate Private", 7.8 ,min = 0, max = 15, step = 0.01),
               sliderInput("rate.s", "Discount Rate Social", 2.8 ,min = 0, max = 8, step = 0.01),
               sliderInput("labor.p", "Cost of Labor Private", 50000 ,min = 40000, max = 200000, step = 1000),
               sliderInput("labor.s", "Cost of Labor Social", 50000 ,min = 40000, max = 200000, step = 1000),
               sliderInput("n.tukar", "Rupiah Exchange Rate", 10000 ,min = 9000, max = 20000, step = 100),
               #sliderInput("area", "Planting Area", 3000 ,min = 0, max = 10000, step = 100)
        )
        )),
    mainPanel(
      tabsetPanel(
        tabPanel("Table Price", dataTableOutput("tabel.1"))
      )
      )))

server <- function(input, output, session) {
  
  #### tabel 1
  data.io<-eventReactive(input$simulate,{
    #browser()
    inFile <- input$file.1
    #if (is.null(inFile)) return(NULL)
    if (is.null(inFile)){
     stop("Table must be upload") 
    }else{
    read.csv(inFile$datapath)#ganti read.csv jika delimiter excelnya ,
    }
  })
  
  data.cap <- eventReactive(input$simulate,{
    inFile <- input$file.2
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath)
  })
  
  
  data.price <- eventReactive(input$simulate,{
    #browser()
    inFile <- input$file.3
    if (is.null(inFile)){
      stop("Table must be upload") 
    }else{
      read.csv(inFile$datapath)#ganti read.csv jika delimiter excelnya ,
    }
  })
  
  
  gab.export<-eventReactive(input$simulate,{
    #browser()
    #input data
    #### ASUMSI : 3 data terinput semua 
    data.1=data.io()
    data.2=data.cap()
    data.3=data.price()
    
    
    #manipulasi data.1
    data.1 <- cbind(Status="General", data.1)
    
    #manipulasi data2
    ## asumsi data capital untuk private = social
    cap.1 <- cbind(Status="Private Budget", data.2)
    cap.2 <- cbind(Status="Social Budget", data.2)
    
    # manipulasi data3 = data price
    p.price<-data.3[-6]
    n=30
    p.year<-data.frame(replicate(n,p.price$Private.Price))
    colnames(p.year)<-paste0(c(rep("Y", n)),1:n)
    p.price<-cbind(Status="Private Price",p.price[c(1:4)],p.year)
    
    s.price<-data.3[-5]
    n=30
    s.year<-data.frame(replicate(n,s.price$Social.Price))
    colnames(s.year)<-paste0(c(rep("Y", n)),1:n)
    s.price<-cbind(Status="Social Price",s.price[c(1:4)],s.year)
    
    #data.gab<-rbind(data1,p.price,s.price,p)
    data.gab<-rbind(data.1,p.price,s.price,cap.1,cap.2)
    
    #export to rdata
    export(data.frame(data.gab), "data/simulasi export/baru.rdata")
    return(data.gab)
    #df = cbind(data.1,data.2,data.3)
    #check_df<-apply(apply(df,2,is.na),2,all)
    #df2=df[,!check_df]
    #return(df2)
    
  })
  
  
  
  output$tabel.1 <- renderDataTable({
    #browser()
    df<-gab.export()
    df
  }) 
  
  
  
  
  
}


shinyApp(ui, server)


