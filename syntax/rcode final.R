###### database
library(FinCal)
library(dplyr)
library(stringr)
library(tidyverse)

# diperlukan 3 dataset
## data1 = data io (1 sd 30 th) (dalam bentuk unit) (30 kolom)
## data2 = data capital (1 sd 30 th) (dalam bentuk Rp) (30 kolom)
## data3 = data price (private dan social) (dalam bentuk Rp) (2 kolom)

# Section1: Input Data ----------------------------------------------------

#set directory
setwd("C:/dw/ICRAF/profitability/data/data clean")

#data oilpalm
io.in <-read.csv("io.in.csv",stringsAsFactors = F)
io.out <- read.csv("io.out.csv",stringsAsFactors = F)
price.in <- read.csv("price.in.csv",stringsAsFactors = F)
price.out <- read.csv("price.out.csv",stringsAsFactors = F)

#data kopi
#setwd("C:/dw/ICRAF/profitability/data/lusita 2.0/data clean")
#io.in <-read.csv("kopi_io_in.csv",stringsAsFactors = F)
#io.out <- read.csv("kopi_io_out.csv",stringsAsFactors = F)
#price.in <- read.csv("kopi_price_in.csv",stringsAsFactors = F)
#price.out <- read.csv("kopi_price_out.csv",stringsAsFactors = F)


#data capital dibuat kondisi if else krn tdk semua ada data capitalnya 
pcap.condition <- function() {
  ANSWER <- readline("Apakah terdapat Modal Kapital Private? [y/n] :")
  if (substr(ANSWER, 1, 1) == "n")
    pcap.con <- cat("Tidak Ada Modal Kapital Private \n")
  else if (substr(ANSWER, 1, 1) == "y")
    pcap.con <- cat("Ada Modal Kapital Private \n")
  else
    print("ERROR: Kondisi Modal Kapital harus ditentukan")
  return(ANSWER)
  
}
pcap.condition <- pcap.condition()
y
#kopi
#n

#library jawaban untuk pcap.condition
## pcap.condition == n : Tidak Ada Modal Kapital Private
## pcap.condition == y : Ada Modal Kapital Private

scap.condition <- function() {
  ANSWER <- readline("Apakah Terdapat Modal Kapital Sosial? [y/n] : ")
  if (substr(ANSWER, 1, 1) == "n")
    scap.con <- cat("Tidak Ada Modal Kapital Sosial \n")
  else if (substr(ANSWER, 1, 1) == "y") 
    scap.con <- cat("Ada Modal Modal Kapital Sosial \n")
  else
    print("ERROR: Kondisi Modal Kapital Sosial harus ditentukan")
  return(ANSWER)
}

######## ISI DULU KONDISI DI ATAS 
if(pcap.condition=="n"){
  pcap <- NULL
  scap <- NULL
}else if (pcap.condition=="y"){
  print("Silahkan Upload Tabel Modal Kapital Private!")
  pcap<- read.csv("capital.p.csv",stringsAsFactors = F)
  scap.condition <- scap.condition()
}else{
  print("ERROR: Kondisi Modal Kapital harus ditentukan") #case utk capital yg tidak ada isinya belum dibuat
}
y
#library jawaban untuk scap.condition
## scap.condition == n : Tidak Ada Modal Kapital Sosial
## scap.condition == y : Ada Modal Kapital Sosial

scap.pcap.con <- function() {
  ANSWER <- readline("Modal Kapital Sosial sama dengan Modal Kapital Private? [y/n] : ")
  if (substr(ANSWER, 1, 1) == "n")
    scap.pcap.con <- cat("Modal Kapital Sosial != Modal Kapital Private \n")
  else if (substr(ANSWER, 1, 1) == "y") 
    scap.pcap.con <- cat("Modal Kapital Sosial = Modal Kapital Private \n")
  else
    print("ERROR: Kondisi Modal Kapital Sosial harus ditentukan")
  return(ANSWER)
}

## ISI DAHULU KONDISI DI ATAS
if(scap.condition=="n"){
  scap <- NULL
  scap.pcap.con <- NULL
}else if (scap.condition=="y"){
  scap.pcap.con <- scap.pcap.con()
}else{
  print("ERROR: Kondisi Modal Kapital Sosial Harus Ditentukan")
}
y
#library jawaban untuk scap.condition
## scap.pcap.con == n : Modal Kapital Sosial != Modal Kapital Private
## scap.pcap.con == y : Modal Kapital Sosial = Modal Kapital Private


######## ISI DULU KONDISI DI ATAS 
if(scap.pcap.con=="n"){
  print("Silahkan Upload Tabel Modal Kapital Sosial!")
  scap<- read.csv("capital.s.csv",stringsAsFactors = F)
}else if (scap.pcap.con=="y"){
  scap <- pcap
}else{
  print("ERROR: Kondisi Modal Kapital Sosial Harus Ditentukan") #case utk capital yg tidak ada isinya belum dibuat
}


#variabel input
#user wajib isi untuk penamaan database 
## tabel input empty
#db.empty <- data.frame(user=character(),prov=character(),kab=character(),kec=character(),desa=character(),
 #                      sut=character(),kom=character(), tahun=integer(),
  #                     n=integer(),nilai.tukar=double(),
   #                    tabel.gab=character(),tabel.summary=character(),
    #                   rate.p=double(),rate.s=double()
#)

#peneliti <- read.csv("C:/dw/ICRAF/profitability/data/lusita 2.0/peneliti.csv",stringsAsFactors = F)
v.user <- "dewi"
v.prov <- "Jambi"
v.kab <- "Tanjung Jabung Timur"
v.kec <- "Kuala Jambi"
v.desa <- "Teluk Majelis"
v.sut <- "mono"
v.kom <- "oil palm"
v.tahun <- 2016
v.nilai.tukar <- 10000
v.rate.p <- 7

tabel.gab <- paste(v.user,"gab",sep = "_")
tabel.summary <- paste(v.user,"summary",sep = "_")

db <- data.frame(user=v.user,prov=v.prov,kab=v.kab,kec=v.kec,desa=v.desa,
                 sut=v.sut,kom=v.kom, tahun=v.tahun,
                 nilai.tukar=v.nilai.tukar,
                 tabel.gab=tabel.gab,
                 tabel.summary=tabel.summary,
                 rate.p=v.rate.p
)

db$rate.s <- db$rate.p - 5 #diskon rate social
#db.gab <- rbind(db.empty,db.simulate)


n=30 #jumlah tahun
profit0 <- 0 


# Section2: Data Pre-Processing -------------------------------------------

#manipulasi data1 = data oi
## convert to lowercase
colnames(io.in) <- tolower(colnames(io.in))
colnames(io.out) <- tolower(colnames(io.out))

io.in[is.na(io.in)] <- 0 #NA replace with zero
io.out[is.na(io.out)] <- 0
data1 <- rbind(io.in,io.out) #combine all data input-output
data1 <- cbind(status="general", data1) #add variable status
data1 <- data1 %>% mutate_if(is.factor,as.character) #change factor var to char var

lowcase <- function(data, index.col){
  for(k in index.col){
  data[,k] <- tolower(data[,k])
  }
  return(data) 
}
data1 <- lowcase(data1,c(1:5))

#manipulasi data2 = data capital
#kalo null g bs pake ini
colnames(pcap) <- tolower(colnames(pcap))
colnames(scap) <- tolower(colnames(scap))

pcap <- lowcase(pcap,c(1:4))
scap <- lowcase(scap,c(1,2,3,4))


if(pcap.condition=="n"){
  pcap <- NULL
  scap <- NULL
}else if (pcap.condition=="y" & scap.condition=="n"){
  pcap[is.na(pcap)] <- 0 #NA replace with zero
  pcap <- cbind(status="private budget", pcap)
  pcap <- pcap %>% mutate_if(is.factor,as.character)
  
  scap <- NULL
}else if (pcap.condition=="y" & scap.condition=="y"){
  pcap[is.na(pcap)] <- 0 #NA replace with zero
  pcap <- cbind(status="private budget", pcap)
  pcap <- pcap %>% mutate_if(is.factor,as.character)
  
  scap[is.na(scap)] <- 0 #NA replace with zero
  scap <- cbind(status="social budget", scap)
  scap <- scap %>% mutate_if(is.factor,as.character)
}


# manipulasi data3 = data price
price.in[is.na(price.in)] <- 0
price.out[is.na(price.out)] <- 0
data3 <- rbind(price.in, price.out)

colnames(data3) <- tolower(colnames(data3))
data3 <- lowcase(data3,c(1:4))
p.price<-data3[-6]
colnames(p.price)[5] <- c("private.price")
p.year<-data.frame(replicate(n,p.price$private.price)) #replicate nilai private price sebanyak n tahun
colnames(p.year)<-paste0(c(rep("y", n)),1:n)
p.price<-cbind(status="private price" ,p.price[c(1:4)],p.year)
p.price <- p.price %>% mutate_if(is.factor,as.character) #change factor var to char var

s.price<-data3[-5]
colnames(s.price)[5] <- c("social.price")
s.year<-data.frame(replicate(n,s.price$social.price))
colnames(s.year)<-paste0(c(rep("y", n)),1:n)
s.price<-cbind(status="social price",s.price[c(1:4)],s.year)
s.price <- s.price %>% mutate_if(is.factor,as.character) #change factor var to char 

## all data
data.gab<-rbind(data1,p.price,s.price,pcap,scap)


# Section 3: Data processing ----------------------------------------------

#perkalian antara general dan Private Price
a <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
b <- filter(data.gab,status == c("private price")) #filter data private price
p.budget <- a[-(1:5)] * b[-c(1:5)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5
p.budget <- cbind(a[1:5],p.budget) #memunculkan kembali variabel 1 sd 5 
p.budget <- p.budget %>% 
  mutate(status = case_when(status == "general" ~ "private budget")) #mengubah status yg General mjd Private Budget

#perkalian antara general dengan Social Price
c <- filter(data.gab, status == c("social price")) #filter data social price
s.budget <- a[-(1:5)] * c[-c(1:5)]
s.budget <- cbind(a[1:5],s.budget)
s.budget <- s.budget %>% 
  mutate(status = case_when(status == "general" ~ "social budget"))

#penggabungan dengan data capital
p.cap <- filter(data.gab, status == c("private budget"))
p.budget <- rbind(p.budget,p.cap)

s.cap <- filter(data.gab, status == c("social budget"))
s.budget <- rbind(s.budget,s.cap)

################ penghitungan NPV
p.cost.input <- p.budget %>% 
  filter(str_detect(grup,"input"))

s.cost.input <- s.budget %>% 
  filter(str_detect(grup,"input")) 

p.sum.cost<- p.cost.input[,-(1:5)] %>% 
  colSums(na.rm = T)
s.sum.cost<- s.cost.input[,-(1:5)] %>% 
  colSums(na.rm = T)

p.rev.output <- p.budget %>% 
  filter(str_detect(grup,"output"))
s.rev.output <- s.budget %>% 
  filter(str_detect(grup,"output"))

p.sum.rev <- p.rev.output[,-(1:5)] %>% 
  colSums(na.rm = T)
s.sum.rev <- s.rev.output[,-(1:5)] %>% 
  colSums(na.rm = T)           


p.profit <- p.sum.rev - p.sum.cost        
s.profit <- s.sum.rev - s.sum.cost 
#profit0 <- 0
p.profit<-c(profit0,p.profit)
s.profit<-c(profit0,s.profit)

npv.p<-npv(db$rate.p/100,p.profit)
npv.s<-npv(db$rate.s/100,s.profit)

hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)

npv.p.us<-npv.p/db$nilai.tukar
npv.s.us<-npv.s/db$nilai.tukar
npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
hsl.npv<-rbind(hsl.npv,npv.us)
rownames(hsl.npv)<-c("NPV (IDR/Ha)", "NPV (US/Ha)")
hsl.npv

############### PENGHITUNGAN NON-LABOR COST
p.tot.cost<- sum(p.sum.cost)
s.tot.cost<- sum(s.sum.cost)

p.budget$var1 <- tolower(p.budget$var1)
p.labor.input <- p.budget %>% filter(str_detect(var1,c("tenaga","tk","kerja")))
s.budget$var1 <- tolower(s.budget$var1)
s.labor.input <- s.budget %>% filter(str_detect(var1,c("tenaga","tk","kerja")))

p.sum.labor <- p.labor.input[,-(1:5)] %>% 
  sum(na.rm = T)
s.sum.labor <- s.labor.input[,-(1:5)] %>% 
  sum(na.rm = T)

nlc.p <- (p.tot.cost - p.sum.labor)/1000000
nlc.s <- (s.tot.cost - s.sum.labor)/1000000
nlc<-data.frame(PRIVATE=nlc.p,SOCIAL=nlc.s)
rownames(nlc)<-c("Non Labor Cost (MRp/Ha)")
nlc


############# PERHITUNGAN ESTABLISHMENT COST
p.ec <- p.sum.cost[[1]]
s.ec <- s.sum.cost[[1]]
ec <- data.frame(p.ec,s.ec)
ec<-data.frame(PRIVATE=p.ec,SOCIAL=s.ec)
rownames(ec)<-c("Establishment cost (1st year only, MRp/ha)")
ec
############# PERHITUNGAN HARVESTING PRODUCT
e <- a
fil.prod <- e %>%  filter(str_detect(grup,"output")) #filter io untuk grup output (hasil panen)
sum.prod <- fil.prod[,-(1:5)] %>% 
  colSums(na.rm = T)
tot.prod <- sum(sum.prod)

fil.labor <- e %>%  filter(str_detect(var1, c("tenaga","tk","kerja"))) 
sum.labor <- fil.labor[,-(1:5)] %>% 
  colSums(na.rm = T)
tot.labor <- sum(sum.labor)

hp <- data.frame(tot.prod/tot.labor)
colnames(hp)<-c("Labor Req for Est (1st year only)")
hp

hp
hp.tib <- as.tibble(hp)

############# PERHITUNGAN LABOR REQ FOR EST
lr <- sum.labor[[1]] #pekerja pada tahun 1

#### buat dataframe summary
summ.npv <- data.frame(hsl.npv[1,1],hsl.npv[1,2],hsl.npv[2,1],hsl.npv[2,2])
summ.nlc <- data.frame(nlc[1,1],nlc[1,2])
data.summary <- data.frame(summ.npv,
                           summ.nlc,
                           ec,hp,lr)
colnames(data.summary) <- c("NPV (Rp/Ha) Privat","NPV (Rp/Ha) Sosial",
                            "NPV (US/Ha) Privat","NPV (US/Ha) Sosial",
                            "Non Labor Cost (Juta Rp/Ha) Privat","Non Labor Cost (Juta Rp/Ha) Sosial",
                            "Establishment Cost",
                            "Harvesting Product",
                            "Labor Req for Est")


# Section 4: Save to database ---------------------------------------------

space_remove <- function(input){
  input <- gsub(" ", "", input)
  return(input)
}
db$user <- tolower(space_remove(db$user))
db$kab <- tolower(space_remove(db$kab))
db$kec <- tolower(space_remove(db$kec))
db$desa <- tolower(space_remove(db$desa))
db$sut <- tolower(space_remove(db$sut))
db$kom <- tolower(space_remove(db$kom))
db$prov <- tolower(space_remove(db$prov))

hist.data <- function(v.user,v.prov,v.kab,v.kec,v.desa,v.sut,v.kom,v.tahun,v.nilai.tukar,v.rate.p){
  db <- NULL 
  db.masterfix <- NULL
  tabel.gab <- NULL #setiap user create suatu tabel dan di namain sesuai nama usernya
  tabel.summary <- NULL #setiap user create suatu tabel hasil kesimpulan analisis dan di namain sesuai nama usernya
  
  if (!is.null(v.user)) {
    tabel.gab <- paste(v.user,"gab",sep = "_")
    tabel.summary <- paste(v.user,"summary",sep = "_")
  }
  
  db <- as.tibble(cbind(user=v.user,
                        prov=v.prov,
                        kab=v.kab,
                        kec=v.kec,
                        desa=v.desa,
                        sut=v.sut,
                        kom=v.kom, 
                        tahun=as.integer(v.tahun),
                        nilai.tukar=as.double(v.nilai.tukar),
                        tabel.gab=as.character(tabel.gab),
                        tabel.summary=as.character(tabel.summary),
                        rate.p=as.double(v.rate.p)))
  db$tahun <- as.integer(db$tahun)
  db$nilai.tukar <- as.double(db$nilai.tukar)
  db$rate.p <- as.double(db$rate.p)
  
  if (!is.null(v.user)){
    db$rate.s <- db$rate.p - 5 #diskon rate social
  }
  
  if (nrow(db)==0) {
    db.masterfix <- NULL
    db.masterfix = rbind(db.masterfix,db)
  }else(db.masterfix = rbind(db.masterfix,db))
  
  db.masterfixbgt <- db.masterfix %>% 
    mutate(mytime = Sys.time()) %>% 
    
    print(db)
  return(db.masterfixbgt)
}

data.id <- as.tibble(NULL)

data.id <- data.id%>% 
  bind_rows(hist.data(db$user,db$prov,db$kab,db$kec,db$desa,db$sut,
                      db$kom,db$tahun,db$nilai.tukar,db$rate.p)) %>% 
  dplyr::select(-tabel.gab,-tabel.summary) %>% 
  mutate(no = row_number(),
         tabel.gab = paste(user,"gab",no,sep="_"),
         tabel.summary = paste(user,"summary",no,sep="_")) %>% 
  dplyr::select(-no)





#mytime <- format(Sys.time(), "%b_%d_%H_%M_%S_%Y")
myfile <- file.path("C:/dw/ICRAF/profitability/data/data clean", 
                    paste0(db$sut, "_",db$kom,"_",db$prov,"_",db$kab,"_",
                           db$kec,"_",db$desa,"_",
                           db$tahun, ".Rdata"))

### ganti nama dataframe
nama.gab <- top_n(data.id[13],n=1) %>% 
  select(tabel.gab) %>% 
  filter(str_detect(tabel.gab,paste(db$user,"gab",sep="_"))) %>% 
  as.character()

assign(nama.gab,data.gab)

nama.summ <- top_n(data.id[14],n=1) %>% 
  select(tabel.summary) %>% 
  filter(str_detect(tabel.summary,paste(db$user,"sum",sep="_"))) %>% 
  as.character()
assign(nama.summ,data.gab)


#save(db,data.id,
 #    n,profit0,data.gab,nama.gab,
 #    data.summary,nama.summ, 
 #     file = myfile)

#save seluruh objek yg ada 
save.image( file = myfile)

#pcap.condition, scap.condition, scap.pcap.con,

# Section 5: Load database ------------------------------------------------

#import data setelah di clear environment
load("mono_oilpalm_jambi_tanjungjabungtimur_kualajambi_telukmajelis_2016.rdata")


# Section 6 : report ------------------------------------------------------


# https://davidgohel.github.io/officer/articles/offcran/word.html#table-and-image-captions
library(magrittr)
library(officer)
library(ggplot2)

#user, kab, kec, desa,

input <- t(data.frame(data.id))
nama.input <- c(
                "Nama Peneliti",
                "Provinsi",
                "Kabupaten",
                "Kecamatan",
                "Desa",
                "Sistem Usaha Tani",
                "Komoditas",
                "Tahun",
                "Nilai Tukar Rupiah",
                "Discount Rate Private",
                "Discount Rate Social",
                "Waktu Input",
                "Nama Tabel Gabungan",
                "Nama Tabel Summary")

input <- cbind(nama.input,input[,1])
input <- as.data.frame(input)
colnames(input) <- c("Variabel Input","Nilai")

doc <- read_docx() %>% 
  body_add_par(value = "Table of content", style = "centered") %>% 
  body_add_toc(level = 2) %>% 
  
  body_add_par(value = "Input", style = "heading 1") %>% 
  body_add_table(value = input, style = "table_template" ) %>% 
  
  body_add_par(value = "Summary", style = "heading 1") %>% 
  
  body_add_par(value = "NPV", style = "heading 2") %>% 
  body_add_table(value = hsl.npv, style = "table_template" ) %>% 
  body_add_par(value = "NPV", style = "table title") %>% 
  shortcuts$slip_in_tableref(depth = 2) %>%
  
  body_add_par(value = "Non Labor Cost (Mrp/Ha)", style = "heading 2") %>% 
  body_add_table(value = nlc, style = "table_template" ) %>% 
  body_add_par(value = "Non Labor Cost (Mrp/Ha)", style = "table title") %>% 
  shortcuts$slip_in_tableref(depth = 2) %>%
  
  body_add_par(value = "Establishment Cost (1st year only, Mrp/Ha)", style = "heading 2") %>% 
  body_add_par(value = ec, style = "centered") %>% 
  
  body_add_par(value = "Harvesting Product (TOn/HOK)", style = "heading 2") %>% 
  body_add_par(value = hp, style = "centered") %>% 
  
  body_add_par(value = "Labor Req for Est (1st year only, HOK/Ha", style = "heading 2") %>% 
  body_add_par(value = lr, style = "centered") 


myfile <- file.path("C:/dw/ICRAF/profitability/data/data clean", 
                    paste0(db$sut, "_",db$kom,"_",db$prov,"_",db$tahun, ".docx")) 

print(doc, target = myfile) 


#plus: bisa replace



# Section 7: Simulation ---------------------------------------------------

# AE COMMENT 21: di bagian ini Dewi bisa mulai mencoba untuk membangun modul scenario. Ini bisa kita diskusikan lebih lanjut nanti.


