###### database
library(FinCal)
library(rio)
library(dplyr)
library(stringr)
#AE COMMENT1: saran saya mungkin package 'rio' tidak diperlukan sekiranya hanya
#akan digunakan untuk menyimpan dan memanggilRData saja. Saran lain adalah
#mungkin Dewi bisa explore package 'rtf' untuk membuat report secara otomatis
#dari R


# diperlukan 3 dataset
## data1=data io (1 sd 30 th) (dalam bentuk unit) (30 kolom)
## data2=data capital (1 sd 30 th) (dalam bentuk Rp) (30 kolom)
## data3= data price (private dan social) (dalam bentuk Rp) (2 kolom)

#AE COMMENT2: saran saya sebaiknya dipisahkan saja data input dan data output menjadi dua input yang berbeda
#AE COMMENT3: apakah data capital yang saat ini dimasukkan sebagai data2 tidak termasuk dalam kategori input seperti data1?
#AE COMMENT4: informasi lain yang digunakan dalam perhitungan dibawah, seperti
#daur tanam dan suku bunga, sebaiknya dimasukkan diawal sebagai input
#AE COMMENT5: baisakan menggunakan section di dalam menulis code yha Dewi, supaya lebih mudah dilihat dan terstruktur

# Section1: Input Data ----------------------------------------------------

setwd("D:/1_Profitability/data")
data1<-read.csv("data1.csv")
data2<-read.csv("data2.csv")
data3<-read.csv("data3.csv")


# Section2: Data Pre-Processing -------------------------------------------

#manipulasi data1
data1 <- cbind(Status="General", data1)
#AE COMMENT6: code pada line 30 diatas akan membuat kolom dengan class "factor". Apakah benar seperti itu yang diinginkan?

#manipulasi data2
## asumsi data capital untuk private = social

## AE COMMENT 7: Asumsi pada line 34 diatas, perlu dimasukkan sebagai input di
## Section1, dengan demikian, user akan diberi pilihan apakah mereka ingin
## memasukkan Private Capital yang sama persis dengan Social Capital.
## Berdasarkan input tersebut, line coding dibawah akan dijalankan.

pcap <- cbind(Status="Private Budget", data2)
scap <- cbind(Status="Social Budget", data2)

# manipulasi data3 = data price
p.price<-data3[-6]
n=30
# AE COMMENT 8: jumlah tahun sebaiknya dimasukkan sebagai input di dalam Section1
p.year<-data.frame(replicate(n,p.price$Private.Price))
colnames(p.year)<-paste0(c(rep("Y", n)),1:n)
p.price<-cbind(Status="Private Price",p.price[c(1:4)],p.year)

s.price<-data3[-5]
n=30
s.year<-data.frame(replicate(n,s.price$Social.Price))
colnames(s.year)<-paste0(c(rep("Y", n)),1:n)
s.price<-cbind(Status="Social Price",s.price[c(1:4)],s.year)

# AE COMMENT 9: Really nice work on those lines above !!! Well done.


# Section 3: Save to database ---------------------------------------------

## all data
#untuk sementara
#data.gab<-rbind(data1,p.price,s.price,p)
data.gab<-rbind(data1,p.price,s.price,pcap,scap)

# AE COMMENT 10: Pada line diatas, sebaiknya juga ditambahkan kolom-kolom yang
# memuat informasi tentang komoditas yang sedang dianalisa, saran saya,
# tambahkan kolom: nama komoditas, sistem usaha tani, lokasi-provinsi,
# lokasi-kabupaten, lokasi-desa dan nama surveyor

#export to rdata
export(data.frame(data.gab), "1.rdata")

# AE COMMENT 11: Selain Rdata, Dewi juga punya pilihan untuk menggunakan fungsi
# saveRDS disini. Sebaiknya Dewi juga sudah mulai mengusulkan sistem penamaan
# yang sistematis, mungkin informasi yang saya usulkan di comment 10 bisa dijadikan
# bahan pemikiran

# Section 4: Load database ------------------------------------------------

#import rdata 
load("1.rdata", gab <- new.env())
ls.str(gab)

# AE COMMENT 12: pada bagian ini, dewi juga mungkin sudah bisa memikirkan
# bagaimana cara pemanggilan yang baik pada saat Rdata yang tersimpan sudah
# banyak, mungkin perlu ditambahkan input seperti yang ada pada comment 10 untuk
# menentukan file mana yang akan dipanggil.


# Section 5: Data processing ----------------------------------------------

# AE COMMENT 13: Dewi, walaupun saya letakkan section divider diatas, sebenarnya
# section 5 ini lebih pas kalau diletakkan sebelum section 4. Sehingga Rdata
# yang dihasilkan, selian menyimpan data mentah, juga menyimpan hasil
# perhitungan NPV dll

#perkalian antara general dan Private Price
a <- filter(gab$x, Status == c("General"))
b <- filter(gab$x, Status == c("Private Price"))
p.budget <- a[-(1:5)] * b[-c(1:5)]
p.budget <- cbind(a[1:5],p.budget)
p.budget <- p.budget %>% 
  mutate(Status = case_when(Status == "General" ~ "Private Budget", TRUE ~ as.character(Status)))

#perkalian antara general dengan Social Price
c <- filter(gab$x, Status == c("Social Price"))
s.budget <- a[-(1:5)] * c[-c(1:5)]
s.budget <- cbind(a[1:5],s.budget)
s.budget <- s.budget %>% 
  mutate(Status = case_when(Status == "General" ~ "Social Budget", TRUE ~ as.character(Status)))

#AE COMMENT 14: Nice works for the lines above!!

#penggabungan dengan capital
p.cap <- filter(gab$x, Status == c("Private Budget"))
p.budget <- rbind(p.budget,p.cap)

s.cap <- filter(gab$x, Status == c("Social Budget"))
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

# AE COMMENT 15: suku bunga 7% dan 2% diatas, perlu dimasukkan sebagai data
# input pada section1, mohon diingat bahwa suku bunga bisa bervariasi dalam
# periode 30 tahun

hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)

nilai.tukar <- 10000
# AE COMMENT 16: nilai tukar usd=10000 idr, perlu dimasukkan sebagai data input
# pada section1, mohon diingat bahwa nilai tukar bisa bervariasi dalam 30 tahun.
# Usul saya, perhitungan ini sebaiknya diintegrasikan kedalam perhitungan cost
# dan benefit yang ada pada line 150-154

npv.p.us<-npv.p/nilai.tukar
npv.s.us<-npv.s/nilai.tukar
npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
hsl.npv<-rbind(hsl.npv,npv.us)
rownames(hsl.npv)<-c("NPV (IDR/Ha)", "NPV (US/Ha)")
hsl.npv

# AE COMMENT 17: object hsl.npv perlu disimpan dalam Rdata, bersama dengan informasi kunci yang ada pada comment 10

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
rownames(nlc)<-c("Non Labor Cost (MRp/Ha)")
nlc



############# PERHITUNGAN ESTABLISHMENT COST
ec <- p.sum.cost[[1]]

# AE COMMENT 18: object ec perlu disimpan dalam Rdata, bersama dengan informasi kunci yang ada pada comment 10


############# PERHITUNGAN HARVESTING PRODUCT
e <- filter(gab$x, Status == c("General"))
fil.prod <- e %>%  filter(str_detect(Grup,"Output"))
sum.prod <- fil.prod[,-(1:5)] %>% 
  colSums(na.rm = T)
tot.prod <- sum(sum.prod)

fil.labor <- e %>%  filter(str_detect(Var1,"Tenaga Kerja"))
sum.labor <- fil.labor[,-(1:5)] %>% 
  colSums(na.rm = T)
tot.labor <- sum(sum.labor)

hp <- tot.prod/tot.labor

# AE COMMENT 19: object tot.labor dan hp perlu disimpan dalam Rdata, bersama dengan informasi kunci yang ada pada comment 10


############# PERHITUNGAN LABOR REQ FOR EST
lr <- sum.labor[[1]]

gab <- list(hsl.npv,nlc,hp,lr)
gab


# Section 6 : report ------------------------------------------------------

# AE COMMENT 20: di bagian ini Dewi bisa mulai mencoba untuk membuat report dari
# hasil-hasil diatas, packae rtf atau officeR bisa jadi pilihan untuk ini


# Section 7: Simulation ---------------------------------------------------

# AE COMMENT 21: di bagian ini Dewi bisa mulai mencoba untuk membangun modul scenario. Ini bisa kita diskusikan lebih lanjut nanti.



