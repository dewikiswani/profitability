###### database
library(FinCal)
library(rio)
library(dplyr)
library(stringr)

# diperlukan 3 dataset
## data1=data io (1 sd 30 th) (dalam bentuk unit) (30 kolom)
## data2=data capital (1 sd 30 th) (dalam bentuk Rp) (30 kolom)
## data3= data price (private dan social) (dalam bentuk Rp) (2 kolom)


setwd("C:/dw/ICRAF/profitability/data/data clean")
data1<-read.csv("data1.csv")
data2<-read.csv("data2.csv")
data3<-read.csv("data3.csv")

#manipulasi data1
data1 <- cbind(Status="General", data1)

#manipulasi data2
## asumsi data capital untuk private = social
pcap <- cbind(Status="Private Budget", data2)
scap <- cbind(Status="Social Budget", data2)

# manipulasi data3 = data price
p.price<-data3[-6]
n=30
p.year<-data.frame(replicate(n,p.price$Private.Price))
colnames(p.year)<-paste0(c(rep("Y", n)),1:n)
p.price<-cbind(Status="Private Price",p.price[c(1:4)],p.year)

s.price<-data3[-5]
n=30
s.year<-data.frame(replicate(n,s.price$Social.Price))
colnames(s.year)<-paste0(c(rep("Y", n)),1:n)
s.price<-cbind(Status="Social Price",s.price[c(1:4)],s.year)

## all data
#untuk sementara
#data.gab<-rbind(data1,p.price,s.price,p)
data.gab<-rbind(data1,p.price,s.price,pcap,scap)

#export to rdata
export(data.frame(data.gab), "1.rdata")

#import rdata 
load("1.rdata", gab <- new.env())
ls.str(gab)

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

hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)

nilai.tukar <- 10000
npv.p.us<-npv.p/nilai.tukar
npv.s.us<-npv.s/nilai.tukar
npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
hsl.npv<-rbind(hsl.npv,npv.us)
rownames(hsl.npv)<-c("NPV (IDR/Ha)", "NPV (US/Ha)")
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
rownames(nlc)<-c("Non Labor Cost (MRp/Ha)")
nlc



############# PERHITUNGAN ESTABLISHMENT COST
ec <- p.sum.cost[[1]]

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

############# PERHITUNGAN LABOR REQ FOR EST
lr <- sum.labor[[1]]

gab <- list(hsl.npv,nlc,hp,lr)
gab

