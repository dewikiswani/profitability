
#### nama rowname
allData<-data.frame(a=rnorm(10),b=rexp(10))
new.names<-NULL
for(i in 1:10){
  new.names[i]<-paste(letters[i],i,sep="")
}

row.names(allData)<-new.names

### Dynamically Name R Data Frame in a For Loop
trial=NULL
Trackfile=1:2
for (i in Trackfile){
  d.cor <- .10 # Desired correlation
  Dataset <- as.data.frame(mvrnorm(20, mu = c(0,0), 
                                   Sigma = matrix(c(1,d.cor,d.cor,1), ncol = 2), 
                                   empirical = TRUE))
  trial = rbind(trial, data.frame(Dataset$V1, Dataset$V2))
}
print(trial)
print(Dataset)


### buat data master
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

db <- NULL 
tabel.gab <- NULL #setiap user create suatu tabel dan di namain sesuai nama usernya
tabel.summary <- NULL #setiap user create suatu tabel hasil kesimpulan analisis dan di namain sesuai nama usernya

track <- 1:3
i=1
for(i in track){
#while(i>0){
  tabel.gab[i] <- paste(v.user,"gab",i,sep = "_")
  tabel.summary [i]<- paste(v.user,"summary",i,sep = "_")
  db <- data.frame(user=v.user,prov=v.prov,kab=v.kab,kec=v.kec,desa=v.desa,
                 sut=v.sut,kom=v.kom, tahun=v.tahun,
                 nilai.tukar=v.nilai.tukar,
                 tabel.gab=tabel.gab[i],
                 tabel.summary=tabel.summary[i],
                 rate.p=v.rate.p
                  )
  db$rate.s <- db$rate.p - 5 #diskon rate social
}




if(is.null(db)){
  #for (i in track){
    db.master <- data.frame(user=character(),prov=character(),kab=character(),kec=character(),desa=character(),
                                              sut=character(),kom=character(), tahun=integer(),
                                             n=integer(),nilai.tukar=double(),
                                            tabel.gab=character(),tabel.summary=character(),
                                          rate.p=double(),rate.s=double()
                        )
    db.master = rbind(db, db.master)
  #}
}else(db.master = rbind(db, db.master))

print(db)
print(db.master)
