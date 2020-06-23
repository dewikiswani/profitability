### buat data master
v.user <- NULL
v.prov <- NULL
v.kab <- NULL
v.kec <- NULL
v.desa <- NULL
v.sut <- NULL
v.kom <- NULL
v.tahun <- NULL
v.nilai.tukar <- NULL
v.rate.p <- NULL
hasil1 <- as.tibble(NULL)

#db.masterfix <- NULL


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
    mutate(input = Sys.time()) %>% 
  
  print(db)
  return(db.masterfixbgt)
}

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

hasil1 <- hasil1 %>% 
  bind_rows(hist.data(v.user,v.prov,v.kab,v.kec,v.desa,v.sut,v.kom,v.tahun,v.nilai.tukar,v.rate.p)) %>% 
  dplyr::select(-tabel.gab,-tabel.summary) %>% 
  mutate(no = row_number(),
         tabel.gab = paste(user,"gab",no,sep="_"),
         tabel.summary = paste(user,"summary",no,sep="_")) %>% 
  dplyr::select(-no)

### ganti nama dataframe
nama.baru <- hasil1 %>% 
  select(tabel.gab) %>% 
  filter(str_detect(tabel.gab,paste(v.user,"gab",sep="_"))) %>% 
  as.character()
assign(nama.baru,data.gab)

v.user <- "ozi"
v.prov <- "Jambi"
v.kab <- "Tanjung Jabung Timur"
v.kec <- "Kuala Jambi"
v.desa <- "Teluk Majelis"
v.sut <- "mono"
v.kom <- "oil palm"
v.tahun <- 2016
v.nilai.tukar <- 10000
v.rate.p <- 7

hasil1 <- hasil1 %>% 
  bind_rows(hist.data(v.user,v.prov,v.kab,v.kec,v.desa,v.sut,v.kom,v.tahun,v.nilai.tukar,v.rate.p)) %>% 
  dplyr::select(-tabel.gab,-tabel.summary) %>% 
  mutate(no = row_number(),
         tabel.gab = paste(user,"gab",no,sep="_"),
         tabel.summary = paste(user,"summary",no,sep="_")) %>% 
  dplyr::select(-no)



v.user <- "aep"
v.prov <- "Sumatera Selatan"
v.kab <- "Tanjung Jabung Timur"
v.kec <- "Kuala Jambi"
v.desa <- "Teluk Majelis"
v.sut <- "mono"
v.kom <- "oil palm"
v.tahun <- 2015
v.nilai.tukar <- 13000
v.rate.p <- 9

hasil1 <- hasil1 %>% 
  bind_rows(hist.data(v.user,v.prov,v.kab,v.kec,v.desa,v.sut,v.kom,v.tahun,v.nilai.tukar,v.rate.p)) %>% 
  dplyr::select(-tabel.gab,-tabel.summary) %>% 
  mutate(no = row_number(),
         tabel.gab = paste(user,"gab",no,sep="_"),
         tabel.summary = paste(user,"summary",no,sep="_")) %>% 
  dplyr::select(-no)


view(hasil1)

v.user <- "aep"
v.prov <- "Sumatera Selatan"
v.kab <- "Tanjung Jabung Timur"
v.kec <- "Kuala Jambi"
v.desa <- "Teluk Majelis"
v.sut <- "mono"
v.kom <- "Padi"
v.tahun <- 2018
v.nilai.tukar <- 15000
v.rate.p <- 8

hasil1 <- hasil1 %>% 
  bind_rows(hist.data(v.user,v.prov,v.kab,v.kec,v.desa,v.sut,v.kom,v.tahun,v.nilai.tukar,v.rate.p)) %>% 
  dplyr::select(-tabel.gab,-tabel.summary) %>% 
  mutate(no = row_number(),
         tabel.gab = paste(user,"gab",no,sep="_"),
         tabel.summary = paste(user,"summary",no,sep="_")) %>% 
  dplyr::select(-no)

v.user <- "dimas"
v.prov <- "Sumatera Selatan"
v.kab <- "Tanjung Jabung Timur"
v.kec <- "Kuala Jambi"
v.desa <- "Teluk Majelis"
v.sut <- "mono"
v.kom <- "Padi"
v.tahun <- 2018
v.nilai.tukar <- 15000
v.rate.p <- 8

hasil1 <- hasil1 %>% 
  bind_rows(hist.data(v.user,v.prov,v.kab,v.kec,v.desa,v.sut,v.kom,v.tahun,v.nilai.tukar,v.rate.p)) %>% 
  dplyr::select(-tabel.gab,-tabel.summary) %>% 
  mutate(no = row_number(),
         tabel.gab = paste(user,"gab",no,sep="_"),
         tabel.summary = paste(user,"summary",no,sep="_")) %>% 
  dplyr::select(-no)
view(hasil1)
