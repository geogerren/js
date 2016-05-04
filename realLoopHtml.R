laos <- read.csv(paste0(boxdata, "lnw.csv"))
names(laos) <- c("number","website")
laoscn <- copy(laos)
laoscn[, website:=paste0(substr(website, 1, length(website)-3), "cn")]


freeList<-c()
for(i in 1:nrow(laos)){
  input <- as.character(laos[i,2])
  txt <- htmlToText(input)
  if(is.na(str_locate(txt, "Admin")[1])){
    freeList <- c(freeList, input)
  }
}

testInput <- "http://www.whois.com/whois/nitamadezhenshigedashabi123456.com"
input <- "http://www.google.co.uk/search?gcx=c&sourceid=chrome&ie=UTF-8&q=r+project#pq=%22hello+%3C+world%22&hl=en&cp=5&gs_id=3r&xhr=t&q=phd+comics&pf=p&sclient=psy-ab&source=hp&pbx=1&oq=phd+c&aq=0&aqi=g4&aql=&gs_sm=&gs_upl=&bav=on.2,or.r_gc.r_pw.r_cp.,cf.osb&fp=27ff09b2758eb4df&biw=1599&bih=904"
txt <- htmlToText(input)
