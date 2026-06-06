#il posizionamento nello spettro di destra o sinistra è identificato dalla
#variabile "lrscale" .
#l'obiettivo dell'analisi è verificare se ci sono differenze significative
#tra coloro che si identificano con posizionamenti agli estremi delle spettro politico
#rispetto a coloro che si identificano con posizionamenti più moderati

library(haven)
library(GPArotation)
library(MatchIt)
library(cobalt)
library(psych)
library(weights)

dati=read_dta("/Users/lucaalberti/Desktop/UNIVERSITA'/STATISTICA SOCIALE/RSTUDIO/ESS10.dta")

#scegliamo 4 nazioni con situazioni socio-economiche simili e creiamo la variabile
#ext che risulta pari a 1 se l'individuo è considerato estremista
dati1=subset(dati, (cntry=="IT" | cntry=="ES"| cntry=="PT"| cntry=="GR"))
dati1$ext=TRUE
dati1$ext=0
dati1$ext[dati1$lrscale>7 | dati1$lrscale<3]<-1
dati1
dati1$err<- TRUE
dati$err=0
dati1$var_test=T

#eliminiamo le entries che hanno dato risposte non coerenti nelle domande test
testing <- function(dati1) {
  dati1$err <- TRUE
  gruppo <- dati1$admrclc
  if (gruppo == 1) {
    var_test <- c("testic34", "testic35", "testic36", "testii1", "testii2", "testii3")
  } else if (gruppo == 2) {
    var_test <- c("testic37", "testic38", "testic39", "testii4", "testii5", "testii6")
  } else {
    var_test <- c("testic40", "testic41", "testic42", "testii7", "testii8", "testii9")
  }
  for (j in 1:nrow(dati1)) {
    for (i in 1:3) {
      risposta1 <- dati1[j, var_test[i]]
      risposta2 <- dati1[j, var_test[i + 3]]
      dif <- abs(risposta1 - risposta2)
      
      if ((gruppo == 1 && dif > 1) || ((gruppo == 2 || gruppo == 3) && dif != 0)) {
        dati1$err[j] <- FALSE
      }
    }
  }
  dati1 <- dati1[dati1$err == TRUE, ]
  
  return(dati1)
}

#creiamo degli indici per ogni ambito per evitare problemi di variabili mancanti.
#Per creare degli indici parziali usiamo l'analisi fattoriale e valutiamo possibili 
#nomenclature per gli indici parziali

###################################################################
######## DOMANDE RIGUARDANTI LA FIDUCIA GENERALE #################################
##################################################################

trst <- subset(dati1, select=c(ext,agea, gndr, ppltrst,pplfair,pplhlp,trstlgl,trstplc,trstep,trstun,trstprt,lrscale))
trst <- na.omit(trst)


strst<-scale(trst[,4:11])
cortrst<- cor(strst)
cortrst
summary(strst)
pca_trst <- prcomp(strst, center = TRUE, scale. = TRUE)
screeplot(pca_trst, type='lines')
summary(pca_trst)

#vediamo che le componenti i cui vettori 
fa_trst <- fa(strst, nfactors = 3, rotate = "oblimin")
print(fa_trst$loadings)
load<-as.matrix(fa_trst$loadings)
load<-load^2
valdel <- function(row) {
  max_value <- max(row)
  row[row != max_value] <- 0
  return(row)
}
load1 <- t(apply(load, 1, valdel))
load1

wtrst<- apply(load1, 2, function(x) x / sum(x))
wtrst

# abbiamo ottenuto i pesi, ora creiamo gli indici in base ai pesi scelti:
# li suddividiamo in ordine in
# INDICE DI FIDUCIA POLITICA(ifpol)
# INDICE DI FIDUCIA SOCIALE (ifsoc)
# INDICE DI FIDUCIA DELLE ISTITUZIONI (ifis)
# INDICE DI FIDUCIA TOTALE (iftot) (un aggregazione pesata degli indici precedenti)

strst <- cbind(trst[,1:4],strst)
strst$lrscale<-trst$lrscale
strst$ifsoc <- T
strst$ifsoc <- strst$ppltrst*wtrst[1,2]+strst$pplfair*wtrst[2,2]+strst$pplhlp*wtrst[3,2]

strst$ifpol <- T
strst$ifpol <- strst$trstep*wtrst[6,1]+strst$trstun*wtrst[7,1]+strst$trstprt*wtrst[8,1]

strst$ifis <- T
strst$ifis <- strst$trstlgl*wtrst[4,3]+strst$trstplc*wtrst[5,3]

wtrst2<- fa_trst$Vaccounted[2,]
wtrst2<- wtrst2/sum(wtrst2)

strst$iftot<- T
strst$iftot<- strst$ifpol* wtrst2[1]+strst$ifsoc*wtrst2[2]+strst$ifis*wtrst2[3]


#eseguiamo il matching pesato per rendere le 2 popolazioni confrontabili
match_trst <- matchit(ext ~ agea + gndr, data = strst, method ="full", distance = "logit")
bal.plot(match_trst, var.name = "agea")
plot(match, type = "density", interactive = FALSE, + which.xs ~agea + gndr)

#testiamo se la differenza è rilevante
wtd.chi.sq(strst$iftot, strst$ext, weight=match_trst$weights) 
wtd.t.test(strst$iftot[strst$ext == 1], y = strst$iftot[strst$ext == 0], 
                     weight = match_trst$weights[strst$ext == 1],weighty = match_trst$weights[strst$ext == 0],alternative = "two.tailed")
hist(strst$iftot[strst$ext == 1])
hist(strst$iftot[strst$ext == 0])

#testiamo ora i singoli indici creati
wtd.chi.sq(strst$ifpol, strst$ext, weight=match_trst$weights)
wtd.t.test(strst$ifpol[strst$ext == 1], y = strst$ifpol[strst$ext == 0], 
           weight = match_trst$weights[strst$ext == 1],weighty = match_trst$weights[strst$ext == 0],alternative = "two.tailed")
hist(strst$ifpol[strst$ext == 1])
hist(strst$ifpol[strst$ext == 0])

wtd.chi.sq(strst$ifsoc, strst$ext, weight=match_trst$weights)
wtd.t.test(strst$ifsoc[strst$ext == 1], y = strst$ifsoc[strst$ext == 0], 
           weight = match_trst$weights[strst$ext == 1],weighty = match_trst$weights[strst$ext == 0],alternative = "two.tailed")

wtd.chi.sq(strst$ifis, strst$ext, weight=match_trst$weights)
wtd.t.test(strst$ifis[strst$ext == 1], y = strst$ifis[strst$ext == 0], 
           weight = match_trst$weights[strst$ext == 1],weighty = match_trst$weights[strst$ext == 0],alternative = "two.tailed")


#proviamo a controllare per lo schieramento, estrema destra o estrema sinistra
strst$extdx=5
strst$extdx[strst$lrscale<3]<-0
strst$extdx[strst$lrscale>7]<-1

strst2<- subset(strst, strst$extdx!=5)
strst2$extdx

match_trst2 <- matchit(extdx ~ agea + gndr, data = strst2, method ="full", distance = "logit")
bal.plot(match_trst2, var.name = "agea")
plot(match_trst2, type = "density", interactive = FALSE, + which.xs ~agea + gndr)

#interessante notare come la composizione sia già molto simile anche prima del matching

wtd.chi.sq(strst2$ifsoc, strst2$extdx, weight=match_trst2$weights) 
wtd.t.test(strst2$ifsoc[strst2$extdx == 1], y = strst2$ifsoc[strst2$extdx == 0], 
           weight = match_trst2$weights[strst2$extdx == 1],weighty = match_trst2$weights[strst2$extdx == 0],alternative = "two.tailed")

wtd.chi.sq(strst2$ifis, strst2$extdx, weight=match_trst2$weights) 
wtd.t.test(strst2$ifis[strst2$extdx == 1], y = strst2$ifis[strst2$extdx == 0], 
           weight = match_trst2$weights[strst2$extdx == 1],weighty = match_trst2$weights[strst2$extdx == 0],alternative = "two.tailed")

wtd.chi.sq(strst2$ifpol, strst2$extdx, weight=match_trst2$weights) 
wtd.t.test(strst2$ifpol[strst$ext == 1], y = strst2$ifpol[strst$ext == 0], 
           weight = match_trst$weights[strst$ext == 1],weighty = match_trst$weights[strst$ext == 0],alternative = "two.tailed")

wtd.chi.sq(strst2$iftot, strst2$extdx, weight=match_trst2$weights) 
wtd.t.test(strst2$iftot[strst$ext == 1], y = strst2$iftot[strst$ext == 0], 
           weight = match_trst$weights[strst$ext == 1],weighty = match_trst$weights[strst$ext == 0],alternative = "two.tailed")


###############################################################################
# DOMANDE RIGUARDANTI LA SODDISFAZIONE GENERALE ##########################
################################################################################
sodd <- subset(dati1, select=c(ext,agea, gndr,lrscale, stfeco,stfgov, stfdem, stfmjob, stflife, happy))
sodd <- na.omit(sodd)

ssodd<-scale(sodd[,5:10])
corsod<- cor(ssodd)

corsod
pca_sodd <- prcomp(ssodd, center = TRUE, scale. = TRUE)
screeplot(pca_sodd, type='lines')
summary(pca_sodd)

#factor analysis SODDISFAZIONE
fa_sodd <- fa(ssodd, nfactors = 2, rotate = "oblimin")
print(fa_sodd$loadings)
summary(fa_trst)

# Possiamo creare degli indici che si dividono in:
# INDICE DI SODDISFAZIONE PER LE ISTITUZIONI (isodis)
# INDICE DI SODDISFAZIONE PER LA PROPRIA VITA (isodlf)
# abbiamo droppato health che sembra non essere utile alla costruzione
# di questi due indici

lsodd<-as.matrix(fa_sodd$loadings)^2
lsodd <- t(apply(lsodd, 1, valdel))

wsodd<- apply(lsodd, 2, function(x) x / sum(x))
wsodd

#creiamo gli indici prima discussi
ssodd <- cbind(sodd[,1:4],ssodd)
ssodd$isodlf <- T
ssodd$isodlf <- ssodd$stfmjob*wsodd[4,2]+ssodd$stflife*wsodd[5,2]+ssodd$happy*wsodd[6,2]

ssodd$isodis <- T
ssodd$isodis <- ssodd$stfeco*wsodd[1,1]+ssodd$stfgov*wsodd[2,1]+ssodd$stfdem*wsodd[3,1]

#ora creiamo i coefficienti di ogni indice per ottenere l'indice aggregato totale
wsodd2<- fa_sodd$Vaccounted[2,]
wsodd2<- wsodd2/sum(wsodd2)

ssodd$istot<- T
ssodd$istot<- ssodd$isodis* wsodd2[1]+ssodd$isodlf*wsodd2[2]

# calcoliamo i pesi dei singoli campioni tramite il matching pesato
#eseguiamo il matching pesato per rendere le 2 popolazioni confrontabili
match_sodd <- matchit(ext ~ agea + gndr, data = ssodd, method ="full", distance = "logit")
bal.plot(match_sodd, var.name = "agea")
plot(match_sodd, type = "density", interactive = FALSE, + which.xs ~agea + gndr)

#verifichiamo attraverso il test del chi^2 pesato e della t di student pesata
#testiamo se la differenza è rilevante
wtd.chi.sq(ssodd$istot, ssodd$ext, weight=match_sodd$weights) 
wtd.t.test(ssodd$istot[ssodd$ext == 1], y = ssodd$istot[sodd$ext == 0], 
           weight = match_sodd$weights[ssodd$ext == 1],weighty = match_sodd$weights[ssodd$ext == 0],alternative = "two.tailed")

#testiamo ora i singoli indici creati
wtd.chi.sq(ssodd$isodlf, ssodd$ext, weight=match_sodd$weights)
wtd.t.test(ssodd$isodlf[ssodd$ext == 1], y = ssodd$isodlf[sodd$ext == 0], 
           weight = match_sodd$weights[ssodd$ext == 1],weighty = match_sodd$weights[ssodd$ext == 0],alternative = "two.tailed")

wtd.chi.sq(ssodd$isodis, ssodd$ext, weight=match_sodd$weights) 
wtd.t.test(ssodd$isodis[ssodd$ext == 1], y = ssodd$isodis[sodd$ext == 0], 
           weight = match_sodd$weights[ssodd$ext == 1],weighty = match_sodd$weights[ssodd$ext == 0],alternative = "two.tailed")


## ora controlliamo per le differenze interne alle frange estremiste
ssodd$extdx=5
ssodd$extdx[ssodd$lrscale<3]<-0
ssodd$extdx[ssodd$lrscale>7]<-1

ssodd2<- subset(ssodd, ssodd$extdx!=5)
ssodd2$extdx

match_sodd2 <- matchit(extdx ~ agea + gndr, data = ssodd2, method ="full", distance = "logit")
bal.plot(match_sodd2, var.name = "agea")
plot(match_sodd2, type = "density", interactive = FALSE, + which.xs ~agea + gndr)

#controlliamo le differenze
wtd.chi.sq(ssodd2$istot, ssodd2$extdx, weight=match_sodd2$weights) 
wtd.t.test(ssodd2$istot[ssodd2$extdx == 1], y = ssodd2$istot[ssodd2$extdx == 0], 
           weight = match_sodd2$weights[ssodd2$extdx == 1],weighty = match_sodd2$weights[ssodd2$extdx == 0],alternative = "two.tailed")

#testiamo ora i singoli indici creati
wtd.chi.sq(ssodd2$isodlf, ssodd2$extdx, weight=match_sodd2$weights)
wtd.t.test(ssodd2$isodlf[ssodd2$extdx == 1], y = ssodd2$isodlf[ssodd2$extdx == 0], 
           weight = match_sodd2$weights[ssodd2$extdx == 1],weighty = match_sodd2$weights[ssodd2$extdx == 0],alternative = "two.tailed")

wtd.chi.sq(ssodd2$isodis, ssodd2$extdx, weight=match_sodd2$weights) 
wtd.t.test(ssodd2$isodis[ssodd2$extdx == 1], y = ssodd2$isodis[ssodd2$extdx == 0], 
           weight = match_sodd2$weights[ssodd2$extdx == 1],weighty = match_sodd2$weights[ssodd2$extdx == 0],alternative = "two.tailed")

########################################################################
##### DOMANDE RIGUARDANTI L'ISTRUZIONE #################################
########################################################################

educ <- subset(dati1, select=c(ext,agea, gndr,lrscale, eduyrs,edulvlpb,edulvlb,edulvlfb,edulvlmb))
educ <- na.omit(educ)

seduc<-scale(educ[,5:9])
coredu<- cor(seduc)
coredu
pca_edu <- prcomp(seduc, center = TRUE, scale. = TRUE)
screeplot(pca_edu, type='lines')
summary(pca_edu)

#factor analysis ISTRUZIONE
fa_edu <- fa(seduc, nfactors = 2, rotate = "oblimin")
print(fa_edu$loadings)
fedu<- as.matrix(fa_edu$loadings)^2
cedu<-t(apply(fedu, 1, valdel))
apply(cedu, 2, function(x) x / sum(x))
fa_edu$Vaccounted
wedu2<- fa_edu$Vaccounted[2,]
wedu2<- wedu2/sum(wedu2)
seduc<-cbind(seduc,educ[1:4])

# possiamo creare 2 indici:
# il primo (ieduin) che tratta l'istruzione dell'individuo e del partner
# il secondo (iedupar) che tratta dell'educazione dei genitori
seduc$iedupar<-T
seduc$ieduin<-T
seduc$ieduin<- seduc$eduyrs*cedu[1,1]+seduc$edulvlpb*cedu[2,1]+seduc$edulvlb*cedu[3,1]
seduc$iedupar<- seduc$edulvlfb*cedu[4,2]+seduc$edulvlmb*cedu[5,2]

seduc$iedutot<-seduc$ieduin*wedu2[1]+seduc$iedupar*wedu2[2]
seduc$iedutot

#ora che abbiamo calcolato gli indici effettuiamo il matching
match_edu <- matchit(ext ~ agea + gndr, data = seduc, method ="full", distance = "logit")
bal.plot(match_edu, var.name = "agea")
plot(match_edu, type = "density", interactive = FALSE, + which.xs ~agea + gndr)

# Controlliamo se le differenze sono significative
wtd.chi.sq(seduc$iedutot, seduc$ext, weight=match_edu$weights) 
wtd.t.test(seduc$iedutot[seduc$ext == 1], y = seduc$iedutot[seduc$ext == 0], 
           weight = match_edu$weights[seduc$ext == 1],weighty = match_edu$weights[seduc$ext == 0],alternative = "two.tailed")

#testiamo ora i singoli indici creati
wtd.chi.sq(seduc$ieduin, seduc$ext, weight=match_edu$weights)
wtd.t.test(seduc$ieduin[seduc$ext == 1], y = seduc$ieduin[seduc$ext == 0], 
           weight = match_edu$weights[seduc$ext == 1],weighty = match_edu$weights[seduc$ext == 0],alternative = "two.tailed")

wtd.chi.sq(seduc$iedupar, seduc$ext, weight=match_edu$weights)
wtd.t.test(seduc$iedupar[seduc$ext == 1], y = seduc$iedupar[seduc$ext == 0], 
           weight = match_edu$weights[seduc$ext == 1],weighty = match_edu$weights[seduc$ext == 0],alternative = "two.tailed")

seduc$extdx=5
seduc$extdx[seduc$lrscale<3]<-0
seduc$extdx[seduc$lrscale>7]<-1

seduc2<- subset(seduc, seduc$extdx!=5)
seduc2$extdx

match_edu2 <- matchit(extdx ~ agea + gndr, data = seduc2, method ="full", distance = "logit")
bal.plot(match_edu2, var.name = "agea")
plot(match_edu2, type = "density", interactive = FALSE, + which.xs ~agea + gndr)

wtd.chi.sq(seduc2$iedutot, seduc2$extdx, weight=match_edu2$weights)
wtd.t.test(seduc2$iedutot[seduc2$extdx == 1], y = seduc2$iedutot[seduc2$extdx == 0], 
           weight = match_edu2$weights[seduc2$extdx == 1],weighty = match_edu2$weights[seduc2$extdx == 0],alternative = "two.tailed")

wtd.chi.sq(seduc2$ieduin, seduc2$extdx, weight=match_edu2$weights)
wtd.t.test(seduc2$ieduin[seduc2$extdx == 1], y = seduc2$ieduin[seduc2$extdx == 0], 
           weight = match_edu2$weights[seduc2$extdx == 1],weighty = match_edu2$weights[seduc2$extdx == 0],alternative = "two.tailed")

wtd.chi.sq(seduc2$iedupar, seduc2$extdx, weight=match_edu2$weights)
wtd.t.test(seduc2$iedupar[seduc2$extdx == 1], y = seduc2$iedupar[seduc2$extdx == 0], 
           weight = match_edu2$weights[seduc2$extdx == 1],weighty = match_edu2$weights[seduc2$extdx == 0],alternative = "two.tailed")

#######################################################################
##### DOMANDE RIGUARDANTI IL COVID-19 #################################
######################################################################

cov19 <- subset(dati1, select=c(ext,agea, gndr,lrscale, secgrdec, scidecpb, gvconc19, gvhanc19,gvimpc19))
cov19 <- na.omit(cov19)

scov19<-scale(cov19[,5:9])
scov19<-cbind(scov19,cov19[,1])
corcov19<- cor(scov19)
corcov19
scov19

pca_cov19 <- prcomp(scov19[,1:5], center = TRUE, scale. = TRUE)
screeplot(pca_cov19, type='lines')
summary(pca_cov19)

#factor analysis COVID
fa_cov19 <- fa(scov19[,1:5], nfactors = 2, rotate = "oblimin")
print(fa_cov19$loadings)
fac19l<- as.matrix(fa_cov19$loadings)^2
c19l<-t(apply(fac19l, 1, valdel))
apply(c19l, 2, function(x) x / sum(x))
wc19<- fa_edu$Vaccounted[2,]
wc19<- wc19/sum(wc19)

#Creiamo 2 variabili
# ic19c = INDICE COVID 19 COMPLOTTO
# ic19gov = INDICE COVID 19 GOVERNO
scov19$ic19c<-T
scov19$ic19gov<-T
scov19$ic19tot<-T
scov19$ic19c<- scov19$secgrdec*c19l[1,1]+scov19$scidecpb*c19l[2,1]+scov19$gvconc19*c19l[3,1]
scov19$ic19gov<-scov19$gvhanc19*c19l[4,2]+scov19$gvimpc19*c19l[5,2]
scov19$ic19tot<-scov19$ic19c*c19l[1]+scov19$ic19gov*c19l[2]

#ora che abbiamo calcolato gli indici effettuiamo il matching
scov19<-cbind(scov19,cov19[,1:4])
match_c19 <- matchit(ext ~ agea + gndr, data = scov19, method ="full", distance = "logit")
bal.plot(match_c19, var.name = "agea")
plot(match_c19, type = "density", interactive = FALSE, + which.xs ~agea + gndr)

#testiamo se le differenze sono significative
wtd.chi.sq(scov19$ic19tot, scov19$ext, weight=match_c19$weights) 
wtd.t.test(scov19$ic19tot[scov19$ext == 1], y = scov19$ic19tot[scov19$ext == 0], 
           weight = match_c19$weights[scov19$ext == 1],weighty = match_c19$weights[scov19$ext == 0],alternative = "two.tailed")

#testiamo ora i singoli indici creati
wtd.chi.sq(scov19$ic19c, scov19$ext, weight=match_c19$weights)
wtd.t.test(scov19$ic19c[scov19$ext == 1], y = scov19$ic19c[scov19$ext == 0], 
           weight = match_c19$weights[scov19$ext == 1],weighty = match_c19$weights[scov19$ext == 0],alternative = "two.tailed")

wtd.chi.sq(scov19$ic19gov, scov19$ext, weight=match_c19$weights)
wtd.t.test(scov19$ic19gov[scov19$ext == 1], y = scov19$ic19gov[scov19$ext == 0], 
           weight = match_c19$weights[scov19$ext == 1],weighty = match_c19$weights[scov19$ext == 0],alternative = "two.tailed")

scov19$extdx=5
scov19$extdx[scov19$lrscale<3]<-0
scov19$extdx[scov19$lrscale>7]<-1

scov192<- subset(scov19, scov19$extdx!=5)
scov192

match_c192 <- matchit(extdx ~ agea + gndr, data = scov192, method ="full", distance = "logit")
bal.plot(match_c192, var.name = "agea")
plot(match_c192, type = "density", interactive = FALSE, + which.xs ~agea + gndr)

#effettuiamo i test
wtd.chi.sq(scov192$ic19c, scov192$extdx, weight=match_c192$weights)
wtd.t.test(scov192$ic19c[scov192$extdx == 1], y = scov192$ic19c[scov192$extdx == 0], 
           weight = match_c192$weights[scov192$extdx == 1],weighty = match_c192$weights[scov192$extdx == 0],alternative = "two.tailed")

wtd.chi.sq(scov192$ic19gov, scov192$extdx, weight=match_c192$weights)
wtd.t.test(scov192$ic19gov[scov192$extdx == 1], y = scov192$ic19gov[scov192$extdx == 0], 
           weight = match_c192$weights[scov192$extdx == 1],weighty = match_c192$weights[scov192$extdx == 0],alternative = "two.tailed")

wtd.chi.sq(scov192$ic19tot, scov192$extdx, weight=match_c192$weights)
wtd.t.test(scov192$ic19tot[scov192$extdx == 1], y = scov192$ic19tot[scov192$extdx == 0], 
           weight = match_c192$weights[scov192$extdx == 1],weighty = match_c192$weights[scov192$extdx == 0],alternative = "two.tailed")


##########################################################################
######## DOMDANDE RIGUARDO LA DEMOCRAZIA E LO STATO #################################
##########################################################################
demo <- subset(dati1, select=c(ext,agea, gndr,lrscale, medcrgvc,cttresac,viepolc,gvctzpvc,grdfincc,dfprtalc,fairelcc))
demo <- na.omit(demo)

sdemo<-scale(demo[,5:11])

pca_demo <- prcomp(sdemo, center = TRUE, scale. = TRUE)
screeplot(pca_demo, type='lines')
summary(pca_demo)

#factor analysis DEMOCRAZIA
fa_demo <- fa(sdemo, nfactors = 2, rotate = "oblimin")
print(fa_demo$loadings)
fdemo<- as.matrix(fa_demo$loadings)^2
wdemo<-t(apply(fdemo, 1, valdel))
apply(wdemo, 2, function(x) x / sum(x))
wdemo2<- fa_demo$Vaccounted[2,]
wdemo2<- wdemo2/sum(wdemo2)
sdemo<- cbind(sdemo, demo[1:4])

#ora creiamo i 2 indici ottenuti dai 2 fattori:
#il primo lo identifichiamo come indice di giustizia sociale (idemogs)
#il secondo lo identifichiamo come indice di libertà di espressione(idemole)
sdemo$idemogs<-T
sdemo$idemole<-T
sdemo$idemotot<-T
sdemo$idemogs<-sdemo$cttresac*wdemo[2,1]+sdemo$viepolc*wdemo[3,1]+sdemo$gvctzpvc*wdemo[4,1]+sdemo$grdfincc*wdemo[5,1]
sdemo$idemole<-sdemo$medcrgvc*wdemo[1,2]+sdemo$dfprtalc*wdemo[6,2]+sdemo$fairelcc*wdemo[7,2]
sdemo$idemotot<-sdemo$idemogs*wdemo2[1]+sdemo$idemogs*wdemo2[2]

#effettuiamo un weighted matching per età e genere per ogni gruppo
match_dem <- matchit(ext ~ agea + gndr, data = sdemo, method ="full", distance = "logit")

#testiamo se le differenze sono significative
wtd.chi.sq(sdemo$idemotot, sdemo$ext, weight=match_dem$weights) 
wtd.t.test(sdemo$idemotot[sdemo$ext == 1], y = sdemo$idemotot[sdemo$ext == 0], 
           weight = match_dem$weights[sdemo$ext == 1],weighty = match_dem$weights[sdemo$ext == 0],alternative = "two.tailed")
#testiamo ora i singoli indici creati
wtd.chi.sq(sdemo$idemogs, sdemo$ext, weight=match_dem$weights)
wtd.t.test(sdemo$idemogs[sdemo$ext == 1], y = sdemo$idemogs[sdemo$ext == 0], 
           weight = match_dem$weights[sdemo$ext == 1],weighty = match_dem$weights[sdemo$ext == 0],alternative = "two.tailed")

wtd.chi.sq(sdemo$idemole, sdemo$ext, weight=match_dem$weights)
wtd.t.test(sdemo$idemole[sdemo$ext == 1], y = sdemo$idemole[sdemo$ext == 0], 
           weight = match_dem$weights[sdemo$ext == 1],weighty = match_dem$weights[sdemo$ext == 0],alternative = "two.tailed")

sdemo$extdx=5
sdemo$extdx[sdemo$lrscale<3]<-0
sdemo$extdx[sdemo$lrscale>7]<-1

sdemo2<- subset(sdemo, sdemo$extdx!=5)
sdemo2$extdx

match_dem2 <- matchit(extdx ~ agea + gndr, data = sdemo2, method ="full", distance = "logit")
bal.plot(match_dem2, var.name = "agea")
plot(match_dem2, type = "density", interactive = FALSE, + which.xs ~agea + gndr)

#effettuiamo i test
wtd.chi.sq(sdemo2$idemotot, sdemo2$extdx, weight=match_dem2$weights) 
wtd.t.test(sdemo2$idemotot[sdemo2$extdx == 1], y = sdemo2$idemotot[sdemo2$extdx == 0], 
           weight = match_dem2$weights[sdemo2$exdx == 1],weighty = match_dem2$weights[sdemo2$extdx == 0],alternative = "two.tailed")

wtd.chi.sq(sdemo2$idemogs, sdemo2$extdx, weight=match_dem2$weights) 
wtd.t.test(sdemo2$idemogs[sdemo2$extdx == 1], y = sdemo2$idemogs[sdemo2$extdx == 0], 
           weight = match_dem2$weights[sdemo2$exdx == 1],weighty = match_dem2$weights[sdemo2$extdx == 0],alternative = "two.tailed")

wtd.chi.sq(sdemo2$idemole, sdemo2$extdx, weight=match_dem2$weights) 
wtd.t.test(sdemo2$idemole[sdemo2$extdx == 1], y = sdemo2$idemole[sdemo2$extdx == 0], 
           weight = match_dem2$weights[sdemo2$exdx == 1],weighty = match_dem2$weights[sdemo2$extdx == 0],alternative = "two.tailed")

