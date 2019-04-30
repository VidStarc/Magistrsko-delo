
#iz zmagovite long pozicije izstopimo ko pridemo do 20 dnevnega dna, iz short 20 dnevni vrh
win_izstop_turtle <- function(tabela, izstop_s2, cena){
  # dna
  dna <- rep(0, nrow(tabela))
  for(i in (izstop_s2+1):nrow(tabela)){
    if(cena[i,] < min(cena[(i-izstop_s2):(i-1),])){dna[i] <- 1}
  }
  # vrhovi
  vrhovi <- rep(0, nrow(tabela))
  for(i in (izstop_s2+1):nrow(tabela)){
    if(cena[i,] > max(cena[(i-izstop_s2):(i-1),])){vrhovi[i] <- 1}
  }
  # izstopi
  izstop <- rep(0, nrow(tabela))
  for(i in 1:nrow(tabela)){
    if(tabela$entry[i] == 1){
      kandidati_izstop <- which(dna == 1)
      izstop[i] <- kandidati_izstop[kandidati_izstop > i][1]
    }
    if(tabela$entry[i] == 2){
      kandidati_izstop <- which(vrhovi == 1)
      izstop[i] <- kandidati_izstop[kandidati_izstop > i][1]
    }
  }
  izstop
}



# izstopimo ko dosežemo 1:3 razmerje med nagrado in tveganjem
win_izstop_rr <- function(tabela, cena, rr){
  izstop <- rep(0, nrow(tabela))
  for(i in 1:nrow(tabela)){
    if(tabela$entry[i] == 2){
      cilj <- cena[i,]- sl*rr*tabela$spr_tedenski_N[i]
      kandidati_cilj <- which(cena[,1] <= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
    if(tabela$entry[i] == 1){
      cilj <- cena[i,] + sl*rr*tabela$spr_tedenski_N[i]
      kandidati_cilj <- which(cena[,1] >= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
  }
  izstop
}


# Chandelier exit
# dnevi = 22
win_izstop_chandelier <- function(tabela, cena, dnevi){
  # 22-day ATR
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  
  # 22-day high, 22-day Low
  hh <- rep(0, nrow(tabela))
  ll <- rep(0, nrow(tabela))
  for(i in dnevi:nrow(tabela)){
    hh[i] <- max(tabela$High[(i-dnevi+1):i])
    ll[i] <- min(tabela$Low[(i-dnevi+1):i])
  }
  
  #izstop
  izstop <- rep(0, nrow(tabela))
  for(i in 1:nrow(tabela)){
    if(tabela$entry[i] == 2){
      cilj <- ll[i] + 3*n[i]
      kandidati_cilj <- which(cena[,1] >= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
    if(tabela$entry[i] == 1){
      cilj <- hh[i] - 3*n[i]
      kandidati_cilj <- which(cena[,1] <= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
  }
  izstop
}

##############################################################################################################

poracuni_vse <- function(tabela, dnevi_N, vstop_s2, izstop_s2, cena, dnevi_ema1, dnevi_ema2, toleranca, rr, 
                     indikator, metoda_izstop){
  library(QuantTools)
  tabela <- tabela[-nrow(tabela),]
  
  if(indikator == "zelve"){
    tab <- spr_N(tabela, dnevi_N)
    cena1 <- odlocitev_cena(tab, cena)
    tab$entry <- spr_entry_s2(tab, vstop_s2, cena1)
  }
  if(indikator == "r&s"){
    tab <- spr_N(tabela, dnevi_N)
    cena1 <- odlocitev_cena(tab, cena)
    cena1 <- log(cena1)
    SinR <- prepoznavalnik_SinR(tab, cena1, velikost_oken = 10)
    tmp <- vstop_SinR(tab, cena1, toleranca, SinR)
    tab$entry <- tmp$entry
    tab$linija <- tmp$linija
    tab$koliko_casa <- tmp$koliko_casa
  }
  if(indikator == "MA"){
    tab <- spr_N(tabela, dnevi_N)
    cena1 <- odlocitev_cena(tab, cena)
    tab$ma1 <- ema(cena1[,1], dnevi_ema1)
    tab$ma2 <- ema(cena1[,1], dnevi_ema2)
    tab$entry <- vstop_MA(tab, cena1)
  }
  
  tab$spr_tedenski_N <- spr_tedenski_N(tab)
  cena1 <- odlocitev_cena(tab, cena)
  if(metoda_izstop == "zelve"){
    tab$izstop <- win_izstop_turtle(tab, izstop_s2, cena1)
    pomoc <- which(win_izstop_turtle(tab, izstop_s2, cena1) > 0)
  }
  if(metoda_izstop == "rr"){
    tab$izstop <- win_izstop_rr(tab, cena1, rr)
    pomoc <- which(win_izstop_rr(tab, cena1, rr) > 0)
  }
  if(metoda_izstop == "chandelier"){
    tab$izstop <- win_izstop_chandelier(tab, cena1, 22)
    pomoc <- which(win_izstop_chandelier(tab, cena1, 22) > 0)
  }
  tab$izstop[pomoc] <- tab$izstop[pomoc] - 22 + 1   #to velja samo za btc_1day
  tab <- tab[tab$spr_tedenski_N > 0,]
  tab
}

dobicki_vse <- function(tabela, dnevi_N = 20, vstop_s2 = 55, izstop_s2 = 20, 
                            obdobje = 1800, zacetni_kapital = 1000000, cena = "Close", add = 1/2, sl = 2, 
                            toleranca = 0.02, rr = 3, dnevi_ema1 = 10, dnevi_ema2 = 50, indikator = "MA", 
                        metoda_izstop = "zelve"){
  tab <- poracuni_vse(tabela, dnevi_N, vstop_s2, izstop_s2, cena, dnevi_ema1, dnevi_ema2, toleranca, rr, 
                      indikator, metoda_izstop)
  dobicki <- c()
  for(i in 1:(nrow(tab) - obdobje + 1)){
    tmp <- tab[i:(obdobje + i - 1),]
    cena2 <- odlocitev_cena(tmp, cena)
    i_izstop <- (i-1)
    dobicki <- c(dobicki, trgovanje(tmp, zacetni_kapital, cena2, add, sl))
  }
  dobicki
}

zelve_zelve <- dobicki_vse(btc_1day, obdobje = 360, indikator = "zelve", metoda_izstop = "zelve")
zelve_rr <- dobicki_vse(btc_1day, obdobje = 360, indikator = "zelve", metoda_izstop = "rr")
zelve_ch <- dobicki_vse(btc_1day, obdobje = 360, indikator = "zelve", metoda_izstop = "chandelier")

SinR_zelve <- dobicki_vse(btc_1day, obdobje = 360, indikator = "r&s", metoda_izstop = "zelve")
SinR_rr <- dobicki_vse(btc_1day, obdobje = 360, indikator = "r&s", metoda_izstop = "rr")
SinR_ch <- dobicki_vse(btc_1day, obdobje = 360, indikator = "r&s", metoda_izstop = "chandelier")

MA_zelve <- dobicki_vse(btc_1day, obdobje = 360, indikator = "MA", metoda_izstop = "zelve")
MA_rr <- dobicki_vse(btc_1day, obdobje = 360, indikator = "MA", metoda_izstop = "rr")
MA_ch <- dobicki_vse(btc_1day, obdobje = 360, indikator = "MA", metoda_izstop = "chandelier")

indi_zelve <- indi_zelve*1000
#indi_rr <- indi2_dobicki_btc_360*1000
indi_rr <- indi_rr*1000
indi_ch <- indi_ch*1000

####################################################################################################

# cagr
strategije_cagr <- data.frame("zelve" = c(cagr(tabela = mean(zelve_zelve), obdobje = 360),
                                     cagr(tabela = mean(SinR_zelve), obdobje = 360),
                                     cagr(tabela = mean(MA_zelve), obdobje = 360),
                                     cagr(tabela = mean(indi_zelve), obdobje = 360)),
                         "rr" = c(cagr(tabela = mean(zelve_rr), obdobje = 360),
                                    cagr(tabela = mean(SinR_rr), obdobje = 360),
                                    cagr(tabela = mean(MA_rr), obdobje = 360),
                                  cagr(tabela = mean(indi_rr), obdobje = 360)),
                         "ce" = c(cagr(tabela = mean(zelve_ch), obdobje = 360),
                                  cagr(tabela = mean(SinR_ch), obdobje = 360),
                                  cagr(tabela = mean(MA_ch), obdobje = 360),
                                  cagr(tabela = mean(indi_ch), obdobje = 360))
)

rownames(strategije_cagr) <- c("zelve", "SinR", "MA", "indi")
library(flextable)
library(officer)
flextabela_matrika(cbind("vstop/izstop" = rownames(strategije_cagr), strategije_cagr), 2)


# količinik
strategije_kol <- data.frame("zelve" = c(mean(zelve_zelve)/sd(zelve_zelve),
                                          mean(SinR_zelve)/sd(SinR_zelve),
                                          mean(MA_zelve)/sd(MA_zelve),
                                         mean(indi_zelve)/sd(indi_zelve)),
                              "rr" = c(mean(zelve_rr)/sd(zelve_rr),
                                       mean(SinR_rr)/sd(SinR_rr),
                                       mean(MA_rr)/sd(MA_rr),
                                       mean(indi_rr)/sd(indi_rr)),
                              "ce" = c(mean(zelve_ch)/sd(zelve_ch),
                                       mean(SinR_ch)/sd(SinR_ch),
                                       mean(MA_ch)/sd(MA_ch),
                                       mean(indi_ch)/sd(indi_ch))
)

rownames(strategije_kol) <- c("zelve", "SinR", "MA", "indi")
library(flextable)
library(officer)
flextabela_matrika(cbind("vstop/izstop" = rownames(strategije_kol), strategije_kol), 2)


