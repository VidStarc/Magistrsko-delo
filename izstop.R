
#iz zmagovite long pozicije izstopimo ko pridemo do 10 dnevnega dna, iz short 10 dnevni vrh
win_izstop_turtle <- function(tabela, izstop_s1, cena){
  # dna
  dna <- rep(0, nrow(tabela))
  for(i in (izstop_s1+1):nrow(tabela)){
    if(cena[i,] < min(cena[(i-izstop_s1):(i-1),])){dna[i] <- 1}
  }
  # vrhovi
  vrhovi <- rep(0, nrow(tabela))
  for(i in (izstop_s1+1):nrow(tabela)){
    if(cena[i,] > max(cena[(i-izstop_s1):(i-1),])){vrhovi[i] <- 1}
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