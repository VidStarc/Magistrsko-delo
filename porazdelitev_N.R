
spr_N <- function(tabela, dnevi){
  library(QuantTools)
  tr <- c()
  tr1 <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    tr <- append(tr, max(h-l, h-pdc, pdc-l))
    dc <- tabela$Close[i]
    tr1 <- append(tr1, max(h-l, h-pdc, pdc-l)/dc)
  }
  tabela$TR <- c(0,tr)
  tabela$TR1 <- c(0, tr1)
  tabela <- tabela[-1,]
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  tabela$spr_N <- n
  
  prvi_n1 <- mean(tabela$TR1[1:dnevi])
  n1 <- c(rep(0,(dnevi-1)), prvi_n1, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n1[i] <- ((dnevi-1)*n1[i-1] + tabela$TR1[i])/dnevi
  }
  tabela$N1 <- n1
  tabela$sma <- sma(tabela$Close, 20)
  tabela
}

spr_tedenski_N <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    t_N[i:(i+6)] <- tabela$spr_N[i]
  }
  t_N[1:nrow(tabela)]
}

izracun_za_analizo <- function(tabela, dnevi_N, vstop_s1, vstop_s2, cena){
  library(QuantTools)
  tabela <- tabela[-nrow(tabela),]
  tab <- spr_N(tabela, dnevi_N)
  cena1 <- odlocitev_cena(tab, cena)
  tab$entry_s1 <- spr_entry_s1(tab, vstop_s1, cena1)
  tab$entry_s2 <- spr_entry_s2(tab, vstop_s2, cena1)
  tab$spr_tedenski_N <- spr_tedenski_N(tab)
  tab$spr_tedenski_N[is.na(tab$spr_tedenski_N)] <- rep(0, length(tab$spr_tedenski_N[is.na(tab$spr_tedenski_N)]))
  tab <- tab[tab$spr_tedenski_N > 0,]
  tab
}

tabela <- izracun_za_analizo(btc_1day, 20, 20, 55, "Close")

ATR_C_N <- tabela$spr_N/tabela$Close
logaritemski_N <- c(tabela$spr_N[1:700], log(tabela$spr_N[701:nrow(tabela)]))
ATR_SMA_N <- tabela$spr_N/tabela$sma
TR_C_N <- tabela$N1

hist(ATR_C_N, breaks = 20)
hist(ATR_SMA_N)
hist(TR_C_N)
hist(logaritemski_N)
hist(tabela$spr_N)

# preveril sem velikost Enote:
# npr. (0.01*1000000)/min(ATR_C_N)
# nikjer ne preseže 1.000.000, lahko pa bi omejili recimo na 250.000, saj potem dodamo 4 enote
# Preveril sem tudi za spodnje omejitve, npr. /max(ATR_C_N), ampak so številke nad 20.000, kar je solidno,
# razen pri max(tabela$spr_N) je najmanjša enota 5 dolarjev


ohlc <- btc_1day[-nrow(btc_1day),2:5]
library(TTR)
hist(volatility(ohlc, n = 20, N = 365 , calc = "close"))
hist(volatility(ohlc, n = 20, N = 365 , calc = "yang.zhang"))


# kolko so si različne volatilnosti
vol_close <- volatility(ohlc, n = 20, N = 1 , calc = "close")
vol_close[is.na(vol_close)] <- 0
vol_gk <- volatility(ohlc, n = 20, N = 1 , calc = "garman.klass")
vol_gk[is.na(vol_gk)] <- 0
vol_par <- volatility(ohlc, n = 20, N = 1 , calc = "parkinson")
vol_par[is.na(vol_par)] <- 0
vol_rs <- volatility(ohlc, n = 20, N = 1 , calc = "rogers.satchell")
vol_rs[is.na(vol_rs)] <- 0
vol_gkyz <- volatility(ohlc, n = 20, N = 1 , calc = "gk.yz")
vol_gkyz[is.na(vol_gkyz)] <- 0
vol_yz <- volatility(ohlc, n = 20, N = 1 , calc = "yang.zhang")
vol_yz[is.na(vol_yz)] <- 0

ATR_C_N <- tabela$spr_N/tabela$Close
logaritemski_N <- c(tabela$spr_N[1:700], log(tabela$spr_N[701:nrow(tabela)]))
ATR_SMA_N <- tabela$spr_N/tabela$sma
TR_C_N <- tabela$N1

plot(ATR_C_N[1:500], type = "l", ylab = "volatility")
#lines(logaritemski_N, col = "red")
lines(ATR_SMA_N[1:500], col = "green")
lines(TR_C_N[1:500], col = "blue")
legend("topleft", c("ATR/C", "ATR/SMA", "TR/C"), col = c("black", "green", "blue"), bty = "n", lwd = 1)


# poglej kaj se zgodi med 1600 in 1700, ki close ful majhen, drugi visoki in se precej razlikujejo
plot(x = tabela$Timestamp[1:700], y = vol_close[23:722], type = "l", 
     ylim = c(0, max(vol_close[23:722], vol_gk[23:722], vol_par[23:722], vol_rs[23:722], 
                     vol_gkyz[23:722], vol_yz[23:722], ATR_C_N[1:700], ATR_SMA_N[1:700], TR_C_N[1:700])),
     ylab = "Volatilnost",
     xlab = "Trgovalni dnevi",
     main = "Mere volatilnosti")
lines(x = tabela$Timestamp[1:700], y = vol_gk[23:722], col = 2)
lines(x = tabela$Timestamp[1:700], y = vol_par[23:722], col = 3)
lines(x = tabela$Timestamp[1:700], y = vol_rs[23:722], col = 4)
lines(x = tabela$Timestamp[1:700], y = vol_gkyz[23:722], col = 5)
lines(x = tabela$Timestamp[1:700], y = vol_yz[23:722], col = 6)
lines(x = tabela$Timestamp[1:700], y = ATR_C_N[1:700], col = 7)
lines(x = tabela$Timestamp[1:700], y = ATR_SMA_N[1:700], col = 8)
lines(x = tabela$Timestamp[1:700], y = TR_C_N[1:700], col = "darkgreen")
legend("topleft", c("close", "gk", "par", "rs", "gkyz", "yz", "ATR/C", "ATR/SMA", "TR/C"), 
       col = c(1:8, "darkgreen"), bty = "n", lwd = 1.5, cex = 0.7)



# omejitev vse štiri enote manj kot 1000000 (lahko tudi samo dve enoti)
trgovanje <- function(tabela, zacetni_kapital, cena, add, sl){
  kandidati <- which(tabela$entry==1 | tabela$entry==2)
  vstop <- kandidati[1]
  if(is.na(vstop)){profit <- 0}
  else{
    kdaj_vstopali <- c()
    kdaj_dodali_enote <- c()
    profit <- 0
    profit1 <- c()
    kdaj_profit <- c()
    money <- zacetni_kapital
    
    while(nrow(tabela) - vstop > 0){
      kdaj_vstopali <- c(kdaj_vstopali, vstop)
      cena_vstop <- cena[vstop,]
      pozicija <- min(money, unit(money, tabela$spr_tedenski_N[vstop]))
      st_btc <- pozicija/cena_vstop
      
      #Buy signal
      if(tabela$entry[vstop]== 1){
        izstop <- ifelse(tabela$izstop[vstop] - i_izstop > nrow(tabela) | 
                           is.na(tabela$izstop[vstop] - i_izstop > nrow(tabela)), nrow(tabela), tabela$izstop[vstop] - i_izstop)
        cena_izstop <- cena[izstop,]
        st_enot <- 1
        stop_loss <- cena_vstop - sl*tabela$spr_tedenski_N[vstop]
        pol_N <- add*tabela$spr_tedenski_N[vstop]
        cena_ko_dodamo <- cena_vstop
        st_btc_dodamo <- st_btc
        for(i in (vstop+1):(izstop-1)){
          vstop <- i
          if((cena[i,] > cena_vstop + st_enot*pol_N) & (st_enot < 4) & (pozicija < money)){
            razlika <- cena[i,] - cena_ko_dodamo[length(cena_ko_dodamo)]
            dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
            do_milijon <- money - pozicija
            enote <- min(do_milijon, unit(money, tabela$spr_tedenski_N[i])*dodamo)
            if(st_enot + dodamo <= 4){
              st_enot <- st_enot + dodamo
              st_btc_dodamo <- c(st_btc_dodamo, (enote/cena[i,]))
              stop_loss <- stop_loss + pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
              pozicija <- pozicija + enote
            }
            else{
              dodamo <- ifelse(st_enot==2, 2, 1)
              st_enot <- st_enot + dodamo
              st_btc_dodamo <- c(st_btc_dodamo, (enote/cena[i,]))
              stop_loss <- stop_loss + pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
              pozicija <- pozicija + enote
            }
          }
          else{
            if(cena[i,] <= stop_loss){
              vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
              kdaj_profit <- c(kdaj_profit, i)
              for(j in 1:length(st_btc_dodamo)){
                profit <- profit + (cena[i,] - cena_ko_dodamo[j])*st_btc_dodamo[j]
              }
              profit1 <- c(profit1, profit)
              break}
          }
        }
      }
      else{
        # Sell signal
        if(tabela$entry[vstop]== 2){
          izstop <- ifelse(tabela$izstop[vstop] - i_izstop > nrow(tabela)  | 
                             is.na(tabela$izstop[vstop] - i_izstop > nrow(tabela)), nrow(tabela), tabela$izstop[vstop] - i_izstop)
          cena_izstop <- cena[izstop,]
          st_enot <- 1
          stop_loss <- cena_vstop + sl*tabela$spr_tedenski_N[vstop]
          pol_N <- add*tabela$spr_tedenski_N[vstop]
          cena_ko_dodamo <- cena_vstop
          st_btc_dodamo <- st_btc
          
          for(i in (vstop+1):(izstop-1)){
            vstop <- i
            if((cena[i,] < cena_vstop - st_enot*pol_N) & (st_enot < 4) & (pozicija < money)){
              razlika <- abs(cena[i,] - cena_ko_dodamo[length(cena_ko_dodamo)])
              dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
              do_milijon <- money - pozicija
              enote <- min(do_milijon, unit(money, tabela$spr_tedenski_N[i])*dodamo)
              if(st_enot + dodamo <= 4){
                st_enot <- st_enot + dodamo
                st_btc_dodamo <- c(st_btc_dodamo, (enote/cena[i,]))
                stop_loss <- stop_loss - pol_N*dodamo
                kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
                pozicija <- pozicija + enote
              }
              else{
                dodamo <- ifelse(st_enot==2, 2, 1)
                st_enot <- st_enot + dodamo
                st_btc_dodamo <- c(st_btc_dodamo, (enote/cena[i,]))
                stop_loss <- stop_loss - pol_N*dodamo
                kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
                pozicija <- pozicija + enote
              }
            }
            else{
              if(cena[i,] >= stop_loss){
                vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
                kdaj_profit <- c(kdaj_profit, i)
                for(j in 1:length(st_btc_dodamo)){
                  profit <- profit + (cena_ko_dodamo[j] - cena[i,])*st_btc_dodamo[j]
                }
                profit1 <- c(profit1, profit)
                break}
            }
          }
        }}
      if(vstop == (izstop-1)){
        vstop <- vstop + 1
        for(j in 1:length(st_btc_dodamo)){
          profit <- profit + (abs(cena_izstop - cena_ko_dodamo[j]))*st_btc_dodamo[j]
        }
        profit1 <- c(profit1, profit)
        kdaj_profit <- c(kdaj_profit, izstop)
        vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
      }
      #if(profit <= -100000){money <- 0.8*money}
    }
    profit
  }}



trgovanje <- function(tabela, zacetni_kapital, cena, add, sl, indikator){
  kandidati <- which(tabela$entry == 1 | tabela$entry == 2)
  if(indikator == "zelve_s1"){
    kandidati_s2 <- which(tabela$entry_s2 == 1 | tabela$entry_s2 == 2)
  }
  vstop <- kandidati[1]
  if(is.na(vstop)){profit <- 0}
  else{
    kdaj_vstopali <- c()
    kdaj_dodali_enote <- c()
    profit <- 0
    profit1 <- c()
    porabljen_kapital <- c()
    kdaj_profit <- c()
    money <- zacetni_kapital
    
    while(nrow(tabela) - vstop > 0){
      kdaj_vstopali <- c(kdaj_vstopali, vstop)
      cena_vstop <- cena[vstop,]
      pozicija <- min(money, unit(money, tabela$tedenski_N[vstop])*cena_vstop)
      st_btc <- floor(pozicija/cena_vstop)
      
      #Buy signal
      if(indikator == "SinR"){
        pogoj <- ((tabela$entry[vstop]== 1 & tabela$linija[vstop] == 1) | (tabela$entry[vstop]== 1 & tabela$linija[vstop] == 2))}
      else{
        pogoj <- (tabela$entry[vstop]== 1)
      }
      if(pogoj){
        izstop <- ifelse((tabela$izstop[vstop] - i_izstop > nrow(tabela)) | 
                           (is.na(tabela$izstop[vstop] - i_izstop > nrow(tabela))), nrow(tabela), tabela$izstop[vstop] - i_izstop)
        cena_izstop <- cena[izstop,]
        st_enot <- 1
        stop_loss <- cena_vstop - sl*tabela$tedenski_N[vstop]
        pol_N <- add*tabela$tedenski_N[vstop]
        cena_ko_dodamo <- cena_vstop
        st_btc_dodamo <- st_btc
        for(i in (vstop+1):(izstop-1)){
          vstop <- i
          if((cena[i,] > (cena_vstop + st_enot*pol_N)) & (st_enot < 4) & (pozicija < money)){
            razlika <- cena[i,] - cena_ko_dodamo[length(cena_ko_dodamo)]
            dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
            do_milijon <- money - pozicija
            denar <- min(do_milijon, st_btc*dodamo*cena_vstop)
            enote <- floor(denar/cena_vstop)
            if(st_enot + dodamo <= 4){
              st_enot <- st_enot + dodamo
              st_btc_dodamo <- c(st_btc_dodamo, enote)
              stop_loss <- stop_loss + pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
              pozicija <- pozicija + denar
            }
            else{
              dodamo <- ifelse(st_enot==2, 2, 1)
              st_enot <- st_enot + dodamo
              st_btc_dodamo <- c(st_btc_dodamo, enote)
              stop_loss <- stop_loss + pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
              pozicija <- pozicija + denar
            }
          }
          else{
            if(cena[i,] <= stop_loss){
              kdaj_profit <- c(kdaj_profit, i)
              vsota <- 0
              for(j in 1:length(st_btc_dodamo)){
                vsota <- vsota + (cena[i,] - cena_ko_dodamo[j])*st_btc_dodamo[j]
              }
              profit <- profit + vsota
              profit1 <- c(profit1, profit)
              porabljen_kapital <- c(porabljen_kapital, pozicija)
              if(indikator == "zelve_s1" & (vsota > 0)){
                vstop <- ifelse(is.na(kandidati_s2[kandidati_s2 > vstop][1]), nrow(tabela), kandidati_s2[kandidati_s2 > vstop][1])
              }
              else{
                vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
              }
              break}
          }
        }
      }
      else{
        if(indikator == "SinR"){
          pogoj <- ((tabela$entry[vstop]== 2 & tabela$linija[vstop] == 2) | (tabela$entry[vstop]== 2 & tabela$linija[vstop] == 1))
        }
        else{
          pogoj1 <- (tabela$entry[vstop]== 2)
        }
        # Sell signal
        if(pogoj1){
          izstop <- ifelse(tabela$izstop[vstop] - i_izstop > nrow(tabela)  | 
                             is.na(tabela$izstop[vstop] - i_izstop > nrow(tabela)), nrow(tabela), tabela$izstop[vstop] - i_izstop)
          cena_izstop <- cena[izstop,]
          st_enot <- 1
          stop_loss <- cena_vstop + sl*tabela$tedenski_N[vstop]
          pol_N <- add*tabela$tedenski_N[vstop]
          cena_ko_dodamo <- cena_vstop
          st_btc_dodamo <- st_btc
          for(i in (vstop+1):(izstop-1)){
            vstop <- i
            if((cena[i,] < cena_vstop - st_enot*pol_N) & (st_enot < 4) & (pozicija < money)){
              razlika <- cena[i,] - cena_ko_dodamo[length(cena_ko_dodamo)]
              dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
              do_milijon <- money - pozicija
              denar <- min(do_milijon, st_btc*dodamo*cena_vstop)
              enote <- floor(denar/cena_vstop)
              if(st_enot + dodamo <= 4){
                st_enot <- st_enot + dodamo
                st_btc_dodamo <- c(st_btc_dodamo, enote)
                stop_loss <- stop_loss - pol_N*dodamo
                kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
                pozicija <- pozicija + denar
              }
              else{
                dodamo <- ifelse(st_enot==2, 2, 1)
                st_enot <- st_enot + dodamo
                st_btc_dodamo <- c(st_btc_dodamo, enote)
                stop_loss <- stop_loss - pol_N*dodamo
                kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
                pozicija <- pozicija + denar
              }
            }
            else{
              if(cena[i,] >= stop_loss){
                kdaj_profit <- c(kdaj_profit, i)
                vsota <- 0
                for(j in 1:length(st_btc_dodamo)){
                  vsota <- vsota + (cena_ko_dodamo[j] - cena[i,])*st_btc_dodamo[j]
                }
                profit <- profit + vsota
                profit1 <- c(profit1, profit)
                porabljen_kapital <- c(porabljen_kapital, pozicija)
                if(indikator == "zelve_s1" & (vsota > 0)){
                  vstop <- ifelse(is.na(kandidati_s2[kandidati_s2 > vstop][1]), nrow(tabela), kandidati_s2[kandidati_s2 > vstop][1])
                }
                else{
                  vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
                }
                break}
            }
          }
        }}
      if(vstop == (izstop-1)){
        vstop <- vstop + 1
        vsota <- 0
        for(j in 1:length(st_btc_dodamo)){
          vsota <- vsota + (abs(cena_izstop - cena_ko_dodamo[j]))*st_btc_dodamo[j]
        }
        profit <- profit + vsota
        profit1 <- c(profit1, profit)
        porabljen_kapital <- c(porabljen_kapital, pozicija)
        kdaj_profit <- c(kdaj_profit, izstop)
        if(indikator == "zelve_s1" & (vsota > 0)){
          vstop <- ifelse(is.na(kandidati_s2[kandidati_s2 > vstop][1]), nrow(tabela), kandidati_s2[kandidati_s2 > vstop][1])
        }
        else{
          vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
        }
      }
      #if(profit <= -100000){money <- 0.8*money}
    }
    profit
    #data.frame(Profit = profit1, kdaj = kdaj_profit)
    #tab_vstop <- data.frame("kdaj" = kdaj_vstopali, "kaj" = rep(1, length(kdaj_vstopali)))
    #tab_dodajali <- data.frame("kdaj" = kdaj_dodali_enote, "kaj" = rep(2, length(kdaj_dodali_enote)))
    #tab_izstop <- data.frame("kdaj" = kdaj_profit, "kaj" = rep(3, length(kdaj_profit)))
    #tab <- rbind(tab_vstop, tab_dodajali, tab_izstop)
    #tab[order(tab$kdaj),]
  }}


število enot (st_enot) ni nujno pravo število enot v poziciji, saj se lahko zaradi omejtve denarja na 
1.000.000 količina enot zmanjša (v for zanki je še vedno st_enot <- st_enot + dodamo). To potrebujemo,
zaradi omejitve enot na 4. Če pa slučajno omejitev denarja prevlada pa je to še ena potrditev, da smo 
"napolnili" pozicijo.

