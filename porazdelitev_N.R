
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


ohlc <- tabela[, 2:5]
library(TTR)
hist(volatility(ohlc, n = 20, N = 365 , calc = "close"))
hist(volatility(ohlc, n = 20, N = 365 , calc = "yang.zhang"))


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
