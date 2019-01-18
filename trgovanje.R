
###########
# Trading #
###########

#izračuna True Range in N
spr_N <- function(tabela, dnevi){
  tr <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    tr <- append(tr, max(h-l, h-pdc, pdc-l))
  }
  tabela$TR <- c(0,tr)
  tabela <- tabela[-1,]
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  tabela$spr_N <- n
  tabela
}

unit <- function(account_size, N){
  floor((0.01*account_size)/N)
}


#N so dobili izračunan na začetku tedna
spr_tedenski_N <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    t_N[i:(i+6)] <- tabela$spr_N[i]
  }
  t_N[1:nrow(tabela)]
}



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
      st_btc <- unit(money, tabela$spr_tedenski_N[vstop])/cena_vstop
      
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
          if((cena[i,] > cena_vstop + st_enot*pol_N) & (st_enot < 4)){
            razlika <- cena[i,] - cena_ko_dodamo[length(cena_ko_dodamo)]
            dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
            if(st_enot + dodamo <= 4){
              st_enot <- st_enot + dodamo
              st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/cena[i,])*dodamo)
              stop_loss <- stop_loss + pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
            }
            else{
              dodamo <- ifelse(st_enot==2, 2, 1)
              st_enot <- st_enot + dodamo
              st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/cena[i,])*dodamo)
              stop_loss <- stop_loss + pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
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
            if((cena[i,] < cena_vstop - st_enot*pol_N) & (st_enot < 4)){
              razlika <- abs(cena[i,] - cena_ko_dodamo[length(cena_ko_dodamo)])
              dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
              if(st_enot + dodamo <= 4){
                st_enot <- st_enot + dodamo
                st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/cena[i,])*dodamo)
                stop_loss <- stop_loss - pol_N*dodamo
                kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
              }
              else{
                dodamo <- ifelse(st_enot==2, 2, 1)
                st_enot <- st_enot + dodamo
                st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/cena[i,])*dodamo)
                stop_loss <- stop_loss - pol_N*dodamo
                kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
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

odlocitev_cena <- function(tabela, cena){
  if(cena == "Close"){cena1 <- data.frame(tabela$Close)}
  if(cena == "Low"){cena1 <- data.frame(tabela$Low)}
  if(cena == "High"){cena1 <- data.frame(tabela$High)}
  if(cena == "Open"){cena1 <- data.frame(tabela$Open)}
  cena1
}

tabela <- btc_1day_vol

poracuni <- function(tabela, dnevi_N, cena, dnevi_ema1, dnevi_ema2, toleranca, rr, indikator){
  library(QuantTools)
  tabela <- tabela[-nrow(tabela),]
  if(indikator == "MA"){
    tab <- spr_N(tabela, dnevi_N)
    cena1 <- odlocitev_cena(tab, cena)
    tab$ma1 <- ema(cena1[,1], dnevi_ema1)
    tab$ma2 <- ema(cena1[,1], dnevi_ema2)
    tab$entry <- vstop_MA(tab, cena1)
  }
  if(indikator == "MA1"){
    tab <- spr_N(tabela, dnevi_N)
    cena1 <- odlocitev_cena(tab, cena)
    tab$ma1 <- sma(cena1[,1], dnevi_ema1)
    tab$ma2 <- sma(cena1[,1], dnevi_ema2)
    tab$entry <- vstop_MA(tab, cena1)
  }
  if(indikator == "adx"){
    tab <- N_adx(tabela, dnevi_N)
    tab$entry <- vstop_ADX(tab)
  }
  if(indikator == "vi"){
    tab <- N_vix(tabela, dnevi_N)
    tab$entry <- vstop_VI(tab)
  }
  if(indikator == "rsi"){
    tab <- N_rsi(tabela, dnevi_N)
    tab$entry <- vstop_RSI(tab)
  }
  if(indikator == "roc"){
    tab <- N_roc(tabela, dnevi_N)
    cena1 <- odlocitev_cena(tab, cena)
    tab$ma <- sma(cena1[,1], 20)
    tab$ma[is.na(tab$ma)] <- 0
    tab$entry <- vstop_ROC(tab, cena1)
  }
  if(indikator == "so"){
    tab <- N_so(tabela, dnevi_N)
    tab$entry <- vstop_SO(tab)
  }
  if(indikator == "ppo"){
    tab <- N_ppo(tabela, dnevi_N)
    tab$entry <- vstop_PPO(tab)
  }
  if(indikator == "fi"){
    tab <- N_fi(tabela, dnevi_N)
    tab$entry <- vstop_FI(tab)
  }
  if(indikator == "mfi"){
    tab <- N_mfi(tabela, dnevi_N)
    tab$entry <- vstop_MFI(tab)
  }
  if(indikator == "co"){
    tab <- N_co(tabela, dnevi_N)
    tab$entry <- vstop_CO(tab)
  }
  tab$spr_tedenski_N <- spr_tedenski_N(tab)
  cena1 <- odlocitev_cena(tab, cena)
  tab$izstop <- win_izstop_MA(tab, cena1, toleranca, rr)
  pomoc <- which(win_izstop_MA(tab, cena1, toleranca, rr) > 0)
  tab$izstop[pomoc] <- tab$izstop[pomoc] - 22 + 1   #to velja samo za btc_1day
  tab <- tab[tab$spr_tedenski_N > 0,]
  tab
}


#cena <- "Close"
dobicki_trgovanje <- function(tabela, dnevi_N = 20, obdobje = 1800, zacetni_kapital = 1000000, 
                              cena = "Close", add = 1/2, sl = 2, toleranca = 0.02, rr = 3, dnevi_ema1 = 10,
                              dnevi_ema2 = 50, indikator = "MA"){
  tab <- poracuni(tabela, dnevi_N, cena, dnevi_ema1, dnevi_ema2, toleranca, rr, indikator)
  dobicki <- c()
  for(i in 1:(nrow(tab) - obdobje + 1)){
    tmp <- tab[i:(obdobje + i - 1),]
    cena2 <- odlocitev_cena(tmp, cena)
    i_izstop <- (i-1)
    dobicki <- c(dobicki, trgovanje(tmp, zacetni_kapital, cena2, add, sl))
  }
  dobicki
}


## Za optimizacijo
spr_dobicki_trgovanje <- function(tabela, dnevi_N = 20, obdobje = 1800, zacetni_kapital = 1000000, 
                              cena = "Close", add = 1/2, sl = 2, toleranca = 0.02, rr = 3, dnevi_ema1 = 10,
                              dnevi_ema2 = 50, indikator = "MA"){
  tab <- poracuni(tabela, dnevi_N, cena, dnevi_ema1, dnevi_ema2, toleranca, rr, indikator)
  dobicki <- c()
  for(i in 1:(nrow(tab) - obdobje + 1)){
    tmp <- tab[i:(obdobje + i - 1),]
    cena2 <- odlocitev_cena(tmp, cena)
    i_izstop <- (i-1)
    dobicki <- c(dobicki, trgovanje(tmp, zacetni_kapital, cena2, add, sl))
  }
  c(mean(dobicki), sd(dobicki))
}

spr <- function(tabela, name){
  if(name == "ma"){tmp <- cbind("short/long" = rownames(tabela), tabela)}
  tmp
}
