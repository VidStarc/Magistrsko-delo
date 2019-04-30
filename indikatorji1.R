pregled_indikatorji <- function(l1, l2, l3){
  l1 <- l1*1000
  l2 <- l2*1000
  l3 <- l3*1000
  st_podatkov <- c(length(l1), length(l2), length(l3))
  povprecje <- c(mean(l1), mean(l2), mean(l3))
  sd <- c(sd(l1), sd(l2), sd(l3))
  min <- c(min(l1), min(l2), min(l3))
  max <- c(max(l1), max(l2), max(l3))
  lsd <- c(paste0(round(cagr(tabela = mean(l1), obdobje = 360), 2), " %"),
           paste0(round(cagr(tabela = mean(l2), obdobje = 360), 2), " %"),
           paste0(round(cagr(tabela = mean(l3), obdobje = 360), 2), " %"))
  s_e <- c("360", "360", "360")
  manj_0 <- c(sum(l1 < 0), sum(l2 < 0), sum(l3 < 0))
  verj <- paste0(round((manj_0/st_podatkov)*100, 2), " %")
  kolicnik <- as.character(round(povprecje/sd,2))
  pregled <- data.frame("obdobje" = s_e, "st_podatkov" = st_podatkov, "povprecje" = povprecje, 
                        "sd" = sd, "min" = min, "max" = max, "lsd" = lsd, "kolicnik" = kolicnik,
                        "st_manj_0" = manj_0, "verj_izgube" = verj)
  pregled
}

krizanje_crt <- pregled_indikatorji(ADX_dobicki_btc_360, VIX_dobicki_btc_360, PPO_dobicki_btc_360)
oversold_overbought <- pregled_indikatorji(RSI_dobicki_btc_360, ROC_dobicki_btc_360, SO_dobicki_btc_360)
se_volume <- pregled_indikatorji(FI_dobicki_btc_360, MFI_dobicki_btc_360, CO_dobicki_btc_360)

indikatorji <- rbind(krizanje_crt, oversold_overbought, se_volume)
imena <- c("ADX", "VIX", "PPO", "RSI", "ROC", "SO", "FI", "MFI", "CO")
indikatorji <- data.frame("indikatorji" = imena, indikatorji[,-1])

flextabela_pregled(indikatorji, 0)

#################
# 1. strategija #
#################

# buy, če > 1 index da buy signal. sell če > 1 index da sell signal
vstopni_signali <- function(tabela, toleranca = 0.02, rr = 3, dnevi_N = 20, dnevi_ema1 = 10, 
                            dnevi_ema2 = 50, cena = "Close"){
  indi <- c("adx", "vi", "ppo", "rsi", "roc", "so", "fi", "mfi", "co")
  signali <- poracuni(tabela, dnevi_N, cena, dnevi_ema1, dnevi_ema2, toleranca, rr, "MA")
  signali <- signali[, c(1:6, 8, 12)]
  for(i in 1:length(indi)){
    tab <- poracuni(tabela, dnevi_N, cena, dnevi_ema1, dnevi_ema2, toleranca, rr, indi[i])
    signali <- data.frame(signali, tab$entry)
  }
  colnames(signali)[9:17] <- indi
  
  short <- rep(0, nrow(signali))
  long <- rep(0, nrow(signali))
  for(i in 1:nrow(signali)){
    short[i] <- sum(signali[i, 9:ncol(signali)] == 2)
    long[i] <- sum(signali[i, 9:ncol(signali)] == 1)
  }
  signali$long <- long
  signali$short <- short
  
  entry <- rep(0, nrow(signali))
  for(i in 1:nrow(signali)){
    if(signali$long[i] > 1 & signali$short[i] == 0){
      entry[i] <- 1
    }
    if(signali$short[i] > 1 & signali$long[i] == 0){
      entry[i] <- 2
    }
  }
  signali$entry <- entry
  signali
}

signali_indi <- vstopni_signali(btc_1day_vol)
cena1 <- odlocitev_cena(signali_indi, "Close")
signali_indi$izstop <- win_izstop_MA(signali_indi, cena1, rr)
#signali_indi$izstop <- win_izstop_rr(signali_indi, cena1, rr)

dobicki_indi <- function(tabela, obdobje = 1800, zacetni_kapital = 1000000, cena = "Close", 
                         add = 1/2, sl = 2){
  tab <- tabela
  dobicki <- c()
  for(i in 1:(nrow(tab) - obdobje + 1)){
    tmp <- tab[i:(obdobje + i - 1),]
    cena2 <- odlocitev_cena(tmp, cena)
    i_izstop <- (i-1)
    dobicki <- c(dobicki, trgovanje(tmp, zacetni_kapital, cena2, add, sl))
  }
  dobicki
}

INDI_dobicki_btc_360 <- dobicki_indi(signali_indi, obdobje = 360)
INDI_dobicki_btc_500 <- dobicki_indi(signali_indi, obdobje = 500)
INDI_dobicki_btc_1000 <- dobicki_indi(signali_indi, obdobje = 1000)
INDI_dobicki_btc_1800 <- dobicki_indi(signali_indi, obdobje = 1800)

INDI_dobicki_btc_360 <- INDI_dobicki_btc_360/1000
INDI_dobicki_btc_500 <- INDI_dobicki_btc_500/1000
INDI_dobicki_btc_1000 <- INDI_dobicki_btc_1000/1000
INDI_dobicki_btc_1800 <- INDI_dobicki_btc_1800/1000

# pregled_trgovanje iz TA_pregled.R
INDI_pregled_btc <- pregled_trgovanje(INDI_dobicki_btc_360, INDI_dobicki_btc_500, INDI_dobicki_btc_1000, 
                                      INDI_dobicki_btc_1800)

flextabela_pregled(INDI_pregled_btc, 0)

#################
# 2. strategija #
#################

# gledamo v časovnemu oknu treh dni

# za uteži

dobicki_pozicij <- function(tabela, zacetni_kapital, cena, add, sl){
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
    profit1
  }}

# funkcije poracuni, odlocitev_cena je iz trgovanje.R
uspesnost_indi <- function(tabela, tabela1, dnevi_N = 20, cena = "Close", dnevi_ema1 = 10, dnevi_ema2 = 50, 
                           toleranca = 0.02, rr = 3, zacetni_kapital = 1000000, add = 0.5,
                           sl = 2){
  indi <- c("adx", "vi", "ppo", "rsi", "roc", "so", "fi", "mfi", "co")
  ratio <- matrix(0, 1, 9)
  for(i in 1:length(indi)){
    tab <- poracuni(tabela, dnevi_N, cena, dnevi_ema1, dnevi_ema2, toleranca, rr, indi[i])
    cena1 <- odlocitev_cena(tab, cena)
    profit1 <- dobicki_pozicij(tab, zacetni_kapital, cena1, add, sl)
    dobicki_poslov <- c(profit1[1], profit1[2:length(profit1)] - profit1[1:(length(profit1)-1)])
    dobicki_poslov[is.na(dobicki_poslov)] <- 0
    izgube <- -sum(dobicki_poslov[dobicki_poslov < 0])
    dobitki <- sum(dobicki_poslov[dobicki_poslov > 0])
    if(izgube == 0){izgube <- 1}
    ratio[1, i] <- (dobitki/izgube)
  }
  ratio <- data.frame(ratio)
  colnames(ratio) <- indi
  ratio
}

ratio <- uspesnost_indi(btc_1day_vol)
utezi <- ratio/max(ratio[1, ])

# pomnožimo matriko vstopnih signalov z utežmi
indi_utezi <- signali_indi[, -(ncol(signali_indi)-3):-ncol(signali_indi)]
A <- as.matrix(indi_utezi[, 9:17])
A[A > 1] <- 1 
library(dplyr)
X <- A %*% t(as.matrix(utezi))
indi_utezi$X <- round(X, 2)

# določimo entry in izstop
short <- rep(0, nrow(indi_utezi))
long <- rep(0, nrow(indi_utezi))
for(i in 1:nrow(indi_utezi)){
  short[i] <- sum(indi_utezi[i, 9:ncol(indi_utezi)] == 2)
  long[i] <- sum(indi_utezi[i, 9:ncol(indi_utezi)] == 1)
}
indi_utezi$long <- long
indi_utezi$short <- short

entry <- rep(0, nrow(indi_utezi))
for(i in 1:(nrow(indi_utezi)-2)){
  l <- 0
  u <- 0
  s <- 0
  for(j in i:(i+2)){
    l <- l + indi_utezi$long[j]
    u <- u + indi_utezi$X[j]
    s <- s + indi_utezi$short[j]
    if((l > 1) & (u > 0.1) & (s == 0)){
      entry[j] <- 1
      break
    }
  }
  s1 <- 0
  u1 <- 0
  l1 <- 0
  for(j in i:(i+2)){
    s1 <- s1 + indi_utezi$short[j]
    u1 <- u1 + indi_utezi$X[j]
    l1 <- l1 + indi_utezi$long[j]
    if((s1 > 1) & (u1 > 0.1) & (l1 == 0)){
      entry[j] <- 2
      break
    }
  }
}
indi_utezi$entry <- entry
cena1 <- odlocitev_cena(indi_utezi, "Close")
indi_utezi$izstop <- win_izstop_MA(indi_utezi, cena1, rr)
#indi_utezi$izstop <- win_izstop_rr(indi_utezi, cena1, rr)

indi2_dobicki_btc_360 <- dobicki_indi(indi_utezi, obdobje = 360)
indi2_dobicki_btc_500 <- dobicki_indi(indi_utezi, obdobje = 500)
indi2_dobicki_btc_1000 <- dobicki_indi(indi_utezi, obdobje = 1000)
indi2_dobicki_btc_1800 <- dobicki_indi(indi_utezi, obdobje = 1800)

indi2_dobicki_btc_360 <- indi2_dobicki_btc_360/1000
indi2_dobicki_btc_500 <- indi2_dobicki_btc_500/1000
indi2_dobicki_btc_1000 <- indi2_dobicki_btc_1000/1000
indi2_dobicki_btc_1800 <- indi2_dobicki_btc_1800/1000

# pregled_trgovanje iz TA_pregled.R
indi2_pregled_btc <- pregled_trgovanje(indi2_dobicki_btc_360, indi2_dobicki_btc_500, indi2_dobicki_btc_1000, 
                                       indi2_dobicki_btc_1800)

flextabela_pregled(indi2_pregled_btc, 0)


#indi_zelve <- dobicki_indi(indi_utezi, obdobje = 360)
#indi_zelve <- indi_zelve/1000
#indi_rr <- dobicki_indi(indi_utezi, obdobje = 360)
#indi_rr <- indi_rr/1000
#indi_ch <- dobicki_indi(indi_utezi, obdobje = 360)
#indi_ch <- indi_ch/1000


###############################################################################################
vstopni_signali1 <- function(tabela, toleranca = 0.02, rr = 3, dnevi_N = 20, dnevi_ema1 = 10, 
                            dnevi_ema2 = 50, cena = "Close"){
  indi <- c("adx", "vi", "ppo", "fi", "mfi", "co")
  signali <- poracuni(tabela, dnevi_N, cena, dnevi_ema1, dnevi_ema2, toleranca, rr, "MA")
  signali <- signali[, c(1:6, 8, 12)]
  for(i in 1:length(indi)){
    tab <- poracuni(tabela, dnevi_N, cena, dnevi_ema1, dnevi_ema2, toleranca, rr, indi[i])
    signali <- data.frame(signali, tab$entry)
  }
  colnames(signali)[9:14] <- indi
  
  short <- rep(0, nrow(signali))
  long <- rep(0, nrow(signali))
  for(i in 1:nrow(signali)){
    short[i] <- sum(signali[i, 9:ncol(signali)] == 2)
    long[i] <- sum(signali[i, 9:ncol(signali)] == 1)
  }
  signali$long <- long
  signali$short <- short
  
  entry <- rep(0, nrow(signali))
  for(i in 1:nrow(signali)){
    if(signali$long[i] > 1 & signali$short[i] == 0){
      entry[i] <- 1
    }
    if(signali$short[i] > 1 & signali$long[i] == 0){
      entry[i] <- 2
    }
  }
  signali$entry <- entry
  signali
}

signali_indi1 <- vstopni_signali1(btc_1day_vol)
cena1 <- odlocitev_cena(signali_indi1, "Close")
signali_indi1$izstop <- win_izstop_MA(signali_indi1, cena1, rr)

uspesnost_indi1 <- function(tabela, tabela1, dnevi_N = 20, cena = "Close", dnevi_ema1 = 10, dnevi_ema2 = 50, 
                           toleranca = 0.02, rr = 3, zacetni_kapital = 1000000, add = 0.5,
                           sl = 2){
  indi <- c("adx", "vi", "ppo", "fi", "mfi", "co")
  ratio <- matrix(0, 1, 6)
  for(i in 1:length(indi)){
    tab <- poracuni(tabela, dnevi_N, cena, dnevi_ema1, dnevi_ema2, toleranca, rr, indi[i])
    cena1 <- odlocitev_cena(tab, cena)
    profit1 <- dobicki_pozicij(tab, zacetni_kapital, cena1, add, sl)
    dobicki_poslov <- c(profit1[1], profit1[2:length(profit1)] - profit1[1:(length(profit1)-1)])
    dobicki_poslov[is.na(dobicki_poslov)] <- 0
    izgube <- -sum(dobicki_poslov[dobicki_poslov < 0])
    dobitki <- sum(dobicki_poslov[dobicki_poslov > 0])
    ratio[1, i] <- (dobitki/izgube)
  }
  ratio <- data.frame(ratio)
  colnames(ratio) <- indi
  ratio
}

ratio <- uspesnost_indi1(btc_1day_vol)
utezi <- ratio/max(ratio[1, ])

# pomnožimo matriko vstopnih signalov z utežmi
indi_utezi1 <- signali_indi1[, -(ncol(signali_indi1)-3):-ncol(signali_indi1)]
A <- as.matrix(indi_utezi1[, 9:14])
A[A > 1] <- 1 
library(dplyr)
X <- A %*% t(as.matrix(utezi))
indi_utezi1$X <- round(X, 2)

# določimo entry in izstop
short <- rep(0, nrow(indi_utezi1))
long <- rep(0, nrow(indi_utezi1))
for(i in 1:nrow(indi_utezi1)){
  short[i] <- sum(indi_utezi1[i, 9:ncol(indi_utezi1)] == 2)
  long[i] <- sum(indi_utezi1[i, 9:ncol(indi_utezi1)] == 1)
}
indi_utezi1$long <- long
indi_utezi1$short <- short

entry <- rep(0, nrow(indi_utezi1))
for(i in 1:(nrow(indi_utezi1)-2)){
  l <- 0
  u <- 0
  s <- 0
  for(j in i:(i+2)){
    l <- l + indi_utezi1$long[j]
    u <- u + indi_utezi1$X[j]
    s <- s + indi_utezi1$short[j]
    if((l > 1) & (u > 0.1) & (s == 0)){
      entry[j] <- 1
      break
    }
  }
  s1 <- 0
  u1 <- 0
  l1 <- 0
  for(j in i:(i+2)){
    s1 <- s1 + indi_utezi1$short[j]
    u1 <- u1 + indi_utezi1$X[j]
    l1 <- l1 + indi_utezi1$long[j]
    if((s1 > 1) & (u1 > 0.1) & (l1 == 0)){
      entry[j] <- 2
      break
    }
  }
}
indi_utezi1$entry <- entry
cena1 <- odlocitev_cena(indi_utezi1, "Close")
indi_utezi1$izstop <- win_izstop_MA(indi_utezi1, cena1, rr)

indi3_dobicki_btc_360 <- dobicki_indi(indi_utezi1, obdobje = 360)
indi3_dobicki_btc_500 <- dobicki_indi(indi_utezi1, obdobje = 500)
indi3_dobicki_btc_1000 <- dobicki_indi(indi_utezi1, obdobje = 1000)
indi3_dobicki_btc_1800 <- dobicki_indi(indi_utezi1, obdobje = 1800)

indi3_dobicki_btc_360 <- indi3_dobicki_btc_360/1000
indi3_dobicki_btc_500 <- indi3_dobicki_btc_500/1000
indi3_dobicki_btc_1000 <- indi3_dobicki_btc_1000/1000
indi3_dobicki_btc_1800 <- indi3_dobicki_btc_1800/1000

# pregled_trgovanje iz TA_pregled.R
indi3_pregled_btc <- pregled_trgovanje(indi3_dobicki_btc_360, indi3_dobicki_btc_500, indi3_dobicki_btc_1000, 
                                       indi3_dobicki_btc_1800)

flextabela_pregled(indi3_pregled_btc, 0)
