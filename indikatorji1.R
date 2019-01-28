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
  cena1 <- odlocitev_cena(signali, "Close")
  signali$izstop <- win_izstop_MA(signali, cena1, rr)
  signali
}

signali_indi <- vstopni_signali(btc_1day_vol)

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

INDI_pregled_btc <- pregled_trgovanje(INDI_dobicki_btc_360, INDI_dobicki_btc_500, INDI_dobicki_btc_1000, 
                                      INDI_dobicki_btc_1800)

flextabela_pregled(INDI_pregled_btc, 0)

#################
# 2. strategija #
#################

# gledamo v časovnemu oknu treh dni

#############
# uspešnost #
#############

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
    ratio[1, i] <- (dobitki/izgube)
  }
  ratio <- data.frame(ratio)
  colnames(ratio) <- indi
  ratio
}

ratio <- uspesnost_indi(btc_1day_vol)
utezi <- ratio/max(ratio[1, ])






