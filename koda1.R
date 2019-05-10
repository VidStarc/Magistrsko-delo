
#################################
# Compaunded annual growth rate #
#################################

cagr <- function(tabela, zacetni_kapital = 1000000, obdobje = 1800){
  tabela <- round(((tabela/zacetni_kapital+1)^(365/obdobje)-1)*100, 2)
  tabela
}


#####################
# količnik v tabeli #
#####################

dodan_kolicnik <- function(tabela){
  kolicnik <- c()
  for(i in 1:ncol(tabela)){
    kolicnik <- c(kolicnik, round(tabela[1, i]/tabela[2, i], 2))
  }
  rbind(tabela, "kolicnik" = kolicnik)
}


######################################
# število dobičkov nad določeno mejo #
######################################

nad_5 <- c(length(btc_360_S1[btc_360_S1 > 5000000]), 
             length(btc_500_S1[btc_500_S1 > 5000000]), 
             length(btc_1000_S1[btc_1000_S1 > 5000000]), 
             length(btc_1800_S1[btc_1800_S1 > 5000000]))
nad_10 <- c(length(btc_360_S1[btc_360_S1 > 10000000]), 
             length(btc_500_S1[btc_500_S1 > 10000000]), 
             length(btc_1000_S1[btc_1000_S1 > 10000000]), 
             length(btc_1800_S1[btc_1800_S1 > 10000000]))
nad_15 <- c(length(btc_360_S1[btc_360_S1 > 15000000]), 
             length(btc_500_S1[btc_500_S1 > 15000000]), 
             length(btc_1000_S1[btc_1000_S1 > 15000000]), 
             length(btc_1800_S1[btc_1800_S1 > 15000000]))
nad_20 <- c(length(btc_360_S1[btc_360_S1 > 20000000]), 
             length(btc_500_S1[btc_500_S1 > 20000000]), 
             length(btc_1000_S1[btc_1000_S1 > 20000000]), 
             length(btc_1800_S1[btc_1800_S1 > 20000000]))
veliki_dobicki <- data.frame(obdobje = c(360, 500, 1000, 1800), 
                             "nad_5" = nad_5, "nad_10" = nad_10, "nad_15" = nad_15, "nad_20" = nad_20)


izpisTabele <- function(data){
  library(flextable)
  library(dplyr)
  ime <- c("obdobje", "nad_5", "nad_10", "nad_15", "nad_20")
  for(i in ime){
    if(i %in% (data %>% names())) {
      data[i] <- format(round(data[i]), big.mark = ".", decimal.mark = ",")  
    }
  }
  ime <- c("[1, q1]", "(q1, q2]", "(q2, q3]", "(q3, q4]")
  for(i in ime){
    if(i %in% (data %>% names())) {
      tmp <- format(round(data[i], 2),  big.mark = ".", decimal.mark = ",") %>% pull(1)
      data[i] <- paste0(tmp, "%")
    }
  }
  ime <- c()
  data %>% regulartable() %>% bg(bg = "coral", part = "header") %>% bg(bg = "cyan", part = "body") %>%
    bold(part = "header") %>% align(align="center", part="header") %>% align(align="right", part="body")
}

veliki_dobicki %>% izpisTabele()


#################################
# povprečni donosi med kvantili #
#################################

q_donosi <- function(vrednosti, obdobje){
  n <- length(vrednosti)
  q <- c(1, n/4, n/2, 3*n/4, n)
  u <- sort(vrednosti)
  donos <- c()
  for(i in 2:length(q)){
    donos <- c(donos, cagr(mean(u[q[1]:q[i]]), obdobje = obdobje))
  }
  kvartili <- c("obdobje", "[1, q1]", "(q1, q2]", "(q2, q3]", "(q3, q4]")
  tab <- data.frame("obdobje" = obdobje, t(donos))
  colnames(tab) <- kvartili
  tab
}

q_btc_donosi <- rbind(q_donosi(btc_360_S1, 360), q_donosi(btc_500_S1, 500), q_donosi(btc_1000_S1, 1000), 
                      q_donosi(btc_1800_S1, 1800))
q_btc_donosi %>% izpisTabele()


########################
# 60% in 90% intervali #
########################

izracun_int <- function(povprecje, frekvenca, verj){
  zacetek <- ceiling(povprecje)
  vsota <- frekvenca$Freq[zacetek]
  i <- 1
  while(round(vsota,1) < verj){
    if((zacetek - i) <= 0 ){
      vsota <- vsota + frekvenca$Freq[zacetek + i]
      int1 <- 0
    }
    else{
      if((zacetek + i) >= nrow(frekvenca)){
        vsota <- vsota + frekvenca$Freq[zacetek - i]
        int2 <- nrow(frekvenca)
      }
      else{
        vsota <- vsota + frekvenca$Freq[zacetek - i] + frekvenca$Freq[zacetek + i]
        int1 <- zacetek - i
        int2 <- zacetek + i
      }
    }
    i <- i + 1
  }
  c(int1, int2)
}

interval <- function(vector, od, do, verj, obdobje){
  vector <- vector[od:do]
  if(od == 1){
    ifelse(obdobje == 360, range <- 29500, range <- 19000)
  }
  else{
    range <- 12000
  }
  vector <- vector/1000
  povprecje <- mean(vector)
  frekvenca <- as.data.frame(table(cut(vector, breaks=seq(0, range, 1)), useNA='ifany')/(length(vector)))
  int <- izracun_int(povprecje, frekvenca, verj)
  
  #tmp1 <- format(int[1]/1000,  big.mark = ".", decimal.mark = ",")
  #tmp2 <- format(int[2]/1000,  big.mark = ".", decimal.mark = ",")
  #interval <- paste0("[", tmp1, ",", tmp2, "]")
  interval <- paste0("[", round(int[1]/1000, 2), ",", round(int[2]/1000, 2), "]")
  
  interval
}

pregled_int <- data.frame("obdobje" = c(360, 500, 1000, 1800),
                          "vecja_0.6" = c(interval(btc_360_S1, 1, 700, 0.6, 360),
                                          interval(btc_500_S1, 1, 700, 0.6, 500),
                                          interval(btc_1000_S1, 1, 700, 0.6, 1000),
                                          interval(btc_1800_S1, 1, 400, 0.6, 1800)),
                          "manjsa_0.6" = c(interval(btc_360_S1, 701, length(btc_360_S1), 0.6, 360),
                                           interval(btc_500_S1, 701, length(btc_500_S1), 0.6, 500),
                                           interval(btc_1000_S1, 701, length(btc_1000_S1), 0.6, 1000),
                                           interval(btc_1800_S1, 401, length(btc_1800_S1), 0.6, 1800)),
                          "vecja_0.9" = c(interval(btc_360_S1, 1, 700, 0.9, 360),
                                          interval(btc_500_S1, 1, 700, 0.9, 500),
                                          interval(btc_1000_S1, 1, 700, 0.9, 1000),
                                          interval(btc_1800_S1, 1, 400, 0.9, 1800)),
                          "manjsa_0.9" = c(interval(btc_360_S1, 701, length(btc_360_S1), 0.9, 360),
                                           interval(btc_500_S1, 701, length(btc_500_S1), 0.9, 500),
                                           interval(btc_1000_S1, 701, length(btc_1000_S1), 0.9, 1000),
                                           interval(btc_1800_S1, 401, length(btc_1800_S1), 0.9, 1800)))


pregled_int %>% izpisTabele()


###############
# BTC vs. SPX #
###############

# primerjava gibanja dobička spx in btc (oba instrumenta trgujemo enako časa (od 1.1.2012 do 27.6.2018))
profit_spx <- function(tabela, dnevi_N = 20, vstop_s1 = 20, vstop_s2 = 55, izstop_s1_s2 = 10, obdobje = 1800,
                       zacetni_kapital = 1000000, cena = "Close", add = 1/2, sl = 2, toleranca = 0.02, rr = 3,
                       dnevi_ema1 = 10, dnevi_ema2 = 50, indikator = "zelve_s1", metoda_izstop = "zelve"){
  tab <- poracuni(tabela, dnevi_N, vstop_s1, vstop_s2, izstop_s1_s2, cena, dnevi_ema1, 
                  dnevi_ema2, toleranca, rr, indikator, metoda_izstop)
  cena2 <- odlocitev_cena(tab, cena)
  i_izstop <- 0
  trgovanje(tab, zacetni_kapital, cena2, add, sl, indikator, i_izstop)
}

# function trgovanje -> odkleni data.frame(Profit = profit1, kdaj = kdaj_profit)
za_graf_spx <- profit_spx(spx_1day)
za_graf_spx$Profit <- za_graf_spx$Profit/1000000
za_graf_btc <- profit_spx(btc_1day)
za_graf_btc$Profit <- za_graf_btc$Profit/1000000

tab_btc <- poracuni(btc_1day, 20, 20, 55, 10, "Close", 10, 50, 0.02, 3, "zelve_s1", "zelve")
tab_spx <- poracuni(spx_1day, 20, 20, 55, 10, "Close", 10, 50, 0.02, 3, "zelve_s1", "zelve")


dodana_rast <- function(tabela){
  rast <- c()
  for(i in 2:nrow(tabela)){
    rast <- c(rast, round(((tabela$Profit[i]/tabela$Profit[i-1])-1)*100, 2))
  }
  tabela$rast <- c(0, rast)
  tabela
}

za_graf_spx <- dodana_rast(za_graf_spx)
za_graf_btc <- dodana_rast(za_graf_btc)

# Pred letom 2014
ind <- which(za_graf_btc$kdaj < 700)
ind1 <- which(za_graf_spx$kdaj < 480)
za_graf_btc$Datum <- as.numeric(as.Date(tab_btc$Timestamp[za_graf_btc$kdaj]))
za_graf_spx$Datum <- as.numeric(as.Date(tab_spx$Date[za_graf_spx$kdaj]))
ggplot()+
  geom_line(data = za_graf_btc[ind,], 
            aes(x = Datum, y = za_graf_btc$rast[ind], color = "btc"))+
  geom_line(data = za_graf_spx[ind1,], 
            aes(x = Datum, y = za_graf_spx$rast[ind1], color = "spx"))+
  scale_color_manual(name = "", values = c("btc" = "blue", "spx" = "red"))+
  ylab("Rast dobička (v %)")+
  xlab("Trgovalni dnevi")+
  ggtitle("Rast dobička pri trgovanju BTC in SPX")+
  theme_minimal()+
  theme(legend.position = c(0.85, 0.5), legend.box.margin = margin(0, 0, -0.5, 0, "cm"), 
        plot.title = element_text(hjust = 1))+
  scale_x_continuous(breaks = seq(15400, 16000, 200), 
                     labels = c("01.03.2012", "17.09.2012", "05.04.2013", "22.10.2013"))


# po letu 2014
ind <- which(za_graf_btc$kdaj > 700)
ind1 <- which(za_graf_spx$kdaj > 480)
ggplot()+
  geom_line(data = za_graf_btc[ind,], 
            aes(x = tab_btc$Timestamp[za_graf_btc$kdaj[ind]], y = za_graf_btc$rast[ind], color = "btc"))+
  geom_line(data = za_graf_spx[ind1,], 
            aes(x = tab_spx$Date[za_graf_spx$kdaj[ind1]], y = za_graf_spx$rast[ind1], color = "spx"))+
  scale_color_manual(name = "", values = c("btc" = "blue", "spx" = "red"))+
  ylab("Rast dobicka (v %)")+
  xlab("Trgovalni dnevi")+
  ggtitle("Rast dobička pri trgovanju BTC in SPX")+
  theme_minimal()+
  theme(legend.position = c(0.15, 0.9), legend.box.margin = margin(0, 0, -0.5, 0, "cm"), 
        plot.title = element_text(hjust = 1))



######################
# Druge kriptovalute #
######################

#ETHEREUM
eth_360_S1 <- dobicki_trgovanje(eth_1day, obdobje = 360)
#eth_360_S2 <- dobicki_trgovanje(eth_1day, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 360)


#LITECOIN
ltc_360_S1 <- dobicki_trgovanje(ltc_1day, obdobje = 360)
#ltc_360_S2 <- dobicki_trgovanje(ltc_1day, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 360)


#RIPPLE
xrp_360_S1 <- dobicki_trgovanje(xrp_1day, obdobje = 360)
#xrp_360_S2 <- dobicki_trgovanje(xrp_1day, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 360)


#MONERO
xmr_360_S1 <- dobicki_trgovanje(xmr_1day, obdobje = 360)
#xmr_360_S2 <- dobicki_trgovanje(xmr_1day, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 360)


#NXT
nxt_360_S1 <- dobicki_trgovanje(nxt_1day, obdobje = 360)
#nxt_360_S2 <- ddobicki_trgovanje(nxt_1day, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 360)


#AUGUR
rep_360_S1 <- dobicki_trgovanje(rep_1day, obdobje = 360)
#rep_360_S2 <- dobicki_trgovanje(rep_1day, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 360)


pregled_coin <- function(vector, coin){
  data.frame("kovanec" = coin,
             "st_podatkov" = length(vector),
             "povprecje" = mean(vector),
             "sd" = sd(vector),
             "min" = min(vector),
             "max" = max(vector),
             "lsd" = round(cagr(tabela = mean(vector), obdobje = 360), 2),
             "st_manj_0" = sum(vector < 0),
             "verj_izgube" = round(sum(vector < 0)/length(vector), 2))
}

coin_pregled <- rbind(pregled_coin(eth_360_S1, "ETH"), pregled_coin(ltc_360_S1, "LTC"),
                      pregled_coin(xrp_360_S1, "XRP"), pregled_coin(xmr_360_S1, "XMR"),
                      pregled_coin(nxt_360_S1, "NXT"), pregled_coin(rep_360_S1, "REP"))

coin_pregled %>% izpisTabele(1)

# pred in po prelomnici
pregled_pred_po <- function(vector, coin, t, leto){
  data.frame("kovanec" = coin,
             "prelomnica" = leto,
             "lsd_pred" = round(cagr(mean(vector[1:t]), obdobje = 360), 2),
             "lsd_po" = round(cagr(mean(vector[t:length(vector)]), obdobje = 360), 2))
}

dobicki_v_casu(eth_360_S1, 360)
dobicki_v_casu(ltc_360_S1, 360)
dobicki_v_casu(xrp_360_S1, 360)
dobicki_v_casu(xmr_360_S1, 360)
dobicki_v_casu(nxt_360_S1, 360)
dobicki_v_casu(rep_360_S1, 360)

ggplot(ltc_1day, aes(Timestamp))+
  geom_line(aes(x = Timestamp, y = Close))+
  theme_minimal()+
  ylab("Zaključna cena dneva")+
  xlab("Trgovalni dnevi")+
  ggtitle("Cena Kriptovalute Ethereum")

coin_pred_po <- rbind(pregled_pred_po(ltc_360_S1, "LTC", 395, "apr. 2016"),
                      pregled_pred_po(xrp_360_S1, "XRP", 600, "apr. 2016"),
                      pregled_pred_po(xmr_360_S1, "XMR", 167, "avg. 2015"), 
                      pregled_pred_po(nxt_360_S1, "NXT", 634, "dec. 2016"))

coin_pred_po %>% izpisTabele(2)



############################################
# 3. strategija združenih 9ih indikatorjev #
############################################

# samo najboljših 6 indikatorjev
vstopni_signali1 <- function(tabela, dnevi_N = 20, vstop_s1 = 20, vstop_s2 = 55, izstop_s1_s2 = 10,
                             cena = "Close", dnevi_ema1 = 10, dnevi_ema2 = 50, toleranca = 0.02, 
                             rr = 3, indikator = "MA", metoda_izstop = "rr"){
  indi <- c("adx", "vi", "ppo", "fi", "mfi", "co")
  signali <- poracuni(tabela, dnevi_N, vstop_s1, vstop_s2, izstop_s1_s2, cena, dnevi_ema1, 
                      dnevi_ema2, toleranca, rr, indikator, metoda_izstop)
  signali <- signali[, c(1:6, 8, 12)]
  for(i in 1:length(indi)){
    tab <- poracuni(tabela, dnevi_N, vstop_s1, vstop_s2, izstop_s1_s2, cena, dnevi_ema1, 
                    dnevi_ema2, toleranca, rr, indikator = indi[i], metoda_izstop)
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
signali_indi1$izstop <- win_izstop_rr(signali_indi1, cena1, rr)
#signali_indi1$izstop <-  win_izstop_turtle(signali_indi1, 20, cena1)
#signali_indi1$izstop <-  win_izstop_chandelier(signali_indi1, cena1, 22)

uspesnost_indi1 <- function(tabela, dnevi_N = 20, vstop_s1 = 20, vstop_s2 = 55, izstop_s1_s2 = 10, 
                            zacetni_kapital = 1000000, cena = "Close", add = 0.5, sl = 2, 
                            toleranca = 0.02, rr = 3, dnevi_ema1 = 10, dnevi_ema2 = 50, 
                            indikator = "MA", metoda_izstop = "rr"){
  indi <- c("adx", "vi", "ppo", "fi", "mfi", "co")
  ratio <- matrix(0, 1, 6)
  for(i in 1:length(indi)){
    tab <- poracuni(tabela, dnevi_N, vstop_s1, vstop_s2, izstop_s1_s2, cena, dnevi_ema1, 
                    dnevi_ema2, toleranca, rr, indikator = indi[i], metoda_izstop)
    cena1 <- odlocitev_cena(tab, cena)
    profit1 <- dobicki_pozicij(tab, zacetni_kapital, cena1, add, sl, indikator)
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
indi_utezi1$izstop <- win_izstop_rr(indi_utezi1, cena1, rr)
#indi_utezi1$izstop <-  win_izstop_turtle(indi_utezi1, 20, cena1)
#indi_utezi1$izstop <-  win_izstop_chandelier(indi_utezi1, cena1, 22)

indi3_dobicki_btc_360 <- dobicki_indi(indi_utezi1, obdobje = 360)
indi3_dobicki_btc_500 <- dobicki_indi(indi_utezi1, obdobje = 500)
indi3_dobicki_btc_1000 <- dobicki_indi(indi_utezi1, obdobje = 1000)
indi3_dobicki_btc_1800 <- dobicki_indi(indi_utezi1, obdobje = 1800)
