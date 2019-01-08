
###################
# Moving averages #
###################

#vstopne točke: 1=go long, 2=go short
vstop_MA <- function(tabela, cena){
  library(QuantTools)
  entry <- rep(0, nrow(tabela))
  for(i in 2:nrow(tabela)){
    if(!is.na(tabela$ma1[i-1]) & !is.na(tabela$ma2[i-1])){
      # Double Crossovers
      if((tabela$ma1[i-1] < tabela$ma2[i-1]) & (tabela$ma1[i] > tabela$ma2[i])){
        entry[i] <- 1
      }
      if((tabela$ma1[i-1] > tabela$ma2[i-1]) & (tabela$ma1[i] < tabela$ma2[i])){
        entry[i] <- 2
      }
      # Price Crossovers
      if((cena[i,] > tabela$ma2[i]) & (cena[i-1,] < tabela$ma1[i-1]) & (cena[i,] > tabela$ma1[i]) & 
         (tabela$ma1[i] < tabela$ma2[i])){
        entry[i] <- 1
      }
      if((cena[i,] < tabela$ma2[i]) & (cena[i-1,] > tabela$ma1[i-1]) & (cena[i,] < tabela$ma1[i]) & 
         (tabela$ma1[i] < tabela$ma2[i])){
        entry[i] <- 2
      }
    }
  }
  entry
}

# Izstop iz zmagovalne pozicije
win_izstop_MA <- function(tabela, cena, toleranca, rr){
  izstop <- rep(0, nrow(tabela))
  for(i in 1:nrow(tabela)){
    if(tabela$entry[i] == 2){
      cilj <- cena[i,]- sl*rr*tabela$spr_tedenski_N[i]
      kandidati_cilj <- which(cena - cilj <= toleranca)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
    if(tabela$entry[i] == 1){
      cilj <- cena[i,] + sl*rr*tabela$spr_tedenski_N[i]
      kandidati_cilj <- which(cena - cilj <= toleranca)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
  }
  izstop
}

source("trgovanje.R")
#EMA
MA_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360)
MA_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500)
MA_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000)
MA_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800)

MA_dobicki_btc_360 <- MA_dobicki_btc_360/1000
MA_dobicki_btc_500 <- MA_dobicki_btc_500/1000
MA_dobicki_btc_1000 <- MA_dobicki_btc_1000/1000
MA_dobicki_btc_1800 <- MA_dobicki_btc_1800/1000

MA_pregled_btc <- pregled_trgovanje(MA_dobicki_btc_360, MA_dobicki_btc_500, MA_dobicki_btc_1000, 
                                      MA_dobicki_btc_1800)

flextabela_pregled(MA_pregled_btc, 0)


# pred/po letu 2014
MA_pred_po_2014 <- pred_po_2014(MA_dobicki_btc_360, MA_dobicki_btc_500, MA_dobicki_btc_1000, 
                                  MA_dobicki_btc_1800, 700)
flextabela_pregled(MA_pred_po_2014, 0)

MA_sd_pred_po_2014 <- sd_pred_po_2014(MA_dobicki_btc_360, MA_dobicki_btc_500, MA_dobicki_btc_1000, 
                                        MA_dobicki_btc_1800, 700)
flextabela_pregled(MA_sd_pred_po_2014, 1)


#SMA
MA1_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "MA1")
MA1_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "MA1")
MA1_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "MA1")
MA1_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "MA1")

MA1_dobicki_btc_360 <- MA1_dobicki_btc_360/1000
MA1_dobicki_btc_500 <- MA1_dobicki_btc_500/1000
MA1_dobicki_btc_1000 <- MA1_dobicki_btc_1000/1000
MA1_dobicki_btc_1800 <- MA1_dobicki_btc_1800/1000

MA1_pregled_btc <- pregled_trgovanje(MA1_dobicki_btc_360, MA1_dobicki_btc_500, MA1_dobicki_btc_1000, 
                                    MA1_dobicki_btc_1800)

flextabela_pregled(MA1_pregled_btc, 0)


# pred/po letu 2014
MA1_pred_po_2014 <- pred_po_2014(MA1_dobicki_btc_360, MA1_dobicki_btc_500, MA1_dobicki_btc_1000, 
                                MA1_dobicki_btc_1800, 700)
flextabela_pregled(MA1_pred_po_2014, 0)

MA1_sd_pred_po_2014 <- sd_pred_po_2014(MA1_dobicki_btc_360, MA1_dobicki_btc_500, MA1_dobicki_btc_1000, 
                                      MA1_dobicki_btc_1800, 700)
flextabela_pregled(MA1_sd_pred_po_2014, 1)

##############
# Optimizacija

# moving average
dobicki_ma <- function(tabela){
  M <- matrix(0, 6, 6)
  N <- matrix(0, 6, 6)
  zap_shortma <- c(5, 10, 12, 20, 26, 50)
  zap_longma <- c(50, 75, 100, 125, 150, 200)
  for(i in 1:6){
    for(j in 1:6){
      tmp <- spr_dobicki_trgovanje(tabela, dnevi_ema1 = zap_shortma[i], dnevi_ema2 = zap_longma[j])
      M[i, j] <- tmp[1]
      N[i, j] <- tmp[2]
    }
  }
  matrika <- data.frame(M, row.names = zap_shortma)
  matrika1 <- data.frame(N, row.names = zap_shortma)
  data.frame(matrika, matrika1)
}

MAmatrika <- dobicki_ma(btc_1day)
MAdobicki <- MAmatrika[,1:6]
colnames(MAdobicki) <- c(50, 75, 100, 125, 150, 200)
MAsd <- MAmatrika[,7:12]
colnames(MAsd) <- c(50, 75, 100, 125, 150, 200)


library(officer)
library(flextable)

flextabela_matrika(spr(cagr(MAdobicki), "ma"), "cagr")
flextabela_matrika(spr(MAdobicki/MAsd, "ma"), "cagr")
