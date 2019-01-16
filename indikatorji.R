
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
      kandidati_cilj <- which(abs(cena - cilj) <= toleranca)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
    if(tabela$entry[i] == 1){
      cilj <- cena[i,] + sl*rr*tabela$spr_tedenski_N[i]
      kandidati_cilj <- which(abs(cena - cilj) <= toleranca)
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

kr_neki <- pregled_trgovanje(MA_dobicki_btc_360[700:length(MA_dobicki_btc_360)], 
                             MA_dobicki_btc_500[700:length(MA_dobicki_btc_500)], 
                             MA_dobicki_btc_1000[700:length(MA_dobicki_btc_1000)], 
                             MA_dobicki_btc_1800)
flextabela_pregled(kr_neki, 0)


# pred/po letu 2014
MA_pred_po_2014 <- pred_po_2014(MA_dobicki_btc_360, MA_dobicki_btc_500, MA_dobicki_btc_1000, 
                                  MA_dobicki_btc_1800, 700)
flextabela_pregled(MA_pred_po_2014, 0)

MA_sd_pred_po_2014 <- sd_pred_po_2014(MA_dobicki_btc_360, MA_dobicki_btc_500, MA_dobicki_btc_1000, 
                                        MA_dobicki_btc_1800, 700)
flextabela_pregled(MA_sd_pred_po_2014, 0)


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
flextabela_pregled(MA1_sd_pred_po_2014, 0)

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




###################
# Bollinger bands #
###################



#############################
# Average Directional index #
#############################

spr_N <- function(tabela, dnevi){
  # TR
  tr <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    tr <- append(tr, max(h-l, h-pdc, pdc-l))
  }
  tabela$TR <- c(0,tr)
  tabela <- tabela[-1,]
  
  # N
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  tabela$spr_N <- round(n, 2)
  
  # +DM in -DM
  plusDM <- rep(0, nrow(tabela))
  minusDM <- rep(0, nrow(tabela))
  for(i in 2:nrow(tabela)){
    dif_high <- tabela$High[i] - tabela$High[i-1]
    dif_low <- tabela$Low[i-1] - tabela$Low[i]
    ifelse(dif_high > dif_low, plusDM[i] <- max(dif_high, 0), plusDM[i] <- 0)
    ifelse(dif_low > dif_high, minusDM[i] <- max(dif_low, 0), minusDM[i] <- 0)
  }
  tabela$plusDM <- plusDM
  tabela$minusDM <- minusDM
  
  # TR14, +DM14, -DM14
  prvi_tr14 <- sum(tabela$TR[1:14])
  prvi_plusDM14 <- sum(tabela$plusDM[2:15])
  prvi_minusDM14 <- sum(tabela$minusDM[2:15])
  tr14 <- c(rep(0, 13), prvi_tr14, rep(0, (nrow(tabela)-14)))
  plusDM14 <- c(rep(0, 14), prvi_plusDM14, rep(0, (nrow(tabela)-15)))
  minusDM14 <- c(rep(0, 14), prvi_minusDM14, rep(0, (nrow(tabela)-15)))
  for(i in (14+1):nrow(tabela)){
    tr14[i] <- tr14[i-1] - tr14[i-1]/14 + tabela$TR[i]
  }
  for(i in (14+2):nrow(tabela)){
    plusDM14[i] <- plusDM14[i-1] - plusDM14[i-1]/14 + tabela$plusDM[i]
    minusDM14[i] <- minusDM14[i-1] - minusDM14[i-1]/14 + tabela$minusDM[i]
  }
  tabela$tr14 <- round(tr14, 2)
  tabela$plusDM14 <- round(plusDM14, 2)
  tabela$minusDM14 <- round(minusDM14, 2)
  
  # +DI14, -DI14
  tabela$plusDI14 <- c(rep(0, 14), round((tabela$plusDM14[15:nrow(tabela)]/tabela$tr14[15:nrow(tabela)])*100, 2))
  tabela$minusDI14 <- c(rep(0, 14), round((tabela$minusDM14[15:nrow(tabela)]/tabela$tr14[15:nrow(tabela)])*100, 2))
  
  # DX
  tabela$DX <- c(rep(0, 14), round((abs(tabela$plusDI14[15:nrow(tabela)] - tabela$minusDI14[15:nrow(tabela)])/(tabela$plusDI14[15:nrow(tabela)] + tabela$minusDI14[15:nrow(tabela)]))*100, 2))
  
  #ADX
  prvi_adx <- mean(tabela$DX[15:28])
  adx <- c(rep(0, 27), prvi_adx, rep(0, (nrow(tabela)-28)))
  for(i in (28+1):nrow(tabela)){
    adx[i] <- (13*adx[i-1] + tabela$DX[i])/14
  }
  tabela$ADX <- round(adx, 2)
  tabela
}


# +DI > -DI , ADX > 20 -> go long
# -Di > +DI , ADX > 20 -> fo short
vstop_ADX <- function(tabela){
  entry <- rep(0, nrow(tabela))
  for(i in 2:nrow(tabela)){
      # DI Crossovers
      if((tabela$plusDI14[i-1] < tabela$minusDI14[i-1]) & (tabela$plusDI14[i] > tabela$minusDI14[i]) & 
         (tabela$ADX[i] > 20)){
        entry[i] <- 1
      }
      if((tabela$plusDI14[i-1] > tabela$minusDI14[i-1]) & (tabela$plusDI14[i] < tabela$minusDI14[i]) & 
         (tabela$ADX[i] > 20)){
        entry[i] <- 2
      }
  }
  entry
}

ADX_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "adx")
ADX_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "adx")
ADX_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "adx")
ADX_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "adx")

ADX_dobicki_btc_360 <- ADX_dobicki_btc_360/1000
ADX_dobicki_btc_500 <- ADX_dobicki_btc_500/1000
ADX_dobicki_btc_1000 <- ADX_dobicki_btc_1000/1000
ADX_dobicki_btc_1800 <- ADX_dobicki_btc_1800/1000


####################
# Vortex indicator #
####################

spr_N <- function(tabela, dnevi){
  # TR
  tr <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    tr <- append(tr, max(h-l, h-pdc, pdc-l))
  }
  tabela$TR <- c(0,tr)
  tabela <- tabela[-1,]
  
  # N
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  tabela$spr_N <- round(n, 2)
  
  # +VM in -VM
  tabela$plusVM <- c(0, abs(tabela$High[2:nrow(tabela)] - tabela$Low[1:(nrow(tabela)-1)]))
  tabela$minusVM <- c(0, abs(tabela$Low[2:nrow(tabela)] - tabela$High[1:(nrow(tabela)-1)]))
  
  # TR14, +VM14, -VM14
  tr14 <- c(rep(0, 13), rep(0, (nrow(tabela)-13)))
  plusVM14 <- c(rep(0, 14), rep(0, (nrow(tabela)-14)))
  minusVM14 <- c(rep(0, 14), rep(0, (nrow(tabela)-14)))
  for(i in 14:nrow(tabela)){
    tr14[i] <- sum(tabela$TR[(i-13):i])
  }
  for(i in 15:nrow(tabela)){
    plusVM14[i] <- sum(tabela$plusVM[(i-13):i])
    minusVM14[i] <- sum(tabela$minusVM[(i-13):i])
  }
  tabela$tr14 <- round(tr14, 2)
  tabela$plusVM14 <- round(plusVM14, 2)
  tabela$minusVM14 <- round(minusVM14, 2)

  # # +VI14, -VI14
  tabela$plusVI14 <- c(rep(0, 14), round((tabela$plusVM14[15:nrow(tabela)]/tabela$tr14[15:nrow(tabela)]), 2))
  tabela$minusVI14 <- c(rep(0, 14), round((tabela$minusVM14[15:nrow(tabela)]/tabela$tr14[15:nrow(tabela)]), 2))

  tabela
}


# +VI > -VI , +VI > 1 -> go long
# -VI > +VI , -VI > 1 -> go short
vstop_VI <- function(tabela){
  entry <- rep(0, nrow(tabela))
  for(i in 2:nrow(tabela)){
    # DI Crossovers
    if((tabela$plusVI14[i-1] < tabela$minusVI14[i-1]) & (tabela$plusVI14[i] > tabela$minusVI14[i]) & 
       (tabela$plusVI14[i] > 1)){
      entry[i] <- 1
    }
    if((tabela$plusVI14[i-1] > tabela$minusVI14[i-1]) & (tabela$plusVI14[i] < tabela$minusVI14[i]) & 
       (tabela$minusVI14[i] > 1)){
      entry[i] <- 2
    }
  }
  entry
}

VIX_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "vi")
VIX_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "vi")
VIX_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "vi")
VIX_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "vi")

VIX_dobicki_btc_360 <- VIX_dobicki_btc_360/1000
VIX_dobicki_btc_500 <- VIX_dobicki_btc_500/1000
VIX_dobicki_btc_1000 <- VIX_dobicki_btc_1000/1000
VIX_dobicki_btc_1800 <- VIX_dobicki_btc_1800/1000


###########################
# Relative strength index #
###########################

spr_N <- function(tabela, dnevi){
  # TR
  tr <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    tr <- append(tr, max(h-l, h-pdc, pdc-l))
  }
  tabela$TR <- c(0,tr)
  
  # Change
  tabela$change <- c(0, tabela$Close[2:nrow(tabela)] - tabela$Close[1:(nrow(tabela)-1)])
  
  tabela <- tabela[-1,]
  
  #Gain, loss
  gain <- rep(0, nrow(tabela))
  loss <- rep(0, nrow(tabela))
  for(i in 1:nrow(tabela)){
    ifelse(tabela$change[i] > 0, gain[i] <- tabela$change[i], 0)
    ifelse(tabela$change[i] < 0, loss[i] <- -tabela$change[i], 0)
  }
  tabela$gain <- gain
  tabela$loss <- loss
  
  # avg gain, avg loss
  avg_gain <- rep(0, nrow(tabela))
  avg_loss <- rep(0, nrow(tabela))
  avg_gain[14] <- mean(tabela$gain[1:14])
  avg_loss[14] <- mean(tabela$loss[1:14])
  for(i in 15:nrow(tabela)){
    avg_gain[i] <- (13*avg_gain[i-1] + tabela$gain[i])/14
    avg_loss[i] <- (13*avg_loss[i-1] + tabela$loss[i])/14
  }
  tabela$avg_gain <- round(avg_gain, 2)
  tabela$avg_loss <- round(avg_loss, 2)
  
  # RS, RSI
  tabela$RS <- tabela$avg_gain/tabela$avg_loss
  tabela$RS[tabela$RS == "NaN"] <- 0
  rsi <- rep(0, nrow(tabela))
  for(i in 1:nrow(tabela)){
    ifelse(tabela$avg_loss[i] == 0, rsi[i] <- 100, rsi[i] <- (100-(100/(1+tabela$RS[i]))))
  }
  tabela$RSI <- round(rsi, 2)
  
  # N
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  tabela$spr_N <- round(n, 2)
  
  tabela
}

# RSI > 70, RSI < 70 -> go short
# RSI < 30, RSI > 30 -> go long
vstop_RSI <- function(tabela){
  entry <- rep(0, nrow(tabela))
  for(i in 2:nrow(tabela)){
    if((tabela$RSI[i-1] < 30) & (tabela$RSI[i] > 30)){
      entry[i] <- 1
    }
    if((tabela$RSI[i-1] > 70) & (tabela$RSI[i] < 70)){
      entry[i] <- 2
    }
  }
  entry
}

RSI_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "rsi")
RSI_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "rsi")
RSI_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "rsi")
RSI_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "rsi")

RSI_dobicki_btc_360 <- RSI_dobicki_btc_360/1000
RSI_dobicki_btc_500 <- RSI_dobicki_btc_500/1000
RSI_dobicki_btc_1000 <- RSI_dobicki_btc_1000/1000
RSI_dobicki_btc_1800 <- RSI_dobicki_btc_1800/1000


##################
# Rate of Change #
##################

spr_N <- function(tabela, dnevi){
  # TR
  tr <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    tr <- append(tr, max(h-l, h-pdc, pdc-l))
  }
  tabela$TR <- c(0,tr)
  
  # ROC
  roc <- function(dnevi){
    roc <- rep(0, nrow(tabela))
    for(i in (dnevi+1):nrow(tabela)){
      roc[i] <- ((tabela$Close[i] - tabela$Close[i-dnevi])/tabela$Close[i-dnevi])*100
    }
    roc
  }
  tabela$roc21 <- round(roc(21), 2)
  
  tabela <- tabela[-1,]
  # N
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  tabela$spr_N <- round(n, 2)

  tabela
}

# ROC < -10, ROC > -10, cena > SMA -> go long
# ROC > 10, ROC < 10, cena < SMA -> go short
vstop_ROC <- function(tabela, cena){
  entry <- rep(0, nrow(tabela))
  for(i in 2:(nrow(tabela)-5)){
    if((tabela$roc21[i-1] < -10) & (tabela$roc21[i] > -10)){
      for(j in i:(i+5)){
        if((cena[(j-1),] < tabela$ma[(j-1)]) & (cena[j,] > tabela$ma[j])){
          entry[j] <- 1
          break
        }
      }
    }
    if((tabela$roc21[i-1] > 10) & (tabela$roc21[i] < 10)){
      for(j in i:(i+5)){
        if((cena[(j-1),] > tabela$ma[(j-1)]) & (cena[j,] < tabela$ma[j])){
          entry[j] <- 2
          break
        }
      }
    }
  }
  entry
}

ROC_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "roc")
ROC_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "roc")
ROC_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "roc")
ROC_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "roc")

ROC_dobicki_btc_360 <- ROC_dobicki_btc_360/1000
ROC_dobicki_btc_500 <- ROC_dobicki_btc_500/1000
ROC_dobicki_btc_1000 <- ROC_dobicki_btc_1000/1000
ROC_dobicki_btc_1800 <- ROC_dobicki_btc_1800/1000



#########################
# Stochastic Oscillator #
#########################

spr_N <- function(tabela, dnevi){
  # TR
  tr <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    tr <- append(tr, max(h-l, h-pdc, pdc-l))
  }
  tabela$TR <- c(0,tr)
  
  # highest high, lowest low
  hh <- rep(0, nrow(tabela))
  ll <- rep(0, nrow(tabela))
  for(i in 14:nrow(tabela)){
    hh[i] <- max(tabela$High[(i-13):i])
    ll[i] <- min(tabela$Low[(i-13):i])
  }
  tabela$hh <- hh
  tabela$ll <- ll
  
  # %K
  fastK <- c(rep(0, 13), ((tabela$Close[14:nrow(tabela)] - tabela$ll[14:nrow(tabela)])/(tabela$hh[14:nrow(tabela)] - tabela$ll[14:nrow(tabela)]))*100)
  tabela$fastK <- round(fastK, 2)
  tabela$slowK <- c(rep(0, 13), sma(tabela$fastK[14:nrow(tabela)], 3))
  tabela$slowK[is.na(tabela$slowK)] <- 0
  
  
  tabela <- tabela[-1,]
  # N
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  tabela$spr_N <- round(n, 2)
  
  tabela
}

# slow K < 20, slow K > 20 -> go long
# slow K > 80, slow K < 80 -> go short
vstop_SO <- function(tabela){
  entry <- rep(0, nrow(tabela))
  for(i in 2:nrow(tabela)){
    if((tabela$slowK[i-1] < 20) & (tabela$slowK[i] > 20)){
      entry[i] <- 1
    }
    if((tabela$slowK[i-1] > 80) & (tabela$slowK[i] < 80)){
      entry[i] <- 2
    }
  }
  entry
}

SO_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "so")
SO_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "so")
SO_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "so")
SO_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "so")

SO_dobicki_btc_360 <- SO_dobicki_btc_360/1000
SO_dobicki_btc_500 <- SO_dobicki_btc_500/1000
SO_dobicki_btc_1000 <- SO_dobicki_btc_1000/1000
SO_dobicki_btc_1800 <- SO_dobicki_btc_1800/1000


###############################
# Percentage Price Oscillator #
###############################

spr_N <- function(tabela, dnevi){
  # TR
  tr <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    tr <- append(tr, max(h-l, h-pdc, pdc-l))
  }
  tabela$TR <- c(0,tr)
  
  # 12-day & 26-day EMA
  tabela$ema12 <- ema(tabela$Close, 12)
  tabela$ema12[is.na(tabela$ema12)] <- 0
  tabela$ema26 <- ema(tabela$Close, 26)
  tabela$ema26[is.na(tabela$ema26)] <- 0
  
  # MACD in PPO
  tabela$macd <- c(rep(0, 25), tabela$ema12[26:nrow(tabela)] - tabela$ema26[26:nrow(tabela)])
  tabela$ppo <- c(rep(0, 25), (tabela$macd[26:nrow(tabela)]/tabela$ema26[26:nrow(tabela)])*100)
  
  # 9-day EMA of PPO
  tabela$ema9ppo <- c(rep(NA, 25), ema(tabela$ppo[26:nrow(tabela)], 9))
  
  tabela <- tabela[-1,]
  # N
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  tabela$spr_N <- round(n, 2)
  
  tabela
}

# ppo < ema9ppo, ppo > ema9ppo -> go long
# ppo > ema9ppo, ppo < ema9ppo -> go short
vstop_PPO <- function(tabela){
  entry <- rep(0, nrow(tabela))
  for(i in 2:nrow(tabela)){
    if(!is.na(tabela$ema9ppo[i-1])){
      if((tabela$ppo[i-1] < tabela$ema9ppo[i-1]) & (tabela$ppo[i] > tabela$ema9ppo[i])){
        entry[i] <- 1
      }
      if((tabela$ppo[i-1] > tabela$ema9ppo[i-1]) & (tabela$ppo[i] < tabela$ema9ppo[i])){
        entry[i] <- 2
      }
    }
  }
  entry
}

PPO_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "ppo")
PPO_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "ppo")
PPO_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "ppo")
PPO_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "ppo")

PPO_dobicki_btc_360 <- PPO_dobicki_btc_360/1000
PPO_dobicki_btc_500 <- PPO_dobicki_btc_500/1000
PPO_dobicki_btc_1000 <- PPO_dobicki_btc_1000/1000
PPO_dobicki_btc_1800 <- PPO_dobicki_btc_1800/1000