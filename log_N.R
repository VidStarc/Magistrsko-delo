
?chaikinVolatility

spr_N <- function(tabela, metoda){
  library(TTR)
  tabela <- tabela[-1,]
  ohlc <- tabela[,-1]
  n <- volatility(ohlc, n = 20, N = 365 , calc = metoda)
  tabela$spr_N <- n
  tabela
}

spr_tedenski_N <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    t_N[i:(i+6)] <- tabela$spr_N[i]
  }
  t_N[1:nrow(tabela)]
}

dobicki_Nclose_360 <- dobicki_hitreje(tabela = btc_1day, dnevi_N = "close", obdobje = 360, strategija = "S1")
dobicki_Ngk_360 <- dobicki_hitreje(tabela = btc_1day, obdobje = 360, strategija = "S1", dnevi_N = "garman.klass")
dobicki_Npar_360 <- dobicki_hitreje(tabela = btc_1day, obdobje = 360, strategija = "S1", dnevi_N = "parkinson")
dobicki_Nrs_360 <- dobicki_hitreje(tabela = btc_1day, obdobje = 360, strategija = "S1", dnevi_N = "rogers.satchell")
dobicki_Ngkyz_360 <- dobicki_hitreje(tabela = btc_1day, obdobje = 360, strategija = "S1", dnevi_N = "gk.yz")
dobicki_Nyz_360 <- dobicki_hitreje(tabela = btc_1day, obdobje = 360, strategija =  "S1", dnevi_N = "yang.zhang")
dobicki_Nclose_360 <- dobicki_Nclose_360/1000
dobicki_Ngk_360 <- dobicki_Ngk_360/1000
dobicki_Npar_360 <- dobicki_Npar_360/1000
dobicki_Nrs_360 <- dobicki_Nrs_360/1000
dobicki_Ngkyz_360 <- dobicki_Ngkyz_360/1000
dobicki_Nyz_360 <- dobicki_Nyz_360/1000



###############
# relativni N #
###############

spr_tedenski_N <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    #ifelse(i < 700, t_N[i:(i+6)] <- tabela$spr_N[i], t_N[i:(i+6)] <- log(tabela$spr_N[i]))
    t_N[i:(i+6)] <- (tabela$spr_N[i]/tabela$Close[i])
  }
  t_N[1:nrow(tabela)]
}

proba5 <- dobicki_hitreje(tabela = btc_1day, obdobje = 360, strategija = "S1")
proba6 <- dobicki_hitreje(tabela = btc_1day, obdobje = 500, strategija = "S1")
proba7 <- dobicki_hitreje(tabela = btc_1day, obdobje = 1000, strategija = "S1")
proba8 <- dobicki_hitreje(tabela = btc_1day, strategija =  "S1")
proba5 <- proba5/1000
proba6 <- proba6/1000
proba7 <- proba7/1000
proba8 <- proba8/1000


# dobički v času
library(gridExtra)
library(ggplot2)
grid.arrange(dobicki_v_casu(proba5, 360), dobicki_v_casu(proba6, 500), 
             dobicki_v_casu(proba7, 1000), dobicki_v_casu(proba8, 1800), 
             nrow = 2, ncol = 2)

grid.arrange(dobicki_v_casu(proba5[500:length(proba5)], 360, 500), 
             dobicki_v_casu(proba6[500:length(proba6)], 500, 500), 
             dobicki_v_casu(proba7[500:length(proba7)], 1000, 500), 
             dobicki_v_casu(proba8[500:length(proba8)], 1800, 500), 
             nrow = 2, ncol = 2)

# pregled dobičkov
preizkus <- pregled_trgovanje(proba5, proba6, proba7, proba8)

flextabela_pregled(preizkus, 0)

# pred/po letu 2014
preizkus_pred_po_2014 <- pred_po_2014(proba5, proba6, proba7, proba8, 700)
flextabela_pregled(preizkus_pred_po_2014, 0)

preizkus_sd_pred_po_2014 <- sd_pred_po_2014(proba5, proba6, proba7, proba8, 700)
flextabela_pregled(preizkus_sd_pred_po_2014, 1)


#########
# Log N #
#########

spr_tedenski_N <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    ifelse(i < 700, t_N[i:(i+6)] <- tabela$spr_N[i], t_N[i:(i+6)] <- log(tabela$spr_N[i]))
    #t_N[i:(i+6)] <- (tabela$spr_N[i]/tabela$Close[i])
  }
  t_N[1:nrow(tabela)]
}

dobicki_logN_360 <- dobicki_hitreje(tabela = btc_1day, obdobje = 360, strategija = "S1")
dobicki_logN_500 <- dobicki_hitreje(tabela = btc_1day, obdobje = 500, strategija = "S1")
dobicki_logN_1000 <- dobicki_hitreje(tabela = btc_1day, obdobje = 1000, strategija = "S1")
dobicki_logN_1800 <- dobicki_hitreje(tabela = btc_1day, strategija =  "S1")
dobicki_logN_360 <- dobicki_logN_360/1000
dobicki_logN_500 <- dobicki_logN_500/1000
dobicki_logN_1000 <- dobicki_logN_1000/1000
dobicki_logN_1800 <- dobicki_logN_1800/1000


# dobički v času
library(gridExtra)
library(ggplot2)
grid.arrange(dobicki_v_casu(dobicki_logN_360, 360), dobicki_v_casu(dobicki_logN_500, 500), 
             dobicki_v_casu(dobicki_logN_1000, 1000), dobicki_v_casu(dobicki_logN_1800, 1800), 
             nrow = 2, ncol = 2)

grid.arrange(dobicki_v_casu(dobicki_logN_360[500:length(dobicki_logN_360)], 360, 500), 
             dobicki_v_casu(dobicki_logN_500[500:length(dobicki_logN_500)], 500, 500), 
             dobicki_v_casu(dobicki_logN_1000[500:length(dobicki_logN_1000)], 1000, 500), 
             dobicki_v_casu(dobicki_logN_1800[500:length(dobicki_logN_1800)], 1800, 500), 
             nrow = 2, ncol = 2)

# pregled dobičkov
pregled_logN <- pregled_trgovanje(dobicki_logN_360, dobicki_logN_500, dobicki_logN_1000, dobicki_logN_1800)

flextabela_pregled(pregled_logN, 1)

# pred/po letu 2014
logN_pred_po_2014 <- pred_po_2014(dobicki_logN_360, dobicki_logN_500, 
                                  dobicki_logN_1000, dobicki_logN_1800, 700)
flextabela_pregled(logN_pred_po_2014, 0)

logN_sd_pred_po_2014 <- sd_pred_po_2014(dobicki_logN_360, dobicki_logN_500, 
                                        dobicki_logN_1000, dobicki_logN_1800, 700)
flextabela_pregled(logN_sd_pred_po_2014, 1)


#########
# N/SMA #
#########

spr_tedenski_N <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    #ifelse(i < 700, t_N[i:(i+6)] <- tabela$spr_N[i], t_N[i:(i+6)] <- log(tabela$spr_N[i]))
    #t_N[i:(i+6)] <- (tabela$spr_N[i]/tabela$Close[i])
    t_N[i:(i+6)] <- (tabela$spr_N[i]/tabela$sma[i])
  }
  t_N[1:nrow(tabela)]
}

dobicki_Nsma_360 <- dobicki_hitreje(tabela = btc_1day, obdobje = 360, strategija = "S1")
dobicki_Nsma_500 <- dobicki_hitreje(tabela = btc_1day, obdobje = 500, strategija = "S1")
dobicki_Nsma_1000 <- dobicki_hitreje(tabela = btc_1day, obdobje = 1000, strategija = "S1")
dobicki_Nsma_1800 <- dobicki_hitreje(tabela = btc_1day, strategija =  "S1")
dobicki_Nsma_360 <- dobicki_Nsma_360/1000
dobicki_Nsma_500 <- dobicki_Nsma_500/1000
dobicki_Nsma_1000 <- dobicki_Nsma_1000/1000
dobicki_Nsma_1800 <- dobicki_Nsma_1800/1000


# dobički v času
library(gridExtra)
library(ggplot2)
grid.arrange(dobicki_v_casu(dobicki_Nsma_360, 360), dobicki_v_casu(dobicki_Nsma_500, 500), 
             dobicki_v_casu(dobicki_Nsma_1000, 1000), dobicki_v_casu(dobicki_Nsma_1800, 1800), 
             nrow = 2, ncol = 2)

grid.arrange(dobicki_v_casu(dobicki_Nsma_360[500:length(dobicki_Nsma_360)], 360, 500), 
             dobicki_v_casu(dobicki_Nsma_500[500:length(dobicki_Nsma_500)], 500, 500), 
             dobicki_v_casu(dobicki_Nsma_1000[500:length(dobicki_Nsma_1000)], 1000, 500), 
             dobicki_v_casu(dobicki_Nsma_1800[500:length(dobicki_Nsma_1800)], 1800, 500), 
             nrow = 2, ncol = 2)

# pregled dobičkov
pregled_Nsma <- pregled_trgovanje(dobicki_Nsma_360, dobicki_Nsma_500, dobicki_Nsma_1000, dobicki_Nsma_1800)

flextabela_pregled(pregled_Nsma, 1)

# pred/po letu 2014
Nsma_pred_po_2014 <- pred_po_2014(dobicki_Nsma_360, dobicki_Nsma_500, 
                                  dobicki_Nsma_1000, dobicki_Nsma_1800, 700)
flextabela_pregled(Nsma_pred_po_2014, 0)

Nsma_sd_pred_po_2014 <- sd_pred_po_2014(dobicki_Nsma_360, dobicki_Nsma_500, 
                                        dobicki_Nsma_1000, dobicki_Nsma_1800, 700)
flextabela_pregled(Nsma_sd_pred_po_2014, 1)


########
# TR/C #
########

spr_N <- function(tabela, dnevi){
  tr <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    dc <- tabela$Close[i]
    tr <- append(tr, max(h-l, h-pdc, pdc-l)/dc)
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

spr_tedenski_N <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    t_N[i:(i+6)] <- tabela$spr_N[i]
  }
  t_N[1:nrow(tabela)]
}

dobicki_TR_360 <- dobicki_hitreje(tabela = btc_1day, obdobje = 360, strategija = "S1")
dobicki_TR_500 <- dobicki_hitreje(tabela = btc_1day, obdobje = 500, strategija = "S1")
dobicki_TR_1000 <- dobicki_hitreje(tabela = btc_1day, obdobje = 1000, strategija = "S1")
dobicki_TR_1800 <- dobicki_hitreje(tabela = btc_1day, strategija =  "S1")
dobicki_TR_360 <- dobicki_TR_360/1000
dobicki_TR_500 <- dobicki_TR_500/1000
dobicki_TR_1000 <- dobicki_TR_1000/1000
dobicki_TR_1800 <- dobicki_TR_1800/1000


# pregled dobičkov
pregled_TR <- pregled_trgovanje(dobicki_TR_360, dobicki_TR_500, dobicki_TR_1000, dobicki_TR_1800)

flextabela_pregled(pregled_TR, 1)

# pred/po letu 2014
TR_pred_po_2014 <- pred_po_2014(dobicki_TR_360, dobicki_TR_500, 
                                  dobicki_TR_1000, dobicki_TR_1800, 700)
flextabela_pregled(TR_pred_po_2014, 0)

TR_sd_pred_po_2014 <- sd_pred_po_2014(dobicki_TR_360, dobicki_TR_500, 
                                        dobicki_TR_1000, dobicki_TR_1800, 700)
flextabela_pregled(TR_sd_pred_po_2014, 1)


###########
# PREGLED #
###########

pred_po_2014 <- function(l1, l2, l3, l4, predpo2014){
  pred_2014 <- c(mean(l1[1:predpo2014]*1000), mean(l2[1:predpo2014]*1000), mean(l3[1:predpo2014]*1000), mean(l4[1:predpo2014]*1000))
  cagr_pred_2014 <- c(cagr(pred_2014[1], obdobje = 360), cagr(pred_2014[2], obdobje = 360), 
                      cagr(pred_2014[3], obdobje = 360), cagr(pred_2014[4], obdobje = 360))
  po_2014 <- c(mean(l1[(predpo2014+1):length(l1)]*1000), mean(l2[(predpo2014+1):length(l2)]*1000), mean(l3[(predpo2014+1):length(l3)]*1000), 
               mean(l4[(predpo2014+1):length(l4)]*1000))
  cagr_po_2014 <- c(cagr(po_2014[1], obdobje = 360), cagr(po_2014[2], obdobje = 360), 
                    cagr(po_2014[3], obdobje = 360), cagr(po_2014[4], obdobje = 360))
  data.frame("sistem_obdobje" = c("S1, 360", "S1, 360", "S1, 360", "S1, 360"), 
             "pred_2014" = pred_2014, "po_2014" = po_2014, 
             "lsd_pred_2014" = paste0(cagr_pred_2014, " %"), 
             "lsd_po_2014" = paste0(cagr_po_2014, " %"))
}
pregled_razlicen_N <- pred_po_2014(dobicki_Nsma_360, dobicki_TR_360, dobicki_Nclose_360, dobicki_Ngk_360, 700)
pregled_razlicen_n <- pred_po_2014(dobicki_Npar_360, dobicki_Nrs_360, dobicki_Ngkyz_360, dobicki_Nyz_360, 700)


razlicen_N <- rbind(S1pred_po_2014[1,], logN_pred_po_2014[1,], preizkus_pred_po_2014[1,], 
                    pregled_razlicen_N, pregled_razlicen_n)
volatilnost <- c("ATR", "log(ATR)", "ATR/Close", "ATR/SMA", "TR/C", "close", "garman.klass", "parkinson", 
                 "rogers.satchell", "gk.yz", "yang.zhang")
razlicen_N <- data.frame("volatilnost" = volatilnost, razlicen_N[,-1])

flex <- flextabela_pregled(razlicen_N, 0)
flex <- align(flex, align = "left", j = 1)
flex <- align(flex, align = "left", j = 1, part = "header")
