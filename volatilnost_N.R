
# Za relativne volatilnosti:
# stop - loss še vedno absoluten N

trgovanje_N <- function(tabela, zacetni_kapital, izstop_s1, cena, add, sl){
  kandidati_s1 <- which(tabela$entry_s1==1 | tabela$entry_s1==2)
  kandidati_s2 <- which(tabela$entry_s2==1 | tabela$entry_s2==2)
  vstop <- kandidati_s1[1]
  if(is.na(vstop)){profit <- 0}
  else{
    kdaj_vstopali <- c()
    kdaj_dodali_enote <- c()
    profit <- 0
    profit1 <- c()
    kdaj_profit <- c()
    money <- zacetni_kapital
    kdaj_izstop_long <- spr_win_izstop_s1_2(tabela, "long", izstop_s1, cena)
    kdaj_izstop_short <- spr_win_izstop_s1_2(tabela, "short", izstop_s1, cena)
    while(nrow(tabela) - vstop > 0){
      kdaj_vstopali <- c(kdaj_vstopali, vstop)
      cena_vstop <- cena[vstop,]
      st_btc <- unit(money, tabela$tedenski_N_relativen[vstop])/cena_vstop
      
      if(tabela$entry_s1[vstop]==1 | tabela$entry_s2[vstop]==1){
        izstop <- ifelse(is.na(kdaj_izstop_long[kdaj_izstop_long > vstop][1]), nrow(tabela), kdaj_izstop_long[kdaj_izstop_long > vstop][1])
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
              st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$tedenski_N_relativen[i])/cena[i,])*dodamo)
              stop_loss <- stop_loss + pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
            }
            else{
              dodamo <- ifelse(st_enot==2, 2, 1)
              st_enot <- st_enot + dodamo
              st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$tedenski_N_relativen[i])/cena[i,])*dodamo)
              stop_loss <- stop_loss + pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
            }
          }
          else{
            if(cena[i,] <= stop_loss){
              vstop <- ifelse(is.na(kandidati_s1[kandidati_s1 > vstop][1]), nrow(tabela), kandidati_s1[kandidati_s1 > vstop][1])
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
        izstop <- ifelse(is.na(kdaj_izstop_short[kdaj_izstop_short > vstop][1]), nrow(tabela), kdaj_izstop_short[kdaj_izstop_short > vstop][1])
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
              st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$tedenski_N_relativen[i])/cena[i,])*dodamo)
              stop_loss <- stop_loss - pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
            }
            else{
              dodamo <- ifelse(st_enot==2, 2, 1)
              st_enot <- st_enot + dodamo
              st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$tedenski_N_relativen[i])/cena[i,])*dodamo)
              stop_loss <- stop_loss - pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
            }
          }
          else{
            if(cena[i,] >= stop_loss){
              vstop <- ifelse(is.na(kandidati_s1[kandidati_s1 > vstop][1]), nrow(tabela), kandidati_s1[kandidati_s1 > vstop][1])
              kdaj_profit <- c(kdaj_profit, i)
              for(j in 1:length(st_btc_dodamo)){
                profit <- profit + (cena_ko_dodamo[j] - cena[i,])*st_btc_dodamo[j]
              }
              profit1 <- c(profit1, profit)
              break}
          }
        }
      }
      if(vstop == (izstop-1)){
        vstop <- vstop + 1
        for(j in 1:length(st_btc_dodamo)){
          profit <- profit + (abs(cena_izstop - cena_ko_dodamo[j]))*st_btc_dodamo[j]
        }
        profit1 <- c(profit1, profit)
        kdaj_profit <- c(kdaj_profit, izstop)
        vstop <- ifelse(is.na(kandidati_s2[kandidati_s2 > vstop][1]), nrow(tabela), kandidati_s2[kandidati_s2 > vstop][1])
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



izracun_za_analizo_vol <- function(tabela, dnevi_N, vstop_s1, vstop_s2, cena, metoda){
  library(QuantTools)
  tabela <- tabela[-nrow(tabela),]
  tab <- spr_N(tabela, dnevi_N)
  tab <- N_relativen(tab, metoda)
  cena1 <- odlocitev_cena(tab, cena)
  # za N/SMA:
  tab$sma <- sma(cena1[,1], dnevi_N)
  tab$sma[is.na(tab$sma)] <- rep(1, length(tab$sma[is.na(tab$sma)]))
  ##
  
  tab$entry_s1 <- spr_entry_s1(tab, vstop_s1, cena1)
  tab$entry_s2 <- spr_entry_s2(tab, vstop_s2, cena1)
  tab$spr_tedenski_N <- spr_tedenski_N(tab)
  tab$tedenski_N_relativen <- tedenski_N_relativen(tab)
  tab <- tab[tab$spr_tedenski_N > 0,]
  
  tab
}

dobicki_volatilnost <- function(tabela, dnevi_N = 20, vstop_s1 = 20, vstop_s2 = 55, izstop_s1 = 10, izstop_s2 = 20, 
                            obdobje = 1800, zacetni_kapital = 1000000, cena = "Close", 
                            add = 1/2, sl = 2, metoda = "close"){
  tab <- izracun_za_analizo_vol(tabela, dnevi_N, vstop_s1, vstop_s2, cena, metoda)
  dobicki <- c()
  for(i in 1:(nrow(tab) - obdobje + 1)){
    tmp <- tab[i:(obdobje + i - 1),]
    cena2 <- odlocitev_cena(tmp, cena)
    dobicki <- c(dobicki, trgovanje_N(tmp, zacetni_kapital, izstop_s1, cena2, add, sl))
  }
  dobicki
}

#######################################
# različne metode funkcije volatility #
#######################################

N_relativen <- function(tabela, metoda){
  library(TTR)
  ohlc <- tabela[,c(-1, -6, -7)]
  n <- volatility(ohlc, n = 20, N = 1 , calc = metoda)
  tabela$N_relativen <- n
  tabela
}

tedenski_N_relativen <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    t_N[i:(i+6)] <- tabela$N_relativen[i]
  }
  t_N[1:nrow(tabela)]
}

dobicki_Nclose_360 <- dobicki_volatilnost(tabela = btc_1day, metoda = "close", obdobje = 360)
dobicki_Ngk_360 <- dobicki_volatilnost(tabela = btc_1day, obdobje = 360, metoda = "garman.klass")
dobicki_Npar_360 <- dobicki_volatilnost(tabela = btc_1day, obdobje = 360, metoda = "parkinson")
dobicki_Nrs_360 <- dobicki_volatilnost(tabela = btc_1day, obdobje = 360, metoda = "rogers.satchell")
dobicki_Ngkyz_360 <- dobicki_volatilnost(tabela = btc_1day, obdobje = 360, metoda = "gk.yz")
dobicki_Nyz_360 <- dobicki_volatilnost(tabela = btc_1day, obdobje = 360, metoda = "yang.zhang")
dobicki_Nclose_360 <- dobicki_Nclose_360/1000
dobicki_Ngk_360 <- dobicki_Ngk_360/1000
dobicki_Npar_360 <- dobicki_Npar_360/1000
dobicki_Nrs_360 <- dobicki_Nrs_360/1000
dobicki_Ngkyz_360 <- dobicki_Ngkyz_360/1000
dobicki_Nyz_360 <- dobicki_Nyz_360/1000



#############
# ATR/Close #
#############

tedenski_N_relativen <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    #ifelse(i < 700, t_N[i:(i+6)] <- tabela$spr_N[i], t_N[i:(i+6)] <- log(tabela$spr_N[i]))
    t_N[i:(i+6)] <- (tabela$spr_N[i]/tabela$Close[i])
  }
  t_N[1:nrow(tabela)]
}

proba5 <- dobicki_volatilnost(tabela = btc_1day, obdobje = 360)
proba6 <- dobicki_volatilnost(tabela = btc_1day, obdobje = 500)
proba7 <- dobicki_volatilnost(tabela = btc_1day, obdobje = 1000)
proba8 <- dobicki_volatilnost(tabela = btc_1day)
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
dobicki_logN_1800 <- dobicki_hitreje(tabela = btc_1day, strategija = "S1")
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

tedenski_N_relativen <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    #ifelse(i < 700, t_N[i:(i+6)] <- tabela$spr_N[i], t_N[i:(i+6)] <- log(tabela$spr_N[i]))
    #t_N[i:(i+6)] <- (tabela$spr_N[i]/tabela$Close[i])
    t_N[i:(i+6)] <- (tabela$spr_N[i]/tabela$sma[i])
  }
  t_N[1:nrow(tabela)]
}

dobicki_Nsma_360 <- dobicki_volatilnost(tabela = btc_1day, obdobje = 360)
dobicki_Nsma_500 <- dobicki_volatilnost(tabela = btc_1day, obdobje = 500)
dobicki_Nsma_1000 <- dobicki_volatilnost(tabela = btc_1day, obdobje = 1000)
dobicki_Nsma_1800 <- dobicki_volatilnost(tabela = btc_1day)
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

N_relativen <- function(tabela, metoda){
  tabela$tr1 <- tabela$TR/tabela$Close
  
  prvi_n <- mean(tabela$tr1[1:20])
  n <- c(rep(0,(20-1)), prvi_n, rep(0, (nrow(tabela)-20)))
  for(i in (20+1):nrow(tabela)){
    n[i] <- ((20-1)*n[i-1] + tabela$tr1[i])/20
  }
  tabela$N_relativen <- n
  tabela
}

tedenski_N_relativen <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    t_N[i:(i+6)] <- tabela$N_relativen[i]
  }
  t_N[1:nrow(tabela)]
}

dobicki_TR_360 <- dobicki_volatilnost(tabela = btc_1day, obdobje = 360)
dobicki_TR_500 <- dobicki_volatilnost(tabela = btc_1day, obdobje = 500)
dobicki_TR_1000 <- dobicki_volatilnost(tabela = btc_1day, obdobje = 1000)
dobicki_TR_1800 <- dobicki_volatilnost(tabela = btc_1day)
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
