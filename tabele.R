library(flextable)
library(officer)

#source("koda1.R")

################
# Lepše tabele #
################

izpisTabele <- function(data, prvi_stolpec = "numeric"){
  library(flextable)
  library(dplyr)
  ime <- c("obdobje", "st_podatkov", "povprecje", "sd", "min", "max", "st_manj_0", "pred_2014", "po_2014",
           "teoreticna_izguba")
  for(i in ime){
    if(i %in% (data %>% names())) {
      data[i] <- format(round(data[i]), big.mark = ".", decimal.mark = ",")  
    }
  }
  ime <- c("kolicnik", "kol_pred", "kol_po")
  for(i in ime){
    if(i %in% (data %>% names())) {
      data[i] <- format(round(data[i], 2), big.mark = ".", decimal.mark = ",")  
    }
  }
  ime <- c("lsd", "verj_izgube", "lsd_pred_2014", "lsd_po_2014", "psd_pred_2014", "psd_po_2014", 
           "psd", "lsd_pred", "lsd_po")
  for(i in ime){
    if(i %in% (data %>% names())) {
      tmp <- format(round(data[i], 2),  big.mark = ".", decimal.mark = ",") %>% pull(1)
      data[i] <- paste0(tmp, "%")
    }
  }
  ime <- "pov_procenti"
  if(ime %in% (data %>% names())) {
    tmp <- format(round(data[ime]),  big.mark = ".", decimal.mark = ",") %>% pull(1)
    data[ime] <- paste0(tmp, "%")
  }
  if(prvi_stolpec == "numeric"){
    data %>% regulartable() %>% bg(bg = "coral", part = "header") %>% bg(bg = "cyan", part = "body") %>%
      bold(part = "header") %>% align(align="center", part="header") %>% align(align="right", part="body")
  }
  else{
    t <- prvi_stolpec
    data %>% regulartable() %>% bg(bg = "coral", part = "header") %>% bg(bg = "cyan", part = "body") %>%
      bold(part = "header") %>% align(align="center", part="header") %>% align(align="right", part="body") %>% 
      align(j = 1:t, align="left")
  }
}


####################
# PREGLED DOBIČKOV #
####################

pregled_dobicki <- function(l1, l2, l3, l4, indikator, zacetni_kapital = 1000000){
  obdobje <- as.integer(c(360, 500, 1000, 1800))
  st_podatkov <- c(length(l1), length(l2), length(l3), length(l4))
  povprecje <- c(mean(l1), mean(l2), mean(l3), mean(l4))
  sd <- c(sd(l1), sd(l2), sd(l3), sd(l4))
  min <- c(min(l1), min(l2), min(l3), min(l4))
  max <- c(max(l1), max(l2), max(l3), max(l4))
  manj_0 <- c(sum(l1 < 0), sum(l2 < 0), sum(l3 < 0), sum(l4 < 0))
  verj <- round((manj_0/st_podatkov)*100, 2)
  
  if(indikator == "zelve"){
    mean_proc <- round((povprecje/zacetni_kapital)*100, 2)
    pregled <- data.frame("obdobje" = obdobje, "st_podatkov" = st_podatkov, "povprecje" = povprecje, 
                        "sd" = sd, "min" = min, "max" = max, "pov_procenti" = mean_proc,
                        "st_manj_0" = manj_0, "verj_izgube" = verj)
  }
  else{
    lsd <- c(round(cagr(tabela = mean(l1), obdobje = 360), 2),
           round(cagr(tabela = mean(l2), obdobje = 500), 2),
           round(cagr(tabela = mean(l3), obdobje = 1000), 2),
           round(cagr(tabela = mean(l4), obdobje = 1800), 2))
    kolicnik <- round(povprecje/sd,2)
    pregled <- data.frame("obdobje" = obdobje, "st_podatkov" = st_podatkov, "povprecje" = povprecje, 
                        "sd" = sd, "min" = min, "max" = max, "lsd" = lsd, "kolicnik" = kolicnik,
                        "st_manj_0" = manj_0, "verj_izgube" = verj)
  }
  
  pregled
}


#########
# Želvja strategija

pregled_btc_S1 <- pregled_dobicki(btc_360_S1, btc_500_S1, btc_1000_S1, btc_1800_S1, "zelve")
pregled_btc_S2 <- pregled_dobicki(btc_360_S2, btc_500_S2, btc_1000_S2, btc_1800_S2, "zelve")

pregled_btc_S1_2 <- pregled_dobicki(btc_360_S1_2, btc_500_S1_2, btc_1000_S1_2, btc_1800_S1_2, "zelve", 
                                    zacetni_kapital = 50000)
pregled_btc_S2_2 <- pregled_dobicki(btc_360_S2_2, btc_500_S2_2, btc_1000_S2_2, btc_1800_S2_2, "zelve",
                                    zacetni_kapital = 50000)

#1h
pregled_btc_1h_S1 <- pregled_dobicki(btc_1h_360_S1, btc_1h_500_S1, btc_1h_1000_S1, btc_1h_1800_S1, "zelve")
pregled_btc_1h_S2 <- pregled_dobicki(btc_1h_360_S2, btc_1h_500_S2, btc_1h_1000_S2, btc_1h_1800_S2, "zelve")

#5min
pregled_btc_5min_S1 <- pregled_dobicki(btc_5min_360_S1, btc_5min_500_S1, btc_5min_1000_S1, 
                                       btc_5min_1800_S1, "zelve")
pregled_btc_5min_S2 <- pregled_dobicki(btc_5min_360_S2, btc_5min_500_S2, btc_5min_1000_S2, 
                                       btc_5min_1800_S2, "zelve")


# 1day
pregled_btc_S1 %>% izpisTabele()
pregled_btc_S2 %>% izpisTabele()
pregled_btc_S1_2 %>% izpisTabele()
pregled_btc_S2_2 %>% izpisTabele()
# 1h
pregled_btc_1h_S1 %>% izpisTabele()
pregled_btc_1h_S2 %>% izpisTabele()
#5min
pregled_btc_5min_S1 %>% izpisTabele()
pregled_btc_5min_S2 %>% izpisTabele()



#########
# Podpore in odpori
SinR_pregled_btc <- pregled_dobicki(SinR_btc_360, SinR_btc_500, SinR_btc_1000, 
                                      SinR_btc_1800, "SinR")
SinR_pregled_btc %>% izpisTabele()


g_SinR_pregled_btc <- pregled_dobicki(g_SinR_btc_360, g_SinR_btc_500, g_SinR_btc_1000, 
                                        g_SinR_btc_1800, "SinR_g")
g_SinR_pregled_btc %>% izpisTabele()


#########
# Drseče sredine
MA_pregled_btc <- pregled_dobicki(MA_btc_360, MA_btc_500, MA_btc_1000, 
                                    MA_btc_1800, "MA")
MA_pregled_btc %>% izpisTabele()


MA1_pregled_btc <- pregled_dobicki(MA1_btc_360, MA1_btc_500, MA1_btc_1000, 
                                     MA1_btc_1800, "MA1")
MA1_pregled_btc %>% izpisTabele()



###########################
# Pregled CAGR + kolicnik #
###########################

# function cagr -> koda1.R

pregled_cagr <- function(tabela, enota, zacetni_kapital = 1000000){
  obdobje <- c(360, 500, 1000, 1800)
  cagr <- c()
  ratio <- c()
  for(i in 1:4){
    cagr <- c(cagr, cagr(tabela$povprecje[i], zacetni_kapital = zacetni_kapital, obdobje = obdobje[i]))
    ratio <- c(ratio, tabela$povprecje[i]/tabela$sd[i])
  }
  if(enota == "d"){
    pregled <- data.frame("obdobje" = obdobje, "kolicnik" = ratio, "lsd" = cagr)
  }
  else{
    pregled <- data.frame("obdobje" = obdobje, "kolicnik" = ratio, "psd" = cagr)
  }
  pregled
}

# 1day
pregled_cagr(pregled_btc_S1, "d") %>% izpisTabele()
pregled_cagr(pregled_btc_S2, "d") %>% izpisTabele()
# 1h
pregled_cagr(pregled_btc_1h_S1, "h") %>% izpisTabele()
pregled_cagr(pregled_btc_1h_S2, "h") %>% izpisTabele()
# 5min
pregled_cagr(pregled_btc_5min_S1, "m") %>% izpisTabele()
pregled_cagr(pregled_btc_5min_S2, "m") %>% izpisTabele()



#############################
# manjsa in vecja uspešnost #
#############################

# povprečje in povprečna stopnja donosa pri manjši in večji uspešnosti
manj_vec <- function(l1, l2, l3, l4, enota, meja, meja_1800){
  manjsa <- c(mean(l1[(meja+1):length(l1)]), mean(l2[(meja+1):length(l2)]), mean(l3[(meja+1):length(l3)]), 
              mean(l4[(meja_1800+1):length(l4)]))
  vecja <- c(mean(l1[1:meja]), mean(l2[1:meja]), mean(l3[1:meja]), mean(l4[1:meja_1800]))
  cagr_manjsa <- c(cagr(manjsa[1], obdobje = 360), cagr(manjsa[2], obdobje = 500), 
                      cagr(manjsa[3], obdobje = 1000), cagr(manjsa[4]))
  cagr_vecja <- c(cagr(vecja[1], obdobje = 360), cagr(vecja[2], obdobje = 500), 
                    cagr(vecja[3], obdobje = 1000), cagr(vecja[4]))
  if(enota == "d"){
    data.frame("obdobje" = c(360, 500, 1000, 1800), 
             "pred_2014" = vecja, "po_2014" = manjsa, 
             "lsd_pred_2014" = cagr_vecja, 
             "lsd_po_2014" = cagr_manjsa)
  }
  else{
    data.frame("obdobje" = c(360, 500, 1000, 1800), 
               "pred_2014" = vecja, "po_2014" = manjsa, 
               "psd_pred_2014" = cagr_vecja, 
               "psd_po_2014" = cagr_manjsa)
  }
}

# 1day
S1_manj_vec <- manj_vec(btc_360_S1, btc_500_S1, btc_1000_S1, btc_1800_S1, "d", 700, 400)

# 1h
S1_1h_manj_vec <- manj_vec(btc_1h_360_S1, btc_1h_500_S1, btc_1h_1000_S1, 
                                   btc_1h_1800_S1, "h", meja, meja_1800)
#5min
S1_5min_manj_vec <- manj_vec(btc_5min_360_S1, btc_5min_500_S1, btc_5min_1000_S1, 
                                     btc_5min_1800_S1, "m", meja, meja_1800)

S1_manj_vec %>% izpisTabele()
#1h
S1_1h_manj_vec %>% izpisTabele()
#5min
S1_5min_manj_vec %>% izpisTabele()


# Standardna deviacija pri manjši in večji uspešnosti
sd_manj_vec <- function(l1, l2, l3, l4, meja, meja_1800){
  manjsa <- c(mean(l1[(meja+1):length(l1)]), mean(l2[(meja+1):length(l2)]), mean(l3[(meja+1):length(l3)]), 
              mean(l4[(meja_1800+1):length(l4)]))
  vecja <- c(mean(l1[1:meja]), mean(l2[1:meja]), mean(l3[1:meja]), mean(l4[1:meja_1800]))
  sd_manjsa <- c(sd(l1[(meja+1):length(l1)]), sd(l2[(meja+1):length(l2)]), sd(l3[(meja+1):length(l3)]), 
                 sd(l4[(meja_1800+1):length(l4)]))
  sd_vecja <- c(sd(l1[1:meja]), sd(l2[1:meja]), sd(l3[1:meja]), sd(l4[1:meja_1800]))
  kolicnik_manjsa <- manjsa/sd_manjsa
  kolicnik_vecja <- vecja/sd_vecja
  data.frame("obdobje" = c(360, 500, 1000, 1800), 
             "pred_2014" = sd_vecja, "po_2014" = sd_manjsa, 
             "kol_pred" = kolicnik_vecja, "kol_po" = kolicnik_manjsa)
}

S1sd_manj_vec <- sd_manj_vec(btc_360_S1, btc_500_S1, btc_1000_S1, btc_1800_S1, 700, 400)
#1h
S1sd_1h_manj_vec <- sd_manj_vec(btc_1h_360_S1, btc_1h_500_S1, btc_1h_1000_S1, 
                                        btc_1h_1800_S1, meja, meja_1800)
#5min
S1sd_5min_manj_vec <- sd_manj_vec(btc_5min_360_S1, btc_5min_500_S1, btc_5min_1000_S1, 
                                          btc_5min_1800_S1, meja, meja_1800)

# Flextabela
S1sd_manj_vec %>% izpisTabele()
#1h
S1sd_1h_manj_vec %>% izpisTabele()
#5min
S1sd_5min_manj_vec %>% izpisTabele()


#########
# Podpore in odpori

SinR_manj_vec <- manj_vec(SinR_btc_360, SinR_btc_500, SinR_btc_1000, 
                                  SinR_btc_1800, "d", 700, 400)
SinR_manj_vec %>% izpisTabele()


SinR_sd_manj_vec <- sd_manj_vec(SinR_btc_360, SinR_btc_500, SinR_btc_1000, 
                                        SinR_btc_1800, 700, 400)
SinR_sd_manj_vec %>% izpisTabele()


#########
# Drseče sredine

# EMA
MA_manj_vec <- manj_vec(MA_btc_360, MA_btc_500, MA_btc_1000, 
                                MA_btc_1800, "d", 700, 400)
MA_manj_vec %>% izpisTabele()


MA_sd_manj_vec <- sd_manj_vec(MA_btc_360, MA_btc_500, MA_btc_1000, 
                                      MA_btc_1800, 700, 400)
MA_sd_manj_vec %>% izpisTabele()


# SMA
MA1_manj_vec <- manj_vec(MA1_btc_360, MA1_btc_500, MA1_btc_1000, 
                                 MA1_btc_1800, "d", 700, 400)
MA1_manj_vec %>% izpisTabele()


MA1_sd_manj_vec <- sd_manj_vec(MA1_btc_360, MA1_btc_500, MA1_btc_1000, 
                                       MA1_btc_1800, 700, 400)
MA1_sd_manj_vec %>% izpisTabele()


##############################################################################################################
# OPTIMIZACIJA
##############################################################################################################

izpisMatrike <- function(data, kaj = "povprecje"){
  library(flextable)
  library(dplyr)
  library(officer)
  n <- nrow(data)
  c <- ncol(data)
  if("kolicnik" %in% (data %>% rownames())) {
   for(j in 2:c){
     data["kolicnik",j] <- format(round(data["kolicnik",j], 2), big.mark = ".", decimal.mark = ",")
     for(i in 1:(n-1)){
       data[i,j] <- format(round(as.numeric(data[i,j])), big.mark = ".", decimal.mark = ",")
     }
   }
  }
  else{
    if(kaj == "povprecje"){
      for(j in 2:c){
        data[,j] <- format(round(data[,j]), big.mark = ".", decimal.mark = ",")
      }
    }
    else{
      for(j in 2:c){
        data[,j] <- format(round(data[,j], 2), big.mark = ".", decimal.mark = ",")
      }
    }
  }
  big_border <- fp_border(color="black", width = 2)
  data %>% regulartable() %>% bg(bg = "coral", part = "header") %>% bg(bg = "cyan", part = "body") %>% 
    bold(part = "header") %>% bold(j = 1, part = "body") %>% align(align = "center", part = "all") %>% 
    bg(bg = "coral", j = 1) %>% border_remove() %>% hline_top(border = big_border, part = "header") %>% 
    hline_bottom(border = big_border, part = "body") %>% vline_left(border = big_border) %>% 
    vline_right(border = big_border) %>% hline(i = 1, j = 2:c, border = big_border, part = "header") %>% 
    vline(i = 1:n, j = 1, border = big_border, part = "body")
}

spr <- function(tabela, name){
  if(name == "vi"){tmp <- cbind("vstop/izstop" = rownames(tabela), tabela)}
  if(name == "N"){tmp <- cbind("N" = rownames(tabela), tabela)}
  if(name == "cena"){tmp <- cbind("cena" = rownames(tabela), tabela)}
  if(name == "add_sl"){tmp <- cbind("dod/stop_loss" = rownames(tabela), tabela)}
  if(name == "tol"){tmp <- cbind("toleranca" = rownames(tabela), tabela)}
  if(name == "rr"){tmp <- cbind("sl/rr" = rownames(tabela), tabela)}
  if(name == "ma"){tmp <- cbind("short/long" = rownames(tabela), tabela)}
  tmp
}


#####################
# Želvja strategija #
#####################

spr(S1povprecje_dob_vstop_izstop, "vi") %>% izpisMatrike()
spr(S1sd_dob_vstop_izstop, "vi") %>% izpisMatrike()
spr(S2povprecje_dob_vstop_izstop, "vi") %>% izpisMatrike()
spr(S2sd_dob_vstop_izstop, "vi") %>% izpisMatrike()

spr(cagr(S1povprecje_dob_vstop_izstop), "vi") %>% izpisMatrike("lsd")
spr(cagr(S2povprecje_dob_vstop_izstop), "vi") %>% izpisMatrike("lsd")
spr(S1povprecje_dob_vstop_izstop/S1sd_dob_vstop_izstop, "vi") %>% izpisMatrike("kolicnik")
spr(S2povprecje_dob_vstop_izstop/S2sd_dob_vstop_izstop, "vi") %>% izpisMatrike("kolicnik")

spr(S1dobicki_N_2, "N") %>% izpisMatrike()
spr(S2dobicki_N_2, "N") %>% izpisMatrike()

spr(S1dobicki_cena_2, "cena") %>% izpisMatrike()
spr(S2dobicki_cena_2, "cena") %>% izpisMatrike()

spr(S1dobicki_add_sl, "add_sl") %>% izpisMatrike()
spr(S1sd_add_sl, "add_sl") %>% izpisMatrike()
spr(S2dobicki_add_sl, "add_sl") %>% izpisMatrike()
spr(S2sd_add_sl, "add_sl") %>% izpisMatrike()

spr(cagr(S1dobicki_add_sl), "add_sl") %>% izpisMatrike("lsd")
spr(cagr(S2dobicki_add_sl), "add_sl") %>% izpisMatrike("lsd")
spr(S1dobicki_add_sl/S1sd_add_sl, "add_sl") %>% izpisMatrike("kolicnik")
spr(S2dobicki_add_sl/S2sd_add_sl, "add_sl") %>% izpisMatrike("kolicnik")


#####
# Najboljši parametri - nova strategija
novi_pregled_btc_S1 <- pregled_dobicki(novi_btc_360_S1, novi_btc_500_S1, novi_btc_1000_S1, 
                                       novi_btc_1800_S1, "zelve")
novi_pregled_btc_S2 <- pregled_dobicki(novi_btc_360_S2, novi_btc_500_S2, novi_btc_1000_S2, 
                                       novi_btc_1800_S2, "zelve")

novi_pregled_btc_S1 %>% izpisTabele()
novi_pregled_btc_S2 %>% izpisTabele()


pregled_cagr(novi_pregled_btc_S1, "d") %>% izpisTabele()
pregled_cagr(novi_pregled_btc_S2, "d") %>% izpisTabele()


# Manjsi in večji trendi
novi_manj_vec <- manj_vec(novi_btc_360_S1, novi_btc_500_S1, novi_btc_1000_S1, novi_btc_1800_S1, "d", 
                          700, 400)
novi_manj_vec %>% izpisTabele()

novi_sd_manj_vec <- sd_manj_vec(novi_btc_360_S1, novi_btc_500_S1, novi_btc_1000_S1, novi_btc_1800_S1, 
                                700, 400)
novi_sd_manj_vec %>% izpisTabele()



#####################
# Podpore in odpori #
#####################

spr(SinRtoleranca_2, "tol") %>% izpisMatrike()

spr(cagr(SinRdobicki_rr), "rr") %>% izpisMatrike("lsd")
spr(SinRdobicki_rr/SinRsd_rr, "rr") %>% izpisMatrike("kolicnik")



###################
# Dreseče sredine #
###################

spr(cagr(MAdobicki), "ma") %>% izpisMatrike("lsd")
spr(MAdobicki/MAsd, "ma") %>% izpisMatrike("kolicnik")



#########################################
# Pregled dobičkov - Ostali indikatorji #
#########################################

pregled_indikatorji <- function(l1, l2, l3){
  obdobje <- c("360", "360", "360")
  st_podatkov <- c(length(l1), length(l2), length(l3))
  povprecje <- c(mean(l1), mean(l2), mean(l3))
  sd <- c(sd(l1), sd(l2), sd(l3))
  min <- c(min(l1), min(l2), min(l3))
  max <- c(max(l1), max(l2), max(l3))
  lsd <- c(round(cagr(tabela = mean(l1), obdobje = 360), 2),
           round(cagr(tabela = mean(l2), obdobje = 360), 2),
           round(cagr(tabela = mean(l3), obdobje = 360), 2))
  kolicnik <- round(povprecje/sd,2)
  manj_0 <- c(sum(l1 < 0), sum(l2 < 0), sum(l3 < 0))
  verj <- round((manj_0/st_podatkov)*100, 2)
  pregled <- data.frame("obdobje" = obdobje, "st_podatkov" = st_podatkov, "povprecje" = povprecje, 
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

indikatorji %>% izpisTabele(1)


#########
# Združitev 9ih indikatorjev

INDI_pregled_btc <- pregled_dobicki(INDI_dobicki_btc_360, INDI_dobicki_btc_500, INDI_dobicki_btc_1000, 
                                      INDI_dobicki_btc_1800, "indi")
INDI_pregled_btc %>% izpisTabele()


indi2_pregled_btc <- pregled_dobicki(indi2_dobicki_btc_360, indi2_dobicki_btc_500, indi2_dobicki_btc_1000, 
                                       indi2_dobicki_btc_1800, "indi")
indi2_pregled_btc %>% izpisTabele()


indi3_pregled_btc <- pregled_dobicki(indi3_dobicki_btc_360, indi3_dobicki_btc_500, indi3_dobicki_btc_1000, 
                                       indi3_dobicki_btc_1800, "indi")
indi3_pregled_btc %>% izpisTabele()



##############
# Primer_kol #
##############

primer <- data.frame("povprecje" = c(5, 90),
                    "sd" = c(10, 100),
                    "kolicnik" = c(0.5, 0.9),
                    "teoreticna_izguba" = c(-5, -10))

primer %>% izpisTabele()







