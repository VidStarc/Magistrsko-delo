
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

flextabela_pregled(preizkus, 1)

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



