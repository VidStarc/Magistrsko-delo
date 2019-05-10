
opt_dobicki <- function(tabela = btc_1day, dnevi_N = 20, vstop_s1 = 20, vstop_s2 = 55, izstop_s1_s2 = 10, 
                              obdobje = 1800, zacetni_kapital = 1000000, cena = "Close", add = 1/2, 
                              sl = 2, toleranca = 0.02, rr = 3, dnevi_ema1 = 10,
                              dnevi_ema2 = 50, indikator = "zelve_s1", metoda_izstop = "zelve"){
  
  tab <- poracuni(tabela, dnevi_N, vstop_s1, vstop_s2, izstop_s1_s2, cena, dnevi_ema1, 
                  dnevi_ema2, toleranca, rr, indikator, metoda_izstop)
  dobicki <- c()
  for(i in 1:(nrow(tab) - obdobje + 1)){
    tmp <- tab[i:(obdobje + i - 1),]
    cena2 <- odlocitev_cena(tmp, cena)
    i_izstop <- (i-1)
    dobicki <- c(dobicki, trgovanje(tmp, zacetni_kapital, cena2, add, sl, indikator, i_izstop))
  }
  c(mean(dobicki), sd(dobicki))
}


#####################
# Želvja strategija #
#####################

###
# Vstopi in izstopi iz pozicij
dobicki_vstop_izstop <- function(indikator){
  M <- matrix(0, 5, 5)
  N <- matrix(0, 5, 5)
  if(indikator == "zelve_s1"){
    zap_vstop <- seq(10, 30, 5)
    zap_izstop <- seq(6, 14, 2)
    for(i in 1:length(zap_vstop)){
      for(j in 1:length(zap_izstop)){
        tmp <- opt_dobicki(vstop_s1 = zap_vstop[i], izstop_s1_s2 = zap_izstop[j], indikator = indikator)
        M[i, j] <- tmp[1]
        N[i, j] <- tmp[2]
      }
    }
    matrika <- data.frame(M, row.names = zap_vstop)
    matrika1 <- data.frame(N, row.names = zap_vstop)
  }
  else{
    zap_vstop <- seq(45, 65, 5)
    zap_izstop <- seq(16, 24, 2)
    for(i in 1:length(zap_vstop)){
      for(j in 1:length(zap_izstop)){
        tmp <- opt_dobicki(vstop_s2 = zap_vstop[i], izstop_s1_s2 = zap_izstop[j], indikator = indikator)
        M[i, j] <- tmp[1]
        N[i, j] <- tmp[2]
      }
    }
    matrika <- data.frame(M, row.names = zap_vstop)
    matrika1 <- data.frame(N, row.names = zap_vstop)
  }
  data.frame(matrika, matrika1)
}

S1matrika_v_i <- dobicki_vstop_izstop("zelve_s1")
S1povprecje_dob_vstop_izstop <- S1matrika_v_i[,1:5]
colnames(S1povprecje_dob_vstop_izstop) <- seq(6, 14, 2)
S1sd_dob_vstop_izstop <- S1matrika_v_i[,6:10]
colnames(S1sd_dob_vstop_izstop) <- seq(6, 14, 2)

S2matrika_v_i <- dobicki_vstop_izstop("zelve_s2")
S2povprecje_dob_vstop_izstop <- S2matrika_v_i[,1:5]
colnames(S2povprecje_dob_vstop_izstop) <- seq(16, 24, 2)
S2sd_dob_vstop_izstop <- S2matrika_v_i[,6:10]
colnames(S2sd_dob_vstop_izstop) <- seq(16, 24, 2)


###
# volatilnost N 
dobicki_N <- function(indikator){
  M1 <- matrix(0, 2, 5)
  zap_N <- seq(10, 30, 5)
  ifelse(indikator == "zelve_s1", izstop_s1_s2 <- 10, izstop_s1_s2 <- 20)
  for(j in 1:length(zap_N)){
    tmp <- opt_dobicki(dnevi_N = zap_N[j], indikator = indikator, izstop_s1_s2 = izstop_s1_s2)
    M1[1, j] <- tmp[1]
    M1[2, j] <- tmp[2]
    }
  matrika <- data.frame(M1, row.names = c("povprecje", "sd"))
  colnames(matrika) <- zap_N
  matrika
}

S1dobicki_N <- dobicki_N("zelve_s1")
S2dobicki_N <- dobicki_N("zelve_s2")
# function dodan_kolicnik -> koda1.R
S1dobicki_N_2 <- dodan_kolicnik(S1dobicki_N)
S2dobicki_N_2 <- dodan_kolicnik(S2dobicki_N)


###
# Close, Open, Low, High cena
dobicki_cena <- function(indikator){
  M <- matrix(0, 2, 4)
  zap_cena <- c("Open", "Low", "High", "Close")
  ifelse(indikator == "zelve_s1", izstop_s1_s2 <- 10, izstop_s1_s2 <- 20)
  for(j in 1:length(zap_cena)){
    tmp <- opt_dobicki(cena = zap_cena[j], indikator = indikator, izstop_s1_s2 = izstop_s1_s2)
    M[1, j] <- tmp[1]
    M[2, j] <- tmp[2]
  }
  matrika <- data.frame(M, row.names = c("povprecje", "sd"))
  colnames(matrika) <- zap_cena
  matrika
}

S1dobicki_cena <- dobicki_cena("zelve_s1")
S2dobicki_cena <- dobicki_cena("zelve_s2")
# function dodan_kolicnik -> koda1.R
S1dobicki_cena_2 <- dodan_kolicnik(S1dobicki_cena)
S2dobicki_cena_2 <- dodan_kolicnik(S2dobicki_cena)


###
# dodajanje enot v pozicijo, stop - loss
dobicki_add_sl <- function(indikator){
  M <- matrix(0, 4, 4)
  N <- matrix(0, 4, 4)
  ifelse(indikator == "zelve_s1", izstop_s1_s2 <- 10, izstop_s1_s2 <- 20)
  zap_add <- c(1/4, 1/2, 3/4, 1)
  zap_sl <- c(1, 2, 3, 4)
  for(i in 1:length(zap_add)){
    for(j in 1:length(zap_sl)){
      tmp <- opt_dobicki(add = zap_add[i], sl = zap_sl[j], indikator = indikator, 
                         izstop_s1_s2 = izstop_s1_s2)
      M[i, j] <- tmp[1]
      N[i, j] <- tmp[2]
    }
  }
  matrika <- data.frame(M,row.names = zap_add)
  matrika1 <- data.frame(N, row.names = zap_add)
  data.frame(matrika, matrika1)
}

S1matrika_add_sl <- dobicki_add_sl("zelve_s1")
S1dobicki_add_sl <- S1matrika_add_sl[,1:4]
colnames(S1dobicki_add_sl) <- c(1, 2, 3, 4)
S1sd_add_sl <- S1matrika_add_sl[,5:8]
colnames(S1sd_add_sl) <- c(1, 2, 3, 4)

S2matrika_add_sl <- dobicki_add_sl("zelve_s2")
S2dobicki_add_sl <- S2matrika_add_sl[,1:4]
colnames(S2dobicki_add_sl) <- c(1, 2, 3, 4)
S2sd_add_sl <- S2matrika_add_sl[,5:8]
colnames(S2sd_add_sl) <- c(1, 2, 3, 4)


#####
# Najboljši parametri - nova strategija

novi_btc_360_S1 <- dobicki_trgovanje(btc_1day, dnevi_N = 10, vstop_s1 = 10, vstop_s2 = 50, izstop_s1_s2 = 10, 
                                     obdobje = 360, cena = "High", add = 0.25, sl = 4)
novi_btc_500_S1 <- dobicki_trgovanje(btc_1day, dnevi_N = 10, vstop_s1 = 10, vstop_s2 = 50, izstop_s1_s2 = 10, 
                                     obdobje = 500, cena = "High", add = 0.25, sl = 4)
novi_btc_1000_S1 <- dobicki_trgovanje(btc_1day, dnevi_N = 10, vstop_s1 = 10, vstop_s2 = 50, izstop_s1_s2 = 10, 
                                      obdobje = 1000, cena = "High", add = 0.25, sl = 4)
novi_btc_1800_S1 <- dobicki_trgovanje(btc_1day, dnevi_N = 10, vstop_s1 = 10, vstop_s2 = 50, izstop_s1_s2 = 10, 
                                      obdobje = 1800, cena = "High", add = 0.25, sl = 4)

novi_btc_360_S2 <- dobicki_trgovanje(btc_1day, dnevi_N = 10, vstop_s1 = 10, vstop_s2 = 50, izstop_s1_s2 = 24, 
                                     obdobje = 360, cena = "High", add = 0.25, sl = 4, indikator = "zelve_s2")
novi_btc_500_S2 <- dobicki_trgovanje(btc_1day, dnevi_N = 10, vstop_s1 = 10, vstop_s2 = 50, izstop_s1_s2 = 24, 
                                     obdobje = 500, cena = "High", add = 0.25, sl = 4, indikator = "zelve_s2")
novi_btc_1000_S2 <- dobicki_trgovanje(btc_1day, dnevi_N = 10, vstop_s1 = 10, vstop_s2 = 50, izstop_s1_s2 = 24, 
                                      obdobje = 1000, cena = "High", add = 0.25, sl = 4, indikator = "zelve_s2")
novi_btc_1800_S2 <- dobicki_trgovanje(btc_1day, dnevi_N = 10, vstop_s1 = 10, vstop_s2 = 50, izstop_s1_s2 = 24, 
                                      obdobje = 1800, cena = "High", add = 0.25, sl = 4, indikator = "zelve_s2")



#####################
# Podpore in odpori #
#####################

# Toleranca
dobicki_toleranca <- function(indikator, metoda_izstop){
  M <- matrix(0, 2, 5)
  zap_toleranca <- c(0.01, 0.015, 0.02, 0.025, 0.03)
  for(j in 1:5){
    tmp <- opt_dobicki(toleranca = zap_toleranca[j], indikator = indikator, metoda_izstop = metoda_izstop)
    M[1, j] <- tmp[1]
    M[2, j] <- tmp[2]
  }
  matrika <- data.frame(M, row.names = c("povprecje", "sd"))
  colnames(matrika) <- zap_toleranca
  matrika
}

SinRtoleranca <- dobicki_toleranca("SinR", "SinR")
SinRtoleranca_2 <- dodan_kolicnik(SinRtoleranca)


# Risk/Reward ratio
dobicki_rr <- function(indikator, metoda_izstop){
  M <- matrix(0, 4, 4)
  N <- matrix(0, 4, 4)
  zap_rr <- c(2, 3, 4, 1.5)
  zap_sl <- c(1, 2, 3, 4)
  for(i in 1:4){
    for(j in 1:4){
      tmp <- opt_dobicki(sl = zap_sl[i], rr = zap_rr[j], indikator = indikator, metoda_izstop = metoda_izstop)
      M[i, j] <- tmp[1]
      N[i, j] <- tmp[2]
    }
  }
  matrika <- data.frame(M,row.names = zap_sl)
  matrika1 <- data.frame(N, row.names = zap_sl)
  data.frame(matrika, matrika1)
}

SinRmatrika_rr <- dobicki_rr("SinR", "SinR")
SinRdobicki_rr <- SinRmatrika_rr[,1:4]
colnames(SinRdobicki_rr) <- c(2, 3, 4, 1.5)
SinRsd_rr <- SinRmatrika_rr[,5:8]
colnames(SinRsd_rr) <- c(2, 3, 4, 1.5)


##################
# Drseče sredine #
##################

dobicki_ma <- function(indikator, metoda_izstop){
  M <- matrix(0, 6, 6)
  N <- matrix(0, 6, 6)
  zap_shortma <- c(5, 10, 12, 20, 26, 50)
  zap_longma <- c(50, 75, 100, 125, 150, 200)
  for(i in 1:6){
    for(j in 1:6){
      tmp <- opt_dobicki(dnevi_ema1 = zap_shortma[i], dnevi_ema2 = zap_longma[j], indikator = indikator, 
                         metoda_izstop = metoda_izstop)
      M[i, j] <- tmp[1]
      N[i, j] <- tmp[2]
    }
  }
  matrika <- data.frame(M, row.names = zap_shortma)
  matrika1 <- data.frame(N, row.names = zap_shortma)
  data.frame(matrika, matrika1)
}

MAmatrika <- dobicki_ma("MA", "rr")
MAdobicki <- MAmatrika[,1:6]
colnames(MAdobicki) <- c(50, 75, 100, 125, 150, 200)
MAsd <- MAmatrika[,7:12]
colnames(MAsd) <- c(50, 75, 100, 125, 150, 200)








