spr_dobicki_trgovanje <- function(tabela, dnevi_N = 20, obdobje = 1800, zacetni_kapital = 1000000, 
                              cena = "Close", add = 1/2, sl = 2, toleranca = 0.02, rr = 3){
  tab <- poracuni(tabela, dnevi_N, cena, toleranca, rr)
  dobicki <- c()
  for(i in 1:(nrow(tab) - obdobje + 1)){
    tmp <- tab[i:(obdobje + i - 1),]
    cena2 <- odlocitev_cena(tmp, cena)
    i_izstop <- (i-1)
    dobicki <- c(dobicki, trgovanje(tmp, zacetni_kapital, cena2, add, sl))
  }
  c(mean(dobicki), sd(dobicki))
}


# Toleranca
dobicki_toleranca <- function(tabela){
  M <- matrix(0, 2, 5)
  zap_toleranca <- c(0.01, 0.015, 0.02, 0.025, 0.03)
  for(j in 1:5){
    tmp <- spr_dobicki_trgovanje(tabela, toleranca = zap_toleranca[j])
    M[1, j] <- tmp[1]
    M[2, j] <- tmp[2]
  }
  matrika <- data.frame(M, row.names = c("povprecje", "sd"))
  colnames(matrika) <- zap_toleranca
  matrika
}

SinRtoleranca <- dobicki_toleranca(btc_1day)

dodan_kolicnik <- function(tabela){
  kolicnik <- c()
  for(i in 1:ncol(tabela)){
    kolicnik <- c(kolicnik, round(tabela[1, i]/tabela[2, i], 2))
  }
  rbind(tabela, "kolicnik" = kolicnik)
}

SinRtoleranca_2 <- dodan_kolicnik(SinRtoleranca)


# Risk/Reward ratio
dobicki_rr <- function(tabela){
  M <- matrix(0, 4, 4)
  N <- matrix(0, 4, 4)
  zap_rr <- c(2, 3, 4, 1.5)
  zap_sl <- c(1, 2, 3, 4)
  for(i in 1:4){
    for(j in 1:4){
      tmp <- spr_dobicki_trgovanje(tabela, sl = zap_sl[i], rr = zap_rr[j])
      M[i, j] <- tmp[1]
      N[i, j] <- tmp[2]
    }
  }
  matrika <- data.frame(M,row.names = zap_sl)
  matrika1 <- data.frame(N, row.names = zap_sl)
  data.frame(matrika, matrika1)
}

SinRmatrika_rr <- dobicki_rr(btc_1day)
SinRdobicki_rr <- SinRmatrika_rr[,1:4]
colnames(SinRdobicki_rr) <- c(2, 3, 4, 1.5)
SinRsd_rr <- SinRmatrika_rr[,5:8]
colnames(SinRsd_rr) <- c(2, 3, 4, 1.5)


spr <- function(tabela, name){
  if(name == "tol"){tmp <- cbind("toleranca" = rownames(tabela), tabela)}
  if(name == "rr"){tmp <- cbind("sl/rr" = rownames(tabela), tabela)}
  tmp
}

library(officer)
library(flextable)
flextabela_matrika(spr(SinRtoleranca_2, "tol"), 1)


flextabela_matrika(spr(cagr(SinRdobicki_rr), "rr"), 2)
flextabela_matrika(spr(SinRdobicki_rr/SinRsd_rr, "rr"), 2)


