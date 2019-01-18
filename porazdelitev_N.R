
spr_N <- function(tabela, dnevi){
  library(QuantTools)
  tr <- c()
  tr1 <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    tr <- append(tr, max(h-l, h-pdc, pdc-l))
    dc <- tabela$Close[i]
    tr1 <- append(tr1, max(h-l, h-pdc, pdc-l)/dc)
  }
  tabela$TR <- c(0,tr)
  tabela$TR1 <- c(0, tr1)
  tabela <- tabela[-1,]
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  tabela$spr_N <- n
  
  prvi_n1 <- mean(tabela$TR1[1:dnevi])
  n1 <- c(rep(0,(dnevi-1)), prvi_n1, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n1[i] <- ((dnevi-1)*n1[i-1] + tabela$TR1[i])/dnevi
  }
  tabela$N1 <- n1
  tabela$sma <- sma(tabela$Close, 20)
  tabela
}

spr_tedenski_N <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    t_N[i:(i+6)] <- tabela$spr_N[i]
  }
  t_N[1:nrow(tabela)]
}

tabela <- izracun_za_analizo(btc_1day, 20, 20, 55, "Close")

ATR_C_N <- tabela$spr_N/tabela$Close
logaritemski_N <- c(tabela$spr_N[1:700], log(tabela$spr_N[701:nrow(tabela)]))
ATR_SMA_N <- tabela$spr_N/tabela$sma
TR_C_N <- tabela$N1

hist(ATR_C_N, breaks = 20)
hist(logaritemski_N)
hist(ATR_SMA_N)
hist(TR_C_N)
hist(tabela$spr_N)



ohlc <- tabela[, 2:5]
library(TTR)
hist(volatility(ohlc, n = 20, N = 365 , calc = "close"))
