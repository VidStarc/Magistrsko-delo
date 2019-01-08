tabela <- btc_1day
cena <- data.frame(tabela$Close)
velikost_oken <- 10


lok_eks <- function(tabela, cena, velikost_oken){
  library(plyr)
  casovna_vrsta <- cena[,1]
  N <- length(casovna_vrsta)
  stp <- velikost_oken
  #st_okvirjev <- floor(N/stp)
  whichMinz <- c()
  whichMaxz <- c()
  st_min <- c()
  st_max <- c()
  for(j in 1:(N-stp)){
    tmp_whichMinz <- which.min(casovna_vrsta[(j+1):(j+stp-2)]) + j
    pogoj <- (length(whichMinz[whichMinz == tmp_whichMinz]) > 0)
    if(!pogoj){
      whichMinz <- c(whichMinz, tmp_whichMinz)
      st_min <- c(st_min, 1)
    }
    else{
      st_min[length(st_min)] <- st_min[length(st_min)] + 1
    }
    
    tmp_whichMaxz <- which.max(casovna_vrsta[(j+1):(j+stp-2)]) + j
    pogoj1 <- (length(whichMaxz[whichMaxz == tmp_whichMaxz]) > 0)
    if(!pogoj1){
      whichMaxz <- c(whichMaxz, tmp_whichMaxz)
      st_max <- c(st_max, 1)
    }
    else{
      st_max[length(st_max)] <- st_max[length(st_max)] + 1
    }
  }
  whichMinz <- whichMinz[st_min > 1]
  whichMaxz <- whichMaxz[st_max > 1]
  minz <- casovna_vrsta[whichMinz]
  maxz <- casovna_vrsta[whichMaxz]
  
  list("kje_min" = whichMinz, "minz" = minz, "kje_max" = whichMaxz, "maxz" = maxz)
}

lok_ekst <- lok_eks(btc_1day, data.frame(tabela$Close), 10)

cas <- btc_1day$Timestamp[1:100]
zak_cena <- btc_1day$Close[1:100]
ts.plot(zak_cena)
points(lok_ekst$kje_min, lok_ekst$minz, col="blue")
points(lok_ekst$kje_max, lok_ekst$maxz, col="red")

