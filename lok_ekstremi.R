tabela <- btc_1day
cena <- data.frame(tabela$Close)
velikost_oken <- 10


lok_ekstremi <- function(tabela, cena, velikost_oken){
  casovna_vrsta <- cena[,1]
  N <- length(casovna_vrsta)
  stp <- velikost_oken
  st_okvirjev <- floor(N/stp)
  minz <- array(0.0, dim=st_okvirjev)
  whichMinz <- array(0, dim=st_okvirjev)
  maxz <- array(0.0, dim=st_okvirjev)
  whichMaxz = array(0, dim=st_okvirjev)
  for(j in 1:(st_okvirjev-1)){
    lft <- (j-1)*stp + 1  #left and right elements of each chunk
    rght <- j*stp
    whichMinz[j] <- which.min(casovna_vrsta[lft:rght]) + lft - 1
    minz[j] <- min(casovna_vrsta[lft:rght])
    whichMaxz[j] <- which.max(casovna_vrsta[lft:rght]) + lft - 1
    maxz[j] <- max(casovna_vrsta[lft:rght])
  }   
  #zadnji okvir
  lft <- j*stp + 1  #left and right elements of each chunk
  rght <- N
  whichMinz[st_okvirjev] <- which.min(casovna_vrsta[lft:rght]) + lft - 1
  minz[st_okvirjev] <- min(casovna_vrsta[lft:rght])
  whichMaxz[st_okvirjev] <- which.max(casovna_vrsta[lft:rght]) + lft - 1
  maxz[st_okvirjev] <- max(casovna_vrsta[lft:rght])
  list("kje_min" = whichMinz, "minz" = minz, "kje_max" = whichMaxz, "maxz" = maxz)
}


# lok_ekst je bila funkcija, ki sme jo dal v r&s ker je boljša v iskanju ekstremov
lok_ekst <- lok_eks(btc_1day, data.frame(tabela$Close), 10)

cas <- btc_1day$Timestamp[1:100]
zak_cena <- btc_1day$Close[1:100]
ts.plot(zak_cena)
points(lok_ekst$kje_min, lok_ekst$minz, col="blue")
points(lok_ekst$kje_max, lok_ekst$maxz, col="red")

