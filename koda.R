library(dplyr)
library(QuantTools)
library(ggplot2)
library(gridExtra)
library(flextable)
library(officer)
library(plyr)
library(Ckmeans.1d.dp)
library(rpatrec)
library(TRR)
library(tictoc)


#izračuna True Range in N
PR_N <- function(tabela, dnevi){
  tr <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    tr <- append(tr, max(h-l, h-pdc, pdc-l))
  }
  tabela$TR <- c(0,tr)
  tabela <- tabela[-1,]
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  tabela$N <- n
  tabela
}



unit <- function(account_size, N){
  floor((0.01*account_size)/N)
}


#N so dobili izračunan na začetku tedna
tedenski_N <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    t_N[i:(i+6)] <- tabela$N[i]
  }
  tabela$tedenski_N <- t_N[1:nrow(tabela)]
  tabela
}

odlocitev_cena <- function(tabela, cena){
  if(cena == "Close"){cena1 <- data.frame(tabela$Close)}
  if(cena == "Low"){cena1 <- data.frame(tabela$Low)}
  if(cena == "High"){cena1 <- data.frame(tabela$High)}
  if(cena == "Open"){cena1 <- data.frame(tabela$Open)}
  cena1
}


#####################
# Želvja strategija #
#####################


#vstopne točke: 1=go long, 2=go short
vstop_TU <- function(tabela, dnevi, cena){
  entry <- rep(0, nrow(tabela))
  for(i in (dnevi+1):nrow(tabela)){
    if(cena[i,] > max(cena[(i-dnevi):(i-1),])){entry[i] <- 1}
    if(cena[i,] < min(cena[(i-dnevi):(i-1),])){entry[i] <- 2}
  }
  entry
}

#izstopne točke
win_izstop_turtle <- function(tabela, dnevi, cena){
  # dna
  dna <- rep(0, nrow(tabela))
  for(i in (dnevi+1):nrow(tabela)){
    if(cena[i,] < min(cena[(i-dnevi):(i-1),])){dna[i] <- 1}
  }
  # vrhovi
  vrhovi <- rep(0, nrow(tabela))
  for(i in (dnevi+1):nrow(tabela)){
    if(cena[i,] > max(cena[(i-dnevi):(i-1),])){vrhovi[i] <- 1}
  }
  # izstopi
  izstop <- rep(0, nrow(tabela))
  for(i in 1:nrow(tabela)){
    if(tabela$entry[i] == 1 | tabela$entry_s2[i] == 1){
      kandidati_izstop <- which(dna == 1)
      izstop[i] <- kandidati_izstop[kandidati_izstop > i][1]
    }
    if(tabela$entry[i] == 2 | tabela$entry_s2[i] == 2){
      kandidati_izstop <- which(vrhovi == 1)
      izstop[i] <- kandidati_izstop[kandidati_izstop > i][1]
    }
  }
  izstop
}


#####################
# Podpore in odpori #
#####################

#####
# Z lokalnimi ekstremi

lok_ekstremi <- function(tabela, cena, velikost_oken){
  library(plyr)
  casovna_vrsta <- cena[,1]
  N <- length(casovna_vrsta)
  stp <- velikost_oken
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

prepoznavalnik_SinR <- function(tabela, cena, velikost_oken){
  library(Ckmeans.1d.dp)
  eks <- lok_ekstremi(tabela, cena, velikost_oken)
  minz <- eks$minz
  maxz <- eks$maxz
  
  #clustering
  mera_podpora <- c()
  mera_odpor <- c()
  for(i in 10:80){
    fit <- kmeans(minz, i)
    fit1 <- kmeans(maxz, i)
    mera_podpora <- c(mera_podpora, fit$tot.withinss)
    mera_odpor <- c(mera_odpor, fit1$tot.withinss)
  }
  st_skupin_podpora <- 9 + which.min(mera_podpora)
  st_skupin_odpor <- 9 + which.min(mera_odpor)
  tmp <- Ckmeans.1d.dp(minz, st_skupin_podpora)
  tmp1 <- Ckmeans.1d.dp(maxz, st_skupin_odpor)
  podpore <- tmp$centers
  odpori <- tmp1$centers
  list("podpore" = podpore, "odpori" = odpori, "odmiki_p" = tmp$tot.withinss, 
       "odmiki_o" = tmp1$tot.withinss, "moc_p" = tmp$size, "moc_o" = tmp1$size,
       "l_min" = minz, "l_max" = maxz, "kje_min" = eks$kje_min, "kje_max" = eks$kje_max)
}


vstop_SinR <- function(tabela, cena, toleranca, crte){
  podpore <- crte$podpore
  odpori <- as.numeric(crte$odpori)
  kje_min <- crte$kje_min
  kje_max <- crte$kje_max
  l_min <- as.numeric(crte$l_min)
  l_max <- as.numeric(crte$l_max)
  entry <- rep(0, nrow(tabela))
  linija <- rep(0, nrow(tabela))
  koliko_casa <- rep(0, nrow(tabela))
  
  # obrat podpora
  for(i in (1:length(l_min))){
    real_time <- kje_min[i]
    if(real_time == 1){
      i <- 2
      real_time <- kje_min[i]}
    opazovana_podpora <- podpore[which.min(abs(l_min[i]-podpore))]
    kje_se_dotika_te_podpore <- which(abs(l_min - opazovana_podpora) <= toleranca)
    if(is.na(kje_se_dotika_te_podpore[1])){kje_se_dotika_te_podpore[1] <- Inf}
    if((abs(l_min[i] - opazovana_podpora) <= toleranca) & (kje_se_dotika_te_podpore[1] < i) & 
       (cena[real_time - 1,] > cena[real_time,]) & (cena[real_time + 1,] > cena[real_time,])){
      entry[real_time + 1] <- 1
      linija[real_time + 1] <- 1
      koliko_casa[real_time + 1] <- 1
    }
  }
  
  # obrat odpor
  for(i in (1:length(l_max))){
    real_time <- kje_max[i]
    if(real_time == 1){
      i <- 2
      real_time <- kje_max[i]
    }
    opazovani_odpor <- odpori[which.min(abs(l_max[i]-odpori))]
    kje_se_dotika_tega_odpora <- which(abs(l_max - opazovani_odpor) <= toleranca)
    if(is.na(kje_se_dotika_tega_odpora[1])){kje_se_dotika_tega_odpora[1] <- Inf}
    if((abs(l_max[i] - opazovani_odpor) <= toleranca) & (kje_se_dotika_tega_odpora[1] < i) & 
       (cena[real_time - 1,] < cena[real_time,]) & (cena[real_time + 1,] < cena[real_time,])){
      entry[real_time + 1] <- 2
      linija[real_time + 1] <- 2
      koliko_casa[real_time + 1] <- 1
    }
  }
  
  # preboj podpore
  for(i in 2:(nrow(tabela)-2)){
    podpore_vmes <- podpore[cena[i-1,] > podpore & cena[i,] < podpore]
    if(length(podpore_vmes) > 0){
      opazovana_podpora <- podpore_vmes[length(podpore_vmes)]
      kje_se_dotika_te_podpore <- which(abs(l_min - opazovana_podpora) <= toleranca)
      if(is.na(kje_se_dotika_te_podpore[1])){kje_se_dotika_te_podpore[1] <- Inf}
    }
    if((length(podpore_vmes) > 0) & (cena[i-1,] > cena[i,]) & (cena[i+1,] < cena[i,]) & 
       (kje_se_dotika_te_podpore[1] < i)){
      if((cena[i + 1, ] > opazovana_podpora - toleranca)){
        if(cena[i + 2, ] < opazovana_podpora - toleranca){
          entry[i + 2] <- 2
          linija[i + 2] <- 1
          koliko_casa[i + 2] <- 2
        }
        else{
          if(cena[i + 2, ] < opazovana_podpora){
            entry[i + 2] <- 1
            linija[i + 2] <- 1
            koliko_casa[i + 2] <- 2}
        }
      }
      else{
        entry[i + 1] <- 2
        linija[i + 1] <- 1
        koliko_casa[i + 1] <- 1}
    }
    
    # preboj odpor
    odpori_vmes <- odpori[cena[i-1,] < odpori &  cena[i,] > odpori]
    if(length(odpori_vmes) > 0){
      opazovani_odpor <- odpori_vmes[length(odpori_vmes)]
      kje_se_dotika_tega_odpora <- which(abs(l_max - opazovani_odpor) <= toleranca)
      if(is.na(kje_se_dotika_tega_odpora[1])){kje_se_dotika_tega_odpora[1] <- Inf}
    }
    if((length(odpori_vmes) > 0) & (cena[i-1,] < cena[i,]) & (cena[i+1,] > cena[i,]) &
       (kje_se_dotika_tega_odpora[1] < i)){
      if((cena[i + 1, ] < opazovani_odpor + toleranca)){
        if(cena[i + 2, ] > opazovani_odpor + toleranca){
          entry[i + 2] <- 1
          linija[i + 2] <- 2
          koliko_casa[i + 2] <- 2
        }
        else{
          if(cena[i + 2, ] > opazovani_odpor){
            entry[i + 2] <- 2
            linija[i + 2] <- 2
            koliko_casa[i + 2] <- 2}
        }
      }
      else{
        entry[i + 1] <- 1
        linija[i + 1] <- 2
        koliko_casa[i + 1] <- 1}
    }
  }
  list("entry" = entry, "linija" = linija, "koliko_casa" = koliko_casa)
}


win_izstop_SinR <- function(tabela, cena, rr){
  izstop <- rep(0, nrow(tabela))
  for(i in 1:nrow(tabela)){
    if(tabela$entry[i] == 1 & tabela$linija[i] == 1){
      izstop[i] <- kje_max[kje_max > i][1] + 1
    }
    if(tabela$entry[i] == 2 & tabela$linija[i] == 2){
      izstop[i] <- kje_min[kje_min > i][1] + 1
    }
    if(tabela$entry[i] == 2 & tabela$linija[i] == 1){
      cilj <- cena[i,]- sl*rr*tabela$tedenski_N[i]
      kandidati_cilj <- which(cena[,1] <= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
    if(tabela$entry[i] == 1 & tabela$linija[i] == 2){
      cilj <- cena[i,] + sl*rr*tabela$tedenski_N[i]
      kandidati_cilj <- which(cena[,1] >= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
  }
  izstop
}


#######
# Z glajenjem

prepoznavalnik_g <- function(jedro){
  library(Ckmeans.1d.dp)
  #clustering
  mera <- c()
  for(i in 10:80){
    fit <- kmeans(jedro, i)
    mera <- c(mera, fit$tot.withinss)
  }
  st_skupin <- 9 + which.min(mera)
  tmp <- Ckmeans.1d.dp(jedro, st_skupin)
  linije <- tmp$centers
  list("linije" = linije, "odmiki_p" = tmp$tot.withinss, "moc" = tmp$size)
}

vstop_SinR_g <- function(tabela, cena, toleranca, crte){
  
  linije <- crte$linije
  entry <- rep(0, nrow(tabela))
  linija <- rep(0, nrow(tabela))
  koliko_casa <- rep(0, nrow(tabela))
  
  for(i in 2:(nrow(tabela)-2)){
    # obrat podpora
    opazovana_podpora <- linije[which.min(abs(cena[i,]-linije))]
    kje_se_dotika_te_podpore <- which(abs(cena - opazovana_podpora) <= toleranca)
    if(is.na(kje_se_dotika_te_podpore[10])){kje_se_dotika_te_podpore[10] <- Inf}
    if((abs(cena[i,] - opazovana_podpora) <= toleranca) & (kje_se_dotika_te_podpore[10] < i) & 
       (cena[i - 1,] > cena[i,]) & (cena[i + 1,] > cena[i,])){
      entry[i + 1] <- 1
      linija[i + 1] <- 1
      koliko_casa[i + 1] <- 1
    }
    
    # obrat odpor
    opazovani_odpor <- linije[which.min(abs(cena[i,]-linije))]
    kje_se_dotika_tega_odpora <- which(abs(cena - opazovani_odpor) <= toleranca)
    if(is.na(kje_se_dotika_tega_odpora[10])){kje_se_dotika_tega_odpora[10] <- Inf}
    if((abs(cena[i,] - opazovani_odpor) <= toleranca) & (kje_se_dotika_tega_odpora[10] < i) & 
       (cena[i - 1,] < cena[i,]) & (cena[i + 1,] < cena[i,])){
      entry[i + 1] <- 2
      linija[i + 1] <- 2
      koliko_casa[i + 1] <- 2
    }
    
    # preboj podpora
    podpore_vmes <- podpore[cena[i-1,] > podpore &  cena[i,] < podpore]
    if(length(podpore_vmes) > 0){
      opazovana_podpora <- podpore_vmes[length(podpore_vmes)]
      kje_se_dotika_te_podpore <- which(abs(cena - opazovana_podpora) <= toleranca)
      if(is.na(kje_se_dotika_te_podpore[10])){kje_se_dotika_te_podpore[10] <- Inf}
    }
    if((length(podpore_vmes) > 0) & (cena[i-1,] > cena[i,]) & (cena[i+1,] < cena[i,]) & 
       (kje_se_dotika_te_podpore[10] < i)){
      if((cena[i + 1, ] > opazovana_podpora - toleranca)){
        if(cena[i + 2, ] < opazovana_podpora - toleranca){
          entry[i + 2] <- 2
          linija[i + 2] <- 1
          koliko_casa[i + 2] <- 2
        }
        else{
          if(cena[i + 2, ] < opazovana_podpora){
            entry[i + 2] <- 1
            linija[i + 2] <- 1
            koliko_casa[i + 2] <- 2}
        }
      }
      else{
        entry[i + 1] <- 2
        linija[i + 1] <- 1
        koliko_casa[i + 1] <- 1}
    }
    
    # preboj odpor
    odpori_vmes <- odpori[cena[i-1,] < odpori &  cena[i,] > odpori]
    if(length(odpori_vmes) > 0){
      opazovani_odpor <- odpori_vmes[length(odpori_vmes)]
      kje_se_dotika_tega_odpora <- which(abs(cena - opazovani_odpor) <= toleranca)
      if(is.na(kje_se_dotika_tega_odpora[10])){kje_se_dotika_tega_odpora[10] <- Inf}
    }
    if((length(odpori_vmes) > 0) & (cena[i-1,] < cena[i,]) & (cena[i+1,] > cena[i,]) &
       (kje_se_dotika_tega_odpora[10] < i)){
      if((cena[i + 1, ] < opazovani_odpor + toleranca)){
        if(cena[i + 2, ] > opazovani_odpor + toleranca){
          entry[i + 2] <- 1
          linija[i + 2] <- 2
          koliko_casa[i + 2] <- 2
        }
        else{
          if(cena[i + 2, ] > opazovani_odpor){
            entry[i + 2] <- 2
            linija[i + 2] <- 2
            koliko_casa[i + 2] <- 2}
        }
      }
      else{
        entry[i + 1] <- 1
        linija[i + 1] <- 2
        koliko_casa[i + 1] <- 1}
    }
  }
  list("entry" = entry, "linija" = linija, "koliko_casa" = koliko_casa)
}

win_izstop_SinR_g <- function(tabela, cena, rr){
  izstop <- rep(0, nrow(tabela))
  for(i in 1:nrow(tabela)){
    if(tabela$entry[i] == 1 & tabela$linija[i] == 1){
      izstop[i] <- which(tabela$linija == 2)[which(tabela$linija == 2) > i][1]
    }
    if(tabela$entry[i] == 2 & tabela$linija[i] == 2){
      izstop[i] <- which(tabela$linija == 1)[which(tabela$linija == 1) > i][1]
    }
    if(tabela$entry[i] == 2 & tabela$linija[i] == 1){
      cilj <- cena[i,]- sl*rr*tabela$tedenski_N[i]
      kandidati_cilj <- which(cena[,1] <= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
    if(tabela$entry[i] == 1 & tabela$linija[i] == 2){
      cilj <- cena[i,] + sl*rr*tabela$tedenski_N[i]
      kandidati_cilj <- which(cena[,1] >= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
  }
  izstop
}


##################
# Drseče sredine #
##################

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

win_izstop_rr <- function(tabela, cena, rr){
  izstop <- rep(0, nrow(tabela))
  for(i in 1:nrow(tabela)){
    if(tabela$entry[i] == 2){
      cilj <- cena[i,]- sl*rr*tabela$tedenski_N[i]
      kandidati_cilj <- which(cena[,1] <= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
    if(tabela$entry[i] == 1){
      cilj <- cena[i,] + sl*rr*tabela$tedenski_N[i]
      kandidati_cilj <- which(cena[,1] >= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
  }
  izstop
}

#############################
# Average Directional index #
#############################

N_adx <- function(tabela, dnevi){
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
  tabela$N <- round(n, 2)
  
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

####################
# Vortex indicator #
####################

N_vix <- function(tabela, dnevi){
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
  tabela$N <- round(n, 2)
  
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

###########################
# Relative strength index #
###########################

N_rsi <- function(tabela, dnevi){
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
  tabela$N <- round(n, 2)
  
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

##################
# Rate of Change #
##################

N_roc <- function(tabela, dnevi){
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
  tabela$N <- round(n, 2)
  
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

#########################
# Stochastic Oscillator #
#########################

N_so <- function(tabela, dnevi){
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
  tabela$N <- round(n, 2)
  
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

###############################
# Percentage Price Oscillator #
###############################

N_ppo <- function(tabela, dnevi){
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
  tabela$N <- round(n, 2)
  
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

###############
# Force Index #
###############

N_fi <- function(tabela, dnevi){
  # TR
  tr <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    tr <- append(tr, max(h-l, h-pdc, pdc-l))
  }
  tabela$TR <- c(0,tr)
  
  # Force index
  tabela$fi1 <- c(0, (tabela$Close[2:nrow(tabela)] - tabela$Close[1:(nrow(tabela)-1)])*tabela$Volume[2:nrow(tabela)])
  tabela$fi13 <- c(0, ema(tabela$fi1[2:nrow(tabela)], 13))
  
  tabela <- tabela[-1,]
  # N
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  tabela$N <- round(n, 2)
  
  tabela
}

# fi13 < 0, fi13 > 0 -> go long
# fi13 > 0, fi13 < 0 -> go short
vstop_FI <- function(tabela){
  entry <- rep(0, nrow(tabela))
  for(i in 2:nrow(tabela)){
    if(!is.na(tabela$fi13[i-1])){
      if((tabela$fi13[i-1] < 0) & (tabela$fi13[i] > 0)){
        entry[i] <- 1
      }
      if((tabela$fi13[i-1] > 0) & (tabela$fi13[i] < 0)){
        entry[i] <- 2
      }
    }
  }
  entry
}

####################
# Money Flow Index #
####################

N_mfi <- function(tabela, dnevi){
  # TR
  tr <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    tr <- append(tr, max(h-l, h-pdc, pdc-l))
  }
  tabela$TR <- c(0,tr)
  
  # TP, RMF
  tabela$tp <- (tabela$High + tabela$Low + tabela$Close)/3
  tabela$rmf <- tabela$tp*tabela$Volume
  
  # pmf, nmf
  pmf1 <- rep(0, nrow(tabela))
  nmf1 <- rep(0, nrow(tabela))
  for(i in 2:nrow(tabela)){
    ifelse(tabela$tp[i] > tabela$tp[i-1], pmf1[i] <- tabela$rmf[i], 0)
    ifelse(tabela$tp[i] < tabela$tp[i-1], nmf1[i] <- tabela$rmf[i], 0)
  }
  tabela$pmf1 <- pmf1
  tabela$nmf1 <- nmf1
  
  pmf14 <- rep(0, nrow(tabela))
  nmf14 <- rep(0, nrow(tabela))
  for(i in 15:nrow(tabela)){
    pmf14[i] <- sum(tabela$pmf1[(i-13):i])
    nmf14[i] <- sum(tabela$nmf1[(i-13):i])
  }
  tabela$pmf14 <- pmf14
  tabela$nmf14 <- nmf14
  
  # MFR, MFI
  tabela$mfr <- tabela$pmf14/tabela$nmf14
  tabela$mfi <- 100-(100/(1+tabela$mfr))
  
  tabela <- tabela[-1,]
  # N
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  tabela$N <- round(n, 2)
  
  tabela
}

# mfi < 10, mfi > 10 -> go long
# mfi > 90, mfi < 90 -> go short
vstop_MFI <- function(tabela){
  entry <- rep(0, nrow(tabela))
  for(i in 2:nrow(tabela)){
    if(!is.na(tabela$mfi[i-1])){
      if((tabela$mfi[i-1] < 20) & (tabela$mfi[i] > 20)){
        entry[i] <- 1
      }
      if((tabela$mfi[i-1] > 80) & (tabela$mfi[i] < 80)){
        entry[i] <- 2
      }
    }
  }
  entry
}

######################
# Chaikin Oscillator #
######################

N_co <- function(tabela, dnevi){
  # TR
  tr <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    tr <- append(tr, max(h-l, h-pdc, pdc-l))
  }
  tabela$TR <- c(0,tr)
  
  # mfm, mfv, adl
  tabela$mfm <- ((tabela$Close - tabela$Low) - (tabela$High - tabela$Close))/(tabela$High - tabela$Low)
  tabela$mfm[is.na(tabela$mfm)] <- 0
  tabela$mfv <- tabela$mfm*tabela$Volume
  tabela$adl <- c(tabela$mfv[1], (tabela$mfv[2:nrow(tabela)] + tabela$mfv[1:(nrow(tabela)-1)]))
  
  # 3-day, 10-day EMA
  tabela$ema3adl <- ema(tabela$adl, 3)
  tabela$ema10adl <- ema(tabela$adl, 10)
  tabela$CO <- tabela$ema3adl - tabela$ema10adl
  
  tabela <- tabela[-1,]
  # N
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  tabela$N <- round(n, 2)
  
  tabela
}

# CO < 1000, CO > 1000 -> go long
# CO > -1000, CO < -1000 -> go short
vstop_CO <- function(tabela){
  entry <- rep(0, nrow(tabela))
  for(i in 2:nrow(tabela)){
    if(!is.na(tabela$CO[i-1])){
      if((tabela$CO[i-1] < 1000) & (tabela$CO[i] > 1000)){
        entry[i] <- 1
      }
      if((tabela$CO[i-1] > -1000) & (tabela$CO[i] < -1000)){
        entry[i] <- 2
      }
    }
  }
  entry
}


#####################################
# Chandelier exit - izstopna metoda #
#####################################

win_izstop_chandelier <- function(tabela, cena, dnevi){
  # 22-day ATR
  prvi_n <- mean(tabela$TR[1:dnevi])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  
  # 22-day high, 22-day Low
  hh <- rep(0, nrow(tabela))
  ll <- rep(0, nrow(tabela))
  for(i in dnevi:nrow(tabela)){
    hh[i] <- max(tabela$High[(i-dnevi+1):i])
    ll[i] <- min(tabela$Low[(i-dnevi+1):i])
  }
  
  #izstop
  izstop <- rep(0, nrow(tabela))
  for(i in 1:nrow(tabela)){
    if(tabela$entry[i] == 2){
      cilj <- ll[i] + 3*n[i]
      kandidati_cilj <- which(cena[,1] >= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
    if(tabela$entry[i] == 1){
      cilj <- hh[i] - 3*n[i]
      kandidati_cilj <- which(cena[,1] <= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
  }
  izstop
}


###################
#tabela <- btc_1h

poracuni <- function(tabela, dnevi_N, vstop_s1, vstop_s2, izstop_s1_s2, cena, dnevi_ema1, 
                     dnevi_ema2, toleranca, rr, indikator, metoda_izstop){
  
  tabela <- tabela[-nrow(tabela),]
  if(indikator == "zelve_s1"){
    tab <- PR_N(tabela, dnevi_N)
    cena1 <- odlocitev_cena(tab, cena)
    tab$entry <- vstop_TU(tab, vstop_s1, cena1)
    tab$entry_s2 <- vstop_TU(tab, vstop_s2, cena1)
  }
  if(indikator == "zelve_s2"){
    tab <- PR_N(tabela, dnevi_N)
    cena1 <- odlocitev_cena(tab, cena)
    tab$entry <- vstop_TU(tab, vstop_s2, cena1)
    tab$entry_s2 <- rep(0, length(tab$entry))
  }
  if(indikator == "SinR"){
    tab <- PR_N(tabela, dnevi_N)
    cena1 <- odlocitev_cena(tab, cena)
    cena1 <- log(cena1)
    SinR <- prepoznavalnik_SinR(tab, cena1, velikost_oken = 10)
    tmp <- vstop_SinR(tab, cena1, toleranca, SinR)
    tab$entry <- tmp$entry
    tab$linija <- tmp$linija
    tab$koliko_casa <- tmp$koliko_casa
  }
  if(indikator == "SinR_g"){
    tab <- PR_N(tabela, dnevi_N)
    cena1 <- odlocitev_cena(tab, cena)
    cena1 <- log(cena1)
    a <- kernel(cena1[,1], 1)
    SinR <- prepoznavalnik_g(a)
    tmp <- vstop_SinR_g(tab, cena1, toleranca, SinR)
    tab$entry <- tmp$entry
    tab$linija <- tmp$linija
    tab$koliko_casa <- tmp$koliko_casa
  }
  if(indikator == "MA"){
    tab <- PR_N(tabela, dnevi_N)
    cena1 <- odlocitev_cena(tab, cena)
    tab$ma1 <- ema(cena1[,1], dnevi_ema1)
    tab$ma2 <- ema(cena1[,1], dnevi_ema2)
    tab$entry <- vstop_MA(tab, cena1)
  }
  if(indikator == "MA1"){
    tab <- PR_N(tabela, dnevi_N)
    cena1 <- odlocitev_cena(tab, cena)
    tab$ma1 <- sma(cena1[,1], dnevi_ema1)
    tab$ma2 <- sma(cena1[,1], dnevi_ema2)
    tab$entry <- vstop_MA(tab, cena1)
  }
  if(indikator == "adx"){
    tab <- N_adx(tabela, dnevi_N)
    tab$entry <- vstop_ADX(tab)
  }
  if(indikator == "vi"){
    tab <- N_vix(tabela, dnevi_N)
    tab$entry <- vstop_VI(tab)
  }
  if(indikator == "rsi"){
    tab <- N_rsi(tabela, dnevi_N)
    tab$entry <- vstop_RSI(tab)
  }
  if(indikator == "roc"){
    tab <- N_roc(tabela, dnevi_N)
    cena1 <- odlocitev_cena(tab, cena)
    tab$ma <- sma(cena1[,1], 20)
    tab$ma[is.na(tab$ma)] <- 0
    tab$entry <- vstop_ROC(tab, cena1)
  }
  if(indikator == "so"){
    tab <- N_so(tabela, dnevi_N)
    tab$entry <- vstop_SO(tab)
  }
  if(indikator == "ppo"){
    tab <- N_ppo(tabela, dnevi_N)
    tab$entry <- vstop_PPO(tab)
  }
  if(indikator == "fi"){
    tab <- N_fi(tabela, dnevi_N)
    tab$entry <- vstop_FI(tab)
  }
  if(indikator == "mfi"){
    tab <- N_mfi(tabela, dnevi_N)
    tab$entry <- vstop_MFI(tab)
  }
  if(indikator == "co"){
    tab <- N_co(tabela, dnevi_N)
    tab$entry <- vstop_CO(tab)
  }
  
  if((nrow(tabela) == 1530) | (nrow(tabela) == 1340)){
    tab <- tab[(3:nrow(tab)),]
  }
  if((nrow(tabela) == 1342) | (nrow(tabela) == 747)){
    tab <- tab[(5:nrow(tab)),]
  }
  tab <- tedenski_N(tab)
  tab$tedenski_N[is.na(tab$tedenski_N)] <- rep(0, length(tab$tedenski_N[is.na(tab$tedenski_N)]))
  ničle <- (which(tab$tedenski_N[1:50] > 0)[1] - 1)
  cena1 <- odlocitev_cena(tab, cena)
  
  if(metoda_izstop == "zelve"){
    tab$izstop <- win_izstop_turtle(tab, izstop_s1_s2, cena1)
    pomoc <- which(win_izstop_turtle(tab, izstop_s1_s2, cena1) > 0)
  }
  if(metoda_izstop == "SinR"){
    tab$izstop <- win_izstop_SinR(tab, cena1, rr)
    pomoc <- which(win_izstop_SinR(tab, cena1, rr) > 0)
  }
  if(metoda_izstop == "SinR_g"){
    tab$izstop <- win_izstop_SinR_g(tab, cena1, rr)
    pomoc <- which(win_izstop_SinR_g(tab, cena1, rr) > 0)
  }
  if(metoda_izstop == "rr"){
    tab$izstop <- win_izstop_rr(tab, cena1, rr)
    pomoc <- which(win_izstop_rr(tab, cena1, rr) > 0)
  }
  if(metoda_izstop == "chandelier"){
    tab$izstop <- win_izstop_chandelier(tab, cena1, 22)
    pomoc <- which(win_izstop_chandelier(tab, cena1, 22) > 0)
  }

  tab$izstop[pomoc] <- tab$izstop[pomoc] - ničle
  tab <- tab[tab$tedenski_N > 0,]
  tab
}



trgovanje <- function(tabela, zacetni_kapital, cena, add, sl, indikator){
  kandidati <- which(tabela$entry == 1 | tabela$entry == 2)
  if(indikator == "zelve_s1"){
    kandidati_s2 <- which(tabela$entry_s2 == 1 | tabela$entry_s2 == 2)
  }
  vstop <- kandidati[1]
  if(is.na(vstop)){profit <- 0}
  else{
    kdaj_vstopali <- c()
    kdaj_dodali_enote <- c()
    profit <- 0
    profit1 <- c()
    kdaj_profit <- c()
    money <- zacetni_kapital
    
    while(nrow(tabela) - vstop > 0){
      kdaj_vstopali <- c(kdaj_vstopali, vstop)
      cena_vstop <- cena[vstop,]
      st_btc <- unit(money, tabela$tedenski_N[vstop])
      
      #Buy signal
      if(indikator == "SinR"){
        pogoj <- ((tabela$entry[vstop]== 1 & tabela$linija[vstop] == 1) | (tabela$entry[vstop]== 1 & tabela$linija[vstop] == 2))}
      else{
        pogoj <- (tabela$entry[vstop]== 1)
      }
      if(pogoj){
        izstop <- ifelse((tabela$izstop[vstop] - i_izstop > nrow(tabela)) | 
                           (is.na(tabela$izstop[vstop] - i_izstop > nrow(tabela))), nrow(tabela), tabela$izstop[vstop] - i_izstop)
        cena_izstop <- cena[izstop,]
        st_enot <- 1
        stop_loss <- cena_vstop - sl*tabela$tedenski_N[vstop]
        pol_N <- add*tabela$tedenski_N[vstop]
        cena_ko_dodamo <- cena_vstop
        st_btc_dodamo <- st_btc
        for(i in (vstop+1):(izstop-1)){
          vstop <- i
          if((cena[i,] > (cena_vstop + st_enot*pol_N)) & (st_enot < 4)){
            razlika <- cena[i,] - cena_ko_dodamo[length(cena_ko_dodamo)]
            dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
            if(st_enot + dodamo <= 4){
              st_enot <- st_enot + dodamo
              st_btc_dodamo <- c(st_btc_dodamo, st_btc*dodamo)
              stop_loss <- stop_loss + pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
            }
            else{
              dodamo <- ifelse(st_enot==2, 2, 1)
              st_enot <- st_enot + dodamo
              st_btc_dodamo <- c(st_btc_dodamo, st_btc*dodamo)
              stop_loss <- stop_loss + pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
            }
          }
          else{
            if(cena[i,] <= stop_loss){
              kdaj_profit <- c(kdaj_profit, i)
              vsota <- 0
              for(j in 1:length(st_btc_dodamo)){
                vsota <- vsota + (cena[i,] - cena_ko_dodamo[j])*st_btc_dodamo[j]
              }
              profit <- profit + vsota
              profit1 <- c(profit1, profit)
              if(indikator == "zelve_s1" & (vsota > 0)){
                vstop <- ifelse(is.na(kandidati_s2[kandidati_s2 > vstop][1]), nrow(tabela), kandidati_s2[kandidati_s2 > vstop][1])
              }
              else{
                vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
              }
              break}
          }
        }
      }
      else{
        if(indikator == "SinR"){
          pogoj <- ((tabela$entry[vstop]== 2 & tabela$linija[vstop] == 2) | (tabela$entry[vstop]== 2 & tabela$linija[vstop] == 1))
        }
        else{
          pogoj1 <- (tabela$entry[vstop]== 2)
        }
        # Sell signal
        if(pogoj1){
          izstop <- ifelse(tabela$izstop[vstop] - i_izstop > nrow(tabela)  | 
                             is.na(tabela$izstop[vstop] - i_izstop > nrow(tabela)), nrow(tabela), tabela$izstop[vstop] - i_izstop)
          cena_izstop <- cena[izstop,]
          st_enot <- 1
          stop_loss <- cena_vstop + sl*tabela$tedenski_N[vstop]
          pol_N <- add*tabela$tedenski_N[vstop]
          cena_ko_dodamo <- cena_vstop
          st_btc_dodamo <- st_btc
          for(i in (vstop+1):(izstop-1)){
            vstop <- i
            if((cena[i,] < cena_vstop - st_enot*pol_N) & (st_enot < 4)){
              razlika <- abs(cena[i,] - cena_ko_dodamo[length(cena_ko_dodamo)])
              dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
              if(st_enot + dodamo <= 4){
                st_enot <- st_enot + dodamo
                st_btc_dodamo <- c(st_btc_dodamo, st_btc*dodamo)
                stop_loss <- stop_loss - pol_N*dodamo
                kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
              }
              else{
                dodamo <- ifelse(st_enot==2, 2, 1)
                st_enot <- st_enot + dodamo
                st_btc_dodamo <- c(st_btc_dodamo, st_btc*dodamo)
                stop_loss <- stop_loss - pol_N*dodamo
                kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
              }
            }
            else{
              if(cena[i,] >= stop_loss){
                kdaj_profit <- c(kdaj_profit, i)
                vsota <- 0
                for(j in 1:length(st_btc_dodamo)){
                  vsota <- vsota + (cena_ko_dodamo[j] - cena[i,])*st_btc_dodamo[j]
                }
                profit <- profit + vsota
                profit1 <- c(profit1, profit)
                if(indikator == "zelve_s1" & (vsota > 0)){
                  vstop <- ifelse(is.na(kandidati_s2[kandidati_s2 > vstop][1]), nrow(tabela), kandidati_s2[kandidati_s2 > vstop][1])
                }
                else{
                  vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
                }
                break}
            }
          }
        }}
      if(vstop == (izstop-1)){
        vstop <- vstop + 1
        vsota <- 0
        for(j in 1:length(st_btc_dodamo)){
          vsota <- vsota + (abs(cena_izstop - cena_ko_dodamo[j]))*st_btc_dodamo[j]
        }
        profit <- profit + vsota
        profit1 <- c(profit1, profit)
        kdaj_profit <- c(kdaj_profit, izstop)
        if(indikator == "zelve_s1" & (vsota > 0)){
          vstop <- ifelse(is.na(kandidati_s2[kandidati_s2 > vstop][1]), nrow(tabela), kandidati_s2[kandidati_s2 > vstop][1])
        }
        else{
          vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
        }
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


#tabela <- btc_1day
#cena <- "Close"

dobicki_trgovanje <- function(tabela, dnevi_N = 20, vstop_s1 = 20, vstop_s2 = 55, izstop_s1_s2 = 10, 
                              obdobje, zacetni_kapital = 1000000, cena = "Close", add = 1/2, 
                              sl = 2, toleranca = 0.02, rr = 3, dnevi_ema1 = 10,
                              dnevi_ema2 = 50, indikator = "zelve_s1", metoda_izstop = "zelve"){
  tab <- poracuni(tabela, dnevi_N, vstop_s1, vstop_s2, izstop_s1_s2, cena, dnevi_ema1, 
                  dnevi_ema2, toleranca, rr, indikator, metoda_izstop)
  dobicki <- c()
  for(i in 1:(nrow(tab) - obdobje + 1)){
    tmp <- tab[i:(obdobje + i - 1),]
    cena2 <- odlocitev_cena(tmp, cena)
    i_izstop <- (i-1)
    dobicki <- c(dobicki, trgovanje(tmp, zacetni_kapital, cena2, add, sl, indikator))
  }
  dobicki
}


###############################
# DOBIČKI - Želvja strategija #
###############################

#1 day
btc_360_S1 <- dobicki_trgovanje(btc_1day, obdobje = 360)
btc_500_S1 <- dobicki_trgovanje(btc_1day, obdobje = 500)
btc_1000_S1 <- dobicki_trgovanje(btc_1day, obdobje = 1000)
btc_1800_S1 <- dobicki_trgovanje(btc_1day, obdobje = 1800)

btc_360_S2 <- dobicki_trgovanje(btc_1day, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 360)
btc_500_S2 <- dobicki_trgovanje(btc_1day, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 500)
btc_1000_S2 <- dobicki_trgovanje(btc_1day, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 1000)
btc_1800_S2 <- dobicki_trgovanje(btc_1day, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 1800)

btc_360_S1_2 <- dobicki_trgovanje(btc_1day, obdobje = 360, zacetni_kapital = 50000)
btc_500_S1_2 <- dobicki_trgovanje(btc_1day, obdobje = 500, zacetni_kapital = 50000)
btc_1000_S1_2 <- dobicki_trgovanje(btc_1day, obdobje = 1000, zacetni_kapital = 50000)
btc_1800_S1_2 <- dobicki_trgovanje(btc_1day, obdobje = 1800, zacetni_kapital = 50000)

btc_360_S2_2 <- dobicki_trgovanje(btc_1day, izstop_s1_s2 = 20, indikator = "zelve_s2",
                                  obdobje = 360, zacetni_kapital = 50000)
btc_500_S2_2 <- dobicki_trgovanje(btc_1day, izstop_s1_s2 = 20, indikator = "zelve_s2",
                                  obdobje = 500, zacetni_kapital = 50000)
btc_1000_S2_2 <- dobicki_trgovanje(btc_1day, izstop_s1_s2 = 20, indikator = "zelve_s2",
                                  obdobje = 1000, zacetni_kapital = 50000)
btc_1800_S2_2 <- dobicki_trgovanje(btc_1day, izstop_s1_s2 = 20, indikator = "zelve_s2",
                                  obdobje = 1800, zacetni_kapital = 50000)

#1h
btc_1h_360_S1 <- dobicki_trgovanje(btc_1h, obdobje = 360)
btc_1h_500_S1 <- dobicki_trgovanje(btc_1h, obdobje = 500)
btc_1h_1000_S1 <- dobicki_trgovanje(btc_1h, obdobje = 1000)
btc_1h_1800_S1 <- dobicki_trgovanje(btc_1h, obdobje = 1800)

btc_1h_360_S2 <- dobicki_trgovanje(btc_1h, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 360)
btc_1h_500_S2 <- dobicki_trgovanje(btc_1h, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 500)
btc_1h_1000_S2 <- dobicki_trgovanje(btc_1h, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 1000)
btc_1h_1800_S2 <- dobicki_trgovanje(btc_1h, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 1800)

#5min
btc_5min_360_S1 <- dobicki_trgovanje(btc_5min, obdobje = 360)
btc_5min_500_S1 <- dobicki_trgovanje(btc_5min, obdobje = 500)
btc_5min_1000_S1 <- dobicki_trgovanje(btc_5min, obdobje = 1000)
#nism še:
btc_5min_1800_S1 <- dobicki_trgovanje(btc_5min, obdobje = 1800)

btc_5min_360_S2 <- dobicki_trgovanje(btc_5min, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 360)
btc_5min_500_S2 <- dobicki_trgovanje(btc_5min, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 500)
btc_5min_1000_S2 <- dobicki_trgovanje(btc_5min, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 1000)
#nism še:
btc_5min_1800_S2 <- dobicki_trgovanje(btc_5min, izstop_s1_s2 = 20, indikator = "zelve_s2", obdobje = 1800)


###############################
# DOBIČKI - Podpore in odpori #
###############################

#(tabela, dnevi_N = 20, vstop_s1 = 20, vstop_s2 = 55, izstop_s1_s2 = 10, 
# obdobje, zacetni_kapital = 1000000, cena = "Close", add = 1/2, 
# sl = 2, toleranca = 0.02, rr = 3, dnevi_ema1 = 10,
# dnevi_ema2 = 50, indikator = "zelve_s1", metoda_izstop = "zelve")

# Z lokalnimi ekstremi
SinR_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "SinR", metoda_izstop = "SinR")
SinR_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "SinR", metoda_izstop = "SinR")
SinR_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "SinR", metoda_izstop = "SinR")
SinR_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "SinR", metoda_izstop = "SinR")

# Z glajenjem
g_SinR_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "SinR_g", metoda_izstop = "SinR_g")
g_SinR_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "SinR_g", metoda_izstop = "SinR_g")
g_SinR_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "SinR_g", metoda_izstop = "SinR_g")
g_SinR_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "SinR_g", metoda_izstop = "SinR_g")


############################
# DOBIČKI - Drseče sredine #
############################


#EMA
MA_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "MA", metoda_izstop = "rr")
MA_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "MA", metoda_izstop = "rr")
MA_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "MA", metoda_izstop = "rr")
MA_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "MA", metoda_izstop = "rr")


#SMA
MA1_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "MA1", metoda_izstop = "rr")
MA1_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "MA1", metoda_izstop = "rr")
MA1_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "MA1", metoda_izstop = "rr")
MA1_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "MA1", metoda_izstop = "rr")


################################
# DOBIČKI - ostali indikatorji # 
################################

ADX_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "adx", metoda_izstop = "rr")
ADX_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "adx", metoda_izstop = "rr")
ADX_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "adx", metoda_izstop = "rr")
ADX_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "adx", metoda_izstop = "rr")

VIX_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "vi", metoda_izstop = "rr")
VIX_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "vi", metoda_izstop = "rr")
VIX_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "vi", metoda_izstop = "rr")
VIX_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "vi", metoda_izstop = "rr")

RSI_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "rsi", metoda_izstop = "rr")
RSI_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "rsi", metoda_izstop = "rr")
RSI_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "rsi", metoda_izstop = "rr")
RSI_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "rsi", metoda_izstop = "rr")

ROC_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "roc", metoda_izstop = "rr")
ROC_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "roc", metoda_izstop = "rr")
ROC_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "roc", metoda_izstop = "rr")
ROC_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "roc", metoda_izstop = "rr")

SO_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "so", metoda_izstop = "rr")
SO_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "so", metoda_izstop = "rr")
SO_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "so", metoda_izstop = "rr")
SO_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "so", metoda_izstop = "rr")

PPO_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360, indikator = "ppo", metoda_izstop = "rr")
PPO_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500, indikator = "ppo", metoda_izstop = "rr")
PPO_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000, indikator = "ppo", metoda_izstop = "rr")
PPO_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day, obdobje = 1800, indikator = "ppo", metoda_izstop = "rr")

FI_dobicki_btc_360 <- dobicki_trgovanje(btc_1day_vol, obdobje = 360, indikator = "fi", metoda_izstop = "rr")
FI_dobicki_btc_500 <- dobicki_trgovanje(btc_1day_vol, obdobje = 500, indikator = "fi", metoda_izstop = "rr")
FI_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day_vol, obdobje = 1000, indikator = "fi", metoda_izstop = "rr")
FI_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day_vol, obdobje = 1800, indikator = "fi", metoda_izstop = "rr")

MFI_dobicki_btc_360 <- dobicki_trgovanje(btc_1day_vol, obdobje = 360, indikator = "mfi", metoda_izstop = "rr")
MFI_dobicki_btc_500 <- dobicki_trgovanje(btc_1day_vol, obdobje = 500, indikator = "mfi", metoda_izstop = "rr")
MFI_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day_vol, obdobje = 1000, indikator = "mfi", metoda_izstop = "rr")
MFI_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day_vol, obdobje = 1800, indikator = "mfi", metoda_izstop = "rr")

CO_dobicki_btc_360 <- dobicki_trgovanje(btc_1day_vol, obdobje = 360, indikator = "co", metoda_izstop = "rr")
CO_dobicki_btc_500 <- dobicki_trgovanje(btc_1day_vol, obdobje = 500, indikator = "co", metoda_izstop = "rr")
CO_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day_vol, obdobje = 1000, indikator = "co", metoda_izstop = "rr")
CO_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day_vol, obdobje = 1800, indikator = "co", metoda_izstop = "rr")


##############################
# Združitev 9ih indikatorjev #
##############################

dobicki_indi <- function(tabela, obdobje = 1800, zacetni_kapital = 1000000, cena = "Close", 
                         add = 1/2, sl = 2, indikator = "indi"){
  tab <- tabela
  dobicki <- c()
  for(i in 1:(nrow(tab) - obdobje + 1)){
    tmp <- tab[i:(obdobje + i - 1),]
    cena2 <- odlocitev_cena(tmp, cena)
    i_izstop <- (i-1)
    dobicki <- c(dobicki, trgovanje(tmp, zacetni_kapital, cena2, add, sl, indikator))
  }
  dobicki
}

############
# 1. strategija

# buy, če > 1 index da buy signal. sell če > 1 index da sell signal
vstopni_signali <- function(tabela, dnevi_N = 20, vstop_s1 = 20, vstop_s2 = 55, izstop_s1_s2 = 10,
                            cena = "Close", dnevi_ema1 = 10, dnevi_ema2 = 50, toleranca = 0.02, 
                            rr = 3, indikator = "MA", metoda_izstop = "rr"){
  indi <- c("adx", "vi", "ppo", "rsi", "roc", "so", "fi", "mfi", "co")
  signali <- poracuni(tabela, dnevi_N, vstop_s1, vstop_s2, izstop_s1_s2, cena, dnevi_ema1, 
                      dnevi_ema2, toleranca, rr, indikator, metoda_izstop)
  signali <- signali[, c(1:6, 8, 12)]
  for(i in 1:length(indi)){
    tab <- poracuni(tabela, dnevi_N, vstop_s1, vstop_s2, izstop_s1_s2, cena, dnevi_ema1, 
                    dnevi_ema2, toleranca, rr, indikator = indi[i], metoda_izstop)
    signali <- data.frame(signali, tab$entry)
  }
  colnames(signali)[9:17] <- indi
  
  short <- rep(0, nrow(signali))
  long <- rep(0, nrow(signali))
  for(i in 1:nrow(signali)){
    short[i] <- sum(signali[i, 9:ncol(signali)] == 2)
    long[i] <- sum(signali[i, 9:ncol(signali)] == 1)
  }
  signali$long <- long
  signali$short <- short
  
  entry <- rep(0, nrow(signali))
  for(i in 1:nrow(signali)){
    if(signali$long[i] > 1 & signali$short[i] == 0){
      entry[i] <- 1
    }
    if(signali$short[i] > 1 & signali$long[i] == 0){
      entry[i] <- 2
    }
  }
  signali$entry <- entry
  signali
}

signali_indi <- vstopni_signali(btc_1day_vol)
cena1 <- odlocitev_cena(signali_indi, "Close")
signali_indi$izstop <- win_izstop_rr(signali_indi, cena1, rr)
#signali_indi$izstop <-  win_izstop_turtle(signali_indi, 20, cena1)
#signali_indi$izstop <-  win_izstop_chandelier(signali_indi, cena1, 22)


INDI_dobicki_btc_360 <- dobicki_indi(signali_indi, obdobje = 360)
INDI_dobicki_btc_500 <- dobicki_indi(signali_indi, obdobje = 500)
INDI_dobicki_btc_1000 <- dobicki_indi(signali_indi, obdobje = 1000)
INDI_dobicki_btc_1800 <- dobicki_indi(signali_indi, obdobje = 1800)



#############
# 2. strategija

# gledamo v časovnemu oknu treh dni

# za uteži
dobicki_pozicij <- function(tabela, zacetni_kapital, cena, add, sl, indikator){
  kandidati <- which(tabela$entry == 1 | tabela$entry == 2)
  if(indikator == "zelve_s1"){
    kandidati_s2 <- which(tabela$entry_s2 == 1 | tabela$entry_s2 == 2)
  }
  vstop <- kandidati[1]
  if(is.na(vstop)){profit <- 0}
  else{
    kdaj_vstopali <- c()
    kdaj_dodali_enote <- c()
    profit <- 0
    profit1 <- c()
    kdaj_profit <- c()
    money <- zacetni_kapital
    
    while(nrow(tabela) - vstop > 0){
      kdaj_vstopali <- c(kdaj_vstopali, vstop)
      cena_vstop <- cena[vstop,]
      st_btc <- unit(money, tabela$tedenski_N[vstop])
      
      #Buy signal
      if(indikator == "SinR"){
        pogoj <- ((tabela$entry[vstop]== 1 & tabela$linija[vstop] == 1) | (tabela$entry[vstop]== 1 & tabela$linija[vstop] == 2))}
      else{
        pogoj <- (tabela$entry[vstop]== 1)
      }
      if(pogoj){
        izstop <- ifelse((tabela$izstop[vstop] - i_izstop > nrow(tabela)) | 
                           (is.na(tabela$izstop[vstop] - i_izstop > nrow(tabela))), nrow(tabela), tabela$izstop[vstop] - i_izstop)
        cena_izstop <- cena[izstop,]
        st_enot <- 1
        stop_loss <- cena_vstop - sl*tabela$tedenski_N[vstop]
        pol_N <- add*tabela$tedenski_N[vstop]
        cena_ko_dodamo <- cena_vstop
        st_btc_dodamo <- st_btc
        for(i in (vstop+1):(izstop-1)){
          vstop <- i
          if((cena[i,] > (cena_vstop + st_enot*pol_N)) & (st_enot < 4)){
            razlika <- cena[i,] - cena_ko_dodamo[length(cena_ko_dodamo)]
            dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
            if(st_enot + dodamo <= 4){
              st_enot <- st_enot + dodamo
              st_btc_dodamo <- c(st_btc_dodamo, st_btc*dodamo)
              stop_loss <- stop_loss + pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
            }
            else{
              dodamo <- ifelse(st_enot==2, 2, 1)
              st_enot <- st_enot + dodamo
              st_btc_dodamo <- c(st_btc_dodamo, st_btc*dodamo)
              stop_loss <- stop_loss + pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
            }
          }
          else{
            if(cena[i,] <= stop_loss){
              kdaj_profit <- c(kdaj_profit, i)
              vsota <- 0
              for(j in 1:length(st_btc_dodamo)){
                vsota <- vsota + (cena[i,] - cena_ko_dodamo[j])*st_btc_dodamo[j]
              }
              profit <- profit + vsota
              profit1 <- c(profit1, profit)
              if(indikator == "zelve_s1" & (vsota > 0)){
                vstop <- ifelse(is.na(kandidati_s2[kandidati_s2 > vstop][1]), nrow(tabela), kandidati_s2[kandidati_s2 > vstop][1])
              }
              else{
                vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
              }
              break}
          }
        }
      }
      else{
        if(indikator == "SinR"){
          pogoj <- ((tabela$entry[vstop]== 2 & tabela$linija[vstop] == 2) | (tabela$entry[vstop]== 2 & tabela$linija[vstop] == 1))
        }
        else{
          pogoj1 <- (tabela$entry[vstop]== 2)
        }
        # Sell signal
        if(pogoj1){
          izstop <- ifelse(tabela$izstop[vstop] - i_izstop > nrow(tabela)  | 
                             is.na(tabela$izstop[vstop] - i_izstop > nrow(tabela)), nrow(tabela), tabela$izstop[vstop] - i_izstop)
          cena_izstop <- cena[izstop,]
          st_enot <- 1
          stop_loss <- cena_vstop + sl*tabela$tedenski_N[vstop]
          pol_N <- add*tabela$tedenski_N[vstop]
          cena_ko_dodamo <- cena_vstop
          st_btc_dodamo <- st_btc
          for(i in (vstop+1):(izstop-1)){
            vstop <- i
            if((cena[i,] < cena_vstop - st_enot*pol_N) & (st_enot < 4)){
              razlika <- abs(cena[i,] - cena_ko_dodamo[length(cena_ko_dodamo)])
              dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
              if(st_enot + dodamo <= 4){
                st_enot <- st_enot + dodamo
                st_btc_dodamo <- c(st_btc_dodamo, st_btc*dodamo)
                stop_loss <- stop_loss - pol_N*dodamo
                kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
              }
              else{
                dodamo <- ifelse(st_enot==2, 2, 1)
                st_enot <- st_enot + dodamo
                st_btc_dodamo <- c(st_btc_dodamo, st_btc*dodamo)
                stop_loss <- stop_loss - pol_N*dodamo
                kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
              }
            }
            else{
              if(cena[i,] >= stop_loss){
                kdaj_profit <- c(kdaj_profit, i)
                vsota <- 0
                for(j in 1:length(st_btc_dodamo)){
                  vsota <- vsota + (cena_ko_dodamo[j] - cena[i,])*st_btc_dodamo[j]
                }
                profit <- profit + vsota
                profit1 <- c(profit1, profit)
                if(indikator == "zelve_s1" & (vsota > 0)){
                  vstop <- ifelse(is.na(kandidati_s2[kandidati_s2 > vstop][1]), nrow(tabela), kandidati_s2[kandidati_s2 > vstop][1])
                }
                else{
                  vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
                }
                break}
            }
          }
        }}
      if(vstop == (izstop-1)){
        vstop <- vstop + 1
        vsota <- 0
        for(j in 1:length(st_btc_dodamo)){
          vsota <- vsota + (abs(cena_izstop - cena_ko_dodamo[j]))*st_btc_dodamo[j]
        }
        profit <- profit + vsota
        profit1 <- c(profit1, profit)
        kdaj_profit <- c(kdaj_profit, izstop)
        if(indikator == "zelve_s1" & (vsota > 0)){
          vstop <- ifelse(is.na(kandidati_s2[kandidati_s2 > vstop][1]), nrow(tabela), kandidati_s2[kandidati_s2 > vstop][1])
        }
        else{
          vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
        }
      }
      #if(profit <= -100000){money <- 0.8*money}
    }
    profit1
  }}



uspesnost_indi <- function(tabela, dnevi_N = 20, vstop_s1 = 20, vstop_s2 = 55, izstop_s1_s2 = 10, 
                           zacetni_kapital = 1000000, cena = "Close", add = 0.5, sl = 2, 
                           toleranca = 0.02, rr = 3, dnevi_ema1 = 10, dnevi_ema2 = 50, 
                           indikator = "MA", metoda_izstop = "rr"){
  indi <- c("adx", "vi", "ppo", "rsi", "roc", "so", "fi", "mfi", "co")
  ratio <- matrix(0, 1, 9)
  for(i in 1:length(indi)){
    tab <- poracuni(tabela, dnevi_N, vstop_s1, vstop_s2, izstop_s1_s2, cena, dnevi_ema1, 
                    dnevi_ema2, toleranca, rr, indikator = indi[i], metoda_izstop)
    cena1 <- odlocitev_cena(tab, cena)
    profit1 <- dobicki_pozicij(tab, zacetni_kapital, cena1, add, sl, indikator)
    dobicki_poslov <- c(profit1[1], profit1[2:length(profit1)] - profit1[1:(length(profit1)-1)])
    dobicki_poslov[is.na(dobicki_poslov)] <- 0
    izgube <- -sum(dobicki_poslov[dobicki_poslov < 0])
    dobitki <- sum(dobicki_poslov[dobicki_poslov > 0])
    if(izgube == 0){izgube <- 1}
    ratio[1, i] <- (dobitki/izgube)
  }
  ratio <- data.frame(ratio)
  colnames(ratio) <- indi
  ratio
}

ratio <- uspesnost_indi(btc_1day_vol)
utezi <- ratio/max(ratio[1, ])

# pomnožimo matriko vstopnih signalov z utežmi
indi_utezi <- signali_indi[, -(ncol(signali_indi)-3):-ncol(signali_indi)]
A <- as.matrix(indi_utezi[, 9:17])
A[A > 1] <- 1 
library(dplyr)
X <- A %*% t(as.matrix(utezi))
indi_utezi$X <- round(X, 2)

# določimo entry in izstop
short <- rep(0, nrow(indi_utezi))
long <- rep(0, nrow(indi_utezi))
for(i in 1:nrow(indi_utezi)){
  short[i] <- sum(indi_utezi[i, 9:ncol(indi_utezi)] == 2)
  long[i] <- sum(indi_utezi[i, 9:ncol(indi_utezi)] == 1)
}
indi_utezi$long <- long
indi_utezi$short <- short

entry <- rep(0, nrow(indi_utezi))
for(i in 1:(nrow(indi_utezi)-2)){
  l <- 0
  u <- 0
  s <- 0
  for(j in i:(i+2)){
    l <- l + indi_utezi$long[j]
    u <- u + indi_utezi$X[j]
    s <- s + indi_utezi$short[j]
    if((l > 1) & (u > 0.1) & (s == 0)){
      entry[j] <- 1
      break
    }
  }
  s1 <- 0
  u1 <- 0
  l1 <- 0
  for(j in i:(i+2)){
    s1 <- s1 + indi_utezi$short[j]
    u1 <- u1 + indi_utezi$X[j]
    l1 <- l1 + indi_utezi$long[j]
    if((s1 > 1) & (u1 > 0.1) & (l1 == 0)){
      entry[j] <- 2
      break
    }
  }
}
indi_utezi$entry <- entry
cena1 <- odlocitev_cena(indi_utezi, "Close")
indi_utezi$izstop <- win_izstop_rr(indi_utezi, cena1, rr)
#indi_utezi$izstop <-  win_izstop_turtle(indi_utezi, 20, cena1)
#indi_utezi$izstop <-  win_izstop_chandelier(indi_utezi, cena1, 22)

indi2_dobicki_btc_360 <- dobicki_indi(indi_utezi, obdobje = 360)
indi2_dobicki_btc_500 <- dobicki_indi(indi_utezi, obdobje = 500)
indi2_dobicki_btc_1000 <- dobicki_indi(indi_utezi, obdobje = 1000)
indi2_dobicki_btc_1800 <- dobicki_indi(indi_utezi, obdobje = 1800)


#####################
# Mere volatilnosti #
#####################



