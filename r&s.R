##########################
# Support and Resistance #
##########################

# lokalne minimume najdemo lahko tudi z drugimi odvodi kernel regression-a (glej kodo npr. na githubu od SIT)
# lokalni minimumi in maksimumi
#set.seed(123)

# boljša funkcija:
lok_ekstremi <- function(tabela, cena, velikost_oken){
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
    # fit <- Ckmeans.1d.dp(minz, i)
    # fit <- Ckmeans.1d.dp(maxz, i)
    mera_podpora <- c(mera_podpora, fit$tot.withinss)
    mera_odpor <- c(mera_odpor, fit1$tot.withinss)
  }
  st_skupin_podpora <- 9 + which.min(mera_podpora)
  st_skupin_odpor <- 9 + which.min(mera_odpor)
  # tmp <- kmeans(minz, st_skupin_podpora)
  # tmp1 <- kmeans(maxz, st_skupin_odpor)
  tmp <- Ckmeans.1d.dp(minz, st_skupin_podpora)
  tmp1 <- Ckmeans.1d.dp(maxz, st_skupin_odpor)
  podpore <- tmp$centers
  odpori <- tmp1$centers
  list("podpore" = podpore, "odpori" = odpori, "odmiki_p" = tmp$tot.withinss, 
       "odmiki_o" = tmp1$tot.withinss, "moc_p" = tmp$size, "moc_o" = tmp1$size,
       "l_min" = minz, "l_max" = maxz, "kje_min" = eks$kje_min, "kje_max" = eks$kje_max)
}

SinR <- prepoznavalnik_SinR(tab, data.frame(log(tab$Close)), 10)
support <- data.frame("podpore" = SinR$podpore, "moc" = SinR$moc_p)
resistance <- data.frame("odpori" = SinR$odpori, "moc" = SinR$moc_o)
resistance <- resistance[order(resistance$odpori),]

# opt_velikost_oken <- function(){
#   dolzina <- c(3, 5, 7, 10, 15, 20)
#   odmiki_p <- c()
#   odmiki_o <- c()
#   for(i in dolzina){
#     tmp <- prepoznavalik_SinR(btc_1day, velikost_oken = i)
#     odmiki_p <- c(odmiki_p, tmp$odmiki_p)
#     odmiki_o <- c(odmiki_o, tmp$odmiki_o)
#   }
#   c(dolzina[which.min(odmiki_p)], dolzina[which.min(odmiki_o)])
# }

# dolzina_oken <- opt_velikost_oken()

ts.plot(log(tab$Close)[1:100])
points(SinR$kje_min, SinR$l_min, col="blue")
points(SinR$kje_max, SinR$l_max, col="red")
lines(x = 1:100, y = rep(resistance$odpori[1],100), col = "red")
lines(x = 1:100, y = rep(resistance$odpori[2],100), col = "red")
lines(x = 1:100, y = rep(resistance$odpori[3],100), col = "red")
lines(x = 1:100, y = rep(support$podpore[1],100), col = "blue")
lines(x = 1:100, y = rep(support$podpore[2],100), col = "blue")
lines(x = 1:100, y = rep(support$podpore[3],100), col = "blue")
lines(x = 1:100, y = rep(support$podpore[4],100), col = "blue")
lines(x = 1:100, y = rep(support$podpore[5],100), col = "blue")
lines(x = 1:100, y = rep(support$podpore[6],100), col = "blue")

# Vstopanje
#entry: 1=go long, 2=go short
#linija: 1 = podpora, 2 = odpor
#koliko_casa: oddaljenost od linije (koliko dni pozneje smo vstopili)
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

# Izstopanje iz zmagovalne pozicije
# rr = risk/reward ... npr. če se odločimo za 1:3 je rr = 3
win_izstop_SinR <- function(tabela, cena, rr, crte){
  podpore <- crte$podpore
  odpori <- as.numeric(crte$odpori)
  izstop <- rep(0, nrow(tabela))
  for(i in 1:nrow(tabela)){
    if(tabela$entry[i] == 1 & tabela$linija[i] == 1){
      izstop[i] <- kje_max[kje_max > i][1] + 1
    }
    if(tabela$entry[i] == 2 & tabela$linija[i] == 2){
      izstop[i] <- kje_min[kje_min > i][1] + 1
    }
    if(tabela$entry[i] == 2 & tabela$linija[i] == 1){
      #opazovana_podpora <- podpore[which.min(abs(cena[i-koliko_casa[i],]-podpore))]
      #cilj <- opazovana_podpora - sl*rr*tabela$spr_tedenski_N[i]
      cilj <- cena[i,]- sl*rr*tabela$spr_tedenski_N[i]
      kandidati_cilj <- which(cena[,1] <= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
    if(tabela$entry[i] == 1 & tabela$linija[i] == 2){
      #opazovani_odpor <- odpori[which.min(abs(cena[i-koliko_casa[i],]-odpori))]
      #cilj <- opazovani_odpor + sl*rr*tabela$spr_tedenski_N[i]
      cilj <- cena[i,] + sl*rr*tabela$spr_tedenski_N[i]
      kandidati_cilj <- which(cena[,1] >= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
  }
  izstop
}


trgovanje <- function(tabela, zacetni_kapital, cena, add, sl){
  kandidati <- which(tabela$entry==1 | tabela$entry==2)
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
      st_btc <- unit(money, tabela$spr_tedenski_N[vstop])/cena_vstop
      
      # obrat na podpori
      if(tabela$entry[vstop]== 1 & tabela$linija[vstop] == 1){
        izstop <- ifelse(tabela$izstop[vstop] - i_izstop > nrow(tabela) | 
                           is.na(tabela$izstop[vstop] - i_izstop > nrow(tabela)), nrow(tabela), tabela$izstop[vstop] - i_izstop)
        cena_izstop <- cena[izstop,]
        st_enot <- 1
        stop_loss <- cena_vstop - sl*tabela$spr_tedenski_N[vstop]
        pol_N <- add*tabela$spr_tedenski_N[vstop]
        cena_ko_dodamo <- cena_vstop
        st_btc_dodamo <- st_btc
        if((vstop+1) <= (izstop-1)){
        for(i in (vstop+1):(izstop-1)){
          vstop <- i
          if((cena[i,] > cena_vstop + st_enot*pol_N) & (st_enot < 4)){
            razlika <- cena[i,] - cena_ko_dodamo[length(cena_ko_dodamo)]
            dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
            if(st_enot + dodamo <= 4){
              st_enot <- st_enot + dodamo
              st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/cena[i,])*dodamo)
              stop_loss <- stop_loss + pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
            }
            else{
              dodamo <- ifelse(st_enot==2, 2, 1)
              st_enot <- st_enot + dodamo
              st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/cena[i,])*dodamo)
              stop_loss <- stop_loss + pol_N*dodamo
              kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
              cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
            }
          }
          else{
            if(cena[i,] <= stop_loss){
              vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
              kdaj_profit <- c(kdaj_profit, i)
              for(j in 1:length(st_btc_dodamo)){
                profit <- profit + (cena[i,] - cena_ko_dodamo[j])*st_btc_dodamo[j]
              }
              profit1 <- c(profit1, profit)
              break}
          }
        }}
      }
      else{
        # obrat na odporu
        if(tabela$entry[vstop]== 2 & tabela$linija[vstop] == 2){
          izstop <- ifelse(tabela$izstop[vstop] - i_izstop > nrow(tabela)  | 
                             is.na(tabela$izstop[vstop] - i_izstop > nrow(tabela)), nrow(tabela), tabela$izstop[vstop] - i_izstop)
          cena_izstop <- cena[izstop,]
          st_enot <- 1
          stop_loss <- cena_vstop + sl*tabela$spr_tedenski_N[vstop]
          pol_N <- add*tabela$spr_tedenski_N[vstop]
          cena_ko_dodamo <- cena_vstop
          st_btc_dodamo <- st_btc
          if((vstop+1) <= (izstop-1)){
          for(i in (vstop+1):(izstop-1)){
            vstop <- i
            if((cena[i,] < cena_vstop - st_enot*pol_N) & (st_enot < 4)){
              razlika <- abs(cena[i,] - cena_ko_dodamo[length(cena_ko_dodamo)])
              dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
              if(st_enot + dodamo <= 4){
                st_enot <- st_enot + dodamo
                st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/cena[i,])*dodamo)
                stop_loss <- stop_loss - pol_N*dodamo
                kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
              }
              else{
                dodamo <- ifelse(st_enot==2, 2, 1)
                st_enot <- st_enot + dodamo
                st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/cena[i,])*dodamo)
                stop_loss <- stop_loss - pol_N*dodamo
                kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
              }
            }
            else{
              if(cena[i,] >= stop_loss){
                vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
                kdaj_profit <- c(kdaj_profit, i)
                for(j in 1:length(st_btc_dodamo)){
                  profit <- profit + (cena_ko_dodamo[j] - cena[i,])*st_btc_dodamo[j]
                }
                profit1 <- c(profit1, profit)
                break}
            }
          }}
        }
        else{
          # preboj pri podpori
          if(tabela$entry[vstop]== 2 & tabela$linija[vstop] == 1){
            izstop <- ifelse((tabela$izstop[vstop] - i_izstop > nrow(tabela)) | 
                               is.na(tabela$izstop[vstop] - i_izstop > nrow(tabela)), nrow(tabela), tabela$izstop[vstop] - i_izstop)
            cena_izstop <- cena[izstop,]
            st_enot <- 1
            stop_loss <- cena_vstop + sl*tabela$spr_tedenski_N[vstop]
            pol_N <- add*tabela$spr_tedenski_N[vstop]
            cena_ko_dodamo <- cena_vstop
            st_btc_dodamo <- st_btc
            if((vstop+1) <= (izstop-1)){
            for(i in (vstop+1):(izstop-1)){
              vstop <- i
              if((cena[i,] < cena_vstop - st_enot*pol_N) & (st_enot < 4)){
                razlika <- abs(cena[i,] - cena_ko_dodamo[length(cena_ko_dodamo)])
                dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
                if(st_enot + dodamo <= 4){
                  st_enot <- st_enot + dodamo
                  st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/cena[i,])*dodamo)
                  stop_loss <- stop_loss - pol_N*dodamo
                  kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                  cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
                }
                else{
                  dodamo <- ifelse(st_enot==2, 2, 1)
                  st_enot <- st_enot + dodamo
                  st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/cena[i,])*dodamo)
                  stop_loss <- stop_loss - pol_N*dodamo
                  kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                  cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
                }
              }
              else{
                if(cena[i,] >= stop_loss){
                  vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
                  kdaj_profit <- c(kdaj_profit, i)
                  for(j in 1:length(st_btc_dodamo)){
                    profit <- profit + (cena_ko_dodamo[j] - cena[i,])*st_btc_dodamo[j]
                  }
                  profit1 <- c(profit1, profit)
                  break}
              }
            }}
          }
          else{
            # preboj pri odporu
            if(tabela$entry[vstop]== 1 & tabela$linija[vstop] == 2){
              izstop <- ifelse((tabela$izstop[vstop] - i_izstop > nrow(tabela)) |
                                 is.na(tabela$izstop[vstop] - i_izstop > nrow(tabela)), nrow(tabela), tabela$izstop[vstop] - i_izstop)
              cena_izstop <- cena[izstop,]
              st_enot <- 1
              stop_loss <- cena_vstop - sl*tabela$spr_tedenski_N[vstop]
              pol_N <- add*tabela$spr_tedenski_N[vstop]
              cena_ko_dodamo <- cena_vstop
              st_btc_dodamo <- st_btc
              if((vstop+1) <= (izstop-1)){
              for(i in (vstop+1):(izstop-1)){
                vstop <- i
                if((cena[i,] > cena_vstop + st_enot*pol_N) & (st_enot < 4)){
                  razlika <- cena[i,] - cena_ko_dodamo[length(cena_ko_dodamo)]
                  dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
                  if(st_enot + dodamo <= 4){
                    st_enot <- st_enot + dodamo
                    st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/cena[i,])*dodamo)
                    stop_loss <- stop_loss + pol_N*dodamo
                    kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                    cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
                  }
                  else{
                    dodamo <- ifelse(st_enot==2, 2, 1)
                    st_enot <- st_enot + dodamo
                    st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/cena[i,])*dodamo)
                    stop_loss <- stop_loss + pol_N*dodamo
                    kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
                    cena_ko_dodamo <- c(cena_ko_dodamo, cena[i,])
                  }
                }
                else{
                  if(cena[i,] <= stop_loss){
                    vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
                    kdaj_profit <- c(kdaj_profit, i)
                    for(j in 1:length(st_btc_dodamo)){
                      profit <- profit + (cena[i,] - cena_ko_dodamo[j])*st_btc_dodamo[j]
                    }
                    profit1 <- c(profit1, profit)
                    break}
                }
              }}
            }
          }}}
      if(vstop == (izstop-1)){
        vstop <- vstop + 1
        for(j in 1:length(st_btc_dodamo)){
          profit <- profit + (abs(cena_izstop - cena_ko_dodamo[j]))*st_btc_dodamo[j]
        }
        profit1 <- c(profit1, profit)
        kdaj_profit <- c(kdaj_profit, izstop)
        vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
      }
      #else{profit <- profit}
      #if(profit <= -100000){money <- 0.8*money}
    }
    profit
  }}


odlocitev_cena <- function(tabela, cena){
  if(cena == "Close"){cena1 <- data.frame(tabela$Close)}
  if(cena == "Low"){cena1 <- data.frame(tabela$Low)}
  if(cena == "High"){cena1 <- data.frame(tabela$High)}
  if(cena == "Open"){cena1 <- data.frame(tabela$Open)}
  cena1
}

#izračuna True Range in N
spr_N <- function(tabela, dnevi){
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
  tabela$spr_N <- n
  tabela
}

poracuni <- function(tabela, dnevi_N, cena, toleranca, rr){
  tabela <- tabela[-nrow(tabela),]
  tab <- spr_N(tabela, dnevi_N)
  cena1 <- odlocitev_cena(tab, cena)
  cena1 <- log(cena1)
  SinR <- prepoznavalnik_SinR(tab, cena1, velikost_oken = 10)
  tmp <- vstop_SinR(tab, cena1, toleranca, SinR)
  tab$entry <- tmp$entry
  tab$linija <- tmp$linija
  tab$koliko_casa <- tmp$koliko_casa
  tab$spr_tedenski_N <- spr_tedenski_N(tab)
  tab$izstop <- win_izstop_SinR(tab, cena1, rr, SinR)
  pomoc <- which(win_izstop_SinR(tab, cena1, rr, SinR) > 0)
  tab$izstop[pomoc] <- tab$izstop[pomoc] - 22 + 1
  tab <- tab[tab$spr_tedenski_N > 0,]
  tab
}


tabela <- btc_1day
cena <- "Close"
dobicki_trgovanje <- function(tabela, dnevi_N = 20, obdobje = 1800, zacetni_kapital = 1000000, 
                              cena = "Close", add = 1/2, sl = 2, toleranca = 0.02, rr = 3){
  tab <- poracuni(tabela, dnevi_N, cena, toleranca, rr)
  dobicki <- c()
  for(i in 1:(nrow(tab) - obdobje + 1)){
    tmp <- tab[i:(obdobje + i - 1),]
    cena2 <- odlocitev_cena(tmp, cena)
    i_izstop <- (i-1)
    dobicki <- c(dobicki, trgovanje(tmp, zacetni_kapital, cena2, add, sl))
  }
  dobicki
}

SinR_dobicki_btc_360 <- dobicki_trgovanje(btc_1day, obdobje = 360)
SinR_dobicki_btc_500 <- dobicki_trgovanje(btc_1day, obdobje = 500)
SinR_dobicki_btc_1000 <- dobicki_trgovanje(btc_1day, obdobje = 1000)
SinR_dobicki_btc_1800 <- dobicki_trgovanje(btc_1day)

SinR_dobicki_btc_360 <- SinR_dobicki_btc_360/1000
SinR_dobicki_btc_500 <- SinR_dobicki_btc_500/1000
SinR_dobicki_btc_1000 <- SinR_dobicki_btc_1000/1000
SinR_dobicki_btc_1800 <- SinR_dobicki_btc_1800/1000

# dobički v času
grid.arrange(dobicki_v_casu(SinR_dobicki_btc_360, 360), dobicki_v_casu(SinR_dobicki_btc_500, 500), 
             dobicki_v_casu(SinR_dobicki_btc_1000, 1000), dobicki_v_casu(SinR_dobicki_btc_1800, 1800),
             nrow = 2, ncol = 2)

grid.arrange(dobicki_v_casu(SinR_dobicki_btc_360[500:length(SinR_dobicki_btc_360)], 360, 500),
             dobicki_v_casu(SinR_dobicki_btc_500[500:length(SinR_dobicki_btc_500)], 500, 500),
             dobicki_v_casu(SinR_dobicki_btc_1000[500:length(SinR_dobicki_btc_1000)], 1000, 500),
             dobicki_v_casu(SinR_dobicki_btc_1800[500:length(SinR_dobicki_btc_1800)], 1800, 500),
             nrow = 2, ncol = 2)


# source("TA_pregled.R")
SinR_pregled_btc <- pregled_trgovanje(SinR_dobicki_btc_360, SinR_dobicki_btc_500, SinR_dobicki_btc_1000, 
                                      SinR_dobicki_btc_1800)


flextabela_pregled(SinR_pregled_btc, 0)


# pred/po letu 2014
SinR_pred_po_2014 <- pred_po_2014(SinR_dobicki_btc_360, SinR_dobicki_btc_500, SinR_dobicki_btc_1000, 
                                  SinR_dobicki_btc_1800, 700)
flextabela_pregled(SinR_pred_po_2014, 0)

SinR_sd_pred_po_2014 <- sd_pred_po_2014(SinR_dobicki_btc_360, SinR_dobicki_btc_500, SinR_dobicki_btc_1000, 
                                        SinR_dobicki_btc_1800, 700)
flextabela_pregled(SinR_sd_pred_po_2014, 0)

#########################################################################################################
# Glajenje r&s

# # SIT
# y <- btc_1day$Close
# n = length(y)
# t = 1:n
# 
# library(sm)
# #cv = cross-validation
# h = h.select(t, y, method = 'cv')
# temp = sm.regression(t, y, h=h, display = 'none')
# mhat = approx(temp$eval.points, temp$estimate, t, method='linear')$y
# 
# temp = diff(sign(diff(mhat)))
# loc = which( temp != 0 ) + 1
# loc.dir = -sign(temp[(loc - 1)])
# 
# temp = c( y[1], y, y[n] )
# temp = cbind(temp[loc], temp[(loc + 1)], temp[(loc + 2)])
# max.index = loc + apply(temp, 1, which.max) - 2
# min.index = loc + apply(temp, 1, which.min) - 2
# data.loc = ifelse(loc.dir > 0, max.index, min.index)
# data.loc = ifelse(data.loc < 1, 1, ifelse(data.loc > n, n, data.loc))
# 
# plot(btc_1day$Timestamp, btc_1day$Close, type = "l")
# points(x = btc_1day$Timestamp[data.loc], btc_1day$Close[data.loc], col = "red")



# Rpatrec_package
library(rpatrec)
#plot(btc_1day$Timestamp[1:100], btc_1day$Close[1:100], type = "l")
#lines(btc_1day$Timestamp[1:100], a[1:100], col="blue")

library(ggplot2)
a <- kernel(btc_1day$Close[1:100], 1)
b <- kernel(btc_1day$Close[1:100], 5)
ggplot()+
  geom_line(aes(btc_1day$Timestamp[1:100], btc_1day$Close[1:100]))+
  geom_line(aes(btc_1day$Timestamp[1:100], a), col = "blue")+
  theme_minimal()+
  ggtitle("Glajenje (širina okolice = 1)")+
  xlab("Trgovalni dnevi")+
  ylab("Zaključna cena dneva")+
  theme(plot.title = element_text(hjust = 0.5, size = 16), axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12))

ggplot()+
  geom_line(aes(btc_1day$Timestamp[1:100], btc_1day$Close[1:100]))+
  geom_line(aes(btc_1day$Timestamp[1:100], b), col = "blue")+
  theme_minimal()+
  ggtitle("Glajenje (širina okolice = 5)")+
  xlab("Trgovalni dnevi")+
  ylab("Zaključna cena dneva")+
  theme(plot.title = element_text(hjust = 0.5, size = 16), axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12))




prepoznavalnik <- function(jedro){
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

SinR <- prepoznavalnik(a)
linije <- data.frame("podpore" = SinR$linije, "moc" = SinR$moc)


a <- log(kernel(btc_1day$Close, 1))
proba <- prepoznavalnik()
ts.plot(log(btc_1day$Close)[1:100])
lines(x = 1:100, y = rep(proba$podpore[1],100), col = "black")
lines(x = 1:100, y = rep(proba$podpore[2],100), col = "black")
lines(x = 1:100, y = rep(proba$podpore[3],100), col = "black")
lines(x = 1:100, y = rep(proba$podpore[4],100), col = "black")
lines(x = 1:100, y = rep(proba$podpore[5],100), col = "black")
lines(x = 1:100, y = rep(proba$podpore[6],100), col = "black")

plot(btc_1day$Timestamp[1:100], a[1:100], col="blue", type = "l")
lines(btc_1day$Timestamp[1:100], y = rep(proba$podpore[1],100), col = "black")
lines(btc_1day$Timestamp[1:100], y = rep(proba$podpore[2],100), col = "black")
lines(btc_1day$Timestamp[1:100], y = rep(proba$podpore[3],100), col = "black")
lines(btc_1day$Timestamp[1:100], y = rep(proba$podpore[4],100), col = "black")
lines(btc_1day$Timestamp[1:100], y = rep(proba$podpore[5],100), col = "black")

#####################################################################################

vstop_SinR <- function(tabela, cena, toleranca, crte){
  
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

# Izstopanje iz zmagovalne pozicije
# rr = risk/reward ... npr. če se odločimo za 1:3 je rr = 3
win_izstop_SinR <- function(tabela, cena, rr, crte){
  linije <- crte$linije
  izstop <- rep(0, nrow(tabela))
  for(i in 1:nrow(tabela)){
    if(tabela$entry[i] == 1 & tabela$linija[i] == 1){
      izstop[i] <- which(tabela$linija == 2)[which(tabela$linija == 2) > i][1]
    }
    if(tabela$entry[i] == 2 & tabela$linija[i] == 2){
      izstop[i] <- which(tabela$linija == 1)[which(tabela$linija == 1) > i][1]
    }
    if(tabela$entry[i] == 2 & tabela$linija[i] == 1){
      cilj <- cena[i,]- sl*rr*tabela$spr_tedenski_N[i]
      kandidati_cilj <- which(cena[,1] <= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
    if(tabela$entry[i] == 1 & tabela$linija[i] == 2){
      cilj <- cena[i,] + sl*rr*tabela$spr_tedenski_N[i]
      kandidati_cilj <- which(cena[,1] >= cilj)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
  }
  izstop
}

poracuni <- function(tabela, dnevi_N, cena, toleranca, rr){
  #library(rpatrec)
  tabela <- tabela[-nrow(tabela),]
  tab <- spr_N(tabela, dnevi_N)
  cena1 <- odlocitev_cena(tab, cena)
  cena1 <- log(cena1)
  a <- kernel(cena1[,1], 1)
  SinR <- prepoznavalnik(a)
  tmp <- vstop_SinR(tab, cena1, toleranca, SinR)
  tab$entry <- tmp$entry
  tab$linija <- tmp$linija
  tab$koliko_casa <- tmp$koliko_casa
  tab$spr_tedenski_N <- spr_tedenski_N(tab)
  tab$izstop <- win_izstop_SinR(tab, cena1, rr, SinR)
  pomoc <- which(win_izstop_SinR(tab, cena1, rr, SinR) > 0)
  tab$izstop[pomoc] <- tab$izstop[pomoc] - 22 + 1
  tab <- tab[tab$spr_tedenski_N > 0,]
  tab
}

proba1 <- dobicki_trgovanje(btc_1day, obdobje = 360)
proba2 <- dobicki_trgovanje(btc_1day, obdobje = 500)
proba3 <- dobicki_trgovanje(btc_1day, obdobje = 1000)
proba4 <- dobicki_trgovanje(btc_1day)

proba1 <- proba1/1000
proba2 <- proba2/1000
proba3 <- proba3/1000
proba4 <- proba4/1000

proba <- pregled_trgovanje(proba1, proba2, proba3, proba4)
library(flextable)
flextabela_pregled(proba, 0)



# #############################################################################################################
# # package quantmod, različne vrste clusteringov, detekcija S&R - preizkušanje
# 
# library(quantmod)
# #https://github.com/joshuaulrich/quantmod/tree/master/R
# #https://cran.r-project.org/web/packages/quantmod/quantmod.pdf
# 
# getSymbols("SPY", from="2012-01-01", to="2012-06-15")
# chartSeries(SPY, theme="white")
# 
# 
# #getSymbols.rda("databtc_1day", from="2013-01-01", to="2013-06-15")
# #proba1 <- getSymbols.csv("podatki/BTC_USD_min", env = .GlobalEnv)
# getSymbols("BTC-USD")
# chartSeries(`BTC-USD`[1:100,], theme = "white", TA = "addWMA()")
# addEMA()
# 
# 
# ##############
# # Clustering #
# ##############
# # Podatki
# podatki <- btc_1day[, c(-1)]
# podatki <- na.omit(podatki)
# #podatki <- scale(podatki)
# 
# 
# library("factoextra")
# # Optimal number of clusters
# wss <- (nrow(podatki)-1)*sum(apply(podatki,2,var))
# for (i in 2:30) wss[i] <- sum(kmeans(podatki, centers=i)$withinss)
# plot(1:30, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")
# #ali:
# fviz_nbclust(podatki, kmeans, method = "gap_stat")
# #ali:
# library("magrittr")
# library("NbClust")
# res.nbclust <- podatki %>%
#   NbClust(distance = "euclidean",
#           min.nc = 2, max.nc = 30, 
#           method = "complete", index ="all")
# fviz_nbclust(res.nbclust, ggtheme = theme_minimal())
# 
# 
# # Partitioning clustering
# # Hartigan and Wong algorithm
# fit <- kmeans(podatki, 30)
# aggregate(podatki,by=list(fit$cluster),FUN=mean)
# #isto je fit$centers
# #podatki <- data.frame(podatki, fit$cluster)
# library(cluster) 
# clusplot(podatki, fit$cluster, color=TRUE, shade=TRUE, 
#          labels=2, lines=0)
# # Centroid Plot against 1st 2 discriminant functions
# library(fpc)
# plotcluster(podatki, fit$cluster)
# 
# km.res <- kmeans(podatki, 30, nstart = 25)
# fviz_cluster(km.res, data = podatki,
#              ellipse.type = "convex",
#              palette = "jco",
#              ggtheme = theme_minimal())
# 
# 
# # Hierarchical Clustering
# d <- dist(podatki, method = "euclidean")
# fit <- hclust(d, method="ward.D") 
# plot(fit)
# groups <- cutree(fit, k=30)
# rect.hclust(fit, k=30, border="red")
# #ali:
# fviz_dend(fit, k = 30,cex = 0.5, 
#           k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
#           color_labels_by_k = TRUE, rect = TRUE)
# 
# 
# # Ward Hierarchical Clustering with Bootstrapped p values
# library(pvclust)
# podatki1 <- data.frame(podatki[,c(1, 4)])
# fit <- pvclust(podatki, method.hclust="ward.D2",
#                method.dist="euclidean")
# plot(fit)
# pvrect(fit, alpha=.95)
# 
# 
# # Model Based Clustering
# library(mclust)
# fit <- Mclust(podatki)
# plot(fit) # plot results 
# summary(fit) # display the best model
# 
# 
# # Clustering validation
# res.hc <- iris[, -5] %>%
#   scale() %>%
#   eclust("hclust", k = 3, graph = FALSE)
# fviz_silhouette(res.hc)
# 
# 
# # Bitcoin
# fit <- kmeans(podatki, 50)
# skupine <- data.frame(povprecje = fit$centers[,4], velikost = fit$size)
# kaj_graf <- btc_1day[1:100,]
# plot(x = kaj_graf$Timestamp, y = kaj_graf$Close, type = "p")
# abline(h = skupine$povprecje, col = "red")
# 
# 
# # Drugi primeri
# 
# # Fuzzy clustering - en element lahko v večih clustrih
# library(cluster) # Loads the cluster library.
# fannyy <- fanny(podatki, k=4, metric = "euclidean", memb.exp = 1.2)
# round(fannyy$membership, 2)[1:4,]
# fannyy$clustering 
# 
# # Principal component analysis
# pca <- prcomp(podatki, scale=T)
# summary(pca)
# plot(pca$x, pch=20, col="blue", type="n") # To plot dots, drop type="n"
# text(pca$x, rownames(pca$x), cex=0.8)
# 
# # Multidimensional Scaling
# library(stats)
# loc <- cmdscale(dist(podatki, method = "euclidean")) 
# plot(loc[,1], -loc[,2], type="n", xlab="", ylab="", main="cmdscale(podatki)")
# text(loc[,1], -loc[,2], rownames(loc), cex=0.8) 
# 
# 
# 
# ## Support and Resistance
# 
# detectSupportResistance <- function(timeSeries, tolerance=0.01, nChunks=10, nPoints=3, plotChart=TRUE)
# {
#   #detect maximums and minimums
#   N = length(timeSeries)
#   stp = floor(N / nChunks)
#   minz = array(0.0, dim=nChunks)
#   whichMinz = array(0, dim=nChunks)
#   maxz = array(0.0, dim=nChunks)
#   whichMaxz = array(0, dim=nChunks)
#   for(j in 1:(nChunks-1)) 
#   {
#     lft = (j-1)*stp + 1  #left and right elements of each chunk
#     rght = j*stp
#     whichMinz[j] = which.min(timeSeries[lft:rght]) + lft
#     minz[j] = min(timeSeries[lft:rght])
#     whichMaxz[j] = which.max(timeSeries[lft:rght]) + lft
#     maxz[j] = max(timeSeries[lft:rght])
#   }   
#   #last chunk
#   lft = j*stp + 1  #left and right elements of each chunk
#   rght = N
#   whichMinz[nChunks] = which.min(timeSeries[lft:rght]) + lft
#   minz[nChunks] = min(timeSeries[lft:rght])
#   whichMaxz[nChunks] = which.max(timeSeries[lft:rght]) + lft
#   maxz[nChunks] = max(timeSeries[lft:rght])
#   
#   result = list()
#   result[["minima"]] = NULL
#   result[["minimaAt"]] = NULL
#   result[["maxima"]] = NULL
#   result[["maximaAt"]] = NULL
#   span = tolerance*(max(maxz) - min(minz))
#   
#   rang = order(minz)[1:nPoints]
#   if((minz[rang[nPoints]] - minz[rang[1]]) <= span)
#   {
#     result[["minima"]] = minz[rang[1:nPoints]]
#     result[["minimaAt"]] = whichMinz[rang[1:nPoints]]
#   } 
#   
#   rang = order(maxz, decreasing = TRUE)[1:nPoints]
#   if((maxz[rang[1]] - maxz[rang[nPoints]]) <= span)
#   {
#     result[["maxima"]] = maxz[rang[1:nPoints]]
#     result[["maximaAt"]] = whichMaxz[rang[1:nPoints]]
#   } 
#   
#   if(plotChart)
#   {
#     ts.plot(timeSeries)
#     points(whichMinz, minz, col="blue")
#     points(whichMaxz, maxz, col="red")
#     if(!is.null(result[["minima"]])  &&  !is.null(result[["minimaAt"]]))
#       abline(lm(result[["minima"]] ~  result[["minimaAt"]]))
#     if(!is.null(result[["maxima"]])  &&  !is.null(result[["maximaAt"]]))
#       abline(lm(result[["maxima"]] ~  result[["maximaAt"]]))
#   } 
#   
#   return(result)    
# }
# 
# # ni dobra funkcija, ker si sam zbereš okvirje na katerih izračuna min in max
# # torej, če si zbral celotno dolžino timeSeries 100 in okvirjev 10 bo min in max manj kot če si zbral okvirjev 50
# # support izriše le če so prve tri točke znotraj tolerance in resistance če so zadnje tri točke
# # za našo analizo bi mogel spremenit funkcijo, da pogleda vsake tri točke
# detectSupportResistance(timeSeries = btc_1day$Close[100:200], tolerance = 0.03, nChunks = 10, nPoints = 3, plotChart = T)
# detectSupportResistance(timeSeries = btc_1day$Close[120:200], tolerance = 0.03, nChunks = 10, nPoints = 3, plotChart = T)

