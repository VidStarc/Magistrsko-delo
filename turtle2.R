###########################
# Spreminjanje parametrov #
###########################

#časovna skala: 1 dan
#začetni kapital: 1000000


#izračuna True Range in N
#dnevi = toliko dnevna eksponentna drseča sredina (default = 20)
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
  prvi_n <- mean(tabela$TR[(1:dnevi)])
  n <- c(rep(0,(dnevi-1)), prvi_n, rep(0, (nrow(tabela)-dnevi)))
  for(i in (dnevi+1):nrow(tabela)){
    n[i] <- ((dnevi-1)*n[i-1] + tabela$TR[i])/dnevi
  }
  tabela$spr_N <- n
  tabela
}

unit <- function(account_size, N){
  floor((0.01*account_size)/N)
}


#vstopne točke: 1=go long, 2=go short
#System 1
#vstop_s1 = toliko dnevni višek ali toliko dnevno dno (default = 20)
spr_entry_s1 <- function(tabela, vstop_s1, cena){
  entry <- rep(0, nrow(tabela))
  for(i in (vstop_s1+1):nrow(tabela)){
    if(cena[i,] > max(cena[(i-vstop_s1):(i-1),])){entry[i] <- 1}
    if(cena[i,] < min(cena[(i-vstop_s1):(i-1),])){entry[i] <- 2}
  }
  entry
}


#System 2
#vstop_s2 = toliko dnevni višek ali toliko dnevno dno (default = 55)
spr_entry_s2 <- function(tabela, vstop_s2, cena){
  entry <- rep(0, nrow(tabela))
  for(i in (vstop_s2+1):nrow(tabela)){
    if(cena[i,] > max(cena[(i-vstop_s2):(i-1),])){entry[i] <- 1}
    if(cena[i,] < min(cena[(i-vstop_s2):(i-1),])){entry[i] <- 2}
  }
  entry
}


#N so dobili izračunan na začetku tedna
spr_tedenski_N <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    t_N[i:(i+6)] <- tabela$spr_N[i]
  }
  t_N[1:nrow(tabela)]
}


##########
#System 1#
##########

#default = iz zmagovite long pozicije izstopimo ko pridemo do 10 dnevnega dna, iz short 10 dnevni vrh
#izstop_s1 = toliko dnevno dno oz. toliko dnevni vrh
spr_win_izstop_s1 <- function(tabela, entry, short_long, izstop_s1, cena){
  if(short_long == "long"){
    izstop <- rep(0, nrow(tabela))
    for(i in (izstop_s1+1):nrow(tabela)){
      if(cena[i,] < min(cena[(i-izstop_s1):(i-1),])){izstop[i] <- 1}
    }
    which(izstop==1)[which(izstop==1)>entry][1]
  }
  else{
    izstop <- rep(0, nrow(tabela))
    for(i in (izstop_s1+1):nrow(tabela)){
      if(cena[i,] > max(cena[(i-izstop_s1):(i-1),])){izstop[i] <- 1}
    }
    which(izstop==1)[which(izstop==1)>entry][1]
  }
}


spr_turtle_s1 <- function(tabela, zacetni_kapital, izstop_s1, cena, add, sl){
  kandidati_s1 <- which(tabela$entry_s1==1 | tabela$entry_s1==2)
  kandidati_s2 <- which(tabela$entry_s2==1 | tabela$entry_s2==2)
  vstop <- kandidati_s1[1]
  profit <- 0
  money <- zacetni_kapital
  while(nrow(tabela) - vstop > 0){
    cena_vstop <- cena[vstop,]
    st_btc <- unit(money, tabela$spr_tedenski_N[vstop])/cena_vstop
    
    if(tabela$entry_s1[vstop]==1 | tabela$entry_s2[vstop]==1){
      izstop <- ifelse(is.na(spr_win_izstop_s1(tabela, vstop, "long", izstop_s1, cena)), nrow(tabela), spr_win_izstop_s1(tabela, vstop, "long", izstop_s1, cena))
      cena_izstop <- cena[izstop,]
      st_enot <- 1
      stop_loss <- cena_vstop - sl*tabela$spr_tedenski_N[vstop]
      pol_N <- add*tabela$spr_tedenski_N[vstop]
      kdaj_dodali_enote <- c()
      cena_ko_dodamo <- cena_vstop
      st_btc_dodamo <- st_btc
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
            vstop <- ifelse(is.na(kandidati_s1[kandidati_s1 > vstop][1]), nrow(tabela), kandidati_s1[kandidati_s1 > vstop][1])
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (cena[i,] - cena_ko_dodamo[j])*st_btc_dodamo[j]
            }
            break}
        }
      }
    }
    else{
      izstop <- ifelse(is.na(spr_win_izstop_s1(tabela, vstop, "short", izstop_s1, cena)), nrow(tabela), spr_win_izstop_s1(tabela, vstop, "short", izstop_s1, cena))
      cena_izstop <- cena[izstop,]
      st_enot <- 1
      stop_loss <- cena_vstop + sl*tabela$spr_tedenski_N[vstop]
      pol_N <- add*tabela$spr_tedenski_N[vstop]
      kdaj_dodali_enote <- c()
      cena_ko_dodamo <- cena_vstop
      st_btc_dodamo <- st_btc
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
            vstop <- ifelse(is.na(kandidati_s1[kandidati_s1 > vstop][1]), nrow(tabela), kandidati_s1[kandidati_s1 > vstop][1])
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (cena_ko_dodamo[j] - cena[i,])*st_btc_dodamo[j]
            }
            break}
        }
      }
    }
    if(vstop == (izstop-1)){
      vstop <- vstop + 1
      for(j in 1:length(st_btc_dodamo)){
        profit <- profit + (abs(cena_izstop - cena_ko_dodamo[j]))*st_btc_dodamo[j]
      }
      vstop <- ifelse(is.na(kandidati_s2[kandidati_s2 > vstop][1]), nrow(tabela), kandidati_s2[kandidati_s2 > vstop][1])
    }
    #if(profit <= -100000){money <- 0.8*money}
  }
  profit
}


##########
#System 2#
##########

#default = iz zmagovite long pozicije izstopimo ko pridemo do 20 dnevnega dna, iz short 20 dnevni vrh
#izstop_s2 = toliko dnevno dno oz. toliko dnevni vrh
spr_win_izstop_s2 <- function(tabela, entry, short_long, izstop_s2, cena){
  if(short_long == "long"){
    izstop <- rep(0, nrow(tabela))
    for(i in (izstop_s2+1):nrow(tabela)){
      if(cena[i,] < min(cena[(i-izstop_s2):(i-1),])){izstop[i] <- 1}
    }
    which(izstop==1)[which(izstop==1)>entry][1]
  }
  else{
    izstop <- rep(0, nrow(tabela))
    for(i in (izstop_s2+1):nrow(tabela)){
      if(cena[i,] > max(cena[(i-izstop_s2):(i-1),])){izstop[i] <- 1}
    }
    which(izstop==1)[which(izstop==1)>entry][1]
  }
}


spr_turtle_s2 <- function(tabela, zacetni_kapital, izstop_s2, cena, add, sl){
  kandidati <- which(tabela$entry_s2==1 | tabela$entry_s2==2)
  vstop <- kandidati[1]
  profit <- 0
  money <- zacetni_kapital
  while(nrow(tabela) - vstop > 0){
    cena_vstop <- cena[vstop,]
    st_btc <- unit(money, tabela$spr_tedenski_N[vstop])/cena_vstop
    
    if(tabela$entry_s2[vstop]==1){
      izstop <- ifelse(is.na(spr_win_izstop_s2(tabela, vstop, "long", izstop_s2, cena)), nrow(tabela), spr_win_izstop_s2(tabela, vstop, "long", izstop_s2, cena))
      cena_izstop <- cena[izstop,]
      st_enot <- 1
      stop_loss <- cena_vstop - sl*tabela$spr_tedenski_N[vstop]
      pol_N <- add*tabela$spr_tedenski_N[vstop]
      kdaj_dodali_enote <- c()
      cena_ko_dodamo <- cena_vstop
      st_btc_dodamo <- st_btc
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
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (cena[i,] - cena_ko_dodamo[j])*st_btc_dodamo[j]
            }
            break}
        }
      }
    }
    else{
      izstop <- ifelse(is.na(spr_win_izstop_s2(tabela, vstop, "short", izstop_s2, cena)), nrow(tabela), spr_win_izstop_s2(tabela, vstop, "short", izstop_s2, cena))
      cena_izstop <- cena[izstop,]
      st_enot <- 1
      stop_loss <- cena_vstop + sl*tabela$spr_tedenski_N[vstop]
      pol_N <- add*tabela$spr_tedenski_N[vstop]
      kdaj_dodali_enote <- c()
      cena_ko_dodamo <- cena_vstop
      st_btc_dodamo <- st_btc
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
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (cena_ko_dodamo[j] - cena[i,])*st_btc_dodamo[j]
            }
            break}
        }
      }
    }
    if(vstop == (izstop-1)){
      vstop <- vstop + 1
      for(j in 1:length(st_btc_dodamo)){
        profit <- profit + (abs(cena_izstop - cena_ko_dodamo[j]))*st_btc_dodamo[j]
      }
      vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
    }
    #if(profit <= -100000){money <- 0.8*money}
  }
  profit
}

###########
# DOBIČKI #
###########

odlocitev_cena <- function(tabela, cena){
  if(cena == "Close"){cena1 <- data.frame(tabela$Close)}
  if(cena == "Low"){cena1 <- data.frame(tabela$Low)}
  if(cena == "High"){cena1 <- data.frame(tabela$High)}
  if(cena == "Open"){cena1 <- data.frame(tabela$Open)}
  cena1
}

izracun_za_analizo <- function(tabela, dnevi_N, vstop_s1, vstop_s2, cena){
  tabela <- tabela[-nrow(tabela),]
  tab <- spr_N(tabela, dnevi_N)
  cena1 <- odlocitev_cena(tab, cena)
  tab$entry_s1 <- spr_entry_s1(tab, vstop_s1, cena1)
  tab$entry_s2 <- spr_entry_s2(tab, vstop_s2, cena1)
  tab$spr_tedenski_N <- spr_tedenski_N(tab)
  tab <- tab[tab$spr_tedenski_N > 0,]
  tab
}



# Spremenljivke:
#   - dnevi_N: 10, 15, D = 20, 25, 30
#   - vstop_s1: 10, 15, D = 20, 25, 30
#   - vstop_s2: 45, 50, D = 55, 60, 65
#   - izstop_s1: 6, 8, D = 10, 12, 14
#   - izstop_s2: 16, 18, D = 20, 22, 24
#   - obdobje: koliko časa trgujemo to strategijo (če imamo tabelo z dnevnimi cenami, 360 pomeni 360 dni), D = 1800
#   - strategija: S1 = System 1, S2 = System 2
#   - cena: Open, Low, D = Close, High
#   - dodajanje enot: 1/4N, 1/2N, 3/4N, N
#   - stop-loss: N, 2N, 3N, 4N
#   - začetni kapital: 50.000 do 1.000.000 s korakom 50.000
spr_dobicki <- function(dnevi_N = 20, vstop_s1 = 20, vstop_s2 = 55, izstop_s1 = 10, izstop_s2 = 20, 
                        obdobje = 1800, zacetni_kapital = 1000000, cena = "Close", strategija, add = 1/2, sl = 2){
  tab <- izracun_za_analizo(btc_1day, dnevi_N, vstop_s1, vstop_s2, cena)
  dobicki <- c()
  if(strategija == "S1"){
    for(i in 1:(nrow(tab) - obdobje + 1)){
      tmp <- tab[i:(obdobje + i - 1),]
      cena2 <- odlocitev_cena(tmp, cena)
      dobicki <- c(dobicki, spr_turtle_s1(tmp, zacetni_kapital, izstop_s1, cena2, add, sl))
    }
  }
  else{
    for(i in 1:(nrow(tab) - obdobje + 1)){
      tmp <- tab[i:(obdobje + i - 1),]
      cena2 <- odlocitev_cena(tmp, cena)
      dobicki <- c(dobicki, spr_turtle_s2(tmp, zacetni_kapital, izstop_s2, cena2, add, sl))
    }
  }
  c(mean(dobicki), sd(dobicki))
}

# Matrika, ki prikazuje povprečne dobičke ali varianco glede na različne vstope in izstope iz pozicij
dobicki_vstop_izstop <- function(strategija){
  M <- matrix(0, 5, 5)
  N <- matrix(0, 5, 5)
  if(strategija == "S1"){
    zap_vstop <- seq(10, 30, 5)
    zap_izstop <- seq(6, 14, 2)
    for(i in zap_vstop){
      for(j in zap_izstop){
        tmp <- spr_dobicki(vstop_s1 = i, izstop_s1 = j, strategija = strategija)
        M[(i/5)-1, (j/2)-2] <- tmp[1]
        N[(i/5)-1, (j/2)-2] <- tmp[2]
      }
    }
    matrika <- data.frame(M, row.names = zap_vstop)
    matrika1 <- data.frame(N, row.names = zap_vstop)
  }
  else{
    zap_vstop <- seq(45, 65, 5)
    zap_izstop <- seq(16, 24, 2)
    for(i in zap_vstop){
      for(j in zap_izstop){
        tmp <- spr_dobicki(vstop_s2 = i, izstop_s2 = j, strategija = strategija)
        M[(i/5)-8, (j/2)-7] <- tmp[1]
        N[(i/5)-8, (j/2)-7] <- tmp[2]
      }
    }
    matrika <- data.frame(M, row.names = zap_vstop)
    matrika1 <- data.frame(N, row.names = zap_vstop)
  }
  data.frame(matrika, matrika1)
}

S1matrika_v_i <- dobicki_vstop_izstop("S1")
S1povprecje_dob_vstop_izstop <- S1matrika_v_i[,1:5]
colnames(S1povprecje_dob_vstop_izstop) <- seq(6, 14, 2)
S1sd_dob_vstop_izstop <- S1matrika_v_i[,6:10]
colnames(S1sd_dob_vstop_izstop) <- seq(6, 14, 2)

S2matrika_v_i <- dobicki_vstop_izstop("S2")
S2povprecje_dob_vstop_izstop <- S2matrika_v_i[,1:5]
colnames(S2povprecje_dob_vstop_izstop) <- seq(16, 24, 2)
S2sd_dob_vstop_izstop <- S2matrika_v_i[,6:10]
colnames(S2sd_dob_vstop_izstop) <- seq(16, 24, 2)


# matrika, ki prikazuje povprečne dobičke in varianco glede na različne poračune N-ja ali 
#glede na različen začetni kapital 
dobicki_spr_N_zk <- function(strategija, N_ali_zk){
  M1 <- matrix(0, 2, 5)
  M2 <- matrix(0, 2, 20)
  zap_N <- seq(10, 30, 5)
  zap_zk <- seq(50000, 1000000, 50000)
  if(N_ali_zk == "N"){
    for(j in zap_N){
        tmp <- spr_dobicki(dnevi_N = j, strategija = strategija)
        M1[1, (j/5)-1] <- tmp[1]
        M1[2, (j/5)-1] <- tmp[2]
    }
    matrika <- data.frame(M1, row.names = c("povprecje", "sd"))
    colnames(matrika) <- zap_N
  }
  else{
    for(i in zap_zk){
      tmp <- spr_dobicki(zacetni_kapital = i, strategija = strategija)
      M2[1, (i/50000)] <- tmp[1]
      M2[2, (i/50000)] <- tmp[2]
    }
    matrika <- data.frame(M2, row.names = c("povprecje", "sd"))
    colnames(matrika) <- zap_zk
  }
  matrika
}

# spremenljivka N
S1dobicki_N <- dobicki_spr_N_zk("S1", "N")
S2dobicki_N <- dobicki_spr_N_zk("S2", "N")

dodan_kolicnik <- function(tabela){
  kolicnik <- c()
  for(i in 1:ncol(tabela)){
    kolicnik <- c(kolicnik, round(tabela[1, i]/tabela[2, i], 1))
  }
  rbind(tabela, "kolicnik" = kolicnik)
}

S1dobicki_N_2 <- dodan_kolicnik(S1dobicki_N)
S2dobicki_N_2 <- dodan_kolicnik(S2dobicki_N)


# zacetni kapital
S1dobicki_zk <- dobicki_spr_N_zk("S1", "zk")
S2dobicki_zk <- dobicki_spr_N_zk("S2", "zk")

dodano_povecanje <- function(tabela){
  povecanje <- c(0)
  kolicnik <- c(round(tabela[1, 1]/tabela[2, 1], 1))
  for(i in 2:ncol(tabela)){
    povecanje <- c(povecanje, round(((tabela[1, i]/tabela[1, (i-1)])-1)*100, 2))
    kolicnik <- c(kolicnik, round(tabela[1, i]/tabela[2, i], 1))
  }
  rbind(tabela, "odstotki" = povecanje, "kolcnik" = kolicnik)
}

S1dobicki_zk_2 <- dodano_povecanje(S1dobicki_zk)
S2dobicki_zk_2 <- dodano_povecanje(S2dobicki_zk)


# Close, Open, Low, High cena
dobicki_cena <- function(strategija){
  M <- matrix(0, 2, 4)
  zap_cena <- c("Open", "Low", "High", "Close")
  for(j in 1:4){
    tmp <- spr_dobicki(cena = zap_cena[j], strategija = strategija)
    M[1, j] <- tmp[1]
    M[2, j] <- tmp[2]
  }
  matrika <- data.frame(M, row.names = c("povprecje", "sd"))
  colnames(matrika) <- zap_cena
  matrika
}

S1dobicki_cena <- dobicki_cena("S1")
S2dobicki_cena <- dobicki_cena("S2")

S1dobicki_cena_2 <- dodan_kolicnik(S1dobicki_cena)
S2dobicki_cena_2 <- dodan_kolicnik(S2dobicki_cena)


# dodajanje enot v pozicijo, stop - loss
dobicki_add_sl <- function(strategija){
  M <- matrix(0, 4, 4)
  N <- matrix(0, 4, 4)
  zap_add <- c(1/4, 1/2, 3/4, 1)
  zap_sl <- c(1, 2, 3, 4)
  for(i in 1:4){
    for(j in 1:4){
      tmp <- spr_dobicki(add = zap_add[i], sl = zap_sl[j], strategija = strategija)
      M[i, j] <- tmp[1]
      N[i, j] <- tmp[2]
    }
  }
  matrika <- data.frame(M,row.names = zap_add)
  matrika1 <- data.frame(N, row.names = zap_add)
  data.frame(matrika, matrika1)
}

S1matrika_add_sl <- dobicki_add_sl("S1")
S1dobicki_add_sl <- S1matrika_add_sl[,1:4]
colnames(S1dobicki_add_sl) <- c(1, 2, 3, 4)
S1sd_add_sl <- S1matrika_add_sl[,5:8]
colnames(S1sd_add_sl) <- c(1, 2, 3, 4)

S2matrika_add_sl <- dobicki_add_sl("S2")
S2dobicki_add_sl <- S2matrika_add_sl[,1:4]
colnames(S2dobicki_add_sl) <- c(1, 2, 3, 4)
S2sd_add_sl <- S2matrika_add_sl[,5:8]
colnames(S2sd_add_sl) <- c(1, 2, 3, 4)

#################################
# Compaunded annual growth rate #
#################################

cagr <- function(tabela, zacetni_kapital = 1000000, obdobje = 1800){
  tabela <- round(((tabela/zacetni_kapital+1)^(365/obdobje)-1)*100,2)
  tabela
}


################
# Lepše tabele #
################


spr <- function(tabela, name){
  if(name == "vi"){tmp <- cbind("vstop/izstop" = rownames(tabela), tabela)}
  if(name == "N"){tmp <- cbind("N" = rownames(tabela), tabela)}
  if(name == "zk"){tmp <- cbind("zk" = rownames(tabela), tabela)}
  if(name == "cena"){tmp <- cbind("cena" = rownames(tabela), tabela)}
  if(name == "add_sl"){tmp <- cbind("dod/stop_loss" = rownames(tabela), tabela)}
  tmp
}

library(officer)
flextabela_matrika <- function(tabela, cagr){
  n <- nrow(tabela)
  c <- ncol(tabela)
  big_border = fp_border(color="black", width = 2)
  tabela <- regulartable(tabela)
  tabela <- bg(tabela, bg = "coral", part = "header")
  tabela <- bg(tabela, bg = "cyan", part = "body")
  tabela <- bold(tabela, part = "header")
  tabela <- bold(tabela, j = 1, part = "body")
  tabela <- align(tabela, align = "center", part = "all")
  if(cagr == "nc"){tabela <- set_formatter_type(tabela, fmt_double = "%.01f")}
  else{tabela <- set_formatter_type(tabela, fmt_double = "%.02f")}
  tabela <- bg(tabela, bg = "coral", j = 1)
  tabela <- border_remove(tabela)
  tabela <- hline_top(tabela, border = big_border, part = "header")
  tabela <- hline_bottom(tabela, border = big_border, part = "body")
  tabela <- vline_left(tabela, border = big_border)
  tabela <- vline_right(tabela, border = big_border)
  tabela <- hline(tabela, i = 1, j = 2:c, border = big_border, part = "header")
  tabela <- vline(tabela, i = 1:n, j = 1, border = big_border, part = "body")
  tabela
}

flextabela_matrika(spr(S1povprecje_dob_vstop_izstop, "vi"), "nc")
flextabela_matrika(spr(S1sd_dob_vstop_izstop, "vi"),"nc")
flextabela_matrika(spr(S2povprecje_dob_vstop_izstop, "vi"),"nc")
flextabela_matrika(spr(S2sd_dob_vstop_izstop, "vi"),"nc")

flextabela_matrika(spr(cagr(S1povprecje_dob_vstop_izstop), "vi"), "cagr")
flextabela_matrika(spr(cagr(S2povprecje_dob_vstop_izstop), "vi"), "cagr")
flextabela_matrika(spr(S1povprecje_dob_vstop_izstop/S1sd_dob_vstop_izstop, "vi"), "cagr")
flextabela_matrika(spr(S2povprecje_dob_vstop_izstop/S2sd_dob_vstop_izstop, "vi"), "cagr")


flextabela_matrika(spr(S1dobicki_N_2, "N"), "nc")
flextabela_matrika(spr(S2dobicki_N_2, "N"), "nc")

flextabela_matrika(spr(S1dobicki_zk_2, "zk"), "nc")
flextabela_matrika(spr(S2dobicki_zk_2, "zk"), "nc")

flextabela_matrika(spr(S1dobicki_cena_2, "cena"), "nc")
flextabela_matrika(spr(S2dobicki_cena_2, "cena"), "nc")

flextabela_matrika(spr(S1dobicki_add_sl, "add_sl"), "nc")
flextabela_matrika(spr(S1sd_add_sl, "add_sl"),"nc")
flextabela_matrika(spr(S2dobicki_add_sl, "add_sl"),"nc")
flextabela_matrika(spr(S2sd_add_sl, "add_sl"),"nc")

flextabela_matrika(spr(cagr(S1dobicki_add_sl), "add_sl"), "cagr")
flextabela_matrika(spr(cagr(S2dobicki_add_sl), "add_sl"), "cagr")
flextabela_matrika(spr(S1dobicki_add_sl/S1sd_add_sl, "add_sl"), "cagr")
flextabela_matrika(spr(S2dobicki_add_sl/S2sd_add_sl, "add_sl"), "cagr")

#######################################
# Najboljši parametri - nova stragija #
#######################################


novi_dobicki <- function(dnevi_N = 10, vstop_s1 = 30, vstop_s2 = 45, izstop_s1 = 8, izstop_s2 = 24, 
                        obdobje = 1800, zacetni_kapital = 1000000, cena = "High", strategija, 
                        add = 0.25, sl = 4){
  tab <- izracun_za_analizo(btc_1day, dnevi_N, vstop_s1, vstop_s2, cena)
  dobicki <- c()
  if(strategija == "S1"){
    for(i in 1:(nrow(tab) - obdobje + 1)){
      tmp <- tab[i:(obdobje + i - 1),]
      cena2 <- odlocitev_cena(tmp, cena)
      dobicki <- c(dobicki, spr_turtle_s1(tmp, zacetni_kapital, izstop_s1, cena2, add, sl))
    }
  }
  else{
    for(i in 1:(nrow(tab) - obdobje + 1)){
      tmp <- tab[i:(obdobje + i - 1),]
      cena2 <- odlocitev_cena(tmp, cena)
      dobicki <- c(dobicki, spr_turtle_s2(tmp, zacetni_kapital, izstop_s2, cena2, add, sl))
    }
  }
  dobicki
}

novi_btc_dobicki_360_S1 <- novi_dobicki(obdobje = 360, strategija = "S1")/1000
novi_btc_dobicki_500_S1 <- novi_dobicki(obdobje = 500, strategija = "S1")/1000
novi_btc_dobicki_1000_S1 <- novi_dobicki(obdobje = 1000, strategija = "S1")/1000
novi_btc_dobicki_1800_S1 <- novi_dobicki(strategija = "S1")/1000

novi_btc_dobicki_360_S2 <- novi_dobicki(obdobje = 360, strategija = "S2")/1000
novi_btc_dobicki_500_S2 <- novi_dobicki(obdobje = 500, strategija = "S2")/1000
novi_btc_dobicki_1000_S2 <- novi_dobicki(obdobje = 1000, strategija = "S2")/1000
novi_btc_dobicki_1800_S2 <- novi_dobicki(strategija = "S2")/1000

novi_pregled_btc_S1 <- pregled_dobicki(novi_btc_dobicki_360_S1, novi_btc_dobicki_500_S1, novi_btc_dobicki_1000_S1, 
                                       novi_btc_dobicki_1800_S1, 1000000, "S1")
novi_pregled_btc_S2 <- pregled_dobicki(novi_btc_dobicki_360_S2, novi_btc_dobicki_500_S2, novi_btc_dobicki_1000_S2, 
                                       novi_btc_dobicki_1800_S2, 1000000, "S2")

flextabela_pregled(novi_pregled_btc_S1, 0)
flextabela_pregled(novi_pregled_btc_S2, 0)

# pregled_cagr je funkcija iz turtle.R
flextabela_pregled(pregled_cagr(novi_pregled_btc_S1, strategija = "S1"), 3)
flextabela_pregled(pregled_cagr(novi_pregled_btc_S2, strategija = "S2"), 3)


novi_pred_po_2014 <- pred_po_2014(novi_btc_dobicki_360_S1, novi_btc_dobicki_500_S1, 
                                  novi_btc_dobicki_1000_S1, novi_btc_dobicki_1800_S1, 700)
flextabela_pregled(novi_pred_po_2014, 0)


novi_sd_pred_po_2014 <- sd_pred_po_2014(novi_btc_dobicki_360_S1, novi_btc_dobicki_500_S1, 
                                        novi_btc_dobicki_1000_S1, novi_btc_dobicki_1800_S1, 700)
flextabela_pregled(novi_sd_pred_po_2014, 1)
