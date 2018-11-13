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
  prvi_n <- mean(tabela$TR[1:dnevi])
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
spr_entry_s1 <- function(tabela, vstop_s1){
  entry <- rep(0, nrow(tabela))
  for(i in (vstop_s1+1):nrow(tabela)){
    if(tabela$Close[i] > max(tabela$Close[(i-vstop_s1):(i-1)])){entry[i] <- 1}
    if(tabela$Close[i] < min(tabela$Close[(i-vstop_s1):(i-1)])){entry[i] <- 2}
  }
  entry
}

#System 2
#vstop_s2 = toliko dnevni višek ali toliko dnevno dno (default = 55)
spr_entry_s2 <- function(tabela, vstop_s2){
  entry <- rep(0, nrow(tabela))
  for(i in (vstop_s2+1):nrow(tabela)){
    if(tabela$Close[i] > max(tabela$Close[(i-vstop_s2):(i-1)])){entry[i] <- 1}
    if(tabela$Close[i] < min(tabela$Close[(i-vstop_s2):(i-1)])){entry[i] <- 2}
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
spr_win_izstop_s1 <- function(tabela, entry, short_long, izstop_s1){
  if(short_long == "long"){
    izstop <- rep(0, nrow(tabela))
    for(i in (izstop_s1+1):nrow(tabela)){
      if(tabela$Close[i] < min(tabela$Close[(i-izstop_s1):(i-1)])){izstop[i] <- 1}
    }
    which(izstop==1)[which(izstop==1)>entry][1]
  }
  else{
    izstop <- rep(0, nrow(tabela))
    for(i in (izstop_s1+1):nrow(tabela)){
      if(tabela$Close[i] > max(tabela$Close[(i-izstop_s1):(i-1)])){izstop[i] <- 1}
    }
    which(izstop==1)[which(izstop==1)>entry][1]
  }
}

spr_turtle_s1 <- function(tabela, zacetni_kapital, izstop_s1){
  kandidati_s1 <- which(tabela$entry_s1==1 | tabela$entry_s1==2)
  kandidati_s2 <- which(tabela$entry_s2==1 | tabela$entry_s2==2)
  vstop <- kandidati_s1[1]
  profit <- 0
  money <- zacetni_kapital
  while(nrow(tabela) - vstop > 0){
    cena_vstop <- tabela$Close[vstop]
    st_btc <- unit(money, tabela$spr_tedenski_N[vstop])/cena_vstop
    
    if(tabela$entry_s1[vstop]==1 | tabela$entry_s2[vstop]==1){
      izstop <- ifelse(is.na(spr_win_izstop_s1(tabela, vstop, "long", izstop_s1)), nrow(tabela), spr_win_izstop_s1(tabela, vstop, "long", izstop_s1))
      cena_izstop <- tabela$Close[izstop]
      st_enot <- 1
      stop_loss <- cena_vstop - 2*tabela$spr_tedenski_N[vstop]
      pol_N <- (1/2)*tabela$spr_tedenski_N[vstop]
      kdaj_dodali_enote <- c()
      cena_ko_dodamo <- cena_vstop
      st_btc_dodamo <- st_btc
      for(i in (vstop+1):(izstop-1)){
        vstop <- i
        if((tabela$Close[i] > cena_vstop + st_enot*pol_N) & (st_enot < 4)){
          razlika <- tabela$Close[i] - cena_ko_dodamo[length(cena_ko_dodamo)]
          dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
          if(st_enot + dodamo <= 4){
            st_enot <- st_enot + dodamo
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss + pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
          else{
            dodamo <- ifelse(st_enot==2, 2, 1)
            st_enot <- st_enot + dodamo
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss + pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
        }
        else{
          if(tabela$Close[i] <= stop_loss){
            vstop <- ifelse(is.na(kandidati_s1[kandidati_s1 > vstop][1]), nrow(tabela), kandidati_s1[kandidati_s1 > vstop][1])
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (tabela$Close[i] - cena_ko_dodamo[j])*st_btc_dodamo[j]
            }
            break}
        }
      }
    }
    else{
      izstop <- ifelse(is.na(spr_win_izstop_s1(tabela, vstop, "short", izstop_s1)), nrow(tabela), spr_win_izstop_s1(tabela, vstop, "short", izstop_s1))
      cena_izstop <- tabela$Close[izstop]
      st_enot <- 1
      stop_loss <- cena_vstop + 2*tabela$spr_tedenski_N[vstop]
      pol_N <- (1/2)*tabela$spr_tedenski_N[vstop]
      kdaj_dodali_enote <- c()
      cena_ko_dodamo <- cena_vstop
      st_btc_dodamo <- st_btc
      for(i in (vstop+1):(izstop-1)){
        vstop <- i
        if((tabela$Close[i] < cena_vstop - st_enot*pol_N) & (st_enot < 4)){
          razlika <- abs(tabela$Close[i] - cena_ko_dodamo[length(cena_ko_dodamo)])
          dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
          if(st_enot + dodamo <= 4){
            st_enot <- st_enot + dodamo
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss - pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
          else{
            dodamo <- ifelse(st_enot==2, 2, 1)
            st_enot <- st_enot + dodamo
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss - pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
        }
        else{
          if(tabela$Close[i] >= stop_loss){
            vstop <- ifelse(is.na(kandidati_s1[kandidati_s1 > vstop][1]), nrow(tabela), kandidati_s1[kandidati_s1 > vstop][1])
            for(i in 1:length(st_btc_dodamo)){
              profit <- profit + (cena_ko_dodamo[j] - tabela$Close[i])*st_btc_dodamo[j]
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
spr_win_izstop_s2 <- function(tabela, entry, short_long, izstop_s2){
  if(short_long == "long"){
    izstop <- rep(0, nrow(tabela))
    for(i in (izstop_s2+1):nrow(tabela)){
      if(tabela$Close[i] < min(tabela$Close[(i-izstop_s2):(i-1)])){izstop[i] <- 1}
    }
    which(izstop==1)[which(izstop==1)>entry][1]
  }
  else{
    izstop <- rep(0, nrow(tabela))
    for(i in (izstop_s2+1):nrow(tabela)){
      if(tabela$Close[i] > max(tabela$Close[(i-izstop_s2):(i-1)])){izstop[i] <- 1}
    }
    which(izstop==1)[which(izstop==1)>entry][1]
  }
}


spr_turtle_s2 <- function(tabela, zacetni_kapital, izstop_s2){
  kandidati <- which(tabela$entry_s2==1 | tabela$entry_s2==2)
  vstop <- kandidati[1]
  profit <- 0
  money <- zacetni_kapital
  while(nrow(tabela) - vstop > 0){
    cena_vstop <- tabela$Close[vstop]
    st_btc <- unit(money, tabela$spr_tedenski_N[vstop])/cena_vstop
    
    if(tabela$entry_s2[vstop]==1){
      izstop <- ifelse(is.na(spr_win_izstop_s2(tabela, vstop, "long", izstop_s2)), nrow(tabela), spr_win_izstop_s2(tabela, vstop, "long", izstop_s2))
      cena_izstop <- tabela$Close[izstop]
      st_enot <- 1
      stop_loss <- cena_vstop - 2*tabela$spr_tedenski_N[vstop]
      pol_N <- (1/2)*tabela$spr_tedenski_N[vstop]
      kdaj_dodali_enote <- c()
      cena_ko_dodamo <- cena_vstop
      st_btc_dodamo <- st_btc
      for(i in (vstop+1):(izstop-1)){
        vstop <- i
        if((tabela$Close[i] > cena_vstop + st_enot*pol_N) & (st_enot < 4)){
          razlika <- tabela$Close[i] - cena_ko_dodamo[length(cena_ko_dodamo)]
          dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
          if(st_enot + dodamo <= 4){
            st_enot <- st_enot + dodamo
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss + pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
          else{
            dodamo <- ifelse(st_enot==2, 2, 1)
            st_enot <- st_enot + dodamo
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss + pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
        }
        else{
          if(tabela$Close[i] <= stop_loss){
            vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (tabela$Close[i] - cena_ko_dodamo[j])*st_btc_dodamo[j]
            }
            break}
        }
      }
    }
    else{
      izstop <- ifelse(is.na(spr_win_izstop_s2(tabela, vstop, "short", izstop_s2)), nrow(tabela), spr_win_izstop_s2(tabela, vstop, "short", izstop_s2))
      cena_izstop <- tabela$Close[izstop]
      st_enot <- 1
      stop_loss <- cena_vstop + 2*tabela$spr_tedenski_N[vstop]
      pol_N <- (1/2)*tabela$spr_tedenski_N[vstop]
      kdaj_dodali_enote <- c()
      cena_ko_dodamo <- cena_vstop
      st_btc_dodamo <- st_btc
      for(i in (vstop+1):(izstop-1)){
        vstop <- i
        if((tabela$Close[i] < cena_vstop - st_enot*pol_N) & (st_enot < 4)){
          razlika <- abs(tabela$Close[i] - cena_ko_dodamo[length(cena_ko_dodamo)])
          dodamo <- min(3, ifelse(floor(razlika/pol_N)==0, 1, floor(razlika/pol_N)))
          if(st_enot + dodamo <= 4){
            st_enot <- st_enot + dodamo
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss - pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
          else{
            dodamo <- ifelse(st_enot==2, 2, 1)
            st_enot <- st_enot + dodamo
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$spr_tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss - pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
        }
        else{
          if(tabela$Close[i] >= stop_loss){
            vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (cena_ko_dodamo[j] - tabela$Close[i])*st_btc_dodamo[j]
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


izracun_za_analizo <- function(tabela, dnevi_N, vstop_s1, vstop_s2){
  tabela <- tabela[-nrow(tabela),]
  tab <- spr_N(tabela, dnevi_N)
  tab$entry_s1 <- spr_entry_s1(tab, vstop_s1)
  tab$entry_s2 <- spr_entry_s2(tab, vstop_s2)
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
spr_dobicki <- function(dnevi_N = 20, vstop_s1 = 20, vstop_s2 = 55, izstop_s1 = 10, izstop_s2 = 20, obdobje = 1800, zacetni_kapital = 1000000, strategija){
  tab <- izracun_za_analizo(btc_1day, dnevi_N, vstop_s1, vstop_s2)
  dobicki <- c()
  if(strategija == "S1"){
    for(i in 1:(nrow(tab) - obdobje + 1)){
      dobicki <- c(dobicki, spr_turtle_s1(tab[i:(obdobje + i - 1),], zacetni_kapital, izstop_s1))
    }
  }
  else{
    for(i in 1:(nrow(tab) - obdobje + 1)){
      dobicki <- c(dobicki, spr_turtle_s2(tab[i:(obdobje + i - 1),], zacetni_kapital, izstop_s2))
    }
  }
  c(mean(dobicki), sd(dobicki))
}

# Matrika, ki prikazuje povprečne dobičke ali varianco glede na različne vstope in izstope iz pozicij
dobicki_vstop_izstop <- function(strategija){
  # kodo zoptimiziraj tako, da vrne obe tabeli hkrati z stolpcem umes praznim (da ne rabi dvakrat istega računat)
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
#glede na različan začetni kapital 
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

S1dobicki_N <- dobicki_spr_N_zk("S1", "N")
S2dobicki_N <- dobicki_spr_N_zk("S2", "N")

S1dobicki_zk <- dobicki_spr_N_zk("S1", "zk")
S2dobicki_zk <- dobicki_spr_N_zk("S2", "zk")

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
  ifelse(name == "vi", tmp <- cbind("vstop/izstop" = rownames(tabela), tabela), 
         ifelse(name == "N", tmp <- cbind("N" = rownames(tabela), tabela), tmp <- cbind("zk" = rownames(tabela), tabela)))
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
  if(cagr == "nc"){tabela <- set_formatter_type(tabela, fmt_double = "%.00f")}
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

flextabela_matrika(spr(S1dobicki_N, "N"), "nc")
flextabela_matrika(spr(S2dobicki_N, "N"), "nc")

flextabela_matrika(spr(S1dobicki_zk, "zk"), "nc")
flextabela_matrika(spr(S2dobicki_zk, "zk"), "nc")
