library(ggplot2)

#izračuna True Range in N
izracun_N <- function(tabela){
  tr <- c()
  for(i in 2:nrow(tabela)){
    h <- tabela$High[i]
    l <- tabela$Low[i]
    pdc <- tabela$Close[i-1]
    tr <- append(tr, max(h-l, h-pdc, pdc-l))
  }
  tabela$TR <- c(0,tr)
  tabela <- tabela[-1,]
  prvi_n <- mean(tabela$TR[1:20])
  n <- c(rep(0,19), prvi_n, rep(0, nrow(tabela)-20))
  for(i in 21:nrow(tabela)){
    n[i] <- (19*n[i-1] + tabela$TR[i])/20
  }
  tabela$N <- n
  tabela
}

unit <- function(account_size, N){
  floor((0.01*account_size)/N)
}

#vstopne točke: 1=go long, 2=go short
#System 1
entry_s1 <- function(tabela){
  entry <- rep(0, nrow(tabela))
  for(i in 21:nrow(tabela)){
    if(tabela$Close[i] > max(tabela$Close[(i-20):(i-1)])){entry[i] <- 1}
    if(tabela$Close[i] < min(tabela$Close[(i-20):(i-1)])){entry[i] <- 2}
  }
  entry
}

#System 2
entry_s2 <- function(tabela){
  entry <- rep(0, nrow(tabela))
  for(i in 56:nrow(tabela)){
    if(tabela$Close[i] > max(tabela$Close[(i-55):(i-1)])){entry[i] <- 1}
    if(tabela$Close[i] < min(tabela$Close[(i-55):(i-1)])){entry[i] <- 2}
  }
  entry
}

#N so dobili izračunan na začetku tedna
tedenski_N <- function(tabela){
  t_N <- c()
  for(i in seq(1,nrow(tabela),7)){
    t_N[i:(i+6)] <- tabela$N[i]
  }
  t_N[1:nrow(tabela)]
}

##########
#System 1#
##########

#iz zmagovite long pozicije izstopimo ko pridemo do 10 dnevnega dna, iz short 10 dnevni vrh
win_izstop_20 <- function(tabela, entry, short_long){
  if(short_long == "long"){
    izstop <- rep(0, nrow(tabela))
    for(i in 11:nrow(tabela)){
      if(tabela$Close[i] < min(tabela$Close[(i-10):(i-1)])){izstop[i] <- 1}
    }
    which(izstop==1)[which(izstop==1)>entry][1]
  }
  else{
    izstop <- rep(0, nrow(tabela))
    for(i in 11:nrow(tabela)){
      if(tabela$Close[i] > max(tabela$Close[(i-10):(i-1)])){izstop[i] <- 1}
    }
    which(izstop==1)[which(izstop==1)>entry][1]
  }
}

turtle_s1_spx <- function(tabela, zacetni_kapital=1000000){
  kandidati_20 <- which(tabela$entry20==1 | tabela$entry20==2)
  kandidati_55 <- which(tabela$entry55==1 | tabela$entry55==2)
  vstop <- kandidati_20[1]
  profit <- 0
  profit1 <- c()
  kdaj_profit <- c()
  money <- zacetni_kapital
  while(nrow(tabela) - vstop > 0){
    cena_vstop <- tabela$Close[vstop]
    st_btc <- unit(money, tabela$tedenski_N[vstop])/cena_vstop
    
    if(tabela$entry20[vstop]==1 | tabela$entry55[vstop]==1){
      izstop <- ifelse(is.na(win_izstop_20(tabela, vstop, "long")), nrow(tabela), win_izstop_20(tabela, vstop, "long"))
      cena_izstop <- tabela$Close[izstop]
      st_enot <- 1
      stop_loss <- cena_vstop - 2*tabela$tedenski_N[vstop]
      pol_N <- (1/2)*tabela$tedenski_N[vstop]
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
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss + pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
          else{
            dodamo <- ifelse(st_enot==2, 2, 1)
            st_enot <- st_enot + dodamo
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss + pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
        }
        else{
          if(tabela$Close[i] <= stop_loss){
            vstop <- ifelse(is.na(kandidati_20[kandidati_20 > vstop][1]), nrow(tabela), kandidati_20[kandidati_20 > vstop][1])
            kdaj_profit <- c(kdaj_profit, i)
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (tabela$Close[i] - cena_ko_dodamo[j])*st_btc_dodamo[j]
            }
            profit1 <- c(profit1, profit)
            break}
        }
      }
    }
    else{
      izstop <- ifelse(is.na(win_izstop_20(tabela, vstop, "short")), nrow(tabela), win_izstop_20(tabela, vstop, "short"))
      cena_izstop <- tabela$Close[izstop]
      st_enot <- 1
      stop_loss <- cena_vstop + 2*tabela$tedenski_N[vstop]
      pol_N <- (1/2)*tabela$tedenski_N[vstop]
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
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss - pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
          else{
            dodamo <- ifelse(st_enot==2, 2, 1)
            st_enot <- st_enot + dodamo
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss - pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
        }
        else{
          if(tabela$Close[i] >= stop_loss){
            vstop <- ifelse(is.na(kandidati_20[kandidati_20 > vstop][1]), nrow(tabela), kandidati_20[kandidati_20 > vstop][1])
            kdaj_profit <- c(kdaj_profit, i)
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (cena_ko_dodamo[j] - tabela$Close[i])*st_btc_dodamo[j]
            }
            profit1 <- c(profit1, profit)
            break}
        }
      }
    }
    if(vstop == (izstop-1)){
      vstop <- vstop + 1
      for(j in 1:length(st_btc_dodamo)){
        profit <- profit + (abs(cena_izstop - cena_ko_dodamo[j]))*st_btc_dodamo[j]
      }
      profit1 <- c(profit1, profit)
      kdaj_profit <- c(kdaj_profit, izstop)
      vstop <- ifelse(is.na(kandidati_55[kandidati_55 > vstop][1]), nrow(tabela), kandidati_55[kandidati_55 > vstop][1])
    }
    #if(profit <= -100000){money <- 0.8*money}
  }
  data.frame(Profit = profit1, kdaj = kdaj_profit)
}



##########
#System 2#
##########

#iz zmagovite long pozicije izstopimo ko pridemo do 20 dnevnega dna, iz short 20 dnevni vrh
win_izstop_55 <- function(tabela, entry, short_long){
  if(short_long == "long"){
    izstop <- rep(0, nrow(tabela))
    for(i in 21:nrow(tabela)){
      if(tabela$Close[i] < min(tabela$Close[(i-20):(i-1)])){izstop[i] <- 1}
    }
    which(izstop==1)[which(izstop==1)>entry][1]
  }
  else{
    izstop <- rep(0, nrow(tabela))
    for(i in 21:nrow(tabela)){
      if(tabela$Close[i] > max(tabela$Close[(i-20):(i-1)])){izstop[i] <- 1}
    }
    which(izstop==1)[which(izstop==1)>entry][1]
  }
}


turtle_s2_spx <- function(tabela, zacetni_kapital=1000000){
  kandidati <- which(tabela$entry55==1 | tabela$entry55==2)
  vstop <- kandidati[1]
  profit <- 0
  profit1 <- c()
  kdaj_profit <- c()
  money <- zacetni_kapital
  while(nrow(tabela) - vstop > 0){
    cena_vstop <- tabela$Close[vstop]
    st_btc <- unit(money, tabela$tedenski_N[vstop])/cena_vstop
    
    if(tabela$entry55[vstop]==1){
      izstop <- ifelse(is.na(win_izstop_55(tabela, vstop, "long")), nrow(tabela), win_izstop_55(tabela, vstop, "long"))
      cena_izstop <- tabela$Close[izstop]
      st_enot <- 1
      stop_loss <- cena_vstop - 2*tabela$tedenski_N[vstop]
      pol_N <- (1/2)*tabela$tedenski_N[vstop]
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
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss + pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
          else{
            dodamo <- ifelse(st_enot==2, 2, 1)
            st_enot <- st_enot + dodamo
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss + pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
        }
        else{
          if(tabela$Close[i] <= stop_loss){
            vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
            kdaj_profit <- c(kdaj_profit, i)
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (tabela$Close[i] - cena_ko_dodamo[j])*st_btc_dodamo[j]
            }
            profit1 <- c(profit1, profit)
            break}
        }
      }
    }
    else{
      izstop <- ifelse(is.na(win_izstop_55(tabela, vstop, "short")), nrow(tabela), win_izstop_55(tabela, vstop, "short"))
      cena_izstop <- tabela$Close[izstop]
      st_enot <- 1
      stop_loss <- cena_vstop + 2*tabela$tedenski_N[vstop]
      pol_N <- (1/2)*tabela$tedenski_N[vstop]
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
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss - pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
          else{
            dodamo <- ifelse(st_enot==2, 2, 1)
            st_enot <- st_enot + dodamo
            st_btc_dodamo <- c(st_btc_dodamo, (unit(money, tabela$tedenski_N[i])/tabela$Close[i])*dodamo)
            stop_loss <- stop_loss - pol_N*dodamo
            kdaj_dodali_enote <- c(kdaj_dodali_enote, i)
            cena_ko_dodamo <- c(cena_ko_dodamo, tabela$Close[i])
          }
        }
        else{
          if(tabela$Close[i] >= stop_loss){
            vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
            kdaj_profit <- c(kdaj_profit, i)
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (cena_ko_dodamo[i] - tabela$Close[j])*st_btc_dodamo[j]
            }
            profit1 <- c(profit1, profit)
            break}
        }
      }
    }
    if(vstop == (izstop-1)){
      vstop <- vstop + 1
      for(j in 1:length(st_btc_dodamo)){
        profit <- profit + (abs(cena_izstop - cena_ko_dodamo[j]))*st_btc_dodamo[j]
      }
      profit1 <- c(profit1, profit)
      vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
      kdaj_profit <- c(kdaj_profit, izstop)
    }
    #if(profit <= -100000){money <- 0.8*money}
  }
  data.frame(Profit = profit1, kdaj = kdaj_profit)
}


# primerjava gibanja dobička spx in btc (oba instrumenta trgujemo enako časa (od 1.1.2012 do 27.6.2018))
profit_spx <- function(tabela, strategija){
  tabela <- tabela[-nrow(tabela),]
  tab <- izracun_N(tabela)
  tab$entry20 <- entry_s1(tab)
  tab$entry55 <- entry_s2(tab)
  tab$tedenski_N <- tedenski_N(tab)
  tab <- tab[tab$tedenski_N > 0,]
  if(strategija == "S1"){
    tmp <- turtle_s1_spx(tab)
  }
  else{
    tmp <- turtle_s2_spx(tab)
  }
  tmp 
}

profit_btc <- function(tabela1, strategija){
  tabela1 <- tabela1[-nrow(tabela1),]
  tab1 <- izracun_N(tabela1)
  tab1$entry20 <- entry_s1(tab1)
  tab1$entry55 <- entry_s2(tab1)
  tab1$tedenski_N <- tedenski_N(tab1)
  tab1 <- tab1[tab1$tedenski_N > 0,]
  if(strategija == "S1"){
    tmp1 <- turtle_s1_spx(tab1)
  }
  else{
    tmp1 <- turtle_s2_spx(tab1)
  }
  tmp1
}

za_graf_spx <- profit_spx(spx_1day, "S1")
za_graf_spx$Profit <- za_graf_spx$Profit/1000
za_graf_btc <- profit_btc(btc_1day, "S1")
za_graf_btc$Profit <- za_graf_btc$Profit/1000

ggplot(btc_1day)+
  geom_line(data = za_graf_btc, size = 0.8, 
            aes(x = btc_1day$Timestamp[za_graf_btc$kdaj], y = za_graf_btc$Profit, color = "btc"))+
  geom_line(data = za_graf_spx, size = 0.8, 
            aes(x = spx_1day$Date[za_graf_spx$kdaj], y = za_graf_spx$Profit, color = "spx"))+
  scale_color_manual(name = "", values = c("btc" = "blue", "spx" = "red"))+
  ylab("Dobiček v 1000")+
  xlab("Čas")+
  theme_minimal()+
  geom_text(data = za_graf_btc, aes(x = btc_1day$Timestamp[za_graf_btc$kdaj[length(za_graf_btc$kdaj)]], 
                                    y = Profit[length(Profit)],
                                    label = round(Profit[length(Profit)],2)), 
            check_overlap = TRUE, nudge_x = - 15000000, nudge_y = 40, size = 2.5)+
  geom_text(data = za_graf_spx, aes(x = spx_1day$Date[za_graf_spx$kdaj[length(za_graf_spx$kdaj)]], 
                                    y = Profit[length(Profit)],
                                    label = round(Profit[length(Profit)],2)), 
            check_overlap = TRUE, nudge_x = - 15000000, nudge_y = 40, size = 2.5)+
  theme(legend.position = "top", legend.box.margin = margin(0, 0, -0.5, 0, "cm"))
