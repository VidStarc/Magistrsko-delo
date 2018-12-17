library(ggplot2)

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

unit <- function(account_size, N){
  floor((0.01*account_size)/N)
}

#vstopne točke: 1=go long, 2=go short
#System 1
spr_entry_s1 <- function(tabela, vstop_s1, cena){
  entry <- rep(0, nrow(tabela))
  for(i in (vstop_s1+1):nrow(tabela)){
    if(cena[i,] > max(cena[(i-vstop_s1):(i-1),])){entry[i] <- 1}
    if(cena[i,] < min(cena[(i-vstop_s1):(i-1),])){entry[i] <- 2}
  }
  entry
}

#System 2
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

#iz zmagovite long pozicije izstopimo ko pridemo do 10 dnevnega dna, iz short 10 dnevni vrh
spr_win_izstop_s1_2 <- function(tabela, short_long, izstop_s1, cena){
  if(short_long == "long"){
    izstop <- rep(0, nrow(tabela))
    for(i in (izstop_s1+1):nrow(tabela)){
      if(cena[i,] < min(cena[(i-izstop_s1):(i-1),])){izstop[i] <- 1}
    }
    kdaj_izstop <- which(izstop==1)#[which(izstop==1)>entry][1]
  }
  else{
    izstop <- rep(0, nrow(tabela))
    for(i in (izstop_s1+1):nrow(tabela)){
      if(cena[i,] > max(cena[(i-izstop_s1):(i-1),])){izstop[i] <- 1}
    }
    kdaj_izstop <- which(izstop==1)#[which(izstop==1)>entry][1]
  }
  kdaj_izstop
}

turtle_s1_spx <- function(tabela, zacetni_kapital, izstop_s1, cena, add, sl){
  kandidati_s1 <- which(tabela$entry_s1==1 | tabela$entry_s1==2)
  kandidati_s2 <- which(tabela$entry_s2==1 | tabela$entry_s2==2)
  vstop <- kandidati_s1[1]
  if(is.na(vstop)){profit <- 0}
  else{
  kdaj_vstopali <- c()
  kdaj_dodali_enote <- c()
  profit <- 0
  profit1 <- c()
  kdaj_profit <- c()
  money <- zacetni_kapital
  kdaj_izstop_long <- spr_win_izstop_s1_2(tabela, "long", izstop_s1, cena)
  kdaj_izstop_short <- spr_win_izstop_s1_2(tabela, "short", izstop_s1, cena)
  while(nrow(tabela) - vstop > 0){
    kdaj_vstopali <- c(kdaj_vstopali, vstop)
    cena_vstop <- cena[vstop,]
    st_btc <- unit(money, tabela$spr_tedenski_N[vstop])/cena_vstop
    
    if(tabela$entry_s1[vstop]==1 | tabela$entry_s2[vstop]==1){
      izstop <- ifelse(is.na(kdaj_izstop_long[kdaj_izstop_long > vstop][1]), nrow(tabela), kdaj_izstop_long[kdaj_izstop_long > vstop][1])
      cena_izstop <- cena[izstop,]
      st_enot <- 1
      stop_loss <- cena_vstop - sl*tabela$spr_tedenski_N[vstop]
      pol_N <- add*tabela$spr_tedenski_N[vstop]
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
            kdaj_profit <- c(kdaj_profit, i)
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (cena[i,] - cena_ko_dodamo[j])*st_btc_dodamo[j]
            }
            profit1 <- c(profit1, profit)
            break}
        }
      }
    }
    else{
      izstop <- ifelse(is.na(kdaj_izstop_short[kdaj_izstop_short > vstop][1]), nrow(tabela), kdaj_izstop_short[kdaj_izstop_short > vstop][1])
      cena_izstop <- cena[izstop,]
      st_enot <- 1
      stop_loss <- cena_vstop + sl*tabela$spr_tedenski_N[vstop]
      pol_N <- add*tabela$spr_tedenski_N[vstop]
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
            kdaj_profit <- c(kdaj_profit, i)
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (cena_ko_dodamo[j] - cena[i,])*st_btc_dodamo[j]
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
      vstop <- ifelse(is.na(kandidati_s2[kandidati_s2 > vstop][1]), nrow(tabela), kandidati_s2[kandidati_s2 > vstop][1])
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



##########
#System 2#
##########

#iz zmagovite long pozicije izstopimo ko pridemo do 20 dnevnega dna, iz short 20 dnevni vrh
spr_win_izstop_s2_2 <- function(tabela, short_long, izstop_s2, cena){
  if(short_long == "long"){
    izstop <- rep(0, nrow(tabela))
    for(i in (izstop_s2+1):nrow(tabela)){
      if(cena[i,] < min(cena[(i-izstop_s2):(i-1),])){izstop[i] <- 1}
    }
    kdaj_izstop <- which(izstop==1)#[which(izstop==1)>entry][1]
  }
  else{
    izstop <- rep(0, nrow(tabela))
    for(i in (izstop_s2+1):nrow(tabela)){
      if(cena[i,] > max(cena[(i-izstop_s2):(i-1),])){izstop[i] <- 1}
    }
    kdaj_izstop <- which(izstop==1)#[which(izstop==1)>entry][1]
  }
  kdaj_izstop
}


turtle_s2_spx <- function(tabela, zacetni_kapital, izstop_s2, cena, add, sl){
  kandidati <- which(tabela$entry_s2==1 | tabela$entry_s2==2)
  vstop <- kandidati[1]
  if(is.na(vstop)){profit <- 0}
  else{
  kdaj_vstopali <- c()
  kdaj_dodali_enote <- c()
  profit <- 0
  profit1 <- c()
  kdaj_profit <- c()
  money <- zacetni_kapital
  kdaj_izstop_long <- spr_win_izstop_s2_2(tabela, "long", izstop_s1, cena)
  kdaj_izstop_short <- spr_win_izstop_s2_2(tabela, "short", izstop_s1, cena)
  while(nrow(tabela) - vstop > 0){
    kdaj_vstopali <- c(kdaj_vstopali, vstop)
    cena_vstop <- cena[vstop,]
    st_btc <- unit(money, tabela$spr_tedenski_N[vstop])/cena_vstop
    
    if(tabela$entry_s2[vstop]==1){
      izstop <- ifelse(is.na(kdaj_izstop_long[kdaj_izstop_long > vstop][1]), nrow(tabela), kdaj_izstop_long[kdaj_izstop_long > vstop][1])
      cena_izstop <- cena[izstop,]
      st_enot <- 1
      stop_loss <- cena_vstop - sl*tabela$spr_tedenski_N[vstop]
      pol_N <- add*tabela$spr_tedenski_N[vstop]
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
            kdaj_profit <- c(kdaj_profit, i)
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (cena[i,] - cena_ko_dodamo[j])*st_btc_dodamo[j]
            }
            profit1 <- c(profit1, profit)
            break}
        }
      }
    }
    else{
      izstop <- ifelse(is.na(kdaj_izstop_short[kdaj_izstop_short > vstop][1]), nrow(tabela), kdaj_izstop_short[kdaj_izstop_short > vstop][1])
      cena_izstop <- cena[izstop,]
      st_enot <- 1
      stop_loss <- cena_vstop + sl*tabela$spr_tedenski_N[vstop]
      pol_N <- add*tabela$spr_tedenski_N[vstop]
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
            kdaj_profit <- c(kdaj_profit, i)
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (cena_ko_dodamo[j] - cena[i,])*st_btc_dodamo[j]
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
  profit
  #data.frame(Profit = profit1, kdaj = kdaj_profit)
  #tab_vstop <- data.frame("kdaj" = kdaj_vstopali, "kaj" = rep(1, length(kdaj_vstopali)))
  #tab_dodajali <- data.frame("kdaj" = kdaj_dodali_enote, "kaj" = rep(2, length(kdaj_dodali_enote)))
  #tab_izstop <- data.frame("kdaj" = kdaj_profit, "kaj" = rep(3, length(kdaj_profit)))
  #tab <- rbind(tab_vstop, tab_dodajali, tab_izstop)
  #tab[order(tab$kdaj),]
}}


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

dobicki_hitreje <- function(tabela, dnevi_N = 20, vstop_s1 = 20, vstop_s2 = 55, izstop_s1 = 10, izstop_s2 = 20, 
                        obdobje = 1800, zacetni_kapital = 1000000, cena = "Close", strategija, 
                        add = 1/2, sl = 2){
  tab <- izracun_za_analizo(tabela, dnevi_N, vstop_s1, vstop_s2, cena)
  dobicki <- c()
  if(strategija == "S1"){
    for(i in 1:(nrow(tab) - obdobje + 1)){
      tmp <- tab[i:(obdobje + i - 1),]
      cena2 <- odlocitev_cena(tmp, cena)
      dobicki <- c(dobicki, turtle_s1_spx(tmp, zacetni_kapital, izstop_s1, cena2, add, sl))
    }
  }
  else{
    for(i in 1:(nrow(tab) - obdobje + 1)){
      tmp <- tab[i:(obdobje + i - 1),]
      cena2 <- odlocitev_cena(tmp, cena)
      dobicki <- c(dobicki, turtle_s2_spx(tmp, zacetni_kapital, izstop_s2, cena2, add, sl))
    }
  }
  dobicki
}

# primerjava gibanja dobička spx in btc (oba instrumenta trgujemo enako časa (od 1.1.2012 do 27.6.2018))
profit_spx <- function(tabela, strategija, dnevi_N = 20, cena = "Close", vstop_s1 = 20, obdobje = 1800, 
                       vstop_s2 = 55, zacetni_kapital = 1000000, izstop_s1 = 10, izstop_s2 = 20, add = 0.5, sl = 2){
  tab <- izracun_za_analizo(tabela, dnevi_N, vstop_s1, vstop_s2, cena)
  if(strategija == "S1"){
    cena2 <- odlocitev_cena(tab, cena)
    tmp <- turtle_s1_spx(tab, zacetni_kapital, izstop_s1, cena2, add, sl)
    
  }
  else{
    cena2 <- odlocitev_cena(tab, cena)
    tmp <- turtle_s2_spx(tab, zacetni_kapital, izstop_s2, cena2, add, sl)
  }
  tmp 
}


za_graf_spx <- profit_spx(tabela = spx_1day, strategija = "S1")
za_graf_spx$Profit <- za_graf_spx$Profit/1000
za_graf_btc <- profit_spx(btc_1day, "S1")
za_graf_btc$Profit <- za_graf_btc$Profit/1000

tab_btc <- izracun_za_analizo(btc_1day, 20, 20, 55, "Close")
tab_spx <- izracun_za_analizo(spx_1day, 20, 20, 55, "Close")

ggplot()+
  geom_line(data = za_graf_btc, size = 0.8, 
            aes(x = tab_btc$Timestamp[za_graf_btc$kdaj], y = za_graf_btc$Profit, color = "btc"))+
  geom_line(data = za_graf_spx, size = 0.8, 
            aes(x = tab_spx$Date[za_graf_spx$kdaj], y = za_graf_spx$Profit, color = "spx"))+
  scale_color_manual(name = "", values = c("btc" = "blue", "spx" = "red"))+
  ylab("Dobicki v 1000")+
  xlab("Trgovalni dnevi")+
  ggtitle("Višina dobička trgovanja BTC in SPX")+
  theme_minimal()+
  geom_text(data = za_graf_btc, aes(x = tab_btc$Timestamp[za_graf_btc$kdaj[length(za_graf_btc$kdaj)]], 
                                    y = Profit[length(Profit)],
                                    label = round(Profit[length(Profit)],2)), 
            check_overlap = TRUE, nudge_x = - 15000000, nudge_y = 40, size = 2.5)+
  geom_text(data = za_graf_spx, aes(x = tab_spx$Date[za_graf_spx$kdaj[length(za_graf_spx$kdaj)]], 
                                    y = Profit[length(Profit)],
                                    label = round(Profit[length(Profit)],2)), 
            check_overlap = TRUE, nudge_x = - 15000000, nudge_y = 40, size = 2.5)+
  theme(legend.position = c(0.85, 0.5), legend.box.margin = margin(0, 0, -0.5, 0, "cm"), 
        plot.title = element_text(hjust = 1))


dodana_rast <- function(tabela){
  rast <- c()
  for(i in 2:nrow(tabela)){
    rast <- c(rast, round(((tabela$Profit[i]/tabela$Profit[i-1])-1)*100, 2))
  }
  tabela$rast <- c(0, rast)
  tabela
}

za_graf_spx <- dodana_rast(za_graf_spx)
za_graf_btc <- dodana_rast(za_graf_btc)

# Pred letom 2014
ind <- which(za_graf_btc$kdaj < 700)
ind1 <- which(za_graf_spx$kdaj < 500)
ggplot()+
  geom_line(data = za_graf_btc[ind,], 
            aes(x = tab_btc$Timestamp[za_graf_btc$kdaj[ind]], y = za_graf_btc$rast[ind], color = "btc"))+
  geom_line(data = za_graf_spx[ind1,], 
            aes(x = tab_spx$Date[za_graf_spx$kdaj[ind1]], y = za_graf_spx$rast[ind1], color = "spx"))+
  scale_color_manual(name = "", values = c("btc" = "blue", "spx" = "red"))+
  ylab("Rast dobička (v %)")+
  xlab("Trgovalni dnevi")+
  ggtitle("Rast dobička pri trgovanju BTC in SPX")+
  theme_minimal()+
  theme(legend.position = c(0.85, 0.5), legend.box.margin = margin(0, 0, -0.5, 0, "cm"), 
        plot.title = element_text(hjust = 1))

# Po letu 2014
ind <- which(za_graf_btc$kdaj > 700)
#ind <- ind[c(-2, -4)]
ind1 <- which(za_graf_spx$kdaj > 500)
ggplot()+
  geom_line(data = za_graf_btc[ind,], 
            aes(x = tab_btc$Timestamp[za_graf_btc$kdaj[ind]], y = za_graf_btc$rast[ind], color = "btc"))+
  geom_line(data = za_graf_spx[ind1,], 
            aes(x = tab_spx$Date[za_graf_spx$kdaj[ind1]], y = za_graf_spx$rast[ind1], color = "spx"))+
  scale_color_manual(name = "", values = c("btc" = "blue", "spx" = "red"))+
  ylab("Rast dobicka (v %)")+
  xlab("Trgovalni dnevi")+
  ggtitle("Rast dobička pri trgovanju BTC in SPX")+
  theme_minimal()+
  theme(legend.position = c(0.15, 0.9), legend.box.margin = margin(0, 0, -0.5, 0, "cm"), 
        plot.title = element_text(hjust = 1))


############################################
# graf vstopov, dodajanja enot in izstopov #
############################################

vdi <- izracun_za_analizo(btc_1day, 20, 20, 55, "Close")[1983:2343,]
vdi_s1 <- turtle_s1_spx(vdi, 1000000, 10, odlocitev_cena(vdi, "Close"))
vdi_s2 <- turtle_s2_spx(vdi, 1000000, 20, odlocitev_cena(vdi, "Close"))

g_vdi <- function(tabela){
  ggplot(vdi)+
    geom_line(aes(Timestamp, Close))+
    geom_point(data = tabela[tabela$kaj == 1,], aes(x = vdi$Timestamp[tabela$kdaj[tabela$kaj == 1]], 
                                  y = vdi$Close[tabela$kdaj[tabela$kaj == 1]]), 
               fill="yellow", color="yellow", shape = 21, size = 2)+
    geom_point(data = tabela[tabela$kaj == 2,], aes(x = vdi$Timestamp[tabela$kdaj[tabela$kaj == 2]], 
                                  y = vdi$Close[tabela$kdaj[tabela$kaj == 2]]), 
               fill="darkgreen", color="darkgreen", shape = 21, size = 2)+
    geom_point(data = tabela[tabela$kaj == 3,], aes(x = vdi$Timestamp[tabela$kdaj[tabela$kaj == 3]], 
                                  y = vdi$Close[tabela$kdaj[tabela$kaj == 3]]), 
               fill="orange", color="orange", shape = 21, size = 2)+
    theme_minimal()+
    ylab("Zaključna cena dneva")+
    xlab("Trgovalni dnevi")+
    ggtitle("Vstopanje, dodajanje, izstopanje")+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(hjust = 0.7))
}

g_vdi(vdi_s1)
g_vdi(vdi_s2)
