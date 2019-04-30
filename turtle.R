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

turtle_s1 <- function(tabela, zacetni_kapital){
  kandidati_20 <- which(tabela$entry20==1 | tabela$entry20==2)
  kandidati_55 <- which(tabela$entry55==1 | tabela$entry55==2)
  vstop <- kandidati_20[1]
  profit <- 0
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
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (tabela$Close[i] - cena_ko_dodamo[j])*st_btc_dodamo[j]
            }
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
      vstop <- ifelse(is.na(kandidati_55[kandidati_55 > vstop][1]), nrow(tabela), kandidati_55[kandidati_55 > vstop][1])
    }
    #if(profit <= -100000){money <- 0.8*money}
  }
  profit
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


turtle_s2 <- function(tabela, zacetni_kapital){
  kandidati <- which(tabela$entry55==1 | tabela$entry55==2)
  vstop <- kandidati[1]
  profit <- 0
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
            for(j in 1:length(st_btc_dodamo)){
              profit <- profit + (tabela$Close[i] - cena_ko_dodamo[j])*st_btc_dodamo[j]
            }
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

################################################################
#5 letni pregled dobičkov in Graf kandidatov za vstop in izstop#
################################################################

# #enote: 1 = 1 day
# #strategija: S1 = System 1, S2 = System 2
# pregled_5ih_let <- function(tabela, zacetni_kapital, enote, strategija){
#   tab <- tabela[1:(387*enote),]
#   tab <- izracun_N(tab)
#   tab$entry20 <- entry_s1(tab)
#   tab$entry55 <- entry_s2(tab)
#   tab$tedenski_N <- tedenski_N(tab)
#   ifelse(strategija == "S1", pregled <- turtle_s1(tab, zacetni_kapital), pregled <- turtle_s2(tab, zacetni_kapital))
#   for(i in 1:4){
#     tab <- tabela[((387*enote) + (i-1)*(365*enote) + i):((387*enote) + i*(365*enote) + i),]
#     tab <- izracun_N(tab)
#     tab$entry20 <- entry_s1(tab)
#     tab$entry55 <- entry_s2(tab)
#     tab$tedenski_N <- tedenski_N(tab)
#     ifelse(strategija == "S1", pregled1 <- turtle_s1(tab, zacetni_kapital), pregled1 <- turtle_s2(tab, zacetni_kapital))
#     pregled <- rbind(pregled, pregled1)
#   }
#   pregled
# }

#graf kandidatov za vstop v pozicijo
#še pike v legendo
g_vstopi <- function(tabela, strategija){
  ifelse(strategija == "S1", ind <- which(tabela$entry_s1 == 1), ind <- which(tabela$entry_s2 == 1))
  ifelse(strategija == "S1", ind1 <- which(tabela$entry_s1 == 2), ind1 <- which(tabela$entry_s2 == 2))
  ggplot(tabela)+
    geom_line(aes(Timestamp, Close))+
    geom_point(data = tabela[ind,], aes(x = Timestamp, y = Close), fill="red", color="red", shape = 21, size = 1.5)+
    geom_point(data = tabela[ind1, ], aes(x = Timestamp, y = Close), fill="blue", color="blue", shape = 21, size = 1.5)+
    #scale_color_manual(name = "", values = c("coin" = "black"))+
    #scale_color_manual(name = "", values = c("long" = "red", "short" = "blue"))+
    theme_minimal()+
    ylab("Zaključna cena dneva")+
    xlab("Trgovalni dnevi")+
    ggtitle("Vstopni signali")+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(hjust = 0.7))
    #theme(panel.grid.minor = element_blank(), legend.text = element_text(size = 8), axis.title.x=element_blank(),
    #      legend.position="bottom")
}

racunanje <- function(tabela){
  tabela <- tabela[-nrow(tabela),]
  tab <- izracun_N(tabela)
  tab$entry_s1 <- entry_s1(tab)
  tab$entry_s2 <- entry_s2(tab)
  tab$tedenski_N <- tedenski_N(tab)
  tab <- tab[tab$tedenski_N > 0,]
  tab
}


g_vstopi(racunanje(btc_1day)[1983:2343,], "S1")
g_vstopi(racunanje(btc_1day)[1983:2343,], "S2")




#########
#BITCOIN#
#########

load("databtc_5min.Rda")
load("databtc_1h.Rda")
load("databtc_1day.Rda")


# ##PRVO LETO
# #2012 je bilo prestopno leto, torej 366 dni + 20 dni za izraun prvega N + 1 dan za true range
# test_1y <- btc_1day[1:387,]
# test_1y <- izracun_N(test_1y)
# 
# test_1y$entry20 <- entry_s1(test_1y)
# test_1y$entry55 <- entry_s2(test_1y)
# g_vstopi(test_1y, "S2")
# 
# test_1y$tedenski_N <- tedenski_N(test_1y)
# 
# razlicni_zk <- rbind(turtle_s2(test_1y, 1000000), turtle_s2(test_1y, 100000), turtle_s2(test_1y, 10000), turtle_s2(test_1y, 1000))
# 
# 
# ##DRUGO LETO
# test_2y <- btc_1day[388:753,]
# test_2y <- izracun_N(test_2y)
# 
# test_2y$entry20 <- entry_s1(test_2y)
# test_2y$entry55 <- entry_s2(test_2y)
# g_vstopi(test_2y, "S2")
# 
# test_2y$tedenski_N <- tedenski_N(test_2y)
# 
# razlicni_zk <- rbind(turtle_s2(test_2y, 1000000), turtle_s2(test_2y, 100000), turtle_s2(test_2y, 10000), turtle_s2(test_2y, 1000))
# 
# 
# ##TRETJE LETO
# test_3y <- btc_1day[754:1119,]
# test_3y <- izracun_N(test_3y)
# 
# test_3y$entry20 <- entry_s1(test_3y)
# test_3y$entry55 <- entry_s2(test_3y)
# g_vstopi(test_3y, "S2")
# 
# test_3y$tedenski_N <- tedenski_N(test_3y)
# 
# razlicni_zk <- rbind(turtle_s2(test_3y, 1000000), turtle_s2(test_3y, 100000), turtle_s2(test_3y, 10000), turtle_s2(test_3y, 1000))
# 
# 
# S1btc_dobicki5let_1dan <- pregled_5ih_let(btc_1day, 1000000, 1, "S1")
# save(S1btc_dobicki5let_1dan, file = "S1btc_dobicki5let_1dan.Rda")
# S1btc_dobicki5let_1h <- pregled_5ih_let(btc_1h, 1000000, 24, "S1")
# save(S1btc_dobicki5let_1h, file = "S1btc_dobicki5let_1h.Rda")
# S1btc_dobicki5let_5min <- pregled_5ih_let(btc_5min, 1000000, 24*12, "S1")
# save(S1btc_dobicki5let_5min, file = "S1btc_dobicki5let_5min.Rda")
# 
# S2btc_dobicki5let_1dan <- pregled_5ih_let(btc_1day, 1000000, 1, "S2")
# save(S2btc_dobicki5let_1dan, file = "S2btc_dobicki5let_1dan.Rda")
# S2btc_dobicki5let_1h <- pregled_5ih_let(btc_1h, 1000000, 24, "S2")
# save(S2btc_dobicki5let_1h, file = "S2btc_dobicki5let_1h.Rda")
# S2btc_dobicki5let_5min <- pregled_5ih_let(btc_5min, 1000000, 24*12, "S2")
# save(S2btc_dobicki5let_5min, file = "S2btc_dobicki5let_5min.Rda")


###########
# DOBIČKI #
###########

#obdobje: koliko časa trgujemo to strategijo (če imamo tabelo z dnevnimi cenami, 360 pomeni 360 dni)
#strategija: S1 = System 1, S2 = System 2
dobicki <- function(tabela, zacetni_kapital, obdobje, strategija){
  tabela <- tabela[-nrow(tabela),]
  tab <- izracun_N(tabela)
  tab$entry20 <- entry_s1(tab)
  tab$entry55 <- entry_s2(tab)
  tab$tedenski_N <- tedenski_N(tab)
  tab <- tab[tab$tedenski_N > 0,]
  dobicki <- c()
  if(strategija == "S1"){
    for(i in 1:(nrow(tab) - obdobje + 1)){
      dobicki <- c(dobicki, turtle_s1(tab[i:(obdobje + i - 1),], zacetni_kapital))
    }
  }
  else{
    for(i in 1:(nrow(tab) - obdobje + 1)){
      dobicki <- c(dobicki, turtle_s2(tab[i:(obdobje + i - 1),], zacetni_kapital))
    }
  }
  dobicki
}

#1day, 1000000
btc_dobicki_360_S1 <- dobicki(btc_1day, 1000000, 360, "S1")
btc_dobicki_500_S1 <- dobicki(btc_1day, 1000000, 500, "S1")
btc_dobicki_1000_S1 <- dobicki(btc_1day, 1000000, 1000, "S1")
btc_dobicki_1800_S1 <- dobicki(btc_1day, 1000000, 1800, "S1")

btc_dobicki_360_S2 <- dobicki(btc_1day, 1000000, 360, "S2")
btc_dobicki_500_S2 <- dobicki(btc_1day, 1000000, 500, "S2")
btc_dobicki_1000_S2 <- dobicki(btc_1day, 1000000, 1000, "S2")
btc_dobicki_1800_S2 <- dobicki(btc_1day, 1000000, 1800, "S2")

#1day, 50000
btc_dobicki_360_S1_2 <- dobicki(btc_1day, 50000, 360, "S1")
btc_dobicki_500_S1_2 <- dobicki(btc_1day, 50000, 500, "S1")
btc_dobicki_1000_S1_2 <- dobicki(btc_1day, 50000, 1000, "S1")
btc_dobicki_1800_S1_2 <- dobicki(btc_1day, 50000, 1800, "S1")

btc_dobicki_360_S2_2 <- dobicki(btc_1day, 50000, 360, "S2")
btc_dobicki_500_S2_2 <- dobicki(btc_1day, 50000, 500, "S2")
btc_dobicki_1000_S2_2 <- dobicki(btc_1day, 50000, 1000, "S2")
btc_dobicki_1800_S2_2 <- dobicki(btc_1day, 50000, 1800, "S2")


#1h
btc_1h_dobicki_360_S1 <- dobicki(btc_1h, 1000000, 360, "S1")
btc_1h_dobicki_500_S1 <- dobicki(btc_1h, 1000000, 500, "S1")
btc_1h_dobicki_1000_S1 <- dobicki(btc_1h, 1000000, 1000, "S1")
btc_1h_dobicki_1800_S1 <- dobicki(btc_1h, 1000000, 1800, "S1")

# btc_1h_dobicki_360_S2 <- dobicki(btc_1h, 1000000, 360, "S2")
# btc_1h_dobicki_500_S2 <- dobicki(btc_1h, 1000000, 500, "S2")
# btc_1h_dobicki_1000_S2 <- dobicki(btc_1h, 1000000, 1000, "S2")
# btc_1h_dobicki_1800_S2 <- dobicki(btc_1h, 1000000, 1800, "S2")
btc_1h_dobicki_360_S2 <- dobicki_hitreje(tabela = btc_1h, obdobje = 360, strategija = "S2")
btc_1h_dobicki_500_S2 <- dobicki_hitreje(tabela = btc_1h, obdobje = 500, strategija = "S2")
btc_1h_dobicki_1000_S2 <- dobicki_hitreje(tabela = btc_1h, obdobje = 1000, strategija = "S2")
btc_1h_dobicki_1800_S2 <- dobicki_hitreje(tabela = btc_1h, strategija =  "S2")


#5 min
btc_5min_dobicki_360_S1 <- dobicki_hitreje(tabela = btc_5min, obdobje = 360, strategija = "S1")
btc_5min_dobicki_500_S1 <- dobicki_hitreje(tabela = btc_5min, obdobje = 500, strategija = "S1")
btc_5min_dobicki_1000_S1 <- dobicki_hitreje(tabela = btc_5min, obdobje = 1000, strategija = "S1")
btc_5min_dobicki_1800_S1 <- dobicki_hitreje(tabela = btc_5min, strategija =  "S1")

btc_5min_dobicki_360_S2 <- dobicki_hitreje(tabela = btc_5min, obdobje = 360, strategija = "S2")
btc_5min_dobicki_500_S2 <- dobicki_hitreje(tabela = btc_5min, obdobje = 500, strategija = "S2")
btc_5min_dobicki_1000_S2 <- dobicki_hitreje(tabela = btc_5min, obdobje = 1000, strategija = "S2")
btc_5min_dobicki_1800_S2 <- dobicki_hitreje(tabela = btc_5min, strategija =  "S2")

# Zaradi grafov delimo s 1000
#btc_dobicki_360_S1 <- btc_dobicki_360_S1/1000
#btc_dobicki_500_S1 <- btc_dobicki_500_S1/1000
#btc_dobicki_1000_S1 <- btc_dobicki_1000_S1/1000
#btc_dobicki_1800_S1 <- btc_dobicki_1800_S1/1000
#btc_dobicki_360_S2 <- btc_dobicki_360_S2/1000
#btc_dobicki_500_S2 <- btc_dobicki_500_S2/1000
#btc_dobicki_1000_S2 <- btc_dobicki_1000_S2/1000
#btc_dobicki_1800_S2 <- btc_dobicki_1800_S2/1000
#btc_dobicki_360_S1_2 <- btc_dobicki_360_S1_2/1000
#btc_dobicki_500_S1_2 <- btc_dobicki_500_S1_2/1000
#btc_dobicki_1000_S1_2 <- btc_dobicki_1000_S1_2/1000
#btc_dobicki_1800_S1_2 <- btc_dobicki_1800_S1_2/1000
#btc_dobicki_360_S2_2 <- btc_dobicki_360_S2_2/1000
#btc_dobicki_500_S2_2 <- btc_dobicki_500_S2_2/1000
#btc_dobicki_1000_S2_2 <- btc_dobicki_1000_S2_2/1000
#btc_dobicki_1800_S2_2 <- btc_dobicki_1800_S2_2/1000
# btc_1h_dobicki_360_S1 <- btc_1h_dobicki_360_S1/1000
# btc_1h_dobicki_500_S1 <- btc_1h_dobicki_500_S1/1000
# btc_1h_dobicki_1000_S1 <- btc_1h_dobicki_1000_S1/1000
# btc_1h_dobicki_1800_S1 <- btc_1h_dobicki_1800_S1/1000
# btc_1h_dobicki_360_S2 <- btc_1h_dobicki_360_S2/1000
# btc_1h_dobicki_500_S2 <- btc_1h_dobicki_500_S2/1000
# btc_1h_dobicki_1000_S2 <- btc_1h_dobicki_1000_S2/1000
# btc_1h_dobicki_1800_S2 <- btc_1h_dobicki_1800_S2/1000
btc_5min_dobicki_360_S1 <- btc_5min_dobicki_360_S1/1000
btc_5min_dobicki_500_S1 <- btc_5min_dobicki_500_S1/1000
btc_5min_dobicki_1000_S1 <- btc_5min_dobicki_1000_S1/1000
btc_5min_dobicki_1800_S1 <- btc_5min_dobicki_1800_S1/1000
btc_5min_dobicki_360_S2 <- btc_5min_dobicki_360_S2/1000
btc_5min_dobicki_500_S2 <- btc_5min_dobicki_500_S2/1000
btc_5min_dobicki_1000_S2 <- btc_5min_dobicki_1000_S2/1000
btc_5min_dobicki_1800_S2 <- btc_5min_dobicki_1800_S2/1000



#########
# GRAFI #
#########

# !!! Vsi grafi so narejeni samo za S1 !!!

#graf kako so razpršeni dobički
g_dobicki <- function(vrednosti, st_bin, obdobje){
  ggplot(data.frame(vrednosti))+
    geom_histogram(aes(vrednosti), bins = st_bin)+
    #scale_y_log10()+
    theme_minimal()+
    ggtitle(paste0(obdobje, " dnevno trgovanje" ))+
    xlab("Dobiček v 1000")+
    ylab("Število obdobij trgovanja")+
    theme(plot.title = element_text(hjust = 0.5))
}


library(gridExtra)
library(ggplot2)
grid.arrange(g_dobicki(btc_dobicki_360_S1, 200, 360), g_dobicki(btc_dobicki_500_S1, 200, 500), 
             g_dobicki(btc_dobicki_1000_S1, 200, 1000), g_dobicki(btc_dobicki_1800_S1, 200, 1800), 
             nrow = 2, ncol = 2)

grid.arrange(g_dobicki(btc_dobicki_360_S1[btc_dobicki_360_S1 > 500], 100, 360),
             g_dobicki(btc_dobicki_500_S1[btc_dobicki_500_S1 > 500], 100, 500),
             g_dobicki(btc_dobicki_1000_S1[btc_dobicki_1000_S1 > 500], 100, 1000), 
             g_dobicki(btc_dobicki_1800_S1[btc_dobicki_1800_S1 > 500], 100, 1800),
             ncol = 2, nrow = 2)

grid.arrange(g_dobicki(btc_dobicki_360_S1[btc_dobicki_360_S1 < 50], 100, 360),
             g_dobicki(btc_dobicki_500_S1[btc_dobicki_500_S1 < 50], 100, 500),
             g_dobicki(btc_dobicki_1000_S1[btc_dobicki_1000_S1 < 50], 100, 1000), 
             g_dobicki(btc_dobicki_1800_S1[btc_dobicki_1800_S1 < 50], 100, 1800),
             ncol = 2, nrow = 2)

grid.arrange(g_dobicki(btc_dobicki_360_S1[btc_dobicki_360_S1 < 7], 100, 360),
             g_dobicki(btc_dobicki_500_S1[btc_dobicki_500_S1 < 7], 100, 500),
             g_dobicki(btc_dobicki_1000_S1[btc_dobicki_1000_S1 < 7], 100, 1000), 
             g_dobicki(btc_dobicki_1800_S1[btc_dobicki_1800_S1 < 45], 100, 1800), 
             ncol = 2, nrow = 2)



# število dobičkov nad določeno mejo
nad_500 <- c(length(btc_dobicki_360_S1[btc_dobicki_360_S1 > 500]), 
             length(btc_dobicki_500_S1[btc_dobicki_500_S1 > 500]), 
             length(btc_dobicki_1000_S1[btc_dobicki_1000_S1 > 500]), 
             length(btc_dobicki_1800_S1[btc_dobicki_1800_S1 > 500]))
nad_650 <- c(length(btc_dobicki_360_S1[btc_dobicki_360_S1 > 650]), 
             length(btc_dobicki_500_S1[btc_dobicki_500_S1 > 650]), 
             length(btc_dobicki_1000_S1[btc_dobicki_1000_S1 > 650]), 
             length(btc_dobicki_1800_S1[btc_dobicki_1800_S1 > 650]))
nad_800 <- c(length(btc_dobicki_360_S1[btc_dobicki_360_S1 > 800]), 
             length(btc_dobicki_500_S1[btc_dobicki_500_S1 > 800]), 
             length(btc_dobicki_1000_S1[btc_dobicki_1000_S1 > 800]), 
             length(btc_dobicki_1800_S1[btc_dobicki_1800_S1 > 800]))
nad_950 <- c(length(btc_dobicki_360_S1[btc_dobicki_360_S1 > 950]), 
             length(btc_dobicki_500_S1[btc_dobicki_500_S1 > 950]), 
             length(btc_dobicki_1000_S1[btc_dobicki_1000_S1 > 950]), 
             length(btc_dobicki_1800_S1[btc_dobicki_1800_S1 > 950]))
veliki_dobicki <- data.frame(strategija = rep("S1", 4), obdobje = c(360, 500, 1000, 1800), 
                        "nad_500" = nad_500, "nad_650" = nad_650, "nad_800" = nad_800, "nad_950" = nad_950)
flextabela_pregled(veliki_dobicki, 0)


# graf dobičkov v času
dobicki_v_casu <- function(tabela, obdobje, prikaz = 1){
  ifelse(prikaz == 500, cas <- "06.06.2013", cas <- "23.01.2012")
  ggplot(data.frame(tabela))+
    geom_point(aes(x = (1:length(tabela)), y = tabela), fill="blue", color="blue", shape = 20, size = 0.01)+
    #scale_y_log10()+
    theme_minimal()+
    ggtitle(paste0("Dobički skozi čas", ", ",obdobje, " dnevno trgovanje" ))+
    ylab("Dobički v 1000")+
    xlab(paste0("Čas (začetek = ", cas, ")"))+
    #ylim(c(min(0, min(tabela)), max(tabela)))+
    theme(plot.title = element_text(hjust = 0.9))
}

grid.arrange(dobicki_v_casu(btc_dobicki_360_S1, 360), dobicki_v_casu(btc_dobicki_500_S1, 500), 
             dobicki_v_casu(btc_dobicki_1000_S1, 1000), dobicki_v_casu(btc_dobicki_1800_S1, 1800), 
             nrow = 2, ncol = 2)

grid.arrange(dobicki_v_casu(btc_dobicki_360_S1[500:length(btc_dobicki_360_S1)], 360, 500), 
             dobicki_v_casu(btc_dobicki_500_S1[500:length(btc_dobicki_500_S1)], 500, 500), 
             dobicki_v_casu(btc_dobicki_1000_S1[500:length(btc_dobicki_1000_S1)], 1000, 500), 
             dobicki_v_casu(btc_dobicki_1800_S1[500:length(btc_dobicki_1800_S1)], 1800, 500), 
             nrow = 2, ncol = 2)


###########
# PREGLED #
###########

#Pregled dobičkov: povprečje, standardni odklon, min, max, 
# povprečje v %, koliko jih je < 0, verjetnost da je dobiček < 0 
pregled_dobicki <- function(l1, l2, l3, l4, zacetni_kapital, strategija){
  l1 <- l1*1000
  l2 <- l2*1000
  l3 <- l3*1000
  l4 <- l4*1000
  st_podatkov <- c(length(l1), length(l2), length(l3), length(l4))
  povprecje <- c(mean(l1), mean(l2), mean(l3), mean(l4))
  sd <- c(sd(l1), sd(l2), sd(l3), sd(l4))
  min <- c(min(l1), min(l2), min(l3), min(l4))
  max <- c(max(l1), max(l2), max(l3), max(l4))
  mean_proc <- paste0(round((povprecje/zacetni_kapital)*100, 2), " %")
  s_e <- c(paste0(strategija, ", ", 360), paste0(strategija, ", ", 500), paste0(strategija, ", ", 1000), 
           paste0(strategija, ", ", 1800))
  manj_0 <- c(sum(l1 < 0), sum(l2 < 0), sum(l3 < 0), sum(l4 < 0))
  verj <- paste0(round((manj_0/st_podatkov)*100, 2), " %")
  pregled <- data.frame("sistem_obdobje" = s_e, "st_podatkov" = st_podatkov, "povprecje" = povprecje, 
                        "sd" = sd, "min" = min, "max" = max, "pov_procenti" = mean_proc,
                        "st_manj_0" = manj_0, "verj_izgube" = verj)
  pregled
}

pregled_btc_S1 <- pregled_dobicki(btc_dobicki_360_S1, btc_dobicki_500_S1, btc_dobicki_1000_S1, 
                                  btc_dobicki_1800_S1, 1000000, "S1")
pregled_btc_S2 <- pregled_dobicki(btc_dobicki_360_S2, btc_dobicki_500_S2, btc_dobicki_1000_S2, 
                                  btc_dobicki_1800_S2, 1000000, "S2")

pregled_btc_S1_2 <- pregled_dobicki(btc_dobicki_360_S1_2, btc_dobicki_500_S1_2, btc_dobicki_1000_S1_2, 
                                  btc_dobicki_1800_S1_2, 50000, "S1")
pregled_btc_S2_2 <- pregled_dobicki(btc_dobicki_360_S2_2, btc_dobicki_500_S2_2, btc_dobicki_1000_S2_2, 
                                  btc_dobicki_1800_S2_2, 50000, "S2")

#1h
pregled_btc_1h_S1 <- pregled_dobicki(btc_1h_dobicki_360_S1, btc_1h_dobicki_500_S1, btc_1h_dobicki_1000_S1, 
                                     btc_1h_dobicki_1800_S1, 1000000, "S1")
pregled_btc_1h_S2 <- pregled_dobicki(btc_1h_dobicki_360_S2, btc_1h_dobicki_500_S2, btc_1h_dobicki_1000_S2, 
                                     btc_1h_dobicki_1800_S2, 1000000, "S2")

#5min
pregled_btc_5min_S1 <- pregled_dobicki(btc_5min_dobicki_360_S1, btc_5min_dobicki_500_S1, btc_5min_dobicki_1000_S1, 
                                     btc_5min_dobicki_1800_S1, 1000000, "S1")
pregled_btc_5min_S2 <- pregled_dobicki(btc_5min_dobicki_360_S2, btc_5min_dobicki_500_S2, btc_5min_dobicki_1000_S2, 
                                     btc_5min_dobicki_1800_S2, 1000000, "S2")


################
# Lepše tabele #
################

#Flex tabela
library(flextable)
flextabela_pregled <- function(tabela, st_decimalk){
  tabela <- regulartable(tabela)
  tabela <- bg(tabela, bg = "coral", part = "header")
  tabela <- bg(tabela, bg = "cyan", part = "body")
  tabela <- bold(tabela, part = "header")
  #tabela <- color(tabela, color = "white", part = "header")
  tabela <- align(tabela, align = "center", part = "all")
  ifelse(st_decimalk == 0, tabela <- set_formatter_type(tabela, fmt_double = "%.00f"), 
         ifelse(st_decimalk == 1, tabela <- set_formatter_type(tabela, fmt_double = "%.01f"), 
                tabela <- set_formatter_type(tabela, fmt_double = "%.03f")))
  #tabela <- color(tabela, ~ povprecje > 3.5, ~ povprecje, color = "red")
  #tabela <- bold(tabela, ~ povprecje > 3.5, ~ povprecje, bold = TRUE)
  tabela
}


flextabela_pregled(pregled_btc_S1, 0)
flextabela_pregled(pregled_btc_S2, 0)
flextabela_pregled(pregled_btc_S1_2, 0)
flextabela_pregled(pregled_btc_S2_2, 0)
#1h
flextabela_pregled(pregled_btc_1h_S1, 0)
flextabela_pregled(pregled_btc_1h_S2, 0)
#5min
flextabela_pregled(pregled_btc_5min_S1, 0)
flextabela_pregled(pregled_btc_5min_S2, 0)


#################################
# Compaunded annual growth rate #
#################################

cagr <- function(tabela, zacetni_kapital = 1000000, obdobje = 1800){
  tabela <- round(((tabela/zacetni_kapital+1)^(365/obdobje)-1)*100,3)
  tabela
}

pregled_cagr <- function(tabela, zacetni_kapital = 1000000, strategija){
  obdobje <- c(360, 500, 1000, 1800)
  s_e <- c()
  cagr <- c()
  ratio <- c()
  for(i in 1:4){
    s_e <- c(s_e, paste0(strategija, ", ", obdobje[i]))
    cagr <- c(cagr, paste0(cagr(tabela$povprecje[i], zacetni_kapital = zacetni_kapital, obdobje = obdobje[i]), " %"))
    ratio <- c(ratio, tabela$povprecje[i]/tabela$sd[i])
  }
  pregled <- data.frame("sistem_obdobje" = s_e, "kolicnik" = ratio, "lsd" = cagr)
  pregled
}

flextabela_pregled(pregled_cagr(pregled_btc_S1, strategija = "S1"), 3)
flextabela_pregled(pregled_cagr(pregled_btc_S2, strategija = "S2"), 3)
flextabela_pregled(pregled_cagr(pregled_btc_S1_2, zacetni_kapital = 50000, "S1"), 3)
flextabela_pregled(pregled_cagr(pregled_btc_S2_2, zacetni_kapital = 50000, "S2"), 3)
#1h
flextabela_pregled(pregled_cagr(pregled_btc_1h_S1, strategija = "S1"), 3)
flextabela_pregled(pregled_cagr(pregled_btc_1h_S2, strategija = "S2"), 3)
#5min
flextabela_pregled(pregled_cagr(pregled_btc_5min_S1, strategija = "S1"), 3)
flextabela_pregled(pregled_cagr(pregled_btc_5min_S2, strategija = "S2"), 3)


#######################
# pred in po letu 2014#
#######################

pred_po_2014 <- function(l1, l2, l3, l4, predpo2014){
  ifelse(predpo2014 == 700, skala <- length(l4), skala <- predpo2014)
  pred_2014 <- c(mean(l1[1:predpo2014]*1000), mean(l2[1:predpo2014]*1000), mean(l3[1:predpo2014]*1000), mean(l4[1:skala]*1000))
  cagr_pred_2014 <- c(cagr(pred_2014[1], obdobje = 360), cagr(pred_2014[2], obdobje = 500), 
                    cagr(pred_2014[3], obdobje = 1000), cagr(pred_2014[4]))
  ifelse(predpo2014 == 700, skala1 <- 0, skala1 <- mean(l4[(predpo2014+1):length(l4)]*1000))
  po_2014 <- c(mean(l1[(predpo2014+1):length(l1)]*1000), mean(l2[(predpo2014+1):length(l2)]*1000), mean(l3[(predpo2014+1):length(l3)]*1000), skala1)
  cagr_po_2014 <- c(cagr(po_2014[1], obdobje = 360), cagr(po_2014[2], obdobje = 500), 
                    cagr(po_2014[3], obdobje = 1000), cagr(po_2014[4]))
  data.frame("sistem_obdobje" = c("S1, 360", "S1, 500", "S1, 1000", "S1, 1800"), 
             "pred_2014" = pred_2014, "po_2014" = po_2014, 
             "lsd_pred_2014" = paste0(cagr_pred_2014, " %"), 
             "lsd_po_2014" = paste0(cagr_po_2014, " %"))
}


S1pred_po_2014 <- pred_po_2014(btc_dobicki_360_S1, btc_dobicki_500_S1, btc_dobicki_1000_S1, btc_dobicki_1800_S1, 700)
#1h
S1_1h_pred_po_2014 <- pred_po_2014(btc_1h_dobicki_360_S1, btc_1h_dobicki_500_S1, btc_1h_dobicki_1000_S1, 
                                   btc_1h_dobicki_1800_S1, 17530)
#5min
S1_5min_pred_po_2014 <- pred_po_2014(btc_5min_dobicki_360_S1, btc_5min_dobicki_500_S1, btc_5min_dobicki_1000_S1, 
                                   btc_5min_dobicki_1800_S1, 210710)

# Flextabela
flextabela_pregled(S1pred_po_2014, 0)
#1h
flextabela_pregled(S1_1h_pred_po_2014, 0)
#5min
flextabela_pregled(S1_5min_pred_po_2014, 0)



sd_pred_po_2014 <- function(l1, l2, l3, l4, predpo2014){
  ifelse(predpo2014 == 700, skala <- length(l4), skala <- predpo2014)
  sd_pred_2014 <- c(sd(l1[1:predpo2014]*1000), sd(l2[1:predpo2014]*1000), sd(l3[1:predpo2014]*1000), sd(l4[1:skala]*1000))
  pred_2014 <- c(mean(l1[1:predpo2014]*1000), mean(l2[1:predpo2014]*1000), mean(l3[1:predpo2014]*1000), mean(l4[1:skala]*1000))
  kolicnik_pred_2014 <- pred_2014/sd_pred_2014
  ifelse(predpo2014 == 700, skala2 <- 0, skala2 <- sd(l4[(predpo2014+1):length(l4)]*1000))
  sd_po_2014 <- c(sd(l1[(predpo2014+1):length(l1)]*1000), sd(l2[(predpo2014+1):length(l2)]*1000), sd(l3[(predpo2014+1):length(l3)]*1000), skala2)
  ifelse(predpo2014 == 700, skala1 <- 0, skala1 <- mean(l4[(predpo2014+1):length(l4)]*1000))
  po_2014 <- c(mean(l1[(predpo2014+1):length(l1)]*1000), mean(l2[(predpo2014+1):length(l2)]*1000), mean(l3[(predpo2014+1):length(l3)]*1000), skala1)
  kolicnik_po_2014 <- po_2014/sd_po_2014
  data.frame("sistem_obdobje" = c("S1, 360", "S1, 500", "S1, 1000", "S1, 1800"), 
             "pred_2014" = sd_pred_2014, "po_2014" = sd_po_2014, 
             "kolicnik_pred" = kolicnik_pred_2014, "kolicnik_po" = kolicnik_po_2014)
}

S1sd_pred_po_2014 <- sd_pred_po_2014(btc_dobicki_360_S1, btc_dobicki_500_S1, btc_dobicki_1000_S1, 
                                   btc_dobicki_1800_S1, 700)
#1h
S1sd_1h_pred_po_2014 <- sd_pred_po_2014(btc_1h_dobicki_360_S1, btc_1h_dobicki_500_S1, btc_1h_dobicki_1000_S1, 
                                   btc_1h_dobicki_1800_S1, 17530)
#5min
S1sd_5min_pred_po_2014 <- sd_pred_po_2014(btc_5min_dobicki_360_S1, btc_5min_dobicki_500_S1, btc_5min_dobicki_1000_S1, 
                                        btc_5min_dobicki_1800_S1, 210710)

# Flextabela
flextabela_pregled(S1sd_pred_po_2014, 1)
#1h
flextabela_pregled(S1sd_1h_pred_po_2014, 1)
#5min
flextabela_pregled(S1sd_5min_pred_po_2014, 1)

