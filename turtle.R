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
    st_btc <- unit(money, tabela$N[vstop])/cena_vstop
    
    if(tabela$entry20[vstop]==1 | tabela$entry55[vstop]==1){
      izstop <- ifelse(is.na(win_izstop_20(tabela, vstop, "long")), nrow(tabela), win_izstop_20(tabela, vstop, "long"))
      cena_izstop <- tabela$Close[izstop]
      st_enot <- 1
      stop_loss <- cena_vstop - 2*tabela$N[vstop]
      pol_N <- (1/2)*tabela$N[vstop]
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
            for(i in 1:length(st_btc_dodamo)){
              profit <- profit + (tabela$Close[i] - cena_ko_dodamo[i])*st_btc_dodamo[i]
            }
            break}
        }
      }
    }
    else{
      izstop <- ifelse(is.na(win_izstop_20(tabela, vstop, "short")), nrow(tabela), win_izstop_20(tabela, vstop, "short"))
      cena_izstop <- tabela$Close[izstop]
      st_enot <- 1
      stop_loss <- cena_vstop + 2*tabela$N[vstop]
      pol_N <- (1/2)*tabela$N[vstop]
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
            for(i in 1:length(st_btc_dodamo)){
              profit <- profit + (cena_ko_dodamo[i] - tabela$Close[i])*st_btc_dodamo[i]
            }
            break}
        }
      }
    }
    if(vstop == (izstop-1)){
      vstop <- vstop + 1
      for(i in 1:length(st_btc_dodamo)){
        profit <- profit + (abs(cena_izstop - cena_ko_dodamo[i]))*st_btc_dodamo[i]
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
    st_btc <- unit(money, tabela$N[vstop])/cena_vstop
    
    if(tabela$entry55[vstop]==1){
      izstop <- ifelse(is.na(win_izstop_55(tabela, vstop, "long")), nrow(tabela), win_izstop_55(tabela, vstop, "long"))
      cena_izstop <- tabela$Close[izstop]
      st_enot <- 1
      stop_loss <- cena_vstop - 2*tabela$N[vstop]
      pol_N <- (1/2)*tabela$N[vstop]
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
            for(i in 1:length(st_btc_dodamo)){
              profit <- profit + (tabela$Close[i] - cena_ko_dodamo[i])*st_btc_dodamo[i]
            }
            break}
        }
      }
    }
    else{
      izstop <- ifelse(is.na(win_izstop_55(tabela, vstop, "short")), nrow(tabela), win_izstop_55(tabela, vstop, "short"))
      cena_izstop <- tabela$Close[izstop]
      st_enot <- 1
      stop_loss <- cena_vstop + 2*tabela$N[vstop]
      pol_N <- (1/2)*tabela$N[vstop]
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
            for(i in 1:length(st_btc_dodamo)){
              profit <- profit + (cena_ko_dodamo[i] - tabela$Close[i])*st_btc_dodamo[i]
            }
            break}
        }
      }
    }
    if(vstop == (izstop-1)){
      vstop <- vstop + 1
      for(i in 1:length(st_btc_dodamo)){
        profit <- profit + (abs(cena_izstop - cena_ko_dodamo[i]))*st_btc_dodamo[i]
      }
      vstop <- ifelse(is.na(kandidati[kandidati > vstop][1]), nrow(tabela), kandidati[kandidati > vstop][1])
    }
    #if(profit <= -100000){money <- 0.8*money}
  }
  profit
}

######################################################
#5 letni pregled dobičkov in Graf kandidatov za vstop#
######################################################

#enote: 1 = 1 day
#strategija: S1 = System 1, S2 = System 2
pregled_5ih_let <- function(tabela, zacetni_kapital, enote, strategija){
  tab <- tabela[1:(387*enote),]
  tab <- izracun_N(tab)
  tab$entry20 <- entry_s1(tab)
  tab$entry55 <- entry_s2(tab)
  tab$tedenski_N <- tedenski_N(tab)
  ifelse(strategija == "S1", pregled <- turtle_s1(tab, zacetni_kapital), pregled <- turtle_s2(tab, zacetni_kapital))
  for(i in 1:4){
    tab <- tabela[((387*enote) + (i-1)*(365*enote) + i):((387*enote) + i*(365*enote) + i),]
    tab <- izracun_N(tab)
    tab$entry20 <- entry_s1(tab)
    tab$entry55 <- entry_s2(tab)
    tab$tedenski_N <- tedenski_N(tab)
    ifelse(strategija == "S1", pregled1 <- turtle_s1(tab, zacetni_kapital), pregled1 <- turtle_s2(tab, zacetni_kapital))
    pregled <- rbind(pregled, pregled1)
  }
  pregled
}

#graf kandidatov za vstop v pozicijo
#še pike v legendo
g_vstopi <- function(tabela, strategija){
  ifelse(strategija == "S1", ind <- which(tabela$entry20 == 1), ind <- which(tabela$entry55 == 1))
  ifelse(strategija == "S1", ind1 <- which(tabela$entry20 == 2), ind1 <- which(tabela$entry55 == 2))
  ggplot(tabela)+
    geom_line(aes(Timestamp, Close, color = "coin"))+
    geom_point(data = tabela[ind,], aes(x = Timestamp, y = Close), fill="red", color="red", shape = 21, size = 1.5)+
    geom_point(data = tabela[ind1, ],aes(x = Timestamp, y = Close), fill="blue", color="blue", shape = 21, size = 1.5)+
    scale_color_manual(name = "", values = c("coin" = "black"))+
    #scale_color_manual(name = "", values = c("long" = "red", "short" = "blue"))+
    theme_bw()+
    ylab("Close Price")+
    theme(panel.grid.minor = element_blank(), legend.text = element_text(size = 8), axis.title.x=element_blank(),
          legend.position="bottom")
}




#########
#BITCOIN#
#########

load("databtc_5min.Rda")
load("databtc_1h.Rda")
load("databtc_1day.Rda")


##PRVO LETO
#2012 je bilo prestopno leto, torej 366 dni + 20 dni za izraun prvega N + 1 dan za true range
test_1y <- btc_1day[1:387,]
test_1y <- izracun_N(test_1y)

test_1y$entry20 <- entry_s1(test_1y)
test_1y$entry55 <- entry_s2(test_1y)
g_vstopi(test_1y, "S2")

test_1y$tedenski_N <- tedenski_N(test_1y)

razlicni_zk <- rbind(turtle_s2(test_1y, 1000000), turtle_s2(test_1y, 100000), turtle_s2(test_1y, 10000), turtle_s2(test_1y, 1000))


##DRUGO LETO
test_2y <- btc_1day[388:753,]
test_2y <- izracun_N(test_2y)

test_2y$entry20 <- entry_s1(test_2y)
test_2y$entry55 <- entry_s2(test_2y)
g_vstopi(test_2y, "S2")

test_2y$tedenski_N <- tedenski_N(test_2y)

razlicni_zk <- rbind(turtle_s2(test_2y, 1000000), turtle_s2(test_2y, 100000), turtle_s2(test_2y, 10000), turtle_s2(test_2y, 1000))


##TRETJE LETO
test_3y <- btc_1day[754:1119,]
test_3y <- izracun_N(test_3y)

test_3y$entry20 <- entry_s1(test_3y)
test_3y$entry55 <- entry_s2(test_3y)
g_vstopi(test_3y, "S2")

test_3y$tedenski_N <- tedenski_N(test_3y)

razlicni_zk <- rbind(turtle_s2(test_3y, 1000000), turtle_s2(test_3y, 100000), turtle_s2(test_3y, 10000), turtle_s2(test_3y, 1000))


btc_dobicki5let_1dan <- pregled_5ih_let(btc_1day, 1000000, 1, "S1")
save(S1skala_1dan, file = "dataS1dobicki_1dan.Rda")
S1skala_1h <- pregled_5ih_let(btc_1h, 1000000, 24, "S1")
save(S1skala_1h, file = "dataS1dobicki_1h.Rda")
S1skala_5min <- pregled_5ih_let(btc_5min, 1000000, 24*12, "S1")
save(S1skala_5min, file = "dataS1dobicki_5min.Rda")

skala_1dan <- pregled_5ih_let(btc_1day, 1000000, 1, "S2")
save(skala_1dan, file = "dataS2dobicki_1dan.Rda")
skala_1h <- pregled_5ih_let(btc_1h, 1000000, 24, "S2")
save(skala_1h, file = "dataS2dobicki_1h.Rda")
skala_5min <- pregled_5ih_let(btc_5min, 1000000, 24*12, "S2")
save(skala_5min, file = "dataS2dobicki_5min.Rda")

###########
# DOBIČKI #
###########

#obdobje: 
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

btc_dobicki <- dobicki(btc_1day, 1000000, 360, "S1")

pregled_dobicki <- function(tabela, zacetni_kapital, strategija){
  obdobje <- c(360, 500, 1000)
  povprecje <- c()
  sd <- c()
  min <- c()
  max <- c()
  mean_proc <- c()
  for(i in obdobje){
    tmp <- dobicki(tabela, zacetni_kapital, i, strategija)
    povprecje <- c(povprecje, mean(tmp))
    sd <- c(sd, sd(tmp))
    min <- c(min, min(tmp))
    max <- c(max, max(tmp))
    mean_proc <- c(mean_proc, (povprecje/zacetni_kapital)*100)
  }
  pregled <- data.frame(strategija = rep(strategija, length(obdobje)), "povprecje" = povprecje, 
                        "sd" = sd, "min" = min, "max" = max, "povprecje.%" = mean_proc)
  pregled
}

pregled_dobicki(btc_1day, 1000000, "S1")
