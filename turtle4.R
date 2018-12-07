##########
#ETHEREUM#
##########

eth_dobicki_360_S1 <- dobicki_hitreje(tabela = eth_1day, obdobje = 360, strategija = "S1")
#eth_dobicki_360_S2 <- dobicki_hitreje(tabela = eth_1day, obdobje = 360, strategija = "S2")

##########
#LITECOIN#
##########

ltc_dobicki_360_S1 <- dobicki_hitreje(tabela = ltc_1day, obdobje = 360, strategija = "S1")
#ltc_dobicki_360_S2 <- dobicki_hitreje(tabela = ltc_1day, obdobje = 360, strategija = "S2")

########
#RIPPLE#
########

xrp_dobicki_360_S1 <- dobicki_hitreje(tabela = xrp_1day, obdobje = 360, strategija = "S1")
#xrp_dobicki_360_S2 <- dobicki_hitreje(tabela = xrp_1day, obdobje = 360, strategija = "S2")

########
#MONERO#
########

xmr_dobicki_360_S1 <- dobicki_hitreje(tabela = xmr_1day, obdobje = 360, strategija = "S1")
#xmr_dobicki_360_S2 <- dobicki_hitreje(tabela = xmr_1day, obdobje = 360, strategija = "S2")

#####
#NXT#
#####

nxt_dobicki_360_S1 <- dobicki_hitreje(tabela = nxt_1day, obdobje = 360, strategija = "S1")
#nxt_dobicki_360_S2 <- dobicki_hitreje(tabela = nxt_1day, obdobje = 360, strategija = "S2")

#######
#AUGUR#
#######

rep_dobicki_360_S1 <- dobicki_hitreje(tabela = rep_1day, obdobje = 360, strategija = "S1")
#rep_dobicki_360_S2 <- dobicki_hitreje(tabela = rep_1day, obdobje = 360, strategija = "S2")

pregled_coin <- function(vector, coin){
  data.frame("kovanec" = coin,
             "st_podatkov" = length(vector),
             "povprecje" = mean(vector),
             "sd" = sd(vector),
             "min" = min(vector),
             "max" = max(vector),
             "lsd" = paste0(round(cagr(tabela = mean(vector), obdobje = 360), 2), " %"),
             "st_manj_0" = sum(vector < 0),
             "verj_izgube" = paste0(round(sum(vector < 0)/length(vector), 2), " %"))
}

coin_pregled <- rbind(pregled_coin(eth_dobicki_360_S1, "ETH"), pregled_coin(ltc_dobicki_360_S1, "LTC"),
                      pregled_coin(xrp_dobicki_360_S1, "XRP"), pregled_coin(xmr_dobicki_360_S1, "XMR"),
                      pregled_coin(nxt_dobicki_360_S1, "NXT"), pregled_coin(rep_dobicki_360_S1, "REP"))

flextabela_pregled(coin_pregled, 0)

pregled_pred_po <- function(vector, coin, int1, int2, leto){
  ifelse(int1 < 100, t <- int2, t <- int1)
  data.frame("kovanec" = coin,
             "prelomnica" = leto,
             "lsd_pred" = paste0(round(cagr(tabela = mean(vector[1:t]), obdobje = 360), 2), " %"),
             "lsd_po" = paste0(round(cagr(tabela = mean(vector[t:length(vector)]), obdobje = 360), 2), " %"))
}

dobicki_v_casu(xrp_dobicki_360_S1/1000, 360)
ggplot(ltc_1day, aes(Timestamp))+
  geom_line(aes(x = Timestamp, y = Close))+
  theme_minimal()+
  ylab("Zaključna cena dneva")+
  xlab("Trgovalni dnevi")+
  ggtitle("Cena Kriptovalute Ethereum")

coin_pred_po <- rbind(pregled_pred_po(eth_dobicki_360_S1, "ETH", 0, 148, "2016"),
                      pregled_pred_po(ltc_dobicki_360_S1, "LTC", 216, length(ltc_dobicki_360_S1), "konec 2015"),
                      pregled_pred_po(xmr_dobicki_360_S1, "XMR", 0, 531, "sredina 2016"),
                      pregled_pred_po(rep_dobicki_360_S1, "REP", 0, 131, "zacetek 2017"))
flextabela_pregled(coin_pred_po, 0)


# Fitanje porazdelitve
library(MASS)
hist(btc_dobicki_360_S1, breaks = 100)
x <- seq(0, 2000, length=2000)
y <- 934000*dnorm(x, mean=fitdistr(btc_dobicki_360_S1, "normal")[[1]][[1]], 
                  sd=fitdistr(btc_dobicki_360_S1, "normal")[[1]][[2]])
y <- 5111*dcauchy(x, location = fitdistr(btc_dobicki_360_S1, "cauchy")$estimate[[1]], 
                  scale = fitdistr(btc_dobicki_360_S1, "cauchy")$estimate[[2]])
y <- 23965.5*dgamma(x, shape = fitdistr(btc_dobicki_360_S1, "gamma")$estimate[[1]], 
                    rate = fitdistr(btc_dobicki_360_S1, "gamma")$estimate[[2]])
lines(x, y, lwd = 1.5)
plot(x, y, type = "l", lwd = 1.5)



# Histogrami pred in po letu 2014 - ne ustrezajo nobeni znani porazdelitvi
ggplot(data.frame(btc_dobicki_360_S1[700:1984]))+
  geom_histogram(aes(btc_dobicki_360_S1[700:1984]), bins = 200)+
  #scale_y_log10()+
  theme_minimal()+
  ggtitle(paste0(360, " dnevno trgovanje" ))+
  xlab("Dobiček v 1000")+
  ylab("Število obdobij trgovanja")+
  theme(plot.title = element_text(hjust = 0.5))




# Lahko bi na roke pogledali kje je dobiček z verjetnostjo 60%, 90%
proba <- as.data.frame(table(cut(btc_dobicki_360_S1, breaks=seq(0, 2200, 10)), useNA='ifany')/1984)
#proba <- as.data.frame(table(cut(btc_dobicki_360_S1[1:700], breaks=seq(0, 2200, 10)), useNA='ifany')/700)

izracun_int <- function(povprecje, frekvenca, verj){
  zacetek <- ceiling(povprecje*2)
  vsota <- frekvenca$Freq[zacetek]
  i <- 1
  while(round(vsota,1) < verj){
    if((zacetek - i) <= 0 ){
      vsota <- vsota + frekvenca$Freq[zacetek + i]
      int1 <- 0
      }
    else{
      if((zacetek + i) >= nrow(frekvenca)){
        vsota <- vsota + frekvenca$Freq[zacetek - i]
        int2 <- nrow(frekvenca)
        }
      else{
        vsota <- vsota + frekvenca$Freq[zacetek - i] + frekvenca$Freq[zacetek + i]
        int1 <- zacetek - i
        int2 <- zacetek + i
        }
    }
    i <- i + 1
  }
  c(int1, int2)
}

interval <- function(vector, pred_po, verj, obdobje){
  if(pred_po == "pred"){
    ifelse(obdobje == 1800, len <- length(vector), len <- 700)
    vector <- vector[1:len]
    povprecje <- mean(vector)
    frekvenca <- as.data.frame(table(cut(vector, breaks=seq(0, 2200, 0.5)), useNA='ifany')/(length(vector)))
    int <- izracun_int(povprecje, frekvenca, verj)
    ifelse(int[1] == 0, zacetek_int <- 0, zacetek_int <- (int[1]/2 - 0.5)*1000)
    interval <- paste0("[", zacetek_int, ",", (int[2]/2)*1000, "]")
  }
  else{
    if(obdobje == 1800){interval <- "/"}
    else{vector <- vector[701:length(vector)]
      povprecje <- mean(vector)
      frekvenca <- as.data.frame(table(cut(vector, breaks=seq(0, 2200, 0.5)), useNA='ifany')/(length(vector)))
      int <- izracun_int(povprecje, frekvenca, verj)
      ifelse(int[1] == 0, zacetek_int <- 0, zacetek_int <- (int[1]/2 - 0.5)*1000)
      interval <- paste0("[", zacetek_int, ",", (int[2]/2)*1000, "]")
      }
  }
  interval
}

pregled_int <- data.frame("sistem_obdobje" = c("S1, 360", "S1, 500", "S1, 1000", "S1, 1800"),
                          "pred_2014_0.6" = c(interval(btc_dobicki_360_S1, "pred", 0.6, 360),
                                              interval(btc_dobicki_500_S1, "pred", 0.6, 500),
                                              interval(btc_dobicki_1000_S1, "pred", 0.6, 1000),
                                              interval(btc_dobicki_1800_S1, "pred", 0.6, 1800)),
                          "po_2014_0.6" = c(interval(btc_dobicki_360_S1, "po", 0.6, 360),
                                            interval(btc_dobicki_500_S1, "po", 0.6, 500),
                                            interval(btc_dobicki_1000_S1, "po", 0.6, 1000),
                                            interval(btc_dobicki_1800_S1, "po", 0.6, 1800)),
                          "pred_2014_0.9" = c(interval(btc_dobicki_360_S1, "pred", 0.9, 360),
                                              interval(btc_dobicki_500_S1, "pred", 0.9, 500),
                                              interval(btc_dobicki_1000_S1, "pred", 0.9, 1000),
                                              interval(btc_dobicki_1800_S1, "pred", 0.9, 1800)),
                          "po_2014_0.9" = c(interval(btc_dobicki_360_S1, "po", 0.9, 360),
                                            interval(btc_dobicki_500_S1, "po", 0.9, 500),
                                            interval(btc_dobicki_1000_S1, "po", 0.9, 1000),
                                            interval(btc_dobicki_1800_S1, "po", 0.9, 1800)))

flextabela_int <- function(tabela, st_decimalk){
  tabela <- regulartable(tabela)
  tabela <- bg(tabela, bg = "coral", part = "header")
  tabela <- bg(tabela, bg = "cyan", part = "body")
  tabela <- bold(tabela, part = "header")
  tabela <- align(tabela, align = "center", part = "all")
  ifelse(st_decimalk == 0, tabela <- set_formatter_type(tabela, fmt_double = "%.00f"), 
         ifelse(st_decimalk == 1, tabela <- set_formatter_type(tabela, fmt_double = "%.01f"), 
                tabela <- set_formatter_type(tabela, fmt_double = "%.03f")))
  tabela <- width(tabela, j = 1, width = .5)
  tabela
}

flextabela_int(pregled_int, 0)


# Cumulative distribution function
cdf <- c()
vsota <- 0
for(i in 1:nrow(proba)){
  vsota <- vsota + proba$Freq[i]
  cdf <- c(cdf, vsota)
}
proba$cdf <- cdf
plot(x = as.numeric(proba$Var1), y = proba$cdf, type = "l")
