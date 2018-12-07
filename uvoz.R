library(RJSONIO)
library(anytime)

#funkcija za minutne podatke
pretvori <- function(coin, enota, minute){
  #enota <- 5min: 300, 1h: 3600, 1day: 86400
  del <- as.numeric(coin$Timestamp)/enota
  ind <- which(del == round(del))
  high <- c()
  low <- c()
  for(i in 1:length(ind)){
    tmp <- coin$High[ind[i]:(ind[i]+minute-1)]
    tmp1 <- coin$Low[ind[i]:(ind[i]+minute-1)]
    high <- append(high, max(tmp))
    low <- append(low, min(tmp1))
  }
  data.frame(Timestamp = coin$Timestamp[ind],
             Open = coin$Open[ind],
             High = high,
             Low = low,
             Close = coin$Close[(ind+minute-1)])
}

#funkcija za 5minutne podatke
pretvori_5min <- function(coin, enota, minute){
  #enota <- 5min: 300, 1h: 3600, 1day: 86400
  enote <- enota/300
  minute <- minute/5
  del <- as.numeric(coin$date)/enota
  ind <- which(del == round(del))
  high <- c()
  low <- c()
  for(i in 1:length(ind)){
    tmp <- coin$high[ind[i]:(ind[i]+minute-1)]
    tmp1 <- coin$low[ind[i]:(ind[i]+minute-1)]
    high <- append(high, max(tmp))
    low <- append(low, min(tmp1))
  }
  data.frame(Timestamp = coin$date[ind],
             Open = coin$open[ind],
             High = high,
             Low = low,
             Close = coin$close[(ind+minute-1)])
}

#########
#BITCOIN#
#########

btc_min <- read.csv("podatki/BTC_USD_min.csv", header = T, sep = ",")
btc_min$Timestamp <- anytime(btc_min$Timestamp)

plot(btc_1day$Timestamp[1700:2200], btc_1day$Close[1700:2200], type = "l")
plot(btc_1day$Timestamp[300:800], btc_1day$Close[300:800], type = "l")

btc_5min <- pretvori(btc_min, 300, 5)
save(btc_5min, file = "databtc_5min.Rda")

btc_1h <- pretvori(btc_min, 3600, 60)
save(btc_1h, file = "databtc_1h.Rda")

btc_1day <- pretvori(btc_min, 86400, 60*24)
save(btc_1day, file = "databtc_1day.Rda")

##########
#ETHEREUM#
##########

eth <- fromJSON("podatki/eth.json")
eth <- do.call("rbind", eth)
eth_5min <- data.frame(eth)
eth_5min$date <- anytime(eth_5min$date)

save(eth_5min, file = "eth_5min.Rda")

eth_1h <- pretvori_5min(eth_5min, 3600, 60)
save(eth_1h, file = "eth_1h.Rda")

eth_1day <- pretvori_5min(eth_5min, 86400, (60*24))
save(eth_1day, file = "eth_1day.Rda")


##########
#LITECOIN#
##########

ltc <- fromJSON("podatki/ltc.json")
ltc <- do.call("rbind", ltc)
ltc_5min <- data.frame(ltc)
ltc_5min$date <- anytime(ltc_5min$date)

save(ltc_5min, file = "ltc_5min.Rda")

ltc_1h <- pretvori_5min(ltc_5min, 3600, 60)
save(ltc_1h, file = "ltc_1h.Rda")

ltc_1day <- pretvori_5min(ltc_5min, 86400, (60*24))
save(ltc_1day, file = "ltc_1day.Rda")


########
#RIPPLE#
########

xrp <- fromJSON("podatki/xrp.json")
xrp <- do.call("rbind", xrp)
xrp_5min <- data.frame(xrp)
xrp_5min$date <- anytime(xrp_5min$date)

save(xrp_5min, file = "xrp_5min.Rda")

xrp_1h <- pretvori_5min(xrp_5min, 3600, 60)
save(xrp_1h, file = "xrp_1h.Rda")

xrp_1day <- pretvori_5min(xrp_5min, 86400, (60*24))
save(xrp_1day, file = "xrp_1day.Rda")

########
#MONERO#
########

xmr <- fromJSON("podatki/xmr.json")
xmr <- do.call("rbind", xmr)
xmr_5min <- data.frame(xmr)
xmr_5min$date <- anytime(xmr_5min$date)

save(xmr_5min, file = "xmr_5min.Rda")

xmr_1h <- pretvori_5min(xmr_5min, 3600, 60)
save(xmr_1h, file = "xmr_1h.Rda")

xmr_1day <- pretvori_5min(xmr_5min, 86400, (60*24))
save(xmr_1day, file = "xmr_1day.Rda")

#####
#NXT#
#####

nxt <- fromJSON("podatki/nxt.json")
nxt <- do.call("rbind", nxt)
nxt_5min <- data.frame(nxt)
nxt_5min$date <- anytime(nxt_5min$date)

save(nxt_5min, file = "nxt_5min.Rda")

nxt_1h <- pretvori_5min(nxt_5min, 3600, 60)
save(nxt_1h, file = "nxt_1h.Rda")

nxt_1day <- pretvori_5min(nxt_5min, 86400, (60*24))
save(nxt_1day, file = "nxt_1day.Rda")


#######
#AUGUR#
#######

rep <- fromJSON("podatki/rep.json")
rep <- do.call("rbind", rep)
rep_5min <- data.frame(rep)
rep_5min$date <- anytime(rep_5min$date)

save(rep_5min, file = "rep_5min.Rda")

rep_1h <- pretvori_5min(rep_5min, 3600, 60)
save(rep_1h, file = "rep_1h.Rda")

rep_1day <- pretvori_5min(rep_5min, 86400, (60*24))
save(rep_1day, file = "rep_1day.Rda")


#graf
# library(ggplot2)
# btc_1day$barva <- ifelse(btc_1day$Open <= btc_1day$Close, "blue", "red")
# btc_1day$barva <- factor(btc_1day$barva, levels= c("red", "blue"))
# ggplot(btc_1day[1:360,], aes(Timestamp, fill=barva, colour=barva))+
#   geom_boxplot(aes(ymin = Low, lower = ifelse(Open < Close, Open, Close), middle = (Open+Close)/2,
#                    upper = ifelse(Open<Close, Close, Open), ymax = High), stat = "identity")+
#   theme_minimal()

ggplot(btc_1day, aes(Timestamp))+
  geom_line(aes(x = Timestamp, y = Close))+
  theme_minimal()+
  ylab("Zaključna cena dneva")+
  xlab("Trgovalni dnevi")+
  ggtitle("Cena Kriptovalute Bitcoin")



###################
# S & P 500 index #
###################
spx <- read.csv("podatki/^GSPC.csv", header = T, sep = ",")

# pretvorba za delo s strategijo Turtle
spx_1day <- spx[1:which(spx$Date == "2018-06-27"),]
spx_1day$Date <- anytime(spx_1day$Date)
