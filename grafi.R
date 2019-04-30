library(gridExtra)
library(ggplot2)
library(rpatrec)

###########################
# Graf zaključne cene BTC #
###########################

ggplot(btc_1day, aes(Timestamp))+
  geom_line(aes(x = Timestamp, y = Close))+
  theme_minimal()+
  ylab("Zaključna cena dneva")+
  xlab("Trgovalni dnevi")+
  ggtitle("Cena kriptovalute Bitcoin")+
  theme(plot.title = element_text(hjust=0.5))


#######################################
# Graf kandidatov za vstop v pozicijo #
#######################################

g_vstopi <- function(tabela, indikator){
  ifelse(indikator == "zelve_s1", ind <- which(tabela$entry == 1), ind <- which(tabela$entry == 1))
  ifelse(indikator == "zelve_s1", ind1 <- which(tabela$entry == 2), ind1 <- which(tabela$entry == 2))
  ggplot(tabela)+
    geom_line(aes(Timestamp, Close))+
    geom_point(data = tabela[ind,], aes(x = Timestamp, y = Close), fill="red", color="red", shape = 21, size = 1.5)+
    geom_point(data = tabela[ind1, ], aes(x = Timestamp, y = Close), fill="blue", color="blue", shape = 21, size = 1.5)+
    theme_minimal()+
    ylab("Zaključna cena dneva")+
    xlab("Trgovalni dnevi")+
    ggtitle("Vstopni signali")+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(hjust = 0.7))
}

# function poracuni -> koda.R
g_vstopi(poracuni(btc_1day, 20, 20, 55, 10, "Close", 10, 50, 0.02, 3, "zelve_s1", "zelve")[1983:2343,], "zelve_s1")
g_vstopi(poracuni(btc_1day, 20, 20, 55, 20, "Close", 10, 50, 0.02, 3, "zelve_s2", "zelve")[1983:2343,], "zelve_s2")



############################################
# graf vstopov, dodajanja enot in izstopov #
############################################

# function trgovanje -> odkleni vse, razen profit in data.frame(Profit = profit1, kdaj = kdaj_profit)
vdi <- poracuni(btc_1day, 20, 20, 55, 10, "Close", 10, 50, 0.02, 3, "zelve_s1", "zelve")[1983:2343,]
vdi_s1 <- trgovanje(vdi, 1000000, odlocitev_cena(vdi, "Close"), 0.5, 2, "zelve_s1")
vdi1 <- poracuni(btc_1day, 20, 20, 55, 20, "Close", 10, 50, 0.02, 3, "zelve_s2", "zelve")[1983:2343,]
vdi_s2 <- trgovanje(vdi1, 1000000, odlocitev_cena(vdi1, "Close"), 0.5, 2, "zelve_s2")


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

##################################
# Histogrami razpršitve dobičkov #
##################################


g_dobicki <- function(vrednosti, st_bin, obdobje){
  vrednosti <- vrednosti/1000000
  q <- quantile(vrednosti, probs = seq(0.25, 0.75, 0.25))
  ggplot(data.frame(vrednosti))+
    geom_histogram(aes(vrednosti), bins = st_bin)+
    geom_vline(xintercept = q, col = "green")+
    #scale_y_log10()+
    theme_minimal()+
    ggtitle(paste0(obdobje, "-dnevno trgovanje" ))+
    xlab("Dobiček v 1.000.000")+
    ylab("Število obdobij trgovanja")+
    theme(plot.title = element_text(hjust = 0.5))
}

grid.arrange(g_dobicki(btc_360_S1, 200, 360), g_dobicki(btc_500_S1, 200, 500), 
             g_dobicki(btc_1000_S1, 200, 1000), g_dobicki(btc_1800_S1, 200, 1800), 
             nrow = 2, ncol = 2)

grid.arrange(g_dobicki(btc_360_S1[btc_360_S1 > 30000000], 50, 360),
             g_dobicki(btc_500_S1[btc_500_S1 > 30000000], 50, 500),
             g_dobicki(btc_1000_S1[btc_1000_S1 > 30000000], 50, 1000), 
             g_dobicki(btc_1800_S1[btc_1800_S1 > 30000000], 50, 1800),
             ncol = 2, nrow = 2)

grid.arrange(g_dobicki(btc_360_S1[btc_360_S1 < 5000000], 100, 360),
             g_dobicki(btc_500_S1[btc_500_S1 < 5000000], 100, 500),
             g_dobicki(btc_1000_S1[btc_1000_S1 < 5000000], 100, 1000), 
             g_dobicki(btc_1800_S1[btc_1800_S1 < 50000000], 100, 1800), 
             ncol = 2, nrow = 2)


#########################
# grafi dobičkov v času #
#########################

dobicki_v_casu <- function(vrednosti, obdobje, prikaz = 1){
  vrednosti <- vrednosti/1000000
  ifelse(prikaz == 500, cas <- "06.06.2013", cas <- "23.01.2012")
  ggplot(data.frame(vrednosti))+
    geom_point(aes(x = (1:length(vrednosti)), y = vrednosti), fill="blue", color="blue", shape = 20, size = 0.01)+
    #scale_y_log10()+
    theme_minimal()+
    ggtitle(paste0("Dobički skozi čas", ", ",obdobje, " dnevno trgovanje" ))+
    ylab("Dobički v 1.000.000")+
    xlab(paste0("Čas (začetek = ", cas, ")"))+
    theme(plot.title = element_text(hjust = 0.9))
}

grid.arrange(dobicki_v_casu(btc_360_S1, 360), dobicki_v_casu(btc_500_S1, 500), 
             dobicki_v_casu(btc_1000_S1, 1000), dobicki_v_casu(btc_1800_S1, 1800), 
             nrow = 2, ncol = 2)

grid.arrange(dobicki_v_casu(btc_360_S1[500:1500], 360, 500), 
             dobicki_v_casu(btc_500_S1[500:1500], 500, 500), 
             nrow = 1, ncol = 2)



###################
# Primer glajenja #
###################

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


#####################
# Mere volatilnosti #
#####################

plot(x = tabela$Timestamp[1:700], y = vol_close[23:722], type = "l", 
     ylim = c(0, max(vol_close[23:722], vol_gk[23:722], vol_par[23:722], vol_rs[23:722], 
                     vol_gkyz[23:722], vol_yz[23:722], ATR_C_N[1:700], ATR_SMA_N[1:700], TR_C_N[1:700])),
     ylab = "Volatilnost",
     xlab = "Trgovalni dnevi",
     main = "Mere volatilnosti")
lines(x = tabela$Timestamp[1:700], y = vol_gk[23:722], col = 2)
lines(x = tabela$Timestamp[1:700], y = vol_par[23:722], col = 3)
lines(x = tabela$Timestamp[1:700], y = vol_rs[23:722], col = 4)
lines(x = tabela$Timestamp[1:700], y = vol_gkyz[23:722], col = 5)
lines(x = tabela$Timestamp[1:700], y = vol_yz[23:722], col = 6)
lines(x = tabela$Timestamp[1:700], y = ATR_C_N[1:700], col = 7)
lines(x = tabela$Timestamp[1:700], y = ATR_SMA_N[1:700], col = 8)
lines(x = tabela$Timestamp[1:700], y = TR_C_N[1:700], col = "darkgreen")
legend("topleft", c("close", "gk", "par", "rs", "gkyz", "yz", "ATR/C", "ATR/SMA", "TR/C"), 
       col = c(1:8, "darkgreen"), bty = "n", lwd = 1.5, cex = 0.7)







































