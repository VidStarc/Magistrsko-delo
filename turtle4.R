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
#proba <- as.data.frame(table(cut(btc_dobicki_360_S1, breaks=seq(0, 2200, 10)), useNA='ifany')/1984)
proba <- as.data.frame(table(cut(btc_dobicki_360_S1[1:700], breaks=seq(0, 2200, 10)), useNA='ifany')/700)
cdf <- c()
vsota <- 0
for(i in 1:nrow(proba)){
  vsota <- vsota + proba$Freq[i]
  cdf <- c(cdf, vsota)
}
proba$cdf <- cdf
plot(x = as.numeric(proba$Var1), y = proba$cdf, type = "l")
