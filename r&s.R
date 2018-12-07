##########################
# Support and Resistance #
##########################

# lokalne minimume najdemo lahko tudi z drugimi odvodi kernel regression-a (glej kodo npr. na githubu od SIT)
# lokalni minimumi in maksimumi
#set.seed(123)
lok_ekstremi <- function(tabela, cena, velikost_oken){
  casovna_vrsta <- cena[,1]
  N <- length(casovna_vrsta)
  stp <- velikost_oken
  st_okvirjev <- floor(N/stp)
  minz <- array(0.0, dim=st_okvirjev)
  whichMinz <- array(0, dim=st_okvirjev)
  maxz <- array(0.0, dim=st_okvirjev)
  whichMaxz = array(0, dim=st_okvirjev)
  for(j in 1:(st_okvirjev-1)){
    lft <- (j-1)*stp + 1  #left and right elements of each chunk
    rght <- j*stp
    whichMinz[j] <- which.min(casovna_vrsta[lft:rght]) + lft - 1
    minz[j] <- min(casovna_vrsta[lft:rght])
    whichMaxz[j] <- which.max(casovna_vrsta[lft:rght]) + lft - 1
    maxz[j] <- max(casovna_vrsta[lft:rght])
  }   
  #zadnji okvir
  lft <- j*stp + 1  #left and right elements of each chunk
  rght <- N
  whichMinz[st_okvirjev] <- which.min(casovna_vrsta[lft:rght]) + lft - 1
  minz[st_okvirjev] <- min(casovna_vrsta[lft:rght])
  whichMaxz[st_okvirjev] <- which.max(casovna_vrsta[lft:rght]) + lft - 1
  maxz[st_okvirjev] <- max(casovna_vrsta[lft:rght])
  list("kje_min" = whichMinz, "minz" = minz, "kje_max" = whichMaxz, "maxz" = maxz)
}

library(Ckmeans.1d.dp)
prepoznavalnik_SinR <- function(tabela, cena, velikost_oken){
  eks <- lok_ekstremi(tabela, cena, velikost_oken)
  minz <- eks$minz
  maxz <- eks$maxz
  
  #clustering
  mera_podpora <- c()
  mera_odpor <- c()
  for(i in 10:80){
    fit <- kmeans(minz, i)
    fit1 <- kmeans(maxz, i)
    # fit <- Ckmeans.1d.dp(minz, i)
    # fit <- Ckmeans.1d.dp(maxz, i)
    mera_podpora <- c(mera_podpora, fit$tot.withinss)
    mera_odpor <- c(mera_odpor, fit1$tot.withinss)
  }
  st_skupin_podpora <- 9 + which.min(mera_podpora)
  st_skupin_odpor <- 9 + which.min(mera_odpor)
  # tmp <- kmeans(minz, st_skupin_podpora)
  # tmp1 <- kmeans(maxz, st_skupin_odpor)
  tmp <- Ckmeans.1d.dp(minz, st_skupin_podpora)
  tmp1 <- Ckmeans.1d.dp(maxz, st_skupin_odpor)
  podpore <- tmp$centers
  odpori <- tmp1$centers
  list("podpore" = podpore, "odpori" = odpori, "odmiki_p" = tmp$tot.withinss, 
       "odmiki_o" = tmp1$tot.withinss, "moc_p" = tmp$size, "moc_o" = tmp1$size,
       "l_min" = minz, "l_max" = maxz, "kje_min" = eks$kje_min, "kje_max" = eks$kje_max)
}

SinR <- prepoznavalnik_SinR(tab, data.frame(log(tab$Close)), 10)
support <- data.frame("podpore" = SinR$podpore, "moc" = SinR$moc_p)
resistance <- data.frame("odpori" = SinR$odpori, "moc" = SinR$moc_o)
resistance <- resistance[order(resistance$odpori),]

# opt_velikost_oken <- function(){
#   dolzina <- c(3, 5, 7, 10, 15, 20)
#   odmiki_p <- c()
#   odmiki_o <- c()
#   for(i in dolzina){
#     tmp <- prepoznavalik_SinR(btc_1day, velikost_oken = i)
#     odmiki_p <- c(odmiki_p, tmp$odmiki_p)
#     odmiki_o <- c(odmiki_o, tmp$odmiki_o)
#   }
#   c(dolzina[which.min(odmiki_p)], dolzina[which.min(odmiki_o)])
# }

# dolzina_oken <- opt_velikost_oken()

ts.plot(log(tab$Close)[1:100])
points(SinR$kje_min, SinR$l_min, col="blue")
points(SinR$kje_max, SinR$l_max, col="red")
lines(x = 1:100, y = rep(resistance$odpori[1],100), col = "red")
lines(x = 1:100, y = rep(resistance$odpori[2],100), col = "red")
lines(x = 1:100, y = rep(resistance$odpori[3],100), col = "red")
lines(x = 1:100, y = rep(support$podpore[1],100), col = "blue")
lines(x = 1:100, y = rep(support$podpore[2],100), col = "blue")
lines(x = 1:100, y = rep(support$podpore[3],100), col = "blue")
lines(x = 1:100, y = rep(support$podpore[4],100), col = "blue")
lines(x = 1:100, y = rep(support$podpore[5],100), col = "blue")
lines(x = 1:100, y = rep(support$podpore[6],100), col = "blue")

# Vstopanje
#entry: 1=go long, 2=go short
#linija: 1 = podpora, 2 = odpor
#koliko_casa: oddaljenost od linije (koliko dni pozneje smo vstopili)
vstop_SinR <- function(tabela, cena, toleranca){

  podpore <- SinR$podpore
  odpori <- as.numeric(SinR$odpori)
  kje_min <- SinR$kje_min
  kje_max <- SinR$kje_max
  l_min <- as.numeric(SinR$l_min)
  l_max <- as.numeric(SinR$l_max)
  entry <- rep(0, nrow(tabela))
  linija <- rep(0, nrow(tabela))
  koliko_casa <- rep(0, nrow(tabela))
  
  # obrat podpora
  # prvi minimum izpustimo, saj ne vemo kakšen je trend
  for(i in (2:length(l_min))){
    real_time <- kje_min[i]
    opazovana_podpora <- podpore[which.min(abs(l_min[i]-podpore))]
    kje_se_dotika_te_podpore <- which(abs(l_min - opazovana_podpora) <= toleranca)
    if((abs(l_min[i] - opazovana_podpora) <= toleranca) & (kje_se_dotika_te_podpore[1] < i) & 
       (cena[real_time - 1,] > cena[real_time,]) & (cena[real_time + 1,] > cena[real_time,])){
      entry[real_time + 1] <- 1
      linija[real_time + 1] <- 1
      koliko_casa[real_time + 1] <- 1
      }
  }
  
  # obrat odpor
  # prvi maksimum izpustimo, saj ne vemo kakšen je trend
  for(i in (2:length(l_max))){
    real_time <- kje_max[i]
    opazovani_odpor <- odpori[which.min(abs(l_max[i]-odpori))]
    kje_se_dotika_tega_odpora <- which(abs(l_max - opazovani_odpor) <= toleranca)
    if((abs(l_max[i] - opazovani_odpor) <= toleranca) & (kje_se_dotika_tega_odpora[1] < i) & 
       (cena[real_time - 1,] < cena[real_time,]) & (cena[real_time + 1,] < cena[real_time,])){
      entry[real_time + 1] <- 2
      linija[real_time + 1] <- 2
      koliko_casa[real_time + 1] <- 1
    }
  }
  
  # preboj podpora
  for(i in 1:nrow(tabela)){
    # še pogoj da je že prej se en miminum dotikal tepodpore
    if((sum(cena[i,] == podpore) > 0) & (cena[i-1,] > cena[i,]) & (cena[i+1,] < cena[i,])){
      if((cena[i + 1, ] > opazovana_podpora - toleranca)){
        if(cena[i + 2, ] < opazovana_podpora - toleranca){
          entry[i + 2] <- 2
          linija[i + 2] <- 1
          koliko_casa[i + 2] <- 2
        }
        else{
          if(cena[i + 2, ] < opazovana_podpora){
          entry[i + 2] <- 1
          linija[i + 2] <- 1
          koliko_casa[i + 2] <- 2}
        }
      }
      else{
        entry[i + 1] <- 2
        linija[i + 1] <- 1
        koliko_casa[i + 1] <- 1}
    }
    
    # preboj odpor
    if((sum(cena[i,] == odpori) > 0) & (cena[i-1,] < cena[i,]) & (cena[i+1,] > cena[i,])){
      if((cena[i + 1, ] < opazovani_odpor + toleranca)){
        if(cena[i + 2, ] > opazovani_odpor + toleranca){
          entry[i + 2] <- 1
          linija[i + 2] <- 2
          koliko_casa[i + 2] <- 2
        }
        else{
          if(cena[i + 2, ] > opazovani_odpor){
            entry[i + 2] <- 2
            linija[i + 2] <- 2
            koliko_casa[i + 2] <- 2}
        }
      }
      else{
        entry[i + 1] <- 1
        linija[i + 1] <- 2
        koliko_casa[i + 1] <- 1}
    }
  }
  list("entry" = entry, "linija" = linija, "koliko_casa" = koliko_casa)
}

# Izstopanje iz zmagovalne pozicije
# rr = risk/reward ... npr. če se odločimo za 1:3 je rr = 3
win_izstop_SinR <- function(tabela, cena, rr, toleranca){
  # to funkcijo moraš dat preden razdeljuješ tab na obdobja (npr. 1:360)
  # poračuna vse izstope
  # na posameznem obdobju glej samo izstope iz tega obdobja
  podpore <- SinR$podpore
  odpori <- as.numeric(SinR$odpori)
  izstop <- rep(0, nrow(tabela))
  for(i in 1:nrow(tabela)){
    if(tabela$entry[i] == 1 & tabela$linija[i] == 1){
      izstop[i] <- kje_max[kje_max > i][1]
    }
    if(tabela$entry[i] == 2 & tabela$linija[i] == 2){
      izstop[i] <- kje_min[kje_min > i][1]
    }
    if(tabela$entry[i] == 2 & tabela$linija[i] == 1){
      #opazovana_podpora <- podpore[which.min(abs(cena[i-koliko_casa[i],]-podpore))]
      #cilj <- opazovana_podpora - sl*rr*tabela$spr_tedenski_N[i]
      cilj <- cena[i,]- sl*rr*tabela$spr_tedenski_N[i]
      kandidati_cilj <- which(cena - cilj <= toleranca)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
    if(tabela$entry[i] == 1 & tabela$linija[i] == 2){
      #opazovani_odpor <- odpori[which.min(abs(cena[i-koliko_casa[i],]-odpori))]
      #cilj <- opazovani_odpor + sl*rr*tabela$spr_tedenski_N[i]
      cilj <- cena[i,] + sl*rr*tabela$spr_tedenski_N[i]
      kandidati_cilj <- which(cena - cilj <= toleranca)
      izstop[i] <- kandidati_cilj[kandidati_cilj > i][1]
    }
  }
  izstop
}





#############################################################################################################
# package quantmod, različne vrste clusteringov, detekcija S&R - preizkušanje

library(quantmod)
#https://github.com/joshuaulrich/quantmod/tree/master/R
#https://cran.r-project.org/web/packages/quantmod/quantmod.pdf

getSymbols("SPY", from="2012-01-01", to="2012-06-15")
chartSeries(SPY, theme="white")


#getSymbols.rda("databtc_1day", from="2013-01-01", to="2013-06-15")
#proba1 <- getSymbols.csv("podatki/BTC_USD_min", env = .GlobalEnv)
getSymbols("BTC-USD")
chartSeries(`BTC-USD`[1:100,], theme = "white", TA = "addWMA()")
addEMA()


##############
# Clustering #
##############
# Podatki
podatki <- btc_1day[, c(-1)]
podatki <- na.omit(podatki)
#podatki <- scale(podatki)


library("factoextra")
# Optimal number of clusters
wss <- (nrow(podatki)-1)*sum(apply(podatki,2,var))
for (i in 2:30) wss[i] <- sum(kmeans(podatki, centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#ali:
fviz_nbclust(podatki, kmeans, method = "gap_stat")
#ali:
library("magrittr")
library("NbClust")
res.nbclust <- podatki %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 30, 
          method = "complete", index ="all")
fviz_nbclust(res.nbclust, ggtheme = theme_minimal())


# Partitioning clustering
# Hartigan and Wong algorithm
fit <- kmeans(podatki, 30)
aggregate(podatki,by=list(fit$cluster),FUN=mean)
#isto je fit$centers
#podatki <- data.frame(podatki, fit$cluster)
library(cluster) 
clusplot(podatki, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(podatki, fit$cluster)

km.res <- kmeans(podatki, 30, nstart = 25)
fviz_cluster(km.res, data = podatki,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())


# Hierarchical Clustering
d <- dist(podatki, method = "euclidean")
fit <- hclust(d, method="ward.D") 
plot(fit)
groups <- cutree(fit, k=30)
rect.hclust(fit, k=30, border="red")
#ali:
fviz_dend(fit, k = 30,cex = 0.5, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, rect = TRUE)


# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
podatki1 <- data.frame(podatki[,c(1, 4)])
fit <- pvclust(podatki, method.hclust="ward.D2",
               method.dist="euclidean")
plot(fit)
pvrect(fit, alpha=.95)


# Model Based Clustering
library(mclust)
fit <- Mclust(podatki)
plot(fit) # plot results 
summary(fit) # display the best model


# Clustering validation
res.hc <- iris[, -5] %>%
  scale() %>%
  eclust("hclust", k = 3, graph = FALSE)
fviz_silhouette(res.hc)


# Bitcoin
fit <- kmeans(podatki, 50)
skupine <- data.frame(povprecje = fit$centers[,4], velikost = fit$size)
kaj_graf <- btc_1day[1:100,]
plot(x = kaj_graf$Timestamp, y = kaj_graf$Close, type = "p")
abline(h = skupine$povprecje, col = "red")


# Drugi primeri

# Fuzzy clustering - en element lahko v večih clustrih
library(cluster) # Loads the cluster library.
fannyy <- fanny(podatki, k=4, metric = "euclidean", memb.exp = 1.2)
round(fannyy$membership, 2)[1:4,]
fannyy$clustering 

# Principal component analysis
pca <- prcomp(podatki, scale=T)
summary(pca)
plot(pca$x, pch=20, col="blue", type="n") # To plot dots, drop type="n"
text(pca$x, rownames(pca$x), cex=0.8)

# Multidimensional Scaling
library(stats)
loc <- cmdscale(dist(podatki, method = "euclidean")) 
plot(loc[,1], -loc[,2], type="n", xlab="", ylab="", main="cmdscale(podatki)")
text(loc[,1], -loc[,2], rownames(loc), cex=0.8) 



## Support and Resistance

detectSupportResistance <- function(timeSeries, tolerance=0.01, nChunks=10, nPoints=3, plotChart=TRUE)
{
  #detect maximums and minimums
  N = length(timeSeries)
  stp = floor(N / nChunks)
  minz = array(0.0, dim=nChunks)
  whichMinz = array(0, dim=nChunks)
  maxz = array(0.0, dim=nChunks)
  whichMaxz = array(0, dim=nChunks)
  for(j in 1:(nChunks-1)) 
  {
    lft = (j-1)*stp + 1  #left and right elements of each chunk
    rght = j*stp
    whichMinz[j] = which.min(timeSeries[lft:rght]) + lft
    minz[j] = min(timeSeries[lft:rght])
    whichMaxz[j] = which.max(timeSeries[lft:rght]) + lft
    maxz[j] = max(timeSeries[lft:rght])
  }   
  #last chunk
  lft = j*stp + 1  #left and right elements of each chunk
  rght = N
  whichMinz[nChunks] = which.min(timeSeries[lft:rght]) + lft
  minz[nChunks] = min(timeSeries[lft:rght])
  whichMaxz[nChunks] = which.max(timeSeries[lft:rght]) + lft
  maxz[nChunks] = max(timeSeries[lft:rght])
  
  result = list()
  result[["minima"]] = NULL
  result[["minimaAt"]] = NULL
  result[["maxima"]] = NULL
  result[["maximaAt"]] = NULL
  span = tolerance*(max(maxz) - min(minz))
  
  rang = order(minz)[1:nPoints]
  if((minz[rang[nPoints]] - minz[rang[1]]) <= span)
  {
    result[["minima"]] = minz[rang[1:nPoints]]
    result[["minimaAt"]] = whichMinz[rang[1:nPoints]]
  } 
  
  rang = order(maxz, decreasing = TRUE)[1:nPoints]
  if((maxz[rang[1]] - maxz[rang[nPoints]]) <= span)
  {
    result[["maxima"]] = maxz[rang[1:nPoints]]
    result[["maximaAt"]] = whichMaxz[rang[1:nPoints]]
  } 
  
  if(plotChart)
  {
    ts.plot(timeSeries)
    points(whichMinz, minz, col="blue")
    points(whichMaxz, maxz, col="red")
    if(!is.null(result[["minima"]])  &&  !is.null(result[["minimaAt"]]))
      abline(lm(result[["minima"]] ~  result[["minimaAt"]]))
    if(!is.null(result[["maxima"]])  &&  !is.null(result[["maximaAt"]]))
      abline(lm(result[["maxima"]] ~  result[["maximaAt"]]))
  } 
  
  return(result)    
}

# ni dobra funkcija, ker si sam zbereš okvirje na katerih izračuna min in max
# torej, če si zbral celotno dolžino timeSeries 100 in okvirjev 10 bo min in max manj kot če si zbral okvirjev 50
# support izriše le če so prve tri točke znotraj tolerance in resistance če so zadnje tri točke
# za našo analizo bi mogel spremenit funkcijo, da pogleda vsake tri točke
detectSupportResistance(timeSeries = btc_1day$Close[100:200], tolerance = 0.03, nChunks = 10, nPoints = 3, plotChart = T)
detectSupportResistance(timeSeries = btc_1day$Close[120:200], tolerance = 0.03, nChunks = 10, nPoints = 3, plotChart = T)

