# pregled dobičkov
pregled_trgovanje <- function(l1, l2, l3, l4){
  l1 <- l1*1000
  l2 <- l2*1000
  l3 <- l3*1000
  l4 <- l4*1000
  st_podatkov <- c(length(l1), length(l2), length(l3), length(l4))
  povprecje <- c(mean(l1), mean(l2), mean(l3), mean(l4))
  sd <- c(sd(l1), sd(l2), sd(l3), sd(l4))
  min <- c(min(l1), min(l2), min(l3), min(l4))
  max <- c(max(l1), max(l2), max(l3), max(l4))
  lsd <- c(paste0(round(cagr(tabela = mean(l1), obdobje = 360), 2), " %"),
           paste0(round(cagr(tabela = mean(l2), obdobje = 500), 2), " %"),
           paste0(round(cagr(tabela = mean(l3), obdobje = 1000), 2), " %"),
           paste0(round(cagr(tabela = mean(l4), obdobje = 1800), 2), " %"))
  s_e <- c("360", "500", "1000", "1800")
  manj_0 <- c(sum(l1 < 0), sum(l2 < 0), sum(l3 < 0), sum(l4 < 0))
  verj <- paste0(round((manj_0/st_podatkov)*100, 2), " %")
  kolicnik <- as.character(round(povprecje/sd,2))
  pregled <- data.frame("obdobje" = s_e, "st_podatkov" = st_podatkov, "povprecje" = povprecje, 
                        "sd" = sd, "min" = min, "max" = max, "lsd" = lsd, "kolicnik" = kolicnik,
                        "st_manj_0" = manj_0, "verj_izgube" = verj)
  pregled
}


flextabela_pregled <- function(tabela, st_decimalk){
  library(flextable)
  tabela <- regulartable(tabela)
  tabela <- bg(tabela, bg = "coral", part = "header")
  tabela <- bg(tabela, bg = "cyan", part = "body")
  tabela <- bold(tabela, part = "header")
  tabela <- align(tabela, align = "center", part = "all")
  ifelse(st_decimalk == 0, tabela <- set_formatter_type(tabela, fmt_double = "%.00f"), 
         ifelse(st_decimalk == 1, tabela <- set_formatter_type(tabela, fmt_double = "%.01f"), 
                tabela <- set_formatter_type(tabela, fmt_double = "%.03f")))
  tabela
}

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
  data.frame("obdobje" = c("360", "500", "1000", "1800"), 
             "pred_2014" = pred_2014, "po_2014" = po_2014, 
             "psd_pred_2014" = paste0(cagr_pred_2014, " %"), 
             "psd_po_2014" = paste0(cagr_po_2014, " %"))
}

sd_pred_po_2014 <- function(l1, l2, l3, l4, predpo2014){
  ifelse(predpo2014 == 700, skala <- length(l4), skala <- predpo2014)
  sd_pred_2014 <- c(sd(l1[1:predpo2014]*1000), sd(l2[1:predpo2014]*1000), sd(l3[1:predpo2014]*1000), sd(l4[1:skala]*1000))
  pred_2014 <- c(mean(l1[1:predpo2014]*1000), mean(l2[1:predpo2014]*1000), mean(l3[1:predpo2014]*1000), mean(l4[1:skala]*1000))
  kolicnik_pred_2014 <- as.character(round(pred_2014/sd_pred_2014,2))
  ifelse(predpo2014 == 700, skala2 <- 0, skala2 <- sd(l4[(predpo2014+1):length(l4)]*1000))
  sd_po_2014 <- c(sd(l1[(predpo2014+1):length(l1)]*1000), sd(l2[(predpo2014+1):length(l2)]*1000), sd(l3[(predpo2014+1):length(l3)]*1000), skala2)
  ifelse(predpo2014 == 700, skala1 <- 0, skala1 <- mean(l4[(predpo2014+1):length(l4)]*1000))
  po_2014 <- c(mean(l1[(predpo2014+1):length(l1)]*1000), mean(l2[(predpo2014+1):length(l2)]*1000), mean(l3[(predpo2014+1):length(l3)]*1000), skala1)
  kolicnik_po_2014 <- as.character(round(po_2014/sd_po_2014,2))
  if(kolicnik_po_2014[length(kolicnik_po_2014)] == "NaN"){kolicnik_po_2014[length(kolicnik_po_2014)] <- ""}
  data.frame("obdobje" = c("360", "500", "1000", "1800"), 
             "pred_2014" = sd_pred_2014, "po_2014" = sd_po_2014, 
             "kolicnik_pred" = kolicnik_pred_2014, "kolicnik_po" = kolicnik_po_2014)
}
