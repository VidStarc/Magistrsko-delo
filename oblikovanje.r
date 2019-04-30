require(flextable)
require(dplyr)
tabela <- data.frame(a=c(1,2,3), povprecje=c(123.33, 1245.22, 123), lsd=c(1.2, 1.777, 1.6))
regulartable(tabela)


izpisTabele <- function(data) {
    ime <- "povprecje"
    if(ime %in% (tabela %>% names())) {
      data[ime] <- format(round(data[ime]), big.mark = ".", decimal.mark = ",")  
    }
    ime <- "lsd"
    if(ime %in% (tabela %>% names())) {
      tmp <- format(round(data[ime], 2),  big.mark = ".", decimal.mark = ",") %>% pull(1)
      data[ime] <- paste0(tmp, "%")
    }
    data %>% regulartable() %>% align(align="right", part="all")
}

tabela %>% izpisTabele
