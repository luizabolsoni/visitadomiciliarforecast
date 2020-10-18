
getwd()
library(readr)
library(tidyverse)
library(forecast)


vd_luisa_tcc <- read_csv("vd_luisa_tcc.csv")

vd_luisa_tcc <- read_delim("vd_luisa_tcc.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)
vd_luisa_tcc$MES_ANO <- paste0(vd_luisa_tcc$MES, "-", vd_luisa_tcc$ANO)

vd_luisa_tcc$MES_ANO

vd_luisa_tcc
florianopolis <- vd_luisa_tcc
unidades <- vd_luisa_tcc

florianopolis <- florianopolis %>% 
    group_by(MES_ANO) %>%
    summarise(VISITAS = sum(VISITAS, na.rm = TRUE))


ggplot(florianopolis, aes(MES_ANO, VISITAS, group = 1)) + 
    geom_line()

modelo1 <- auto.arima(florianopolis$VISITAS, lambda = 0 , biasadj = TRUE,
                      stepwise = FALSE, approximation = FALSE, ic = "aic")

modelo1 %>% summary()

modelo1 %>% forecast(h=20) %>% plot()

unidades <- unidades %>% 
    group_by(MES_ANO, UNIDADE) %>%
    summarise(VISITAS = sum(VISITAS, na.rm = TRUE))

modelo = list()
for (i in unique( unidades$UNIDADE)){ 
    projecao <- subset(unidades, unidades$UNIDADE == i )
    
    modelo[[i]] <- auto.arima(projecao$VISITAS, lambda = "auto", biasadj = TRUE,
                              stepwise = FALSE, approximation = FALSE, ic = "aic")
}

modelo[[1]] %>% summary()

modelo %>% summary()

modelo [[1]] %>% forecast(h=20) %>% plot()
