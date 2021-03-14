# visitadomiciliarforecast
previsão do número de visitas domiciliares para as próximas 20 observações(meses)

#instala e carrega pacote exigido
getwd()
library(readr)
library(tidyverse)
library(forecast)
library(lubridate)

vd_luisa_tcc <- read.csv("~/Desktop/vd_luisa_tcc.csv", sep=";")

vd_luisa_tcc <- read_delim("~/Desktop/vd_luisa_tcc.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

vd_luisa_tcc$MES_ANO <- paste0(vd_luisa_tcc$MES, "-", vd_luisa_tcc$ANO)


#organizar por mes/ano
aggregate(vd_luisa_tcc$VISITAS,list(unidade = vd_luisa_tcc$UNIDADE, MES_ANO = vd_luisa_tcc$MES_ANO),sum) -> tabela

tabela

#transforma em MES_ANO
tabela$MES_ANO = parse_date_time(tabela$MES_ANO,"my")
tabela$MES_ANO

tabela <- tabela[!(as.character(tabela$MES_ANO) %in% c("2019-03-01","2019-04-01","2019-05-01")),]

tabela


#ordena crono
tabela = tabela[order(tabela$MES_ANO),]

tabela


#meses de greve (em formato de MES_ANO)

greve = parse_date_time( c('15/05','16/03','17/01','17/02','18/04','18/05'),"ym")
greve

#repete para cada unidade
for(j in unique(tabela$unidade)) {
    
    #tabela temporaria com só um
    turnip = tabela[tabela$unidade==j,]
    
    #repete para cada mes
    for(i in turnip$MES_ANO ){
        
        #se for um mes de greve
        if (i %in% greve){
            
            #seleciona os meses anterior e posterior:
            
            
            ant= max(                                   # pega o maior mês (mais recente)
                turnip$MES_ANO[!(turnip$MES_ANO %in% greve) & # que não está em greve E
                                turnip$MES_ANO < i           # que é menor (antes) do atual
                ]                
            )
            
            #mesma lógica na linha de baixo 
            pos= min(turnip$MES_ANO[!(turnip$MES_ANO %in% greve) & turnip$MES_ANO > i])
            
            
            if(any(is.na(c(format(ant),format(pos))))) {         #testa se achou um mes antes e outro depois (pq pode não existir)
                
                NA -> tabela[tabela$MES_ANO==i & tabela$unidade==j,3]    #atribui NA nesse caso
                
            } else {         
            mean(c(turnip[turnip$MES_ANO==ant,3],turnip[turnip$MES_ANO==pos,3])) -> tabela[tabela$MES_ANO==i & tabela$unidade==j,3]     # se tiver mes antes e depois, tira média e atribui
            }
        }
    }
}
            
        #fecha if do mes de greve
    #fecha loop de MES_ANO
#fecha loop de unidade

tabela$greve =  tabela$MES_ANO %in% greve 
#marca os meses de greve na tabela, pq não?

#tabela$MES_ANO = format(tabela$MES_ANO,"%m/%Y") 
#transforma de volta em mês/ano. O negativo é que volta a ser caracter, o positivo é que é mais fácil de ler.

tabela
colnames(tabela) = c("UNIDADES","MES_ANO","VISITAS","GREVE")
novo_objeto <- tabela


florianopolis <- novo_objeto
unidades <- novo_objeto

florianopolis <- florianopolis %>% 
    group_by(MES_ANO) %>%
    summarise(VISITAS = sum(VISITAS, na.rm = TRUE))
florianopolis

#dev.off()

ggplot(florianopolis, aes(MES_ANO, VISITAS, group = 1)) + 
    geom_line()

modelo1 <- auto.arima(florianopolis$VISITAS, lambda = 0 , biasadj = TRUE,
                      stepwise = FALSE, approximation = FALSE, ic = "aic")

modelo1 %>% summary()


modelo1 %>% forecast(h=20) %>% plot()
modelo1 %>% forecast(h=20) %>% plot(main= "Projeção de visitas domiciliares em Florianópolis, para o período de 20 meses")
modelo1

title( xlab = "Meses observados" , ylab = "Número de visitas")

modelo2 <- modelo1 %>% forecast(h=20) 

modelo2

unidades <- unidades %>% 
    group_by(MES_ANO, UNIDADES) %>%
    summarise(VISITAS = sum(VISITAS, na.rm = TRUE))

modelo = list()
for (i in unique( unidades$UNIDADES)){ 
    ABRAAO <- subset(unidades, unidades$UNIDADES == i )
    
}

modelo = list()
for (i in unique( unidades$UNIDADES)){ 
    projecao <- subset(unidades, unidades$UNIDADES == i )

    }


modelo[[3]] %>% summary()

modelo %>% summary()

modelo

#ABRAAO
modelo [[1]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Abrãao, para o período de 20 meses")

title( xlab = "Meses observados" , ylab = "Número de visitas domiciliares")
modelo2 <- modelo[[1]] %>% forecast(h=20) 
modelo2 

#Agronomica
modelo [[2]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Agronômica, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[2]] %>% forecast(h=20) 
modelo2 

#alto ribeirão 
modelo [[3]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Alto Ribeirão, para o período de 20 meses")


title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[3]] %>% forecast(h=20) 
modelo2 

#armação 
modelo [[4]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Armação, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[4]] %>% forecast(h=20) 
modelo2 

# balneario 
modelo [[5]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Balneário, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[5]] %>% forecast(h=20) 
modelo2 
 
#BARRA  
modelo [[6]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Barra da Lagoa, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[6]] %>% forecast(h=20) 
modelo2 

#cachoeira 
modelo [[7]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Cachoeira do Bom Jesus, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[7]] %>% forecast(h=20) 
modelo2 

# campeche
modelo [[8]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Campeche, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[8]] %>% forecast(h=20) 
modelo2 

#canasvieiras
modelo [[9]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Canasvieiras, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[9]] %>% forecast(h=20) 
modelo2 

#carianos
modelo [[10]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Carianos, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[10]] %>% forecast(h=20) 
modelo2 

#centro 
modelo [[11]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Centro, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[11]] %>% forecast(h=20) 
modelo2 

#coloninha
modelo [[12]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Coloninha, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[12]] %>% forecast(h=20) 
modelo2 

#coqueiros 
modelo [[13]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Coqueiros, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[13]] %>% forecast(h=20) 
modelo2 

#corrego
modelo [[14]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Córrego Grande, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[14]] %>% forecast(h=20) 
modelo2 

#[15] "CS COSTA DA LAGOA"    
modelo [[15]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Costa da Lagoa, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[15]] %>% forecast(h=20) 
modelo2 

# 16 costeira
 
modelo [[16]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Costeira do Pirajubaé, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[16]] %>% forecast(h=20) 
modelo2 

# 17 estreito 

modelo [[17]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Estreito, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[17]] %>% forecast(h=20) 
modelo2 

# 18 fazenda RT 

modelo [[18]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Fazenda do Rio Tavares, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[18]] %>% forecast(h=20) 
modelo2 

# ingleses 

modelo [[19]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Ingleses, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[19]] %>% forecast(h=20) 
modelo2 

# 20 itacorubi 

modelo [[20]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Itacorubi, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[20]] %>% forecast(h=20) 
modelo2 

#21 jardim atlantico 


modelo [[21]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Jardim Atlântico, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[21]] %>% forecast(h=21) 
modelo2 

# 22 joao paulo 


modelo [[22]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde João Paulo, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[22]] %>% forecast(h=20) 
modelo2 

#23 jurere 

modelo [[23]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Jurerê, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[23]] %>% forecast(h=20) 
modelo2 

# 24 monte cristo

modelo [[24]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Monte Cristo, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[24]] %>% forecast(h=20) 
modelo2 

# 25 morro das pedras 


modelo [[25]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Morro das Pedras, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[25]] %>% forecast(h=20) 
modelo2 

# novo continente 


modelo [[26]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Novo Continente, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[26]] %>% forecast(h=20) 
modelo2 

# 27 pantanal 

modelo [[27]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Pantanal, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[27]] %>% forecast(h=20) 
modelo2 

# 28 pantano 

modelo [[28]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Pântano do Sul, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[28]] %>% forecast(h=20) 
modelo2 

# 29 prainha 

modelo [[29]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Prainha, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[29]] %>% forecast(h=20) 
modelo2 

# 30 ratones 

modelo [[30]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Ratones, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[30]] %>% forecast(h=20) 
modelo2 

# 31 ribeirao da ilha 

modelo [[31]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Ribeirão da Ilha, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[31]] %>% forecast(h=20) 
modelo2 

# 32 rio vermelho


modelo [[32]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Rio Vermelho, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[32]] %>% forecast(h=20) 
modelo2 

# 33 saco dos limões 

modelo [[33]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Saco dos Limões, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[33]] %>% forecast(h=20) 
modelo2 

# 34 saco grande

modelo [[34]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Saco Grande, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[34]] %>% forecast(h=20) 
modelo2 

# santinho 

modelo [[35]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Santinho, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[35]] %>% forecast(h=20) 
modelo2 

# sto antonio

modelo [[36]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Santo Antônio de Lisboa, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[36]] %>% forecast(h=20) 
modelo2 

#sape 

modelo [[37]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Sapé, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[37]] %>% forecast(h=20) 
modelo2 

# tapera 

modelo [[38]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Tapera, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[38]] %>% forecast(h=20) 
modelo2 

# trindade

modelo [[39]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Trindade, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[39]] %>% forecast(h=20) 
modelo2 

# canto da Lagoa 

modelo [[40]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Canto da Lagoa, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[40]] %>% forecast(h=20) 
modelo2 

# lagoa da conceição

modelo [[41]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Lagoa da Conceição, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[41]] %>% forecast(h=20) 
modelo2 

# Monte Serrat

modelo [[42]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Monte Serrat, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[42]] %>% forecast(h=20) 
modelo2 

# Ponta das canas

modelo [[43]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Ponta das Canas, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[43]] %>% forecast(h=20) 
modelo2 


# rio tavares

modelo [[44]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Rio Tavares, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[44]] %>% forecast(h=20) 
modelo2 

# vargem pequena

modelo [[45]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Vargem Pequena, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[45]] %>% forecast(h=20) 
modelo2 

# vila aparecida 
modelo [[46]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Vila Aparecida, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[46]] %>% forecast(h=20) 
modelo2 

# capoeiras

modelo [[47]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Capoeiras, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[47]] %>% forecast(h=20) 
modelo2 

# vargem grande  

modelo [[48]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Vargem Grande, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[48]] %>% forecast(h=20) 
modelo2 

# Caieira Barra do Sul

modelo [[49]] %>% forecast(h=20) %>% plot(main= "Projeção de atendimentos domiciliares no Centro de Saúde Caieira Barra do Sul, para o período de 20 meses")

title( xlab = "Meses Observados" , ylab = "Número de Atendimentos Domiciliares")
modelo2 <- modelo[[49]] %>% forecast(h=20) 
modelo2 
