library(tidyverse)
library(onsvplot)
library(plotly)
library(cowplot)
library(stringi)
library(knitr)


# BAIXANDO OS DADOS E IMPORTANDO ####

#url2011 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2011.zip"
#url2012 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2012.zip"
#url2013 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2013.zip"
#url2014 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2014.zip"
#url2015 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2015.zip"
#url2016 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2016_atual.zip"
#url2017 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2017.zip"
#url2018 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2018.zip"
#url2019 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2019.zip"
#url2020 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2020.zip"
#url2021 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2021.zip"
#url2022 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2022.zip"
#url2023 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2023.zip"

# NA PRIMEIRA VEZ QUE FOR RODAR ESSE SCRIPT, DEVE-SE RETIRAR O HASHTAG DA FRENTE DOS DOWNSLOAD.FILE e DOS UNZIP
#download.file(url2011, "acidentes2011.zip")
#download.file(url2012, "acidentes2012.zip")
#download.file(url2013, "acidentes2013.zip")
#download.file(url2014, "acidentes2014.zip")
#download.file(url2015, "acidentes2015.zip")
#download.file(url2016, "acidentes2016_atual.zip")
#download.file(url2017, "acidentes2017.zip")
#download.file(url2018, "acidentes2018.zip")
#download.file(url2019, "acidentes2019.zip")
#download.file(url2020, "acidentes2020.zip")
#download.file(url2021, "acidentes2021.zip")
#download.file(url2022, "acidentes2022.zip")
#download.file(url2023, "acidentes2023.zip")

#unzip(zipfile = "data-raw/acidentes2011.zip")
#unzip(zipfile = "data-raw/acidentes2012.zip")
#unzip(zipfile = "data-raw/acidentes2013.zip")
#unzip(zipfile = "data-raw/acidentes2014.zip")
#unzip(zipfile = "data-raw/acidentes2015.zip")
#unzip(zipfile = "data-raw/acidentes2016_atual.zip")
#unzip(zipfile = "data-raw/acidentes2017.zip")
#unzip(zipfile = "data-raw/acidentes2018.zip")
#unzip(zipfile = "data-raw/acidentes2019.zip")
#unzip(zipfile = "data-raw/acidentes2020.zip")
#unzip(zipfile = "data-raw/acidentes2021.zip")
#unzip(zipfile = "data-raw/acidentes2022.zip")
#unzip(zipfile = "data-raw/acidentes2023.zip")


acidentes2011 <- read.csv("data-raw/acidentes2011.csv")
acidentes2012 <- read.csv("data-raw/acidentes2012.csv")
acidentes2013 <- read.csv("data-raw/acidentes2013.csv")
acidentes2014 <- read.csv("data-raw/acidentes2014.csv")
acidentes2015 <- read.csv("data-raw/acidentes2015.csv")
acidentes2016 <- read.csv("data-raw/acidentes2016_atual.csv", sep = ";")
acidentes2017 <- read.csv("data-raw/acidentes2017.csv", sep = ";")
acidentes2018 <- read.csv("data-raw/acidentes2018.csv", sep = ";")
acidentes2019 <- read.csv("data-raw/acidentes2019.csv", sep = ";")
acidentes2020 <- read.csv("data-raw/acidentes2020.csv", sep = ";")
acidentes2021 <- read.csv("data-raw/acidentes2021.csv", sep = ";")
acidentes2022 <- read.csv("data-raw/acidentes2022.csv", sep = ";")
acidentes2023 <- read.csv("data-raw/acidentes2023.csv", sep = ";")


# AJUSTANDO AS CLASSIFICAÇÕES DAS VARIÁVEIS E CRIANDO UM DATAFRAME GERAL ####

acidentes2011$id_veiculo <- as.numeric(acidentes2011$id_veiculo)
acidentes2011$ano_fabricacao_veiculo <- as.numeric(acidentes2011$ano_fabricacao_veiculo)

acidentes2012$br <- as.numeric(acidentes2012$br)
acidentes2012$km <- as.numeric(acidentes2012$km)
acidentes2012$id_veiculo <- as.numeric(acidentes2012$id_veiculo)
acidentes2012$ano_fabricacao_veiculo <- as.numeric(acidentes2012$ano_fabricacao_veiculo)

acidentes2013$id_veiculo <- as.numeric(acidentes2013$id_veiculo)
acidentes2013$ano_fabricacao_veiculo <- as.numeric(acidentes2013$ano_fabricacao_veiculo)

acidentes2014$id_veiculo <- as.numeric(acidentes2014$id_veiculo)
acidentes2014$ano_fabricacao_veiculo <- as.numeric(acidentes2014$ano_fabricacao_veiculo)

acidentes2015$id_veiculo <- as.numeric(acidentes2015$id_veiculo)
acidentes2015$ano_fabricacao_veiculo <- as.numeric(acidentes2015$ano_fabricacao_veiculo)

acidentes2016$km <- as.numeric(acidentes2016$km)

acidentes2017$km <- as.numeric(acidentes2017$km)

acidentes2018$km <- as.numeric(acidentes2018$km)

acidentes2019$km <- as.numeric(acidentes2019$km)

acidentes2020$km <- as.numeric(acidentes2020$km)
acidentes2020$latitude <- as.character(acidentes2020$latitude)
acidentes2020$longitude <- as.character(acidentes2020$longitude)

acidentes2021$km <- as.numeric(acidentes2021$km)

acidentes2022$km <- as.numeric(acidentes2022$km)

acidentes2023$km <- as.numeric(acidentes2023$km)

acidentes2011 <- acidentes2011 %>% 
  mutate(ano = 2011)
acidentes2012 <- acidentes2012 %>% 
  mutate(ano = 2012)
acidentes2013 <- acidentes2013 %>% 
  mutate(ano = 2013)
acidentes2014 <- acidentes2014 %>% 
  mutate(ano = 2014)
acidentes2015 <- acidentes2015 %>% 
  mutate(ano = 2015)
acidentes2016 <- acidentes2016 %>% 
  mutate(ano = 2016)
acidentes2017 <- acidentes2017 %>% 
  mutate(ano = 2017)
acidentes2018 <- acidentes2018 %>% 
  mutate(ano = 2018)
acidentes2019 <- acidentes2019 %>% 
  mutate(ano = 2019)
acidentes2020 <- acidentes2020 %>% 
  mutate(ano = 2020)
acidentes2021 <- acidentes2021 %>% 
  mutate(ano = 2021)
acidentes2022 <- acidentes2022 %>% 
  mutate(ano = 2022)
acidentes2023 <- acidentes2023 %>% 
  mutate(ano = 2023)

acidentes_2011_2023 <- bind_rows(acidentes2011,acidentes2012,acidentes2013,
                                 acidentes2014,acidentes2015,
                                 acidentes2016,acidentes2017,acidentes2018,
                                 acidentes2019,acidentes2020,acidentes2021,
                                 acidentes2022,acidentes2023)

# MANIPULANDO DOS DADOS ####
## idade ####
acidentes_2011_2023$idade <- replace(
  acidentes_2011_2023$idade
  , acidentes_2011_2023$idade>120 | acidentes_2011_2023$idade < 0, NA
)

## dia da semana #####

acidentes_2011_2023 <- acidentes_2011_2023 %>% 
  mutate(dia_semana = dplyr::recode(dia_semana,
                                    "Segunda" = "segunda-feira",
                                    "segunda-feira" = "segunda-feira",
                                    "Ter\xe7a" = "terca-feira",
                                    "Ter\xe7a  " = "terca-feira",
                                    "ter\xe7a-feira" = "terca-feira",
                                    "Quarta" = "quarta-feira",
                                    "Quarta " = "quarta-feira",
                                    "quarta-feira" = "quarta-feira",
                                    "Quinta" = "quinta-feira",
                                    "Quinta " = "quinta-feira",
                                    "quinta-feira" = "quinta-feira",
                                    "Sexta" = "sexta-feira",
                                    "Sexta  " = "sexta-feira",
                                    "sexta-feira" = "sexta-feira",
                                    "S\xe1bado" = "sabado",
                                    "S\xe1bado " = "sabado",
                                    "s\xe1bado" = "sabado",
                                    "Domingo" = "domingo"))

table(acidentes_2011_2023$dia_semana)
## estado fisico #####
acidentes_2011_2023 <- acidentes_2011_2023 %>% 
  mutate(estado_fisico = dplyr::recode(estado_fisico,
                                       "Ileso" = "ileso",
                                       "Ileso       " = "ileso",
                                       "Ferido Leve" = "ferido leve",
                                       "Ferido Leve " = "ferido leve",
                                       "Les\xf5es Leves" = "ferido leve",
                                       "Ferido Grave" = "ferido grave",
                                       "Les\xf5es Graves" = "ferido grave",
                                       "Morto" = "morto",
                                       "Morto       " = "morto",
                                       "\xd3bito" = "morto",
                                       "Ignorado" = "ignorado",
                                       "Ignorado    " = "ignorado",
                                       "N\xe3o Informado" = "ignorado",
                                       "(null)" = "ignorado"
  ))

table(acidentes_2011_2023$estado_fisico)

acidentes_2011_2023$estado_fisico[acidentes_2011_2023$estado_fisico == ""] <- "ignorado"


## sexo ####
acidentes_2011_2023 <- acidentes_2011_2023 %>% 
  mutate(sexo = dplyr::recode(sexo, 
                              "M" = "masculino",
                              "Masculino" = "masculino",
                              "F" = "feminino",
                              "Feminino" = "feminino",
                              "Ignorado" = "ignorado",
                              "I" = "ignorado",
                              "N\xe3o Informado" = "ignorado",
                              "Inv\xe1lido" = "ignorado"))
acidentes_2011_2023$sexo[is.na(acidentes_2011_2023$sexo)] <- "ignorado"
acidentes_2011_2023$sexo[acidentes_2011_2023$sexo == ""] <- "ignorado"

table(acidentes_2011_2023$sexo)
## criando faixa etária ####
library(RcmdrMisc)
table(acidentes_2011_2023$idade, useNA = "always")

acidentes_2011_2023 <- acidentes_2011_2023 %>% 
  mutate(faixa_etaria = binVariable(idade, bins=6, 
                                    method='intervals', labels=NULL))
## classificacao do sinistro ####
acidentes_2011_2023 <- acidentes_2011_2023 %>% 
  mutate(classificacao_acidente = dplyr::recode(classificacao_acidente,
                                                "Sem V\xedtimas" = "sem vitimas",
                                                "Sem V\xedtimas        " = "sem vitimas",
                                                "Com V\xedtimas Fatais " = "com vitimas fatais",
                                                "Com V\xedtimas Fatais" = "com vitimas fatais",
                                                "Com V\xedtimas Feridas" = "com vitimas feridas",
                                                "Ignorado" = "ignorado",
                                                "Ignorado           " = "ignorado",
                                                "(null)" = "ignorado"
  ))

acidentes_2011_2023$classificacao_acidente[acidentes_2011_2023$classificacao_acidente == ""] <- "ignorado"

table(acidentes_2011_2023$classificacao_acidente)
## tipo pista ####
acidentes_2011_2023 <- acidentes_2011_2023 %>% 
  mutate(tipo_pista = dplyr::recode(tipo_pista,
                                    "Dupla" = "dupla",
                                    "Dupla   " = "dupla",
                                    "M\xfaltipla" = "multipla",
                                    "Simples" = "simples",
                                    "Simples " = "simples"))
acidentes_2011_2023$tipo_pista[acidentes_2011_2023$tipo_pista == "(null)"] <- NA
## fase dia ####
acidentes_2011_2023 <- acidentes_2011_2023 %>% 
  mutate(fase_dia = dplyr::recode(fase_dia,
                                  "Plena Noite" = "Plena noite"))

acidentes_2011_2023$fase_dia[acidentes_2011_2023$fase_dia == "(null)" ] <- NA
acidentes_2011_2023$fase_dia[acidentes_2011_2023$fase_dia == "" ] <- NA
## uso solo ####
acidentes_2011_2023 <- acidentes_2011_2023 %>% 
  mutate(uso_solo = dplyr::recode(uso_solo,
                                  "N\xe3o" = "Nao",
                                  "Rural " = "Rural"))

acidentes_2011_2023$uso_solo[acidentes_2011_2023$uso_solo == "(null)"] <- NA
## condicao metereologica ####
acidentes_2011_2023 <- acidentes_2011_2023 %>% 
  mutate(condicao_metereologica = dplyr::recode(condicao_metereologica,
                                                "C\xe9u Claro" = "Ceu Claro",
                                                "Ignorada" = "Ignorado",
                                                "Nevoeiro/neblina" = "Nevoeiro/Neblina"))

acidentes_2011_2023$condicao_metereologica[acidentes_2011_2023$condicao_metereologica == "(null)" ] <- NA
acidentes_2011_2023$condicao_metereologica[acidentes_2011_2023$condicao_metereologica == "" ] <- NA

## tracado via ####
acidentes_2011_2023 <- acidentes_2011_2023 %>% 
  mutate(tracado_via = dplyr::recode(tracado_via,
                                     "Curva     " = "Curva",
                                     "Desvio Tempor\xe1rio" = "Desvio Temporario",
                                     "Interse\xe7\xe3o de vias" = "Interseccao de vias",
                                     "N\xe3o Informado" = "Nao Informado",
                                     "Reta      " = "Reta",
                                     "Rotat\xf3ria" = "Rotatoria",
                                     "T\xfanel" = "Tunel"))

acidentes_2011_2023$tracado_via[acidentes_2011_2023$tracado_via == "(null)"] <- NA
## tipo acidente ####
acidentes_2011_2023 <- acidentes_2011_2023 %>% 
  mutate(tipo_acidente = dplyr::recode(tipo_acidente,
                                       "Atropelamento de animal" = "Atropelamento de Animal",
                                       "Atropelamento de pessoa" = "Atropelamento de Pedestre",
                                       "Colis\xe3o com bicicleta" = "Colisao com bicicleta",
                                       "Colis\xe3o com objeto" = "Colisao com objeto",
                                       "Colis\xe3o com objeto em movimento" = "Colisao com objeto",
                                       "Colis\xe3o com objeto est\xe1tico" = "Colisao com objeto",
                                       "Colis\xe3o com objeto fixo" = "Colisao com objeto",
                                       "Colis\xe3o com objeto m\xf3vel" = "Colisao com objeto",
                                       "Colis\xe3o frontal" = "Colisao frontal",
                                       "Colis\xe3o lateral" = "Colisao lateral",
                                       "Colis\xe3o lateral mesmo sentido" = "Colisao lateral",
                                       "Colis\xe3o lateral sentido oposto" = "Colisao lateral",
                                       "Colis\xe3o transversal" = "Colisao transversal",
                                       "Colis\xe3o Transversal" = "Colisao transversal",
                                       "Colis\xe3o traseira" = "Colisao traseira",
                                       "Danos Eventuais" = "Danos eventuais",
                                       "Derramamento de Carga" = "Derramamento de carga",
                                       "Eventos at\xedpicos" = "Eventos atipicos",
                                       "Inc\xeandio" = "Incendio",
                                       "Queda de motocicleta / bicicleta / ve\xedculo" = "Queda de motocicleta / bicicleta / veiculo",
                                       "Queda de ocupante de ve\xedculo" = "Queda de ocupante de veiculo",
                                       "Sa\xedda de leito carro\xe7\xe1vel" = "Saida de leito carrocavel",
                                       "Sa\xedda de Pista" = "Saida de Pista"))

acidentes_2011_2023$tipo_acidente[acidentes_2011_2023$tipo_acidente == ""] <- NA

# Representativida do transp de pass e de carg ----
library(fleetbr)
library(roadtrafficdeaths)
# veiculos de pass/carg representam X% da frota nacional  
frota <- fleetbr %>% 
  filter(ano == "2022") %>%
  filter(mes == 12) %>% 
  group_by(modal) %>% 
  summarise(total = sum(frota))
  
p_pass_frota <- sum(frota$total[frota$modal == "ONIBUS"|
               frota$modal == "MICRO-ONIBUS"])/sum(frota$total)

p_carg_frota <- sum(frota$total[frota$modal == "CAMINHAO"|
                  frota$modal == "CAMINHAO TRATOR" |
                  frota$modal == "REBOQUE"])/sum(frota$total)

# veiculos de pass/carg representam X% dos sinistros nas rodovias federais

porcentagem_sinistro <- acidentes_2011_2023 %>% 
  filter(ano == 2022) %>% 
  group_by(tipo_veiculo) %>% 
  summarise(total = n())

p_pass_sin <- sum(porcentagem_sinistro$total[porcentagem_sinistro$tipo_veiculo == "\xd4nibus"| 
                                 porcentagem_sinistro$tipo_veiculo == "Micro\xf4nibus"])/sum(porcentagem_sinistro$total)

p_carg_sin <- sum(porcentagem_sinistro$total[porcentagem_sinistro$tipo_veiculo == "Caminh\xe3o"| 
                                 porcentagem_sinistro$tipo_veiculo == "Caminh\xe3o-Trator"| 
                                 porcentagem_sinistro$tipo_veiculo == "Caminh\xe3o-trator"|
                                 porcentagem_sinistro$tipo_veiculo == "Caminh\xe3o-Tanque"|
                                 porcentagem_sinistro$tipo_veiculo == "Reboque"])/sum(porcentagem_sinistro$total)

# veiculos de pass/carg representam X% dos obitos
obitos22 <- rtdeaths %>% 
  filter(ano_ocorrencia == 2022) 

obitos22 <- obitos22$modal_vitima %>%
  table()

obitos22 <- as.data.frame(obitos22)

p_pass_obt <- obitos22$Freq[obitos22$. == "Ônibus"]/sum(obitos22$Freq)
p_carg_obt <- obitos22$Freq[obitos22$. == "Caminhão"]/sum(obitos22$Freq)

# AGRUPANDO OS DADOS PARA O TRANSP DE PASSAGEIROS E DE CARGA ####
#passageiro
pass_sin <- subset(acidentes_2011_2023, tipo_veiculo == "\xd4nibus" | tipo_veiculo == "Micro\xf4nibus" )

#carga
carg_sin <- subset(acidentes_2011_2023, 
                   tipo_veiculo == "Caminh\xe3o"| 
                     tipo_veiculo == "Caminh\xe3o-Trator"| 
                     tipo_veiculo == "Caminh\xe3o-trator"|
                     tipo_veiculo == "Caminh\xe3o-Tanque"|
                     tipo_veiculo == "Reboque")
# ANALISE DE DADOS A PARTIR DOS DADOS DE SINISTROS - CONSIDERANDO ID COMO CHAVE PRIMÁRIA ####
acidentes11_23unique <- acidentes_2011_2023[!duplicated(acidentes_2011_2023$id), ]
carg11_23unique <- carg_sin[!duplicated(carg_sin$id), ]
pass11_23unique <- pass_sin[!duplicated(pass_sin$id), ]

acidentes11_23unique <- acidentes11_23unique %>% 
  mutate(hora = as.numeric(substr(horario, start = 1, stop = 2)))
carg11_23unique <- carg11_23unique %>% 
  mutate(hora = as.numeric(substr(horario, start = 1, stop = 2)))
pass11_23unique <- pass11_23unique %>% 
  mutate(hora = as.numeric(substr(horario, start = 1, stop = 2)))

carg_total<- (carg11_23unique %>% 
                group_by(classificacao_acidente, ano) %>% 
                summarise(
                  quantidade = n()
                ))

carg_total <- carg_total %>% 
  mutate(
    grupo  = "Transporte de Cargas"
  )

pass_total<- (pass11_23unique %>% 
                group_by(classificacao_acidente, ano) %>% 
                summarise(
                  quantidade = n()
                ))

pass_total <- pass_total %>% 
  mutate(
    grupo = "Transporte de Passageiros"
  )

total_sinistros <- (bind_rows(carg_total,pass_total))


