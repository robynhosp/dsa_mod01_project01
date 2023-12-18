
# Definindo a pasta de trabalho
setwd("/Users/robsonbonfim/Library/Mobile Documents/com~apple~CloudDocs/Education/dsa/FCD/01-BigDataRAzure/Cap20/Projeto1")
getwd()

# Importando as bibliotecas necessárias
library(readxl)
library(stringr)
library(corrplot)
library(dplyr)
library(caTools)

# Leitura dos dados
df = read_excel("data/FEV-data-Excel.xlsx") 

# Exploração Inicial dos Dados
dim(df)
# 53 25

str(df)
View(df)

# Renomeando as colunas para melhor entendimento do dataset
# Uso o for para criar o código para alteração do nome dos campos
for (linha in colnames(df)){
  result <- paste("names(df)[names(df) == '",str_trim(linha),"'] <- '' #", sep="", collapse=NULL)
  print(result)
}

# Nome completo do carro
names(df)[names(df) == 'Car full name'] <- 'nome'
# Nome do fabricante
names(df)[names(df) == 'Make'] <- 'fabricante'
# Nome do modelo
names(df)[names(df) == 'Model'] <- 'modelo'
# Valor Minimo bruto em PLN (Moeda Polonesa)
names(df)[names(df) == 'Minimal price (gross) [PLN]'] <- 'preco'
# Cavalos de Potencia
names(df)[names(df) == 'Engine power [KM]'] <- 'potencia'
# Torque Máximo
names(df)[names(df) == 'Maximum torque [Nm]'] <- 'torque'
# Tipo dos freios
names(df)[names(df) == 'Type of brakes'] <- 'freios' 
# Traçao Dianteira, Traseira ou 4x4
names(df)[names(df) == 'Drive type'] <- 'tracao' 
# Capacidade da Bateria em kWh
names(df)[names(df) == 'Battery capacity [kWh]'] <- 'capacidade-bateria'
# O que é o ciclo WLTP?
# O ciclo WLTP (WLTC) consiste numa série de arranques, acelerações e paragens num ambiente controlado durante um determinado período de tempo.
# Autonomia (km) = energia útil da bateria (Wh) / consumo de energia da bateria (Wh/km)
names(df)[names(df) == 'Range (WLTP) [km]'] <- 'target'
# Distancia entre os eixos em centimetros
names(df)[names(df) == 'Wheelbase [cm]'] <- 'distancia-eixos' 
# Comprimento em centimetros
names(df)[names(df) == 'Length [cm]'] <- 'comprimento'
# Largura em centimetros
names(df)[names(df) == 'Width [cm]'] <- 'largura'
# Altura em centimetros
names(df)[names(df) == 'Height [cm]'] <- 'altura-cm' 
# Peso Minimo Vazio em Kg
names(df)[names(df) == 'Minimal empty weight [kg]'] <- 'peso-vazio' 
# Peso total permitido em Kg
names(df)[names(df) == 'Permissable gross weight [kg]'] <- 'peso-total'
# Capacidade Maxima de Carga em Kg
names(df)[names(df) == 'Maximum load capacity [kg]'] <- 'carga-maxima' 
# Quantidade de Assentos
names(df)[names(df) == 'Number of seats'] <- 'assentos'
# Quantidade de Portas
names(df)[names(df) == 'Number of doors'] <- 'portas' 
# Tamanho da roda em polegadas
names(df)[names(df) == 'Tire size [in]'] <- 'roda' 
# Velocidade Máxima em Milhas por hora
names(df)[names(df) == 'Maximum speed [kph]'] <- 'velocidade-maxima'
# Capacidade do porta-malas em litros
names(df)[names(df) == 'Boot capacity (VDA) [l]'] <- 'capacidade-porta-malas'
# Aceleracao de 0-100 kilometros por hora em segundos
names(df)[names(df) == 'Acceleration 0-100 kph [s]'] <- 'aceleracao0-100'
# Potencia Máxima da bateria em Kw
names(df)[names(df) == 'Maximum DC charging power [kW]'] <- 'potencia-max-bateria'
# Consumo médio da bateria kWh a cada 100kms
names(df)[names(df) == 'mean - Energy consumption [kWh/100 km]'] <- 'consumo-medio' 

names(which(colSums(is.na(df))>0))

# Procurando valores N/A no dataset
# As seguintes variaveis tem valores N/A
# freios
# peso-total
# carga-maxima
# capacidade-porta-malas
# aceleracao0-100
# consumo-medio     

# Fiz a consulta no site que deriva o dataset e inclui as informacoes uma vez que tinha acesso aos dados
# Como o dataset não tem tantos dados os valores N/A são importantes que seja preenchidos

View(df[c('nome','fabricante','assentos',names(which(colSums(is.na(df))>0)))])

# Trata-se de um microonibus que não tem porta-malas
df$`capacidade-porta-malas`[df$nome== 'Mercedes-Benz EQV (long)'] <- 0
df$`aceleracao0-100`[df$nome== 'Mercedes-Benz EQV (long)'] <- 12.0
df$freios[df$nome== 'Mercedes-Benz EQV (long)'] <- 'disc (front + rear)'

# Trata-se de um utilitario de entrega
df$`aceleracao0-100`[df$nome== 'Nissan e-NV200 evalia'] <- 14.0

# No site não tem a informação então fiz o calculo autonomia x capacidade da bateria
df$`consumo-medio`[df$nome== 'Citroën ë-C4'] <- 15.6

# Peugeto 2008
df$`peso-total`[df$nome== 'Peugeot e-2008'] <- 2030
df$`carga-maxima`[df$nome== 'Peugeot e-2008'] <- 480
df$`aceleracao0-100`[df$nome== 'Peugeot e-2008'] <- 9.9
df$`consumo-medio`[df$nome== 'Peugeot e-2008'] <- 15.9

# Para os modelos da Tesla algumas informacoes estão indisponiveis, então vou usar a média do dataset conforme a coluna

# Para o peso total, vou fazer a diferença do peso total pela peso vazio, tirar a média e somar ao valor do peso vazio que já temos para os Teslas
df_media <- na.omit(df)
View(df_media)

# Tratando o campo peso-total para os Teslas
media_peso_total = mean(df_media$`peso-total`)
media_peso_vazio = mean(df_media$`peso-vazio`)
media_peso = round(media_peso_total-media_peso_vazio,0)
df$`peso-total`[df$fabricante== 'Tesla'] <- df$`peso-vazio`[df$fabricante== 'Tesla']+media_peso

# Tratando o campo carga-maxima para os Teslas
media_carga_maxima = mean(df_media$`carga-maxima`)
df$`carga-maxima`[df$fabricante== 'Tesla'] <- media_carga_maxima

# Tratando o campo consumo-medio
# Vou preencher os valores NA do Tesla para o campo consumo-medio pela media da coluna
media_consumo = mean(df_media$`consumo-medio`)
df$`consumo-medio`[df$fabricante== 'Tesla'] <- media_consumo

# Verificacao para saber se ainda temos valores nulos
colSums(is.na(df))

# Tudo certo!!
View(df)
str(df)

# Transformar as variaveis String em categoricas
df$fabricante = as.factor(df$fabricante)
df$freios = as.factor(df$freios)
df$tracao = as.factor(df$tracao)
df$assentos = as.factor(df$assentos)
df$portas = as.factor(df$portas)
df$roda = as.factor(df$roda)

str(df)
summary(df)

# Vou criar um subset com as variasveis numericas para fazer o mapa de correlação
df_num <- select(df,-c('nome','fabricante','modelo','freios','tracao','assentos','portas','roda'))
df_num

df_corr <- cor(df_num)
corrplot(df_corr)

# Analisando o mapa de correlação vejo que algumas variaveis como as dimensoes do carro nao tem correlação com a variavel target,
# então vou retirar do dataset
new_df <- select(df, -c('nome','fabricante','modelo','preco','largura','altura-cm','peso-vazio','peso-total','carga-maxima','consumo-medio'))
str(new_df)

# Analisei novamente a correlação, retirando as variaveis Factor
df_num <- select(new_df,-c('freios','tracao','assentos','portas','roda'))
df_corr <- cor(df_num)
corrplot(df_corr)

View(new_df)

# Histograma da primeira variavel alvo que considero, pois não ficou muito claro pra mim a proposta do projeto, a principio vou usar a variavel 
# que traz o valor do WLTP por carro para prever o consumo do carro.
# Em uma segunda analise vou colocar a variavel target a coluna capacidade-bateria

hist(new_df$target,
     xlab = "Kms", 
     main = "Histograma da Variável WLTP", 
     xlim = range(100:700))

# Usando o modelo para verificar as variaveis que mais se correlacionam
modelo_v1 <- lm(target ~ ., 
                 data = new_df)

summary(modelo_v1)

# A partir da analise do resultado do summary, identifiquei as variaveis que mais se correlacionam
modelo_v1 <- lm(target ~ torque+ `capacidade-bateria` + `velocidade-maxima` + `potencia-max-bateria` + tracao, 
                data = new_df)

summary(modelo_v1)

# Criei o segundo modelo com as variaveis que tem mais correlacao
modelo_v2 <- lm(target ~ torque+ `capacidade-bateria` + `velocidade-maxima` + `potencia-max-bateria`, 
                data = new_df)

summary(modelo_v2)

# funcao para criar 10 previsoes distintas do modelo
previsoes <- function(modelo, target){
    dimensao_df = dim(new_df)[1]
    index_sample = sample(1:dimensao_df,1, replace=F)
    valor_atual <-c()
    valor_previsto <- c()
    erro <-c()
    for (i in seq(1,10, by=1)){
      
      # selecionando uma linha para prever a variavel target
      index_sample = sample(1:dimensao_df,1, replace=F)
      previsao <- predict(modelo, select(new_df, -c(target))[index_sample,])
      
      atual <- new_df[[index_sample, c(target)]]
      previsao <- round(previsao)
      
      if(atual > previsao){
        diferenca = 1 - (previsao/atual)
      } else {
        diferenca = (1 - (previsao/atual))*-1
      }
      
      valor_atual[i] <- atual
      valor_previsto[i] <- previsao
      erro[i] <- round(diferenca,2)
    }
    return (data.frame("atual" = valor_atual, "previsto" = valor_previsto, "erro" = erro))
}
  
# verificando o percentual de acerto para uma amostragem de 10 exemplo do proprio dataset
df_predict = previsoes(modelo_v2, 'target')
View(df_predict)
media_acerto = (1-mean(df_predict$erro))*100
media_acerto

# Vou abordar a outra variavel que considero target em relacao ao consumo no caso agora de energia em kWh (capacidade-bateria)
names(new_df)[names(new_df) == 'target'] <- 'wltp-km'

modelo_v1_var2 <- lm(`capacidade-bateria` ~ ., 
                data = new_df)

summary(modelo_v1_var2)

# A partir da analise do resultado do summary, identifiquei as variaveis que mais se correlacionam
modelo_v1_var2 <- lm(`capacidade-bateria` ~ `potencia-max-bateria` + `velocidade-maxima` +  
                       `wltp-km` + torque + `aceleracao0-100` + `capacidade-porta-malas`, 
                     data = new_df)

summary(modelo_v1_var2)

# Criei o segundo modelo com as variaveis que tem mais correlacao
modelo_v2_var2 <- lm(`capacidade-bateria` ~ `potencia-max-bateria` + `velocidade-maxima` +  
                       `wltp-km` + torque, 
                     data = new_df)

summary(modelo_v2_var2)

# verificando o percentual de acerto para uma amostragem de 10 exemplo do proprio dataset
df_predict = previsoes(modelo_v1_var2, 'capacidade-bateria')
media_acerto = (1-mean(df_predict$erro))*100
media_acerto

# verificacao detalhada do percentual de erro para cada amostra
View(df_predict)

