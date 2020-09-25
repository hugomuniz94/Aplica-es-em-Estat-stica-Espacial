#-------------------------------------------------------------------------------------#
# ------------------------ Importacao dos dados e do shape  --------------------------#
#-------------------------------------------------------------------------------------#
getwd()
setwd("c:/Users/Pichau/Documents/MBA/Ap Estatistica Espacial/Trabalho")
#Carregando os pacotes necessarios
library(tidyverse)
library(sf)
library(MASS)
library(RColorBrewer)
library(spdep)
library(tidyverse)
library(cartography)
library(tmap)
library(spatialreg)
library(corrplot)

#Importando a base de dados de incendios da regiao de Castilla-La Mancha
homicidios <- read_csv2("Dados Homicidios RJ.csv")
filter(homicidios, Nome == "Angra dos Reis")

#Importando o shapefile de Castilla-La Mancha
RJ_shp <- read_sf("municipiosRJ_2010.shp")
RJ_shp <- RJ_shp[,-c(1,3,4,5,6,7)]
RJ_shp
st_crs(RJ_shp)
plot(RJ_shp)

# Juntei a base de dados com o shapefile
base <- left_join(RJ_shp, homicidios, by=c("nome" = "Nome"))
base
table(base$IDADE)
table(base$nome)

# Conferi se existe NA's
apply(base, 2, function(x) (sum(is.na(x))))
base <- filter(base, !is.na(IDADE))
base

#Checando NA's novamente
apply(homicidios, 2, function(x) (sum(is.na(x))))

# Transformei para um sistemas de coordenadas em metros
#base <- st_transform(x=base,crs=32723)
#base

# Agrupei por cidade para obter o numero de homicidios em cada cidade
base_homic <- base %>% group_by(nome) %>% summarise(homic = sum(homic))
base_homic

# proporcao homem e mulher por cidade

# Filtrei por sexo feminino e agrupei por nome obtendo a quantidade de pessoas por sexo para cada cidade
base_m <-  base %>% filter(SEXO == 0) %>% group_by(nome) %>% summarise(num_m = sum(Populacao))
base_m

# Filtrei por sexo masculino e agrupei por nome obtendo a quantidade de pessoas por sexo para cada cidade
base_h <- base %>% filter(SEXO == 1) %>% group_by(nome) %>% summarise(num_h = sum(Populacao))
base_h

# Juntei as duas bases
base_mh <- cbind(base_m[,2],base_h[,2])
base_mh

# Calculei a proporcao de mulheres por cidade
base_mh$prop_m <- (base_mh$num_m/(base_mh$num_m + base_mh$num_h)) * 100
base_mh

# Calculei a proporcao de homens por cidade
base_mh$prop_h <- (base_mh$num_h/(base_mh$num_m + base_mh$num_h)) * 100
base_mh


# Apliquei group_by, filter e sumarise por soma da Populacao para cada codigo de idade para calcular a proporcao do codigo 4 posteriormente

base_idade1 <- base %>% group_by(nome, IDADE) %>% filter(IDADE == 1) %>% summarise(soma_idade_1 = sum(Populacao))
base_idade1

base_idade2 <- base %>% group_by(nome, IDADE) %>% filter(IDADE == 2) %>% summarise(soma_idade_2 = sum(Populacao))
base_idade2

base_idade3 <- base %>% group_by(nome, IDADE) %>% filter(IDADE == 3) %>% summarise(soma_idade_3 = sum(Populacao))
base_idade3

base_idade4 <- base %>% group_by(nome, IDADE) %>% filter(IDADE == 4) %>% summarise(soma_idade_4 = sum(Populacao))
base_idade4

base_idade5 <- base %>% group_by(nome, IDADE) %>% filter(IDADE == 5) %>% summarise(soma_idade_5 = sum(Populacao))
base_idade5

base_idade6 <- base %>% group_by(nome, IDADE) %>% filter(IDADE == 6) %>% summarise(soma_idade_6 = sum(Populacao))
base_idade6

base_idade7 <- base %>% group_by(nome, IDADE) %>% filter(IDADE == 7) %>% summarise(soma_idade_7 = sum(Populacao))
base_idade7

base_idade8 <- base %>% group_by(nome, IDADE) %>% filter(IDADE == 8) %>% summarise(soma_idade_8 = sum(Populacao))
base_idade8

base_idade9 <- base %>% group_by(nome, IDADE) %>% filter(IDADE == 9) %>% summarise(soma_idade_9 = sum(Populacao))
base_idade9

base_idade10 <- base %>% group_by(nome, IDADE) %>% filter(IDADE == 10) %>% summarise(soma_idade_10 = sum(Populacao))
base_idade10

# Juntei todas as tabelas
base_idade_total <- cbind(base_idade1,base_idade2,base_idade3,base_idade4,base_idade5,base_idade6,base_idade7,base_idade8,base_idade9,base_idade10)
base_idade_total

# Somei a populacao de cada codigo de idade exceto o codigo 4
base_idade_total$total_idade<- base_idade_total$soma_idade_1 + base_idade_total$soma_idade_2 + base_idade_total$soma_idade_3 + base_idade_total$soma_idade_5 +base_idade_total$soma_idade_6+base_idade_total$soma_idade_7+base_idade_total$soma_idade_8+base_idade_total$soma_idade_9+base_idade_total$soma_idade_10
base_idade_total


# Calculei a proporcao de idade entre 31 a 40 anos
base_mh$prop_idade_31a40 <- (base_idade4$soma_idade_4 / base_idade_total$total_idade) * 100
base_mh

# Agrupei por nome e IDH
base_idh <- base %>% group_by(nome, IDH) %>% summarise()
base_idh

# Juntei todas as bases
base_completa <- cbind(base_mh, base_idh, base_homic)
base_completa

# Removi colunas repetidas
base_completa <- base_completa[,-c(1,2,8,10,11,12,13)]
str(base_completa)
base_completa

## 3)
#Definindo o tipo do mapa como estatico
tmap_mode("plot")

#Construindo o mapa para homic
tm_shape(base_completa) + 
  tm_fill(col = "homic",
          breaks = c(0,100,200,300,500,1000,1500,2500,3000, 4300),
          palette = "Blues",
          title = "Casos de Homicídios no RJ",
          midpoint = NA)

summary(base_completa)

# Transformei base_completa em data frame
base_completa_df <- as.data.frame(base_completa)
base_completa_df
correlacao = cor(base_completa_df[,c(1,2,3,5,6)])
corrplot(correlacao, method = "color")
corrplot(correlacao, method = "number", tl.col = blue)

#W com o criterio queen
W.queen = poly2nb(pl = base_completa,
                  row.names = base_completa$homic, 
                  queen = TRUE)

## pesos padronizado por linhas
W.Queen.pesoW <- nb2listw(neighbours = W.queen, 
                          style="W", zero.policy=TRUE) #outras opcoes: B, C, S e U


## pesos binario
W.Queen.pesoB = nb2listw(neighbours = W.queen,
                         style="B", zero.policy=TRUE)

#-------------------------------------------------------------------------------------#
# Criando a matriz W com 3 vizinhos para cada regiao e com peso padronizado por linha #
#-------------------------------------------------------------------------------------#

#Extraindo os centroides do shape
centroides <- st_centroid(base_completa)

#Extraindo os k vizinhos mais proximos
k3viz <- knearneigh(x = centroides, 
                   k = 3)

## knearneigh - define os k vizinhos mais proximos
#Argumentos:
#x - objeto do tipo SpatialPoints
#k - numero de vizinhos

#Contruindo W com 3 vizinhos
W.3viz <- knn2nb(knn = k3viz)

## knn2nb - define uma lista de vizinhanca de um objeto knn
#Argumentos:
#knn - um objeto retornado por knearneigh

## Defina os pesos da matriz W.3viz
## pesos padronizado por linhas
## Lista de vizinhanca espacial com pesos
W.3viz.pesoW <- nb2listw(neighbours = W.3viz, 
                        style="W") #outras opcoes: B, C, S e U



#-------------------------------------------------------------------------------------#
# ------------------------------ Autocorrelacao global  ------------------------------#
#-------------------------------------------------------------------------------------#

#Moran global com W baseada no criterio QUEEN
# e peso padronizado por linha
moran.test(x = base_completa$homic, 
           listw = W.Queen.pesoW, zero.policy = TRUE)


#Moran global com W baseada no criterio QUEEN
# e peso binario
moran.test(x = base_completa$homic, 
           listw = W.Queen.pesoB, zero.policy = TRUE)

#Moran global com W baseada no criterio distancia
# com 3 vizinhos e peso padronizado por linhas
moran.test(x = base_completa$homic, 
           listw = W.3viz.pesoW, zero.policy = TRUE)

#Calculando o Moran Local
moranlocREC <- localmoran(x = base_completa$homic,
                         listw = W.Queen.pesoW, 
                         na.action = na.exclude, 
                         zero.policy = TRUE)

#Acrescentando o Moran local no shape
base_completa$Moran_local = moranlocREC[,1]

#Definindo o tipo do mapa como interativo
tmap_mode("plot")

tm_shape(base_completa) + 
  tm_fill(col = "Moran_local",
          n = 5,
          palette = "RdYlGn",
          title = "Moran Local") +
  tm_borders()

#Ajustando um modelo linear multiplo
ajusteML <- lm(formula = homic ~ prop_m + prop_h + IDH + prop_idade_31a40, 
              data = base_completa)

summary(ajusteML)

#Normalidade dos resÃ­duos
ggplot(tibble(res = residuals(ajusteML)), aes(sample = res)) + 
  stat_qq() + 
  stat_qq_line()

#Verificando independencia dos residuos
moran.test(x = residuals(ajusteML),
           listw = W.Queen.pesoW,
           zero.policy = TRUE)
# existe dependencia espacial

#Ajustando um modelo SAR1
ajusteSAR1 <- spautolm(formula = homic ~ prop_m + prop_h + IDH + prop_idade_31a40, 
                      data = base_completa, 
                      listw = W.Queen.pesoW, 
                      family = "SAR")

summary(ajusteSAR1)

#Ajustando um modelo SAR2
ajusteSAR2b <- spautolm(formula = homic^(1/3) ~ prop_m + IDH + prop_idade_31a40, 
                      data = base_completa, 
                      listw = W.Queen.pesoB, 
                      family = "SAR")
summary(ajusteSAR2b)

ajusteSAR2w <- spautolm(formula = homic^(1/3) ~ prop_m + IDH + prop_idade_31a40, 
                       data = base_completa, 
                       listw = W.Queen.pesoW, 
                       family = "SAR")
summary(ajusteSAR2w)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-3.34497 -1.41805 -0.20645  0.94470  7.81496 
#
#Coefficients: 
#  Estimate Std. Error z value  Pr(>|z|)
#(Intercept)      -64.74063   16.60163 -3.8997 9.633e-05
#prop_m             0.80933    0.37066  2.1835  0.029001
#IDH               17.75869   10.71362  1.6576  0.097402
#prop_idade_31a40   0.80065    0.26828  2.9843  0.002842

#Lambda: 0.34647 LR test value: 4.0489 p-value: 0.044199 
#Numerical Hessian standard error of lambda: 0.15713 

#Log likelihood: -128.9172 
#ML residual variance (sigma squared): 4.4553, (sigma: 2.1108)
#Number of observations: 59 
#Number of parameters estimated: 6 
#AIC: 269.83

# A variavel IDH nao é estatisticamente relevante
# A variavel prop_m é siginificativa e possui impacto positivo sobre a variavel resposta homic


# menor AIC é o peso W

ajusteSAR23w <- spautolm(formula = homic^(1/3) ~ prop_m + IDH + prop_idade_31a40, 
                       data = base_completa, 
                       listw = W.3viz.pesoW, 
                       family = "SAR")
summary(ajusteSAR23w)

#Normalidade dos residuos
ggplot(tibble(res = residuals(ajusteSAR2w)), aes(sample = res)) + 
  stat_qq() + 
  stat_qq_line()

#Verificando independencia dos residuos
moran.test(x = residuals(ajusteSAR2w),
           listw = W.Queen.pesoW,
           zero.policy = TRUE)

# não existe dependencia espacial
# os residuos sao independentes
# residuo está se comportando de maneira normal
# o modelo serve

#Ajustando um modelo CAR - W não funciona por nao ter estrutura espacial
ajusteCAR <- spautolm(formula = homic^(1/3) ~ prop_m + prop_h + IDH + prop_idade_31a40, 
                     data = base_completa, 
                     listw = W.Queen.pesoB, 
                     family = "CAR")

summary(ajusteCAR)

#Comparando o AIC
AIC(ajusteML)
AIC(ajusteSAR1)
AIC(ajusteSAR2w)# melhor modelo
AIC(ajusteCAR)


#Comparando os erros
erroML = sum((ajusteML$fitted.values - base_completa$homic)^2)
erroSARw = sum((ajusteSAR2w$fit$fitted.values^3 - base_completa$homic)^2)
erroSARb = sum((ajusteSAR2b$fit$fitted.values^3 - base_completa$homic)^2)

erroML
erroSARw
erroSARb

#Criando a variavel residuo dentro do shape
base_completa$resML = resid(ajusteML)
base_completa$resSAR2w = resid(ajusteSAR2w)

#Grafico para mais de uma variavel

tm_shape(base_completa) + 
  tm_fill(col = c("resML","resSAR2w"), 
          palette = "RdYlGn",
          midpoint = NA) +
  tm_facets(nrow = 1, 
            sync = FALSE ) +
  tm_layout(legend.position = c("right","top"))












