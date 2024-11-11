## Criacao base de dados das comunas de Medelín:
#########################################################
library(dplyr)
library(writexl)

#Importando a base com o tamanho da populacao (projecao)
# fonte: https://www.medellin.gov.co/geomedellin/datosAbiertos/264

poblacion__proyeccion_201 <- read.csv("C:/Users/malut/Google Drive/Eventos/2024/World Data Forum - 2024/Datathon/poblacion__proyeccion_201.csv", header=FALSE)
View(poblacion__proyeccion_201)
base <- data.frame(poblacion__proyeccion_201)
base <- df %>% select(-c(V1,V4,V5,V6,V7,V8,V9,V11,V12,V13,V14,V15,V16))
print(base)
write_xlsx(base,"C:\\Users\\malut\\Google Drive\\Eventos\\2024\\World Data Forum - 2024\\Datathon\\base.xlsx")

#Importando a base de atrações turisticas
atractivos_turisticos <- read.csv("C:/Users/malut/Google Drive/Eventos/2024/World Data Forum - 2024/Datathon/csv_atractivos_turisticos/atractivos_turisticos.csv")
View(atractivos_turisticos)
atractivos_turisticos <- data.frame(atractivos_turisticos)
write_xlsx(atractivos_turisticos,"C:\\Users\\malut\\Google Drive\\Eventos\\2024\\World Data Forum - 2024\\Datathon\\atractivos_turisticos.xlsx")

#Importando a base de estabelecimentos comerciais
Establecimiento_Comercial <- read.csv("C:/Users/malut/Google Drive/Eventos/2024/World Data Forum - 2024/Datathon/csv_establecimientos_de_indus/establecimientos_de_indus.csv")
View(Establecimiento_Comercial)
Establecimiento_Comercial <- data.frame(Establecimiento_Comercial)
write_xlsx(Establecimiento_Comercial,"C:\\Users\\malut\\Google Drive\\Eventos\\2024\\World Data Forum - 2024\\Datathon\\Establecimiento_Comercial.xlsx")

#Importando a base de estratos socioeconomicos
estratos_socioecon <- read.csv("C:/Users/malut/Google Drive/Eventos/2024/World Data Forum - 2024/Datathon/csv_estrato_socioeconomico/estrato_socioeconomico.csv")
View(estratos_socioecon)
estratos_socioecon <- data.frame(estratos_socioecon)
write_xlsx(estratos_socioecon,"C:\\Users\\malut\\Google Drive\\Eventos\\2024\\World Data Forum - 2024\\Datathon\\estratos_socioecon.xlsx")

########################################################
#Importando a base construída:
base_R <- read_excel("C:/Users/malut/Google Drive/Eventos/2024/World Data Forum - 2024/Datathon/base_R.xlsx")
base_R = data.frame(base_R)
head(base_R)
summary(base_R)
class(base_R$Perc_cobert_acueducto)
library('corrr')
library(ggcorrplot)
library("FactoMineR")

#Explorando os dados:
str(base_R)

#deixando apenas as variaveis chave:
base_R2 = subset(base_R, select = c(Perc_transp_sustent,taxa_hurto,
                                    Taxa_estabelec_comerc,emissaoCO2_carroparticular))

#Normalizing the data:
data_normalized <- scale(base_R2)
head(data_normalized)

#Applying PCA:
data.pca <- princomp(data_normalized)
summary(data.pca)

#valores das cargas das duas primeiras componentes:
data.pca$loadings[, 1:3]


#### comecando de uma base mais simples:
#deixando apenas as variaveis chave:
base_R2 = subset(base_R, select = c(Perc_gostam_transp_publico,Perc_cobert_aseo,
                                    Perc_transp_sustent,taxa_hurto,
                                    taxa_mortal_acid_trans,emissaoCO2_carroparticular))

#renomeando as variaveis:
colnames(base_R2)
names(base_R2)[names(base_R2) == "Perc_gostam_transp_publico"] <- "approv_public_transp"
names(base_R2)[names(base_R2) == "Perc_cobert_aseo"] <- "waste_collection_serv"
names(base_R2)[names(base_R2) == "Perc_transp_sustent"] <- "use_sustainab_transp"
names(base_R2)[names(base_R2) == "taxa_hurto"] <- "theft_rate "
names(base_R2)[names(base_R2) == "taxa_mortal_acid_trans"] <- "traff_accid_mort"
names(base_R2)[names(base_R2) == "emissaoCO2_carroparticular"] <- "CO2_emission"

#Normalizing the data:
data_normalized <- scale(base_R2)
head(data_normalized)

#Applying PCA:
data.pca <- princomp(data_normalized)
summary(data.pca)

#valores das cargas das duas primeiras componentes:
data.pca$loadings[, 1:3]

#screeplot
library("FactoMineR")
library(factoextra)
fviz_eig(data.pca, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(data.pca, col.var = "black")

# Contribution of each variable 
fviz_cos2(data.pca, choice = "var", axes = 1:2)

#Biplot combined with cos2 
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "green", "orange"),
             repel = TRUE)

#computing the attractiveness indicator:
base_R2$attractiveness_indicator<-0.4937603*base_R2$approv_public_transp+
  0.6029778*base_R2$waste_collection_serv+
  0.1336798*base_R2$use_sustainab_transp-
  0.4442921*base_R2$theft_rate-
  0.3934700*base_R2$traff_accid_mort-
  0.1501139*base_R2$CO2_emission          

data_normalized = data.frame(data_normalized)
data_normalized$attractiveness_indicator<-0.4937603*data_normalized$approv_public_transp+
  0.6029778*data_normalized$waste_collection_serv+
  0.1336798*data_normalized$use_sustainab_transp-
  0.4442921*data_normalized$theft_rate-
  0.3934700*data_normalized$traff_accid_mort-
  0.1501139*data_normalized$CO2_emission   
  

