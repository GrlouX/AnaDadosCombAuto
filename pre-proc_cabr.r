# Pré-processamento dos dados de combustíveis no Brasil

# Carregamento dos dados

# Fusão dos dados semestrais do mesmo ano
df <- list()
year <- 2014
while (year < 2022){
  fl1 <- unzip(paste0("./dados/ca-",year,"-01.zip"), exdir = "./dados")
  df1 <- read.csv(fl1, sep=";", dec=",", encoding="UTF-8")
  fl2 <- unzip(paste0("./dados/ca-",year,"-02.zip"), exdir = "./dados")
  df2 <- read.csv(fl2, sep=";", dec=",", encoding="UTF-8")
  df[[year-2013]] <- rbind(df1,df2)
  year <- year + 1
}
rm(fl1,df1,df2,year)

# Fusão do período considerado (2014-2020) em um único conjunto de dados
DF_cabr <- df[[1]]
for (i in 2:length(df)){
  DF_cabr <- rbind(DF_cabr,df[[i]])
}
colnames(DF_cabr) <- c("Regiao_Sigla","Estado_Sigla","Municipio", 
                 "Nome_da_Revenda","CNPJ_da_Revenda","Nome_da_Rua","Numero_Rua", 
                 "Complemento","Bairro","Cep","Produto",
                 "Data_da_Coleta","Valor_de_Venda","Valor_de_Compra",
                 "Unidade_de_Medida","Bandeira")
rm(df,i)

save.image('var-bruto_cabr.RData')

# Limpeza e transformação nos dados

# Visualização inicial da estrutura dos dados
str(DF_cabr)

head(DF_cabr)

# Contagem de valores ausentes explícitos nas 16 variáveis existentes
colSums(is.na(DF_cabr)) 

# Cópia do data frame original para efetuar os tratamentos necessários
DF1_cabr <- DF_cabr

# Tratamento das variáveis textuais(14)- exceto Valor_de_Venda e Valor_de_Compra

# Contagem e percentual de strings vazias("") nas variáveis
colSums(DF1_cabr=="")
round(colSums(DF1_cabr=="")/nrow(DF1_cabr)*100, 6)

# Investigação da ausência de informação na data - potencialmente relevante
DF1_cabr[c(which(DF1_cabr$Data_da_Coleta=="")), ]

# Exclusão do registro, pois não há informações de 5 variáveis importantes:
# "Data_da_Coleta, Unidade_de_medida, Bandeira, Valor_de_Venda, Valor_de_Compra"
DF1_cabr <- DF1_cabr[-c(which(DF1_cabr$Data_da_Coleta=="")), ]

# Nova contagem de valores ausentes após exclusão do registro
colSums(is.na(DF1_cabr))

# Verificação de um caso especial nas unidades de medida
unique(DF1_cabr$Unidade_de_Medida)

# Nenhum potencial problema detectado, pois a medida "R$ / m³" foi usada apenas
#em todos os registros do combustível GNV.
unique(DF1_cabr$Produto[which(DF1_cabr$Unidade_de_Medida=="R$ / m³")])
sum(DF1_cabr$Produto=="GNV")==sum(DF1_cabr$Unidade_de_Medida=="R$ / m³")

# Tratamento das variáveis numéricas(2)

# Percentual de valores ausentes identificados como NA 
mean(is.na(DF1_cabr$Valor_de_Venda))*100
mean(is.na(DF1_cabr$Valor_de_Compra))*100

# Eliminar coluna "Valor_de_Compra" por ter mais da metade dos valores ausentes
DF1_cabr$Valor_de_Compra <- NULL

colSums(is.na(DF1_cabr))

# Novo conjunto de dados com as modificações realizadas

rm(DF_cabr)

save.image('var-prep_cabr.RData')

write.csv(DF1_cabr,"./dados/ca_2014-2020.csv",row.names=F,fileEncoding="UTF-8")
