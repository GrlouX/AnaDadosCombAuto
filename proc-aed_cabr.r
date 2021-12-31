# Consultas para análise exploratória dos preços de combustíveis no Brasil

# Pacote necessário

library(sqldf) # Manipulação de dados com linguagem SQL via SQLite

# Carga dos conjunto de dados preparados para análise
load('./dados/var-prep_cabr.RData') 

#Para sumário estatístico dos preços de venda agrupados por categoria
SumEstCateg <- function(categ){
  tryCatch(Query <- sqldf(paste('SELECT',categ,',
                      ROUND(AVG("Valor_de_Venda"),3) AS Media_Venda, 
                      ROUND(STDEV("Valor_de_Venda"),3) AS DesvPad_Venda,
                      ROUND(MIN("Valor_de_Venda"),3) AS Min_Venda, 
                      ROUND(MAX("Valor_de_Venda"),3) AS Max_Venda
              FROM DF1_cabr GROUP BY',categ,'ORDER BY Media_Venda DESC'),
              drv="SQLite"),
  error = function(cond) {
    message("Categoria inválida!") 
    return(NA) } )
  return(Query)
}

#Para resumo estatístico dos preços de venda de combustíveis por ano
SumEstCombAn <- function(vetcomb){
  comb <- ""
  if(length(vetcomb)>1){
    for(elem in vetcomb[-c(length(vetcomb))]){
      comb <- paste0(comb,'"',elem,'",')
    }
  }
  comb <- paste0(comb,'"',vetcomb[length(vetcomb)],'"') 
  tryCatch(Query<-sqldf(paste('SELECT "Produto", SUBSTRING("Data_da_Coleta",7,4) 
                                      AS Ano_da_Coleta,
              ROUND(AVG("Valor_de_Venda"),3) AS Media_Venda, 
              ROUND(STDEV("Valor_de_Venda"),3) AS DesvPad_Venda,
              ROUND(MIN("Valor_de_Venda"),3) AS Min_Venda, 
              ROUND(MAX("Valor_de_Venda"),3) AS Max_Venda
            FROM DF1_cabr WHERE "Produto" IN (',comb,')
            GROUP BY "Produto","Ano_da_Coleta"'), drv="SQLite"),
           error = function(cond) {
             message("Entradas inválidas!") 
             return(NA) } )
 return(Query)  
}

#Para calcular índice anual de aumento do preço de combustível por região
TotAumComb <- function(vetcomb){
  comb <- ""
  if(length(vetcomb)>1){
    for(elem in vetcomb[-c(length(vetcomb))]){
      comb <- paste0(comb,'"',elem,'",')
    }
  }
  comb <- paste0(comb,'"',vetcomb[length(vetcomb)],'"')
tryCatch(Query <- sqldf(paste('SELECT DF2."Regiao_Sigla", DF2."Ano_da_Coleta",
DF2."Media_Venda",ROUND(DF2."Media_Venda"*POWER(LAG(DF2."Media_Venda",1,DF2."Media_Venda") 
OVER(PARTITION BY DF2."Regiao_Sigla" ORDER BY DF2."Ano_da_Coleta"),-1),4) AS Ind
  FROM (SELECT "Regiao_Sigla", SUBSTRING("Data_da_Coleta",7,4) AS Ano_da_Coleta,
                AVG("Valor_de_Venda") AS Media_Venda 
        FROM DF1_cabr WHERE "Produto" IN (',comb,')        
        GROUP BY "Regiao_Sigla", "Ano_da_Coleta") AS DF2'), drv="SQLite"),
        error = function(cond) {
           message("Entrada inválida!") 
           return(NA) } )
  return(Query)
}

#Para calcular variação acumulada do preço de combustível por estado da região
VarAcumReg <- function(vetcomb,vetreg){
  comb <- ""
  if(length(vetcomb)>1){
    for(elem in vetcomb[-c(length(vetcomb))]){
      comb <- paste0(comb,'"',elem,'",')
    }
  }
  comb <- paste0(comb,'"',vetcomb[length(vetcomb)],'"')
  reg <- ""
  if(length(vetreg)>1){
    for(elem in vetreg[-c(length(vetreg))]){
      reg <- paste0(reg,'"',elem,'",')
    }
  }
  reg <- paste0(reg,'"',vetreg[length(vetreg)],'"')
tryCatch(Query <- sqldf(paste('SELECT DF2."Estado_Sigla",DF2."Media_Venda_2014",
DF2."Media_Venda_2020",
    ROUND(DF2."Media_Venda_2020"*POWER(DF2."Media_Venda_2014",-1),4) AS Acum
  FROM (SELECT "Estado_Sigla",
              AVG(CASE WHEN SUBSTRING("Data_da_Coleta",7,4)="2014" THEN
              "Valor_de_Venda" ELSE NULL END) AS Media_Venda_2014,
              AVG(CASE WHEN SUBSTRING("Data_da_Coleta",7,4)="2020" THEN
              "Valor_de_Venda" ELSE NULL END) AS Media_Venda_2020
    FROM DF1_cabr WHERE "Produto" IN (',comb,') AND "Regiao_Sigla" IN (',reg,')
                  AND SUBSTRING("Data_da_Coleta",7,4) IN ("2014","2020")
                  GROUP BY "Estado_Sigla") AS DF2'), drv="SQLite"),
         error = function(cond) {
          message("Entradas inválidas!") 
          return(NA) } )
  return(Query)
}

#Para estimar variabilidade do preço de combustível por município do estado no ano   
VarPrecEst <- function(vetcomb,vetest,vetano){
  comb <- ""
  if(length(vetcomb)>1){
    for(elem in vetcomb[-c(length(vetcomb))]){
      comb <- paste0(comb,'"',elem,'",')
    }
  }
  comb <- paste0(comb,'"',vetcomb[length(vetcomb)],'"')
  est <- ""
  if(length(vetest)>1){
    for(elem in vetest[-c(length(vetest))]){
      est <- paste0(est,'"',elem,'",')
    }
  }
  est <- paste0(est,'"',vetest[length(vetest)],'"')
  ano <- ""
  if(length(vetano)>1){
    for(elem in vetano[-c(length(vetano))]){
      ano <- paste0(ano,'"',elem,'",')
    }
  }
  ano <- paste0(ano,'"',vetano[length(vetano)],'"')
tryCatch(Query <- sqldf(paste('SELECT "Municipio",
ROUND(AVG("Valor_de_Venda"),3) AS Media_Venda, 
ROUND(STDEV("Valor_de_Venda"),3) AS DesvPad_Venda,
ROUND(STDEV("Valor_de_Venda")*POWER(AVG("Valor_de_Venda"),-1)*100,2) AS CV_Venda
    FROM DF1_cabr WHERE "Produto" IN (',comb,') AND "Estado_Sigla" IN (',est,')
                 AND SUBSTRING("Data_da_Coleta",7,4) IN (',ano,')       
                 GROUP BY "Municipio" ORDER BY "CV_Venda" DESC'), drv="SQLite"),
         error = function(cond) {
           message("Entradas inválidas!") 
           return(NA) } )
  return(Query)
}

#Para calcular percentual de participação das bandeiras em grupo de municípios
TotPartBand <- function(vetcomb,vetmun,vetano){
  comb <- ""
  if(length(vetcomb)>1){
    for(elem in vetcomb[-c(length(vetcomb))]){
      comb <- paste0(comb,'"',elem,'",')
    }
  }
  comb <- paste0(comb,'"',vetcomb[length(vetcomb)],'"')
  mun <- ""
  if(length(vetmun)>1){
    for(elem in vetmun[-c(length(vetmun))]){
      mun <- paste0(mun,'"',elem,'",')
    }
  }
  mun <- paste0(mun,'"',vetmun[length(vetmun)],'"')
  ano <- ""
  if(length(ano)>1){
    for(elem in vetano[-c(length(vetano))]){
      ano <- paste0(ano,'"',elem,'",')
    }
  }
  ano <- paste0(ano,'"',vetano[length(vetano)],'"')
tryCatch(Query <- sqldf(paste('SELECT "Bandeira",
          ROUND(COUNT("Bandeira")*POWER(DF2."Total_bandeiras",-1)*100,2) 
      AS Parte_bandeiras FROM (SELECT COUNT(*) AS Total_bandeiras FROM DF1_cabr 
      WHERE "Produto" IN (',comb,') AND "Municipio" IN (',mun,')
      AND SUBSTRING("Data_da_Coleta",7,4) = "2020") AS DF2,
      DF1_cabr WHERE "Produto" IN (',comb,') AND "Municipio" IN (',mun,')
      AND SUBSTRING("Data_da_Coleta",7,4) IN (',ano,')
      GROUP BY "Bandeira" ORDER BY "Parte_bandeiras"'), drv="SQLite"),
            error = function(cond) {
            message("Entradas inválidas!") 
            return(NA) } )
  return(Query)
}

# Salvamento das funções criadas para realizar as consultas necessárias
save.image('var-query_cabr.RData')

# Fechamento da conexão com RSQLite
#if(!is.null(getOption("sqldf.connection"))) sqldf()   
