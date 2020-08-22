####### calculando matrizes do sistema de insumo produto do Brasil - 2017 -------------------------------------

### a matriz de insumo-produto utilizada neste código é a elaborada pelo Nereus/USP e pode ser encontrada em: http://www.usp.br/nereus/wp-content/uploads/MIP-BR-CN10-68S-2017.xlsx

# carregando datasets

Z <- as.matrix(read.csv2("matriz Z.csv", header = F, dec = ","))

X <- read.csv2("VBP.csv", header = F, colClasses = "numeric")

export <- read.csv2("Export.csv", header = F, colClasses = "numeric")

consumo <- read.csv2("Consumo.csv", header = F, colClasses = "numeric")

FBKF <- read.csv2("FBKF.csv", header = F, colClasses = "numeric") 

salarios <- read.csv2("salarios.csv", header = F, colClasses = "numeric")

# calculando as matrizes de coeficientes técnicos (A) e inversa de leontief (B)

VBP <- diag(X$V1)

A <- Z %*% solve(VBP)

B <- solve(diag(68) - A)

######## parte 1 - análise do sistema aberto -----------------------------------------------------
# Calculando o impacto do aumento de um aumento de 9,323% nas exportações de minério de ferro 

percent <- 9.323

delta_export <- as.matrix(c(rep(0,68)))
delta_export[6,] <- export[6,]*(percent/100)

delta_x <- B %*% delta_export
sum(delta_x)

# Calculando o impacto do aumento de 0,1204% no consumo das famílias

percent_consumo <- 0.1204

consumo_total <- 4153992

delta_consumo <- as.matrix(consumo/consumo_total*(consumo_total*percent_consumo/100))

delta_x_consumo <- B %*% delta_consumo

# Calculando o impacto do aumento de 5 bilhões na FBKF

total_FBKF <- 958779

delta_FBKF <- as.matrix((FBKF/total_FBKF) * 5000)

delta_x_FBKF <- B %*% delta_FBKF

####### Parte 2 - Análise do sistema fechado ---------------------------------------------------------
# calculando os coeficientes de consumo (Hc) e renda do trabalho (Hr)

total_salarios <- 2312290

hc <- consumo/total_salarios

#calculando hr

hr <- salarios/X

# Calculando a matriz de coeficientes técnicos do sistma fechado (A_barra)

A_barra <- cbind(rbind(A, t(hr)), rbind(hc,0))

# Calculando a matriz inversa de leontief do sistema fechado (B_barra)

B_barra <- solve(diag(69) - A_barra)

# Calculando os efeitos do sistema fechado

efeito_total <- (as.matrix(apply(B_barra, 2, sum)) + 1)[1:68,]

efeito_renda <- (as.matrix(apply(B_barra, 2, sum)))[1:68,] - as.matrix(apply(B, 2, sum))

efeito_direto <- as.matrix(apply(A, 2, sum))

efeito_indireto <- as.matrix(apply(B, 2, sum)) - efeito_direto
