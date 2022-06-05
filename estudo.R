#instalando os pacotes
#install.packages('openxlsx')
#install.packages('magrittr')
#install.packages('dplyr')
#install.packages('psych')
#install.packages('DescTools')
#install.packages('ggfortify')

#biblioteca
library(openxlsx)
library(magrittr)
library(dplyr)
library(psych)
library(DescTools)
library(ggfortify)

#1 Coletando os dados
dados_ideb <-openxlsx::read.xlsx('data/divulgacao_anos_finais_escolas_2011_a_2019.xlsx')
head(dados_ideb)

#2 Analise explonatoria e preparando os dados
#2.1 Preparando dos dados

#selecionado as variaveis para o estudo
dados_ideb <- dados_ideb[,c("Tx.Aprovacao.serie.6.a.9.ano", "IDEB")]

#renomeando as colunas
dados_ideb <- dados_ideb %>%
  rename(
    Tx_Aprovacao = Tx.Aprovacao.serie.6.a.9.ano,
    IDEB = IDEB
  )

#verficando os valores ausentes
png('imagem/valores_ausentes.png')
PlotMiss(
  dados_ideb,
  col = colorRampPalette(c('gray10', 'gray90'))(1),
  main = 'Quantidades de valores ausentes'
)
dev.off()

#excluido os valores ausentes
dados_ideb <- na.omit(dados_ideb)

#3 Analise descritivas

col1 <- Desc(dados_ideb$Tx_Aprovacao, digits = 1, plotit = F)
print(col1)
col2 <- Desc(dados_ideb$IDEB, digits = 1, plotit = F)
print(col2)


# teste de normalidade
print(shapiro.test(dados_ideb$Tx_Aprovacao))

print(shapiro.test(dados_ideb$IDEB))

# 4 Analise preditiva

# 4.1 Coeficiente de correlação
print( paste0('O coeficiente de Correlação é ',cor(dados_ideb$Tx_Aprovacao, dados_ideb$IDEB) ))

# 4.2 Grafico de dispersão
png('imagem/grafico_dispersao.png')
plot(
  dados_ideb$Tx_Aprovacao,
  dados_ideb$IDEB,
  xlab = 'Taxa de Aprovacao em %',
  ylab = 'Notas do IDEB'
)
dev.off()

# 4.2 Criando um modelo de analise de regressão em R

modelo <- lm(
  formula = IDEB ~ Tx_Aprovacao,
  data = dados_ideb
)
print(modelo)

# Analisar a significância do modelo
summary(modelo)


#4.3 Analise dos resíduos

#4.3.1 Analise de Linearidade

png('imagem/linearidade.png')

fig6_6 <- autoplot(modelo, which = c(2), ncol = 1)
fig6_6

dev.off()


#4.3.2 Analise de Normalidade

png('imagem/normalidade.png')

fig6_7 <- autoplot(modelo, which = c(1), ncol = 1)
fig6_7

dev.off()

#teste de normalidade do modelo
print(shapiro.test(residuals(modelo)))

#5 Resultado
hist(dados_ideb$Tx_Aprovacao)

# fazendo a transformção da variavel Tx_Aprovacao para atender o pressuposto de normalidade

lambda <- BoxCoxLambda(dados_ideb$Tx_Aprovacao)
trans <- BoxCox(dados_ideb$Tx_Aprovacao, lambda)

hist(trans)
shapiro.test(trans)

png('imagem/histograma.png')
par(mfrow = c(1,2))
hist(
  dados_ideb$Tx_Aprovacao,
  main = 'Sem Transformação',
  xlab = 'Taxa de Aprovação %'
)
hist(
  trans,
  main = 'Com Transformação',
  xlab = 'Taxa de Aprovação Normalizado'
)
dev.off()
shapiro.test(trans)

#Adicionando a coluna ao dataframe
dados_ideb_trans <- dados_ideb %>%
  tibble::add_column(trans = trans)

#Reafazendo o modelo linaer apos a transformação de normalidade da variavel Tx_Aprovacao.
modelo2 <- lm(
   formula = IDEB ~ trans,
  data = dados_ideb_trans
)

print(modelo2)
summary(modelo2)
plot(dados_ideb_trans$trans, dados_ideb_trans$IDEB)
abline(modelo2)

# grafico do modelo de regressão
png('imagem/modelo_final.png')
par(mfrow = c(1,1))
plot(
  x = dados_ideb$Tx_Aprovacao,
  y = dados_ideb$IDEB,
  xlab = 'Taxa de Aprovação em %',
  ylab = 'Notas do IDEB',
  main = sprintf(
    'Regressão Linear Simples\nR-quadrado=%1.2f\nEquação: %1.2f+%1.2fX',
    summary(modelo)$r.squared,
    summary(modelo)$coefficients[1],
    summary(modelo)$coefficients[2]
  )
)
abline(modelo, col = 'red')
dev.off()

# Podemos afirmar, que os principais pressupostos para um modelo linear simples não foram violados e com uma confiança
# de 95% este modelo é estatisticamente válido para explicar que a taxa de aprovação dos anos finais tem um peso de
# 85% na nota do IDEB das escolas estuais e municipais da cidade de Três Lagoas do estado Mato Grosso do Sul.