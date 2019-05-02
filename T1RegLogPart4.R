#Análise Categórica de Dados
#09|03|2018
#Regressão Logistica
#Trabalho 1.d-j

dados <- read.csv2("C:\\Users\\rebis1\\Desktop\\Universidade\\Categorica\\Bankloan.csv", header=T)
#Categorização da Variavel idade (idade<35=>0; idade >=35 =>1)
dados$idadeCat <- as.numeric(dados$idade >= 35)
#criação variavel educaçãoCat que é igual a educação mas junta as categorias 4 e 5 
dados$educaçãoCat <- dados$educação
dados$educaçãoCat[dados$educação %in% 5] <- 4
attach (dados)


library (Hmisc)
library (car)
library (faraway)
library(Epi)


##Modelo final

modf<-glm(Incumprimento ~ t_emprego + t_endereço +
            dívida + dívida_cc, family=binomial("logit")) 
summary(modf)


#---------------------- Avaliação da bondade do ajustamento ----------------------
library(rcompanion)
nagelkerke(modf)

#Teste de Hosmer e Lemeshow
hosmerlem = function(y, yhat, g=10) {
  cutyhat = cut(yhat,
                breaks = quantile(yhat, probs=seq(0,
                                                  1, 1/g)), include.lowest=TRUE)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq = sum((obs - expect)^2/expect)
  P = 1 - pchisq(chisq, g - 2)
  return(list(chisq=chisq,p.value=P))
}
hosmerlem(Incumprimento, fitted(modf)) 

#Valor de AUC
somers2(fitted(modf),Incumprimento) 



#---------------------- Análise de Resíduos do Modelo Final ----------------------

#Resíduos deviance
rd<-residuals (modf, type="deviance")
plot(predict(modf), rd, xlab= "Valores Estimados", ylab="Resíduos Deviance") 
#Identificação de observações
identify(predict(modf), residuals (modf, type="deviance"))

#resíduos standardizados de Pearson
rp<-rstandard (modf, type="pearson")
plot(predict(modf), rp, xlab= "Valores Estimados", ylab="Resíduos de Pearson") 
#Identificação de observações
identify(predict(modf), residuals (modf, type="pearson")) 

#resíduos DfBetas
rdf<-dfbetas(modf); rdf

#Representação dos DfBetas contra as observações 
plot(rdf[, 2], xlab="Observações", ylab="Resíduos para o coeficiente de t_emprego")
plot(rdf[, 3], xlab="Observações", ylab="Resíduos para o coeficiente de t_endereço")
plot(rdf[, 4], xlab="Observações", ylab="Resíduos para o coeficiente de dívida")
plot(rdf[, 5], xlab="Observações", ylab="Resíduos para o coeficiente de dívida_cc")

#Representação dos DfBetas contra as respectivas covariáveis
plot(t_emprego, rdf[, 2], xlab="t_emprego", ylab="Resíduos para o coeficiente de t_emprego")
plot(t_endereço, rdf[, 3], xlab="t_endereço", ylab="Resíduos para o coeficiente de t_endereço")
boxplot (rdf[, 3]~t_endereço, xlab="t_endereço", ylab="Resíduos para o coeficiente de t_endereço")
plot(dívida, rdf[, 4], xlab="dívida", ylab="Resíduos para o coeficiente de dívida")
plot(dívida_cc, rdf[, 5], xlab="dívida_cc", ylab="Resíduos para o coeficiente de dívida_cc")
boxplot (rdf[, 5]~dívida_cc, xlab="dívida_cc", ylab="Resíduos para o coeficiente de dívida_cc")
# Efeito no qui-quadrado de Pearson
plot (predict(modf), rp^2)
plot(rp^2)

#Distância de Cook e gráfico halfnormal dos resíduos
plot(cooks.distance(modf), xlab="Observações", ylab="Distância de Cook")
halfnorm(cooks.distance(modf))
halfnorm(residuals(modf)) 
#Leverage
h <- influence(modf) 
plot(h$hat)
halfnorm(h$hat) 



#-------------------- Curva ROC: poder discriminate do modelo --------------------

formula.final <- Incumprimento ~ t_emprego + t_endereço + dívida + dívida_cc
ROC(form = formula.final, data = dados, plot = "ROC", PV = T, MX = T, MI = F, AUC = T, main = "Curva ROC")
# 0.8<=AUC<0.9 -> discriminação boa (todos os modelos)


#evitar falsos positivos - ponto de corte
pt<-0.440000
tp<-sum(dados$Incumprimento==1)
tp
vp<-sum((modf$fitted>=pt) & (dados$Incumprimento==1))
vp
sensb<-vp/tp 
sensb #sensibilidade do modelo para o ponto de corte definido
tn<-sum(dados$Incumprimento==0)
tn
vn<-sum((modf$fitted<pt) & (dados$Incumprimento==0))
vn
esp<-vn/tn
esp #especificidade do modelo para o ponto de corte definido


#evitar falsos negativos - ponto de corte
pt<-0.142000
tp<-sum(dados$Incumprimento==1)
tp
vp<-sum((modf$fitted>=pt) & (dados$Incumprimento==1))
vp
sensb<-vp/tp 
sensb #sensibilidade do modelo para o ponto de corte definido
tn<-sum(dados$Incumprimento==0)
tn
vn<-sum((modf$fitted<pt) & (dados$Incumprimento==0))
vn
esp<-vn/tn
esp #especificidade do modelo para o ponto de corte definido


#----------------------------- Cálculo Probabilidade -----------------------------
#Probabilidade e intervalo de confiança estimada para um individuo com 
#idade = 40, n.educação = 3, t_emprego = 3, t_endereço = 5, rendimento = 60
#dívida = 17, dívida_cc = 70, outras_dív = 3
coef(modf)
x0 <- c(1, 3, 5, 17, 70) 
eta0 <- sum(x0*coef(modf)); eta0
#Probabilidade estimada
ilogit(eta0)
c<-summary(modf)$cov.scaled
se<-sqrt(t(x0) %*% c %*% x0)
se
#Intervalo de confiança para a probabilidade estimada
ilogit(c(eta0-1.96*se,eta0+1.96*se)) 
