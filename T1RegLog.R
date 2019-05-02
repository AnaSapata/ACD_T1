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
str(dados)



#Análise Univariada

#Ajustar modelo nulo
fit<-glm(Incumprimento~1, family=binomial("logit"))
summary (fit)

#Ajustar modelo com a variável idade
fit1<-glm(Incumprimento~idade, family=binomial("logit"))
summary (fit1)
#Odds Ratio da variável idade
OR<-exp(fit1$coef[2]) 
OR  
(OR-1)*100
# matriz de covariâncias do modelo
mc<-summary(fit1)$cov.scaled
#Intervalo de confiança para o OR a 95%
exp(c(fit1$coef[2]-qnorm(0.975)*sqrt(mc[2,2]),fit1$coef[2]+qnorm(0.975)*sqrt(mc[2,2]))) 
#Tabela de contingência entre Incumprimento e idade
table(Incumprimento, idade)       
#Qui-quadrado de Pearson entre Incumprimento e idade
chisq.test(table(Incumprimento, idade))  
chisq.test(table(Incumprimento, idade))$expected
fisher.test (table(Incumprimento, idade))

#Ajustar modelo com a variável educação
fit2<-glm(Incumprimento~educação, family=binomial("logit")) 
summary (fit2)
#Odds Ratio da variável educação
OR<-exp(fit2$coef[2]) 
OR  
# matriz de covariâncias do modelo
mc<-summary(fit2)$cov.scaled 
#Intervalo de confiança para o OR a 95%
exp(c(fit2$coef[2]-qnorm(0.975)*sqrt(mc[2,2]),fit2$coef[2]+qnorm(0.975)*sqrt(mc[2,2])))
#Tabela de contingência entre Incumprimento e educação
table(Incumprimento, educação)        
#Qui-quadrado de Pearson entre Incumprimento e educação
chisq.test(table(Incumprimento, educação))  
chisq.test(table(Incumprimento, educação))$expected
fisher.test (table(Incumprimento, educação))



#Ajustar modelo com a variável t_emprego
fit3<-glm(Incumprimento ~ t_emprego, family=binomial("logit")) 
summary (fit3)
#Odds Ratio da variável t_emprego
OR<-exp(fit3$coef[2]) 
OR  
# matriz de covariâncias do modelo
mc<-summary(fit3)$cov.scaled 
#Intervalo de confiança para o OR a 95%
exp(c(fit3$coef[2]-qnorm(0.975)*sqrt(mc[2,2]),fit3$coef[2]+qnorm(0.975)*sqrt(mc[2,2])))
#Tabela de contingência entre Incumprimento e t_emprego
table(Incumprimento, t_emprego)        
#Qui-quadrado de Pearson entre Incumprimento e t_emprego
chisq.test(table(Incumprimento, t_emprego))  
chisq.test(table(Incumprimento, t_emprego))$expected
fisher.test (table(Incumprimento, t_emprego))

#Ajustar modelo com a variável t_endereço
fit4<-glm(Incumprimento ~ t_endereço, family=binomial("logit")) 
summary (fit4)
#Odds Ratio da variável t_endereço
OR<-exp(fit4$coef[2]) 
OR  
# matriz de covariâncias do modelo
mc<-summary(fit4)$cov.scaled 
#Intervalo de confiança para o OR a 95%
exp(c(fit4$coef[2]-qnorm(0.975)*sqrt(mc[2,2]),fit4$coef[2]+qnorm(0.975)*sqrt(mc[2,2])))
#Tabela de contingência entre Incumprimento e t_endereço
table(Incumprimento, t_endereço)        
#Qui-quadrado de Pearson entre Incumprimento e t_endereço
chisq.test(table(Incumprimento, t_endereço))  
chisq.test(table(Incumprimento, t_endereço))$expected
fisher.test (table(Incumprimento, t_endereço))

#Ajustar modelo com a variável rendimento
fit5<-glm(Incumprimento ~ rendimento, family=binomial("logit")) 
summary (fit5)
#Odds Ratio da variável rendimento
OR<-exp(fit5$coef[2]) 
OR  
(OR-1)*100
# matriz de covariâncias do modelo
mc<-summary(fit5)$cov.scaled 
#Intervalo de confiança para o OR a 95%
exp(c(fit5$coef[2]-qnorm(0.975)*sqrt(mc[2,2]),fit5$coef[2]+qnorm(0.975)*sqrt(mc[2,2])))
#Tabela de contingência entre Incumprimento e rendimento
table(Incumprimento, rendimento)        
#Qui-quadrado de Pearson entre Incumprimento e rendimento
chisq.test(table(Incumprimento, rendimento))  
chisq.test(table(Incumprimento, rendimento))$expected
fisher.test (table(Incumprimento, rendimento))

#Ajustar modelo com a variável dívida
fit6<-glm(Incumprimento ~ dívida, family=binomial("logit")) 
summary (fit6)
#Odds Ratio da variável dívida
OR<-exp(fit6$coef[2]) 
OR  
(OR-1)*100
# matriz de covariâncias do modelo
mc<-summary(fit6)$cov.scaled 
#Intervalo de confiança para o OR a 95%
exp(c(fit6$coef[2]-qnorm(0.975)*sqrt(mc[2,2]),fit6$coef[2]+qnorm(0.975)*sqrt(mc[2,2])))
#Tabela de contingência entre Incumprimento e dívida
table(Incumprimento, dívida)        
#Qui-quadrado de Pearson entre Incumprimento e dívida
chisq.test(table(Incumprimento, dívida))  
chisq.test(table(Incumprimento, dívida))$expected
fisher.test (table(Incumprimento, dívida))

#Ajustar modelo com a variável dívida_cc
fit7<-glm(Incumprimento ~ dívida_cc, family=binomial("logit")) 
summary (fit7)
#Odds Ratio da variavel dívida_cc
OR<-exp(fit7$coef[2]) 
OR 
(OR-1)*100
# matriz de covariâncias do modelo
mc<-summary(fit7)$cov.scaled 
#Intervalo de confiança para o OR a 95%
exp(c(fit7$coef[2]-qnorm(0.975)*sqrt(mc[2,2]),fit7$coef[2]+qnorm(0.975)*sqrt(mc[2,2])))
#Tabela de contingência entre Incumprimento e dívida_cc
table(Incumprimento, dívida_cc)        
#Qui-quadrado de Pearson entre Incumprimento e dívida_cc
chisq.test(table(Incumprimento, dívida_cc))  
chisq.test(table(Incumprimento, dívida_cc))$expected
fisher.test (table(Incumprimento, dívida_cc))

#Ajustar modelo com a nova variável outras_dív
fit8<-glm(Incumprimento ~ outras_dív, family=binomial("logit")) 
summary (fit8)
#Odds Ratio da variável outras_dív
OR<-exp(fit8$coef[2]) 
OR  
(OR-1)*100
# matriz de covariâncias do modelo
mc<-summary(fit8)$cov.scaled 
#Intervalo de confiança para o OR a 95%
exp(c(fit8$coef[2]-qnorm(0.975)*sqrt(mc[2,2]),fit8$coef[2]+qnorm(0.975)*sqrt(mc[2,2])))
#Tabela de contingência entre Incumprimento e outras_dív
table(Incumprimento, outras_dív)        
#Qui-quadrado de Pearson entre Incumprimento e outras_dív
chisq.test(table(Incumprimento, outras_dív))  
chisq.test(table(Incumprimento, outras_dív))$expected
fisher.test (table(Incumprimento, outras_dív))

#Ajustar modelo com a nova variável idadeCat
fit9<-glm(Incumprimento ~ idadeCat, family=binomial("logit")) 
summary (fit9)
#Odds Ratio da variável idadeCat
OR<-exp(fit9$coef[2]) 
OR  
(OR-1)*100
# matriz de covariâncias do modelo
mc<-summary(fit9)$cov.scaled 
#Intervalo de confiança para o OR a 95%
exp(c(fit9$coef[2]-qnorm(0.975)*sqrt(mc[2,2]),fit9$coef[2]+qnorm(0.975)*sqrt(mc[2,2])))
#Tabela de contingência entre Incumprimento e idadeCat
table(Incumprimento, idadeCat)        
#Qui-quadrado de Pearson entre Incumprimento e idadeCat
chisq.test(table(Incumprimento, idadeCat))  
chisq.test(table(Incumprimento, idadeCat))$expected
fisher.test (table(Incumprimento, idadeCat))

#Ajustar modelo com a nova variável educaçãoCat
fit10<-glm(Incumprimento ~ educaçãoCat, family=binomial("logit")) 
summary (fit10)
#Odds Ratio da variável educaçãoCat
OR<-exp(fit10$coef[2]) 
OR  
(OR-1)*100
# matriz de covariâncias do modelo
mc<-summary(fit10)$cov.scaled 
#Intervalo de confiança para o OR a 95%
exp(c(fit10$coef[2]-qnorm(0.975)*sqrt(mc[2,2]),fit10$coef[2]+qnorm(0.975)*sqrt(mc[2,2])))
#Tabela de contingência entre Incumprimento e educaçãoCat
table(Incumprimento, educaçãoCat)        
#Qui-quadrado de Pearson entre Incumprimento e educaçãoCat
chisq.test(table(Incumprimento, educaçãoCat))  
chisq.test(table(Incumprimento, educaçãoCat))$expected
fisher.test (table(Incumprimento, educaçãoCat))


#Teste da razão de verosimilhanças para cada variável
anova (fit, fit1, test="Chisq") #idade sig.
anova (fit, fit2, test="Chisq") #educação sig.
anova (fit, fit3, test="Chisq") #t_emprego sig.
anova (fit, fit4, test="Chisq") #t_endereço sig.
anova (fit, fit5, test="Chisq") #rendimento Não sig.
anova (fit, fit6, test="Chisq") #dívida sig.
anova (fit, fit7, test="Chisq") #dívida_cc sig.
anova (fit, fit8, test="Chisq") #outras_dív sig.
anova (fit, fit9, test="Chisq") #idadeCat sig.
anova (fit, fit10, test="Chisq") #educaçãoCat sig.

