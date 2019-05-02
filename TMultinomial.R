#Análise Categórica de Dados
#09|03|2018
#Multinomial
#Trabalho 2.



dados <- read.csv2("C:\\Users\\rebis1\\Desktop\\Universidade\\Categorica\\Multinomial.csv")
attach (dados)
str(dados)

library (nnet)

#com constante
table(y)
fit0<-multinom(y ~ 1)
summary(fit0)

ftable (y ~ dif_ano)
fit <- multinom(y ~ dif_ano)
summary(fit)
anova(fit, fit0, test = "Chisq") #A variavel é significativa
vcov(fit)

ftable(y ~ sexo)
fit1 <- multinom(y ~ sexo)
summary(fit1)
anova(fit1, fit0, test = "Chisq") #A variavel é significativa

ftable(y ~ classesocial)
fit2 <- multinom(y ~ classesocial)
summary(fit2)
anova(fit2, fit0, test = "Chisq") #A variavel é significativa

fit3 <- multinom( y ~ 1 + dif_ano)
summary(fit3)
anova(fit3, fit0, test = "Chisq")

fit4 <- multinom(y ~ 1 + dif_ano + sexo)
summary(fit4)
anova(fit4, fit0, test = "Chisq")

fit5 <- multinom(y ~ 1 + dif_ano + sexo + classesocial)
summary(fit5)
anova(fit5, fit0, test = "Chisq")

exp(coef(fit5))

#sem constante
fit31 <- multinom( y ~ dif_ano + sexo)
summary(fit31)
anova(fit31, fit0, test = "Chisq")

fit41 <- multinom(y ~ dif_ano + sexo + classesocial)
summary(fit41)
anova(fit41, fit0, test = "Chisq")

exp(coef(fit41))


#mlogit
library(mlogit)
dados$y<-as.factor(dados$y)

mldata<-mlogit.data(dados, varying=NULL, choice="y", shape="wide")
str(mldata)

mlogit1<- mlogit(y~1 | 1 + dif_ano + sexo + classesocial, data = mldata)
summary (mlogit1)
mlogit2<- mlogit(y~1 | dif_ano + sexo + classesocial, data = mldata)
summary (mlogit2)
mc<-vcov(mlogit2)
data.frame(exp(coef(mlogit2)))
#
#exp.coef.mlogit2..
#1:(intercept)           0.48617849
#2:(intercept)           0.04442249
#1:dif_ano               1.52483784
#2:dif_ano               1.95895716
#1:sexo                  0.87753648
#2:sexo                  3.98383045
#1:classesocialB         1.11242423
#2:classesocialB         1.53373297
#1:classesocialC         0.99924829
#2:classesocialC        12.22811374
(data.frame(exp(coef(mlogit2)))-1)*100
#                exp.coef.mlogit2..
#1:(intercept)         -51.38215053
#2:(intercept)         -95.55775080
#1:dif_ano              52.48378364
#2:dif_ano              95.89571567
#1:sexo                -12.24635240
#2:sexo                298.38304528
#1:classesocialB        11.24242273
#2:classesocialB        53.37329677
#1:classesocialC        -0.07517105
#2:classesocialC      1122.81137367


#Logistica y=0,2 -> 2.c)

dados2 <- dados[dados$y!=1,]
View(dados2)
dados2$y[dados2$y %in% 2] <- 1

mod1 <- glm(dados2$y ~ 1, family = binomial("logit"))
summary(mod1)

mod2 <- glm(dados2$y ~ dados2$dif_ano, family = binomial("logit"))
summary(mod2)

OR<-exp(mod2$coef[2]) 
OR  
(OR-1)*100
# matriz de covariancias do modelo
mc<-summary(mod2)$cov.scaled
#Intervalo de confiança para o OR a 95%
exp(c(mod2$coef[2]-qnorm(0.975)*sqrt(mc[2,2]),mod2$coef[2]+qnorm(0.975)*sqrt(mc[2,2]))) 
#Tabela de contingência entre Incumprimento e idade
table(dados2$y, dados2$dif_ano)[1:2,]
#Qui-quadrado de Pearson entre Incumprimento e idade
chisq.test(table(dados2$y, dados2$dif_ano)[1:2,])  
chisq.test(table(dados2$y, dados2$dif_ano)[1:2,])$expected
fisher.test (table(dados2$y, dados2$dif_ano)[1:2,])


mod3 <- glm(dados2$y ~ dados2$sexo, family = binomial("logit"))
summary(mod3)

OR<-exp(mod3$coef[2]) 
OR  
(OR-1)*100
# matriz de covariancias do modelo
mc<-summary(mod3)$cov.scaled
#Intervalo de confian�a para o OR a 95%
exp(c(mod3$coef[2]-qnorm(0.975)*sqrt(mc[2,2]),mod3$coef[2]+qnorm(0.975)*sqrt(mc[2,2]))) 
#Tabela de contingência entre Incumprimento e idade
table(dados2$y, dados2$sexo)[1:2,]
#Qui-quadrado de Pearson entre Incumprimento e idade
chisq.test(table(dados2$y, dados2$sexo)[1:2,])  
chisq.test(table(dados2$y, dados2$sexo)[1:2,])$expected
fisher.test (table(dados2$y, dados2$sexo)[1:2,])



mod4 <- glm(dados2$y ~ dados2$classesocial, family = binomial("logit"))
summary(mod4)

OR<-exp(mod4$coef[2:3]) 
OR  
(OR-1)*100
# matriz de covariancias do modelo
mc<-summary(mod4)$cov.scaled
#Intervalo de confiança para o OR a 95%
exp(c(mod4$coef[2]-qnorm(0.975)*sqrt(mc[2,2]),mod4$coef[2]+qnorm(0.975)*sqrt(mc[2,2]))) 
exp(c(mod4$coef[3]-qnorm(0.975)*sqrt(mc[3,3]),mod4$coef[3]+qnorm(0.975)*sqrt(mc[3,3]))) 
#Tabela de contingência entre Incumprimento e idade
table(dados2$y, dados2$classesocial)[1:2,]
#Qui-quadrado de Pearson entre Incumprimento e idade
chisq.test(table(dados2$y, dados2$classesocial)[1:2,])  
chisq.test(table(dados2$y, dados2$classesocial)[1:2,])$expected
fisher.test (table(dados2$y, dados2$classesocial)[1:2,])



mod5 <- glm(dados2$y ~ dados2$dif_ano + dados2$sexo + dados2$classesocial, family = binomial("logit"))
summary(mod5)
anova(mod1, mod5, test = "Chisq")

