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


#-------------------------- Análise Multivariada do Modelo --------------------------
#Modelo com as variáveis significativas a 25% mais as variáveis importantes ainda que 
#não significativas

mod1<-glm(Incumprimento ~ idade + educação + t_emprego + t_endereço +
            dívida + dívida_cc + outras_dív + idadeCat + educaçãoCat, family=binomial("logit")) 
summary(mod1)

#Retirar as não significativas uma a uma começando na que tem maior valor p (neste caso apenas a educaçãoCat)
mod2<-glm(Incumprimento ~ idade + educação + t_emprego + t_endereço +
            dívida + dívida_cc + outras_dív + idadeCat, family=binomial("logit")) 
summary(mod2)
anova(mod1, mod2, test="Chisq") # teste de razão de verosimilhanças para exclusão da variável educaçãoCat

#Retirada da variavel educação
mod3<-glm(Incumprimento ~ idade + t_emprego + t_endereço +
            dívida + dívida_cc + outras_dív + idadeCat, family=binomial("logit"))
summary(mod3)
anova(mod2, mod3, test="Chisq") #Teste razão de verosimilhanças para exclusão da variável educação

#Adicionar variavel educaçãoCat
mod4<-glm(Incumprimento ~ idade + t_emprego + t_endereço + educaçãoCat +
            dívida + dívida_cc + outras_dív + idadeCat, family=binomial("logit"))
summary(mod4)
anova(mod3, mod4, test="Chisq") #Teste razão de verosimilhanças para testar significância da variável educaçãoCat

#Retirada da variavel idadeCat
mod5<-glm(Incumprimento ~ idade + t_emprego + t_endereço +
            dívida + dívida_cc + outras_dív, family=binomial("logit"))
summary(mod5)
anova(mod4, mod5, test="Chisq") #Teste razão de verosimilhanças para testar significância da variável idadeCat

#Adicionar variavel educaçãoCat
mod6<-glm(Incumprimento ~ idade + t_emprego + t_endereço + educaçãoCat +
            dívida + dívida_cc + outras_dív, family=binomial("logit"))
summary(mod6)
anova(mod5, mod6, test="Chisq") #Teste razão de verosimilhanças para testar significância da variável educaçãoCat

#Adicionar variavel educação
mod7<-glm(Incumprimento ~ idade + t_emprego + t_endereço + educação +
            dívida + dívida_cc + outras_dív, family=binomial("logit"))
summary(mod7)
anova(mod5, mod7, test="Chisq") #Teste razão de verosimilhanças para testar significância da variável educação

#Retirada da variavel outras_dív
mod8<-glm(Incumprimento ~ idade + t_emprego + t_endereço +
            dívida + dívida_cc, family=binomial("logit"))
summary(mod8)
anova(mod4, mod8, test="Chisq") #Teste razão de verosimilhanças para testar significância da variável outras_dív

#Adicionar variavel idadeCat
mod9<-glm(Incumprimento ~ idade + t_emprego + t_endereço + idadeCat +
            dívida + dívida_cc, family=binomial("logit"))
summary(mod9)
anova(mod8, mod9, test="Chisq") #Teste razão de verosimilhanças para testar significância da variável idadeCat

#Adicionar variavel educaçãoCat
mod10<-glm(Incumprimento ~ idade + t_emprego + t_endereço + educaçãoCat +
            dívida + dívida_cc, family=binomial("logit"))
summary(mod10)
anova(mod8, mod10, test="Chisq") #Teste razão de verosimilhanças para testar significância da variável educaçãoCat

#Adicionar variavel educação
mod11<-glm(Incumprimento ~ idade + t_emprego + t_endereço + educação +
            dívida + dívida_cc, family=binomial("logit"))
summary(mod11)
anova(mod8, mod11, test="Chisq") #Teste razão de verosimilhanças para testar significância da variável educação

#Retirada da variavel idade
mod12<-glm(Incumprimento ~ t_emprego + t_endereço +
            dívida + dívida_cc, family=binomial("logit"))
summary(mod12)
anova(mod8, mod12, test="Chisq") #Teste razão de verosimilhanças para testar significância da variável idade

#Adicionar  variavel outras_dív
mod13<-glm(Incumprimento ~ t_emprego + t_endereço + outras_dív +
             dívida + dívida_cc, family=binomial("logit"))
summary(mod13)
anova(mod12, mod13, test="Chisq") #Teste razão de verosimilhanças para testar significância da variável outras_dív

#Adicionar variavel idadeCat
mod14<-glm(Incumprimento ~ t_emprego + t_endereço + idadeCat +
             dívida + dívida_cc, family=binomial("logit"))
summary(mod14)
anova(mod12, mod14, test="Chisq") #Teste razão de verosimilhanças para testar significância da variável idadeCat

#Adicionar variavel educaçãoCat
mod15<-glm(Incumprimento ~ t_emprego + t_endereço + educaçãoCat +
             dívida + dívida_cc, family=binomial("logit"))
summary(mod15)
anova(mod12, mod15, test="Chisq") #Teste razão de verosimilhanças para testar significância da variável educaçãoCat

#Adicionar variavel educação
mod16<-glm(Incumprimento ~ t_emprego + t_endereço + educação +
             dívida + dívida_cc, family=binomial("logit"))
summary(mod16)
anova(mod12, mod16, test="Chisq") #Teste razão de verosimilhanças para testar significância da variável educação

#Modelo com efeitos principais significativos a 10%
modp<-glm(Incumprimento ~ t_emprego + t_endereço +
            dívida + dívida_cc, family=binomial("logit"))
summary(modp)
